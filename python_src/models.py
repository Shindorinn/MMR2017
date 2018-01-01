from sklearn.ensemble import RandomForestRegressor
from sklearn.feature_selection import SelectKBest, mutual_info_regression
from sklearn.svm import SVR
from sklearn.linear_model import LogisticRegression as LR
from sklearn.neural_network import MLPRegressor
from sklearn.metrics import r2_score, mean_squared_error, explained_variance_score

import pandas as pd
import numpy as np
from abc import ABCMeta
import time

def run_models():
    start_time = time.time()
    datafile= 'preprocessed_data.csv'
    data = pd.read_csv(datafile, index_col=[0,1]).dropna()

    label_cols = ['interest', 'complexity', 'comprehension', 'familiarity']
    data_cols = [c for c in data.columns if c not in label_cols]
    data[label_cols] = (data[label_cols] * 100).astype(int)
    data = normalize(data, data_cols)

    train_data, eval_data = split(data)

    performance_metric = 'r2'
    models = [
        RandomForest(ntrees_domain=[10, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000], metric=performance_metric),
        #RandomForest(ntrees_domain=[10], feature_selection = mutual_info_regression, metric=performance_metric)#,
        SVM(kernel='rbf', C_domain=[2**-5, 2**-3, 2**-1, 1.0, 2**1, 2**3, 2**5, 2**7, 2**9, 2**11, 2**13, 2**15], gamma_domain=['auto'], metric=performance_metric),
        LogisticRegression(C_domain=[2**-5, 2**-3, 2**-1, 1.0, 2**1, 2**3, 2**5, 2**7, 2**9, 2**11, 2**13, 2**15], metric=performance_metric),
        MLP(alpha_domain=[0.1, 0.001, 0.0001, 0.00001], metric=performance_metric)
    ]
    results = []
    for label in label_cols:
        print()
        print('Training model on label', label)
        for model in models:
            result = model.gridsearch(train_data, data_cols, label)
            results.append(model.evaluate(train_data, eval_data, result, data_cols, label))
    df = pd.DataFrame(results)
    print(df)
    print("-- Run time : ", time.time() - start_time, " seconds. --")

def normalize(data, data_cols):
    norm = (data - data.mean())/data.std()
    data[data_cols] = norm[data_cols]
    return data


def split(data, split=(0.9, 0.1)):
    s = data.index.unique().levels[0].values # use [1] for split on text
    n_persons = len(s)
    perm = np.random.permutation(s)
    train_persons = perm[:int(n_persons*split[0])]
    eval_persons = perm[int(n_persons*split[0]):]
    print('Chosen split on indexes:', train_persons, eval_persons)
    train_data = data.loc[train_persons, :]
    eval_data = data.loc[eval_persons, :]
    return train_data, eval_data


def calculate_performance(truth, prediction):
    return {
        'mean_squared_error': mean_squared_error(truth, prediction),
        'explained_variance': explained_variance_score(truth, prediction),
        'r2': r2_score(truth, prediction)
    }


class Model(metaclass=ABCMeta):
    
    def gridsearch(self, data, data_cols, label):
        return None
    
    def evaluate(self, train_data, eval_data, gridsearch_results, data_cols, label):
        return {
                "model": "Model Name",
                "gridsearch result": gridsearch_results,
                "label": label,
                "evaluation metric": "performance of best gridsearch parameters on eval_data"
               }


class RandomForest(Model):
    # http://scikit-learn.org/stable/modules/generated/sklearn.ensemble.RandomForestRegressor.html
    
    def __init__(self, ntrees_domain=[500], njobs=-1, metric='r2', feature_selection=None):
        self.ntrees_domain = ntrees_domain
        self.feature_selection = feature_selection
        self.njobs = njobs
        self.metric = metric
    
    def gridsearch(self, data, data_cols, label):
        print('RF')
        best_performance = -1000000
        best_ntrees = None
        if self.feature_selection is not None:
            selector = SelectKBest(self.feature_selection, k=int(data.shape[1]/2)).fit(data[data_cols], data[label])
            data = selector.transform(data[data_cols])
        person_ids = data.index.levels[0].unique().values.tolist()
        n_ids = len(person_ids)
        for ntrees in self.ntrees_domain:
            print("Calculating performance for parameter n-trees =", ntrees)
            performance = 0
            for person in person_ids:
                train_data = data.loc[[p for p in person_ids if p != person], :]
                dev_data = data.loc[person,:]
                if dev_data.shape[0] == 0: # Eval data index is still in the df, but does not have any data.
                    n_ids -= 1
                else:
                    rf = RandomForestRegressor(n_estimators=ntrees, n_jobs=self.njobs)
                    rf.fit(train_data[data_cols].values, train_data[label].values)
                    prediction = rf.predict(dev_data[data_cols].values)
                    performance += calculate_performance(dev_data[label], prediction)[self.metric]
            if performance > best_performance:
                best_performance = performance
                best_ntrees = ntrees
        return best_ntrees, best_performance/n_ids
    
    def evaluate(self, train_data, eval_data, gridsearch_results, data_cols, label):
        summary = {
            "model": "RF Regressor",
            "label": label,
            "n trees": gridsearch_results[0],
            "feature selection": self.feature_selection is not None
        }
        rf = RandomForestRegressor(n_estimators=gridsearch_results[0], n_jobs=self.njobs)
        rf.fit(train_data[data_cols].values, train_data[label].values)
        prediction = rf.predict(eval_data[data_cols].values)
        summary.update(calculate_performance(eval_data[label], prediction))
        return summary


class SVM(Model):
    # http://scikit-learn.org/stable/modules/generated/sklearn.svm.SVR.html#sklearn.svm.SVR
    
    def __init__(self, kernel='rbf', C_domain=[1.0], gamma_domain=['auto'], metric='r2', feature_selection=None):
        self.kernel = kernel
        self.C_domain = C_domain
        self.gamma_domain = gamma_domain
        self.metric = metric
        self.feature_selection = feature_selection
    
    def gridsearch(self, data, data_cols, label):
        print('SVM')
        best_performance = -1000000
        best_C = None
        best_gamma = None
        if self.feature_selection is not None:
            selector = SelectKBest(self.feature_selection, k=int(data.shape[1] / 2)).fit(data[data_cols], data[label])
            data = selector.transform(data[data_cols])
        person_ids = data.index.levels[0].unique().values.tolist()
        n_ids = len(person_ids)

        for C in self.C_domain:
            for gamma in self.gamma_domain:
                print("Calculating performance for C=",C, " and gamma=",gamma)
                for person in person_ids:
                    train_data = data.loc[[p for p in person_ids if p != person], :]
                    dev_data = data.loc[person, :]
                    if dev_data.shape[0] == 0:  # Eval data index is still in the df, but does not have any data.
                        n_ids -= 1
                    else:
                        svm = SVR(kernel=self.kernel, gamma=gamma, C=C)
                        svm.fit(train_data[data_cols].values, train_data[label].values)
                        prediction = svm.predict(dev_data[data_cols].values)
                        performance = calculate_performance(dev_data[label], prediction)[self.metric]
                        if performance > best_performance:
                            best_performance = performance
                            best_C = C
                            best_gamma = gamma
        return best_gamma, best_C, best_performance/n_ids

    def evaluate(self, train_data, eval_data, gridsearch_results, data_cols, label):
        summary = {
            "model": "SVM",
            "label": label,
            "best gamma": gridsearch_results[0],
            "best C": gridsearch_results[1],
            "feature selection": self.feature_selection is not None
        }
        svm = SVR(kernel=self.kernel, gamma=gridsearch_results[0], C=gridsearch_results[1])
        svm.fit(train_data[data_cols].values, train_data[label].values)
        prediction = svm.predict(eval_data[data_cols].values)
        summary.update(calculate_performance(eval_data[label], prediction))
        return summary


class LogisticRegression(Model):
    # http://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LogisticRegression.html#sklearn.linear_model.LogisticRegression
    
    def __init__(self, C_domain=[1.0], njobs=-1, metric='r2', feature_selection=None):
        self.C_domain = C_domain
        self.njobs = njobs
        self.metric = metric
        self.feature_selection = feature_selection
    
    def gridsearch(self, data, data_cols, label):
        print('LR')
        best_performance = -1000000
        best_C = 0
        if self.feature_selection is not None:
            selector = SelectKBest(self.feature_selection, k=int(data.shape[1] / 2)).fit(data[data_cols], data[label])
            data = selector.transform(data[data_cols])
        person_ids = data.index.levels[0].unique().values.tolist()
        n_ids = len(person_ids)
        for C in self.C_domain:
            print("Calculating performance for C=", C)
            for person in person_ids:
                train_data = data.loc[[p for p in person_ids if p != person], :]
                dev_data = data.loc[person, :]
                if dev_data.shape[0] == 0:  # Eval data index is still in the df, but does not have any data.
                    n_ids -= 1
                else:
                    lr = LR(C=C, n_jobs=self.njobs)
                    lr.fit(train_data[data_cols].values, train_data[label].values)
                    prediction = lr.predict(dev_data[data_cols].values)
                    performance = calculate_performance(dev_data[label], prediction)[self.metric]
                    if performance > best_performance:
                        best_performance = performance
                        best_C = C
        return best_C, best_performance/n_ids

    def evaluate(self, train_data, eval_data, gridsearch_results, data_cols, label):
        summary = {
            "model": "Logistic Regression",
            "label": label,
            "best C": gridsearch_results[0],
            "feature selection": self.feature_selection is not None
        }
        lr = LR(C=gridsearch_results[0], n_jobs=self.njobs)
        lr.fit(train_data[data_cols].values, train_data[label].values)
        prediction = lr.predict(eval_data[data_cols].values)
        summary.update(calculate_performance(eval_data[label], prediction))
        return summary


class MLP(Model):
    # http://scikit-learn.org/stable/modules/generated/sklearn.neural_network.MLPRegressor.html#sklearn.neural_network.MLPRegressor
    
    def __init__(self, alpha_domain=[0.9], metric='r2', feature_selection=None):
        self.alpha_domain = alpha_domain
        self.metric = metric
        self.feature_selection=feature_selection
    
    def gridsearch(self, data, data_cols, label):
        print('MLP')
        best_performance = -1000000
        best_alpha = 0
        if self.feature_selection is not None:
            selector = SelectKBest(self.feature_selection, k=int(data.shape[1] / 2)).fit(data[data_cols], data[label])
            data = selector.transform(data[data_cols])
        person_ids = data.index.levels[0].unique().values.tolist()
        n_ids = len(person_ids)
        for alpha in self.alpha_domain:
            print("Calculating performance for parameter alpha =", alpha)
            for person in person_ids:
                train_data = data.loc[[p for p in person_ids if p != person], :]
                dev_data = data.loc[person, :]
                if dev_data.shape[0] == 0:  # Eval data index is still in the df, but does not have any data.
                    n_ids -= 1
                else:
                    mlp = MLPRegressor(alpha=alpha)
                    mlp.fit(train_data[data_cols].values, train_data[label].values)
                    prediction = mlp.predict(dev_data[data_cols].values)
                    performance = calculate_performance(dev_data[label], prediction)[self.metric]
                    if performance > best_performance:
                        best_performance = performance
                        best_alpha = alpha
        return best_alpha, best_performance

    def evaluate(self, train_data, eval_data, gridsearch_results, data_cols, label):
        summary = {
            "model": "Multi-layer Perceptron",
            "label": label,
            "alpha": gridsearch_results[0],
            "feature selection": self.feature_selection is not None
        }
        mlp = MLPRegressor(alpha=gridsearch_results[0])
        mlp.fit(train_data[data_cols].values, train_data[label].values)
        prediction = mlp.predict(eval_data[data_cols].values)
        summary.update(calculate_performance(eval_data[label], prediction))
        return summary



