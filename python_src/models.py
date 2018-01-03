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
    split_type = 'person'
    cvdata = CrossValidationData(data, (0.9, 0.1), split_type)

    performance_metric = 'r2'
    models = [
        RandomForest(ntrees_domain=[10, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000], metric=performance_metric),
        RandomForest(ntrees_domain=[10, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000], feature_selection=mutual_info_regression, metric=performance_metric),
        SVM(kernel='rbf', C_domain=[2**-5, 2**-3, 2**-1, 1.0, 2**1, 2**3, 2**5, 2**7, 2**9, 2**11, 2**13, 2**15], gamma_domain=['auto'], metric=performance_metric),
        LogisticRegression(C_domain=[2**-5, 2**-3, 2**-1, 1.0, 2**1, 2**3, 2**5, 2**7, 2**9, 2**11, 2**13, 2**15], metric=performance_metric),
        MLP(alpha_domain=[0.1, 0.001, 0.0001, 0.00001], metric=performance_metric)
    ]

    results = []
    for label in label_cols:
        print()
        print('Training model on label', label)
        for model in models:
            result = model.gridsearch(cvdata, data_cols, label)
            results.append(model.evaluate(cvdata, result, data_cols, label))
    df = pd.DataFrame(results)
    df.to_csv(split_type + ' results.csv')
    print(df)
    print("-- Run time : ", time.time() - start_time, " seconds. --")

def normalize(data, data_cols):
    norm = (data - data.mean())/data.std()
    data[data_cols] = norm[data_cols]
    return data


def calculate_performance(truth, prediction):
    return {
        'mean_squared_error': mean_squared_error(truth, prediction),
        'explained_variance': explained_variance_score(truth, prediction),
        'r2': r2_score(truth, prediction)
    }


class CrossValidationData():

    def __init__(self, data, split, type):
        self.type = type
        self.split = split
        self.data = data
        if type == 'text':
            s = data.index.unique().levels[1].values  # use [1] for split on text
            n_texts = len(s)
            perm = np.random.permutation(s)
            train_texts = perm[:int(n_texts * split[0])]
            eval_texts = perm[int(n_texts * split[0]):]
            print('Chosen split on text ids:', train_texts, eval_texts)
            self.train_data = data.loc[(slice(None), train_texts), :]
            self.eval_data = data.loc[(slice(None), eval_texts), :]
        elif type == 'person':
            s = data.index.unique().levels[0].values  # use [1] for split on text
            n_persons = len(s)
            perm = np.random.permutation(s)
            train_persons = perm[:int(n_persons * split[0])]
            eval_persons = perm[int(n_persons * split[0]):]
            print('Chosen split on person ids:', train_persons, eval_persons)
            self.train_data = data.loc[train_persons, :]
            self.eval_data = data.loc[eval_persons, :]
        print(self.train_data.index)
        self.index_set = s

    def __iter__(self):
        self.current = 0
        return self

    def __next__(self):
        if self.current >= len(self.index_set):
            raise StopIteration
        else:
            if self.type == 'person':
                person = self.index_set[self.current]
                train_slice = ([p for p in self.index_set if p != person], slice(None))
                test_slice = (person, slice(None))
            elif self.type == 'text':
                text = self.index_set[self.current]
                print(text)
                train_slice = (slice(None), [t for t in self.index_set if t != text])
                test_slice = (slice(None), text)
            else:
                raise StopIteration
            train_data = self.train_data.loc[train_slice, :]
            dev_data = self.train_data.loc[test_slice, :]
            self.current += 1
            if train_data.shape[0] == 0:
                return self.__next__()
            if dev_data.shape[0] == 0:
                return self.__next__()
            return train_data, dev_data


class Model(metaclass=ABCMeta):
    
    def gridsearch(self, cvdata, data_cols, label):
        return None
    
    def evaluate(self, cvdata, gridsearch_results, data_cols, label):
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
    
    def gridsearch(self, cvdata, data_cols, label):
        print('RF')
        best_performance = -1000000
        best_ntrees = None
        if self.feature_selection is not None:
            selector = SelectKBest(self.feature_selection, k=int(len(data_cols)/2)).fit(cvdata.train_data[data_cols], cvdata.train_data[label])
        else:
            selector = None
        n_iter = len(cvdata.index_set)
        for ntrees in self.ntrees_domain:
            print("Calculating performance for parameter n-trees =", ntrees)
            performance = 0
            for train_data, dev_data in cvdata:
                train_labels = train_data[label].values
                dev_labels = dev_data[label].values
                if selector is not None:
                    train_data = selector.transform(train_data[data_cols])
                    dev_data = selector.transform(dev_data[data_cols])
                else:
                    train_data = train_data[data_cols].values
                    dev_data = dev_data[data_cols].values
                rf = RandomForestRegressor(n_estimators=ntrees, n_jobs=self.njobs)
                rf.fit(train_data, train_labels)
                prediction = rf.predict(dev_data)
                performance += calculate_performance(dev_labels, prediction)[self.metric]
            if performance > best_performance:
                best_performance = performance
                best_ntrees = ntrees
        return best_ntrees, selector, best_performance/n_iter
    
    def evaluate(self, cvdata, gridsearch_results, data_cols, label):
        summary = {
            "model": "RF Regressor",
            "label": label,
            "n trees": gridsearch_results[0],
            "feature selection": gridsearch_results[1] is not None
        }
        train_labels = cvdata.train_data[label]
        eval_labels = cvdata.eval_data[label]
        if gridsearch_results[1] is not None:
            selector = gridsearch_results[1]
            train_data = selector.transform(cvdata.train_data[data_cols])
            eval_data = selector.transform(cvdata.eval_data[data_cols])
        else:
            train_data = cvdata.train_data[data_cols].values
            eval_data = cvdata.eval_data[data_cols].values
        rf = RandomForestRegressor(n_estimators=gridsearch_results[0], n_jobs=self.njobs)
        rf.fit(train_data, train_labels.values)
        prediction = rf.predict(eval_data)
        summary.update(calculate_performance(eval_labels, prediction))
        return summary


class SVM(Model):
    # http://scikit-learn.org/stable/modules/generated/sklearn.svm.SVR.html#sklearn.svm.SVR
    
    def __init__(self, kernel='rbf', C_domain=[1.0], gamma_domain=['auto'], metric='r2', feature_selection=None):
        self.kernel = kernel
        self.C_domain = C_domain
        self.gamma_domain = gamma_domain
        self.metric = metric
        self.feature_selection = feature_selection
    
    def gridsearch(self, cvdata, data_cols, label):
        print('SVM')
        best_performance = -1000000
        best_C = None
        best_gamma = None
        n_ids = len(cvdata.index_set)

        for C in self.C_domain:
            for gamma in self.gamma_domain:
                print("Calculating performance for C=",C, " and gamma=",gamma)
                performance = 0
                for train_data, dev_data in cvdata:
                    svm = SVR(kernel=self.kernel, gamma=gamma, C=C)
                    svm.fit(train_data[data_cols].values, train_data[label].values)
                    prediction = svm.predict(dev_data[data_cols].values)
                    performance += calculate_performance(dev_data[label], prediction)[self.metric]
                if performance > best_performance:
                    best_performance = performance
                    best_C = C
                    best_gamma = gamma
        return best_gamma, best_C, best_performance/n_ids

    def evaluate(self, cvdata, gridsearch_results, data_cols, label):
        summary = {
            "model": "SVM",
            "label": label,
            "best gamma": gridsearch_results[0],
            "best C": gridsearch_results[1],
            "feature selection": self.feature_selection is not None
        }
        svm = SVR(kernel=self.kernel, gamma=gridsearch_results[0], C=gridsearch_results[1])
        svm.fit(cvdata.train_data[data_cols].values, cvdata.train_data[label].values)
        prediction = svm.predict(cvdata.eval_data[data_cols].values)
        summary.update(calculate_performance(cvdata.eval_data[label], prediction))
        return summary


class LogisticRegression(Model):
    # http://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LogisticRegression.html#sklearn.linear_model.LogisticRegression
    
    def __init__(self, C_domain=[1.0], njobs=-1, metric='r2', feature_selection=None):
        self.C_domain = C_domain
        self.njobs = njobs
        self.metric = metric
        self.feature_selection = feature_selection
    
    def gridsearch(self, cvdata, data_cols, label):
        print('LR')
        best_performance = -1000000
        best_C = 0
        n_ids = len(cvdata.index_set)
        for C in self.C_domain:
            print("Calculating performance for C=", C)
            performance = 0
            for train_data, dev_data in cvdata:
                lr = LR(C=C, n_jobs=self.njobs)
                lr.fit(train_data[data_cols].values, train_data[label].values)
                prediction = lr.predict(dev_data[data_cols].values)
                performance += calculate_performance(dev_data[label], prediction)[self.metric]
            if performance > best_performance:
                best_performance = performance
                best_C = C
        return best_C, best_performance/n_ids

    def evaluate(self, cvdata, gridsearch_results, data_cols, label):
        summary = {
            "model": "Logistic Regression",
            "label": label,
            "best C": gridsearch_results[0],
            "feature selection": self.feature_selection is not None
        }
        lr = LR(C=gridsearch_results[0], n_jobs=self.njobs)
        lr.fit(cvdata.train_data[data_cols].values, cvdata.train_data[label].values)
        prediction = lr.predict(cvdata.eval_data[data_cols].values)
        summary.update(calculate_performance(cvdata.eval_data[label], prediction))
        return summary


class MLP(Model):
    # http://scikit-learn.org/stable/modules/generated/sklearn.neural_network.MLPRegressor.html#sklearn.neural_network.MLPRegressor
    
    def __init__(self, alpha_domain=[0.9], metric='r2', feature_selection=None):
        self.alpha_domain = alpha_domain
        self.metric = metric
        self.feature_selection=feature_selection
    
    def gridsearch(self, cvdata, data_cols, label):
        print('MLP')
        best_performance = -1000000
        best_alpha = 0
        n_ids = len(cvdata.index_set)
        for alpha in self.alpha_domain:
            print("Calculating performance for parameter alpha =", alpha)
            performance = 0
            for train_data, dev_data in cvdata:
                mlp = MLPRegressor(alpha=alpha)
                mlp.fit(train_data[data_cols].values, train_data[label].values)
                prediction = mlp.predict(dev_data[data_cols].values)
                performance += calculate_performance(dev_data[label], prediction)[self.metric]
            if performance > best_performance:
                best_performance = performance
                best_alpha = alpha
        return best_alpha, best_performance/n_ids

    def evaluate(self, cvdata, gridsearch_results, data_cols, label):
        summary = {
            "model": "Multi-layer Perceptron",
            "label": label,
            "alpha": gridsearch_results[0],
            "feature selection": self.feature_selection is not None
        }
        mlp = MLPRegressor(alpha=gridsearch_results[0])
        mlp.fit(cvdata.train_data[data_cols].values, cvdata.train_data[label].values)
        prediction = mlp.predict(cvdata.eval_data[data_cols].values)
        summary.update(calculate_performance(cvdata.eval_data[label], prediction))
        return summary



