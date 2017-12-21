import pandas as pd
import os

from preprocessing import preprocess


def read_data(datapath, eye_tracking_folder, label_file):
    print('Reading in label file', datapath + label_file)
    labels = pd.read_excel(datapath + label_file)[['person ID', 'text ID', 'interest', 'complexity', 'comprehension', 'familiarity']]
    dfs = []
    for path, folders, files in os.walk(datapath + eye_tracking_folder):
        for file in files[:2]:  # TODO remove the slice, just to make the test size smaller
            df = pd.read_csv(path + file, delimiter='\t')
            # label all rows in df with the person id
            print(file)
            # Get person id from filename, cast to int, replicate number of rows (df.shape[0]) times.
            df['person ID'] = [int(file[1:3])] * df.shape[0]
            dfs += [df]
    print('Done reading in files')
    return pd.concat(dfs), labels


def __main__():
    print('Starting application')
    datapath = '../data/'
    eye_tracking_folder = 'eye-tracking_data/'
    label_file = 'participant_ratings.xlsx'
    training_data_file = 'trainingData.csv'
    data, labels = read_data(datapath, eye_tracking_folder, label_file)
    print('Unique person IDs : ', data['person ID'].unique())
    print('Starting preprocessing')
    data = preprocess(data, labels)
    print('##### END RESULT #####')
    print(data)



if __name__ == '__main__':
    __main__()