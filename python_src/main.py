import pandas as pd
import os
import sys


from preprocessing import preprocess
from models import run_models


def read_data(datapath, eye_tracking_folder, label_file):
    print('Reading in label file', datapath + label_file)
    labels = pd.read_excel(datapath + label_file, index_col=[0, 1])
    dfs = []
    for path, folders, files in os.walk(datapath + eye_tracking_folder):
        for file in files[:]:  # TODO remove the slice, just to make the test size smaller
            df = pd.read_csv(path + file, delimiter='\t')
            # label all rows in df with the person id
            print(file)
            # Get person id from filename, cast to int, replicate number of rows (df.shape[0]) times.
            df['person ID'] = [int(file[1:3])] * df.shape[0]
            dfs += [df]
    print('Done reading in files')
    data = pd.concat(dfs).rename(columns={'stimulus_id': 'text ID'})
    return data, labels





def __main__():
    args = sys.argv
    if len(args) > 1:
        preprocessing = args[1] == 'False'
    else:
        preprocessing = False
    if preprocessing :
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
        data.to_csv('preprocessed_data.csv')

    run_models()


if __name__ == '__main__':
    __main__()