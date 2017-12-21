import pandas as pd
import numpy as np
import scipy.stats as stats
from scipy.spatial.distance import euclidean as dist


def preprocess(data, labels, dispersion=100):
    '''
    :param data: DataFrame containing csv data
    :param labels: DataFrame participants ratings
    :param dispersion: float max dispersion, default=100
    :return:
    '''
    # Remove irrelevant data
    data = clean_data(data)
    transformedData = transformData(data, dispersion)
    metrics = computeMetrics(transformedData, labels, dispersion)
    return pd.merge(metrics, labels, how='outer', left_on=['stimulus_id', 'person ID'], right_on=['text ID', 'person ID'])

def clean_data(data):
    data = data.dropna(subset=['stimulus_id', 'person ID'])
    return data[data['stimulus_name'].str.contains('stimulus')]

def computeMetrics(transformedData, ratings, dispersion):
    metrics = []
    for index, row in ratings.iterrows():
        person_text_data = transformedData[(transformedData['person ID'] == row['person ID']) & (transformedData['text ID'] == row['text ID'])]
        df = computeFixationMetrics(person_text_data[['person ID', 'text ID'] + [col for col in person_text_data.columns if 'fixation' in col]], dispersion)
        df.merge(computeSaccadeMetrics(person_text_data[['person ID', 'text ID'] + [col for col in person_text_data.columns if 'saccade' in col]]), how='outer', on=['person ID', 'text ID'])
        df.merge(computeBlinkMetrics(person_text_data[['person ID', 'text ID'] + [col for col in person_text_data.columns if 'blink' in col]]), how='outer', on=['person ID', 'text ID'])
        n_rows = df.shape[0]
        df['person ID'] = [row['person ID']]*n_rows
        df['text ID'] = [row['text ID']]*n_rows
        metrics.append(df)
    return pd.concat(metrics)

def isProgressiveFixation(fixation1, fixation2, dispersion):
    '''
    :param: fixation1 = {x, y}
    :param: fixation2 = {x, y}
    :param: dispersion = float
    :returns: bool
    '''
    # A fixation is classified as progressive if it either has a larger x-coordinate and roughly the same
    # y-coordinate(on the same line) or when it has a larger y-coordinate(on a lower line)
    if (fixation2['fixation_x'] >= fixation1['fixation_x'] and fixation2['fixation_y'] >= fixation1['fixation_y'] - (dispersion / 2)) or\
            (fixation2['fixation_y'] >= fixation1['fixation_y'] + (dispersion / 2)):
        return True
    else:
        return False


def computeFixationMetrics(fixations, dispersion):
    '''
    Description: this function computes the aggregate metrics of the fixation events
    :param fixations: DataFrame of the aggregate metrics of the blink events
    :param dispersion: float
    :return:
    '''
    fixations = fixations.reset_index(drop=True)
    print(fixations.columns)
    # Check special cases so that we will not get any errors
    if fixations.shape[0] == 1:
        '''
        return(list(Fixations = 1,
                    Fixations_Progressive_Fraction = 1,
                    Fixations_Regressive_Fraction = 0,
                    Fixation_Progressive_Duration_Mean = mean(fixations['$dur']),
                    Fixation_Regressive_Duration_Mean = 0,
                    Fixation_Progressive_Duration_SD = sd(fixations['$dur']),
                    Fixation_Regressive_Duration_SD = 0))
        '''
        return pd.DataFrame(
            {
                'person ID': fixations['person ID'][0],
                'text ID': fixations['text ID'][0],
                'Fixations': 1,
                'Fixations_Progressive_Fraction'            : 1,
                'Fixations_Regressive_Fraction'             : 0,
                'Fixation_Progressive_Duration_Mean'        : np.mean(fixations['fixation_duration']),
                'Fixation_Regressive_Duration_Mean'         : 0,
                'Fixation_Progressive_Duration_SD'          : np.std(fixations['fixation_duration']),
                'Fixation_Regressive_Duration_SD'           : 0,
                'Fixation_Progressive_Duration_Skewness'    : stats.skew(fixations['fixation_duration']),
                'Fixation_Regressive_Duration_Skewness'     : 0,
                'Fixation_Progressive_Duration_Kurtosis'    : stats.kurtosis(fixations['fixation_duration']),
                'Fixation_Regressive_Duration_Kurtosis'     : 0,
                'Fixation_Progressive_Duration_Variation'   : stats.variation(fixations['fixation_duration']),
                'Fixation_Regressive_Duration_Variation'    : 0
            }
        )

    # The first fixation is always a progressive fixation
    progressiveFixations = {
        'start': [fixations.iloc[0]['fixation_start']],
        'end': [fixations.iloc[0]['fixation_end']],
        'dur': [fixations.iloc[0]['fixation_duration']],
        'x': [fixations.iloc[0]['fixation_x']],
        'y': [fixations.iloc[0]['fixation_y']]
    }
    regressiveFixations = {
        'start': [],
        'end': [],
        'dur': [],
        'x': [],
        'y': []
    }

    # Loop over all but the first fixation event to determine which fixations are progressive and which fixations
    # are regressive
    for index, row in fixations[1:].iterrows():
        # Check if the fixation is progressive
        if isProgressiveFixation(fixations.iloc[index - 1], row, dispersion):
            progressiveFixations['start'].append(row['fixation_start'])
            progressiveFixations['end'].append(row['fixation_end'])
            progressiveFixations['dur'].append(row['fixation_duration'])
            progressiveFixations['x'].append(row['fixation_x'])
            progressiveFixations['y'].append(row['fixation_y'])
        # The fixation is regressive
        else:
            regressiveFixations['start'].append(row['fixation_start'])
            regressiveFixations['end'].append(row['fixation_end'])
            regressiveFixations['dur'].append(row['fixation_duration'])
            regressiveFixations['x'].append(row['fixation_x'])
            regressiveFixations['y'].append(row['fixation_y'])

    progressiveFixations['dur'] = np.asarray(progressiveFixations['dur'])
    regressiveFixations['dur'] = np.asarray(regressiveFixations['dur'])
    return pd.DataFrame(
        {
            'person ID': fixations['person ID'][0],
            'text ID': fixations['text ID'][0],
            'Fixations': fixations.shape[0],
            'Fixations_Progressive_Fraction'            : [len(progressiveFixations['dur'])],
            'Fixations_Regressive_Fraction'             : [len(regressiveFixations['dur'])],
            'Fixation_Progressive_Duration_Mean'        : [np.mean(progressiveFixations['dur'])],
            'Fixation_Regressive_Duration_Mean'         : [np.mean(regressiveFixations['dur'])],
            'Fixation_Progressive_Duration_SD'          : [np.std(progressiveFixations['dur'])],
            'Fixation_Regressive_Duration_SD'           : [np.std(regressiveFixations['dur'])],
            'Fixation_Progressive_Duration_Skewness'    : [stats.skew(progressiveFixations['dur'])],
            'Fixation_Regressive_Duration_Skewness'     : [stats.skew(regressiveFixations['dur'])],
            'Fixation_Progressive_Duration_Kurtosis'    : [stats.kurtosis(progressiveFixations['dur'])],
            'Fixation_Regressive_Duration_Kurtosis'     : [stats.kurtosis(regressiveFixations['dur'])],
            'Fixation_Progressive_Duration_Variation'   : [stats.variation(progressiveFixations['dur'])],
            'Fixation_Regressive_Duration_Variation'    : [stats.variation(regressiveFixations['dur'])]
        }
    )


def computeSaccadeMetrics(saccades):
    '''
    Description: this function computes the aggregate metrics of the saccade events
    :param saccades: DataFrame containing the aggregate metrics of the saccade events
    :return: DataFrame
    '''
    print(saccades.columns)
    saccades = saccades.reset_index(drop=True)
    return pd.DataFrame({
        'person ID': saccades['person ID'][0],
        'text ID': saccades['text ID'][0],
        'Saccades'                    : [saccades.shape[0]],
        'Saccade_Duration_Mean'       : [np.mean(saccades['saccade_duration'])],
        'Saccade_Amplitude_Mean'      : [np.mean(saccades['saccade_amplitude'])],
        'Saccade_Velocity_Mean'       : [np.mean(saccades['saccade_velocity'])],
        'Saccade_Duration_SD'         : [np.std(saccades['saccade_duration'])],
        'Saccade_Amplitude_SD'        : [np.std(saccades['saccade_amplitude'])],
        'Saccade_Velocity_SD'         : [np.std(saccades['saccade_velocity'])],
        'Saccade_Duration_Skewness'   : [stats.skew(saccades['saccade_duration'])],
        'Saccade_Amplitude_Skewness'  : [stats.skew(saccades['saccade_amplitude'])],
        'Saccade_Velocity_Skewness'   : [stats.skew(saccades['saccade_velocity'])],
        'Saccade_Duration_Kurtosis'   : [stats.kurtosis(saccades['saccade_duration'])],
        'Saccade_Amplitude_Kurtosis'  : [stats.kurtosis(saccades['saccade_amplitude'])],
        'Saccade_Velocity_Kurtosis'   : [stats.kurtosis(saccades['saccade_velocity'])],
        'Saccade_Duration_Variation'  : [stats.variation(saccades['saccade_duration'])],
        'Saccade_Amplitude_Variation' : [stats.variation(saccades['saccade_amplitude'])],
        'Saccade_Velocity_Variation'  : [stats.variation(saccades['saccade_velocity'])]
    })


def computeBlinkMetrics(blinks):
    '''
    Description: this function computes the aggregate metrics of the blink events
    :param blinks: DataFrame
    :return: DataFrame: this function returns a DataFrame of aggregate metrics of the blink events
    '''
    print(blinks.columns)
    blinks = blinks.reset_index(drop=True)
    # Make sure that we do not calculate NA for the standard deviation if we have only 1 blink or less
    if blinks.shape[0] <= 1:
        return pd.DataFrame({
        'person ID': blinks['person ID'][0],
        'text ID': blinks['text ID'][0],
        'Blinks'                    : [blinks.shape[0]],
        'Blink_Duration_Mean'       : [np.mean(blinks['blink_duration'])],
        'Blink_Duration_SD'         : [0],
        'Blink_Duration_Skewness'   : [0],
        'Blink_Duration_Kurtosis'   : [0],
        'Blink_Duration_Variation'  : [0]
    })

    return pd.DataFrame({
        'person ID': blinks['person ID'][0],
        'text ID': blinks['text ID'][0],
        'Blinks'                    : [blinks.shape[0]],
        'Blink_Duration_Mean'       : [np.mean(blinks['blink_duration'])],
        'Blink_Duration_SD'         : [np.std(blinks['blink_duration'])],
        'Blink_Duration_Skewness'   : [stats.skew(blinks['blink_duration'])],
        'Blink_Duration_Kurtosis'   : [stats.kurtosis(blinks['blink_duration'])],
        'Blink_Duration_Variation'  : [stats.variation(blinks['blink_duration'])]
    })


def filterData(data, filterNegativeEyePositions=True):
    '''
    Description : Filter all samples where data is missing and all data samples where the position of the left eye is
    different from the position of the right eye and all negative eye positions
    :param data: DataFrame containing raw Data
    :param filterNegativeEyePositions: bool should the negative eye positions be filtered?
    :return: DataFrame with filtered Data
    '''
    cols = ['Time', 'L POR X [px]', 'L POR Y [px]', 'R POR X [px]', 'R POR Y [px]']
    # Filter all samples where data is missing
    data = data.dropna(subset=cols, how='any', axis=0)

    # Filter out all data samples where the position of the left eye is different from the position of the right eye.
    # It does not look like this is ever the case, but to be sure we filter out these samples anyway
    data = data[data['L POR X [px]'] == data['R POR X [px]']][data['L POR Y [px]'] == data['R POR Y [px]']]
    if filterNegativeEyePositions:
        # Filter all negative eye positions
        data = data[
           (data['L POR X [px]'] >= 0) &
           (data['L POR Y [px]'] >= 0) &
           (data['R POR X [px]'] >= 0) &
           (data['R POR Y [px]'] >= 0)
        ]
    return data


def transformData(data, dispersion):
    print('Transforming Data')
    fixations = fixationData(data, dispersion, 5)
    saccades = saccadeData(fixations)
    blinks = blinkData(data)
    # fixation = fixationData(data[data['person ID'] == person], dispersion, 5)
    # personData['fixations'].append(fixation)
    # personData['saccades'].append(saccadeData(fixation))
    # personData['blinks'].append(blinkData(data[data['person ID'] == person]))

    df = pd.merge(fixations, saccades, how='outer', on=['person ID', 'text ID'], suffixes=['_fixation', '_saccades'])
    return pd.merge(df, blinks, how='outer', on=['person ID', 'text ID'], suffixes=['_df', '_blink'])


def fixationData(data, dispersion, minSamples):
    print('Processing Fixations')
    # Compute all fixation events
    data = filterData(data)

    fixations = []
    for index, row in data.drop_duplicates(subset=['person ID', 'stimulus_id']).iterrows():
        person = row['person ID']
        text = row['stimulus_id']
        dataslice = data[(data['person ID'] == person) & (data['stimulus_id'] == text)]
        idtResults = idt(
                dataslice['Time'],
                dataslice['L POR X [px]'],
                dataslice['L POR Y [px]'],
                dispersion,
                minSamples
            )
        n_fixations = idtResults.shape[0]
        idtResults['person ID'] = [person]*n_fixations
        idtResults['text ID'] = [text]*n_fixations

        fixations.append(
            idtResults
        )
    # Remove all fixations of negative coordinates
    fixations = pd.concat(fixations)
    return fixations[fixations['fixation_x'] > 0][fixations['fixation_y'] > 0]


def saccadeData(allFixations):
    '''
    Calculates all the saccade data from the fixations
    :param fixations: DataFrame containing all the fixations
    :return: DataFrame containing all the saccades
    '''
    print('Processing SaccadeData')
    # Stores all saccade events
    saccades = {'saccade_start': [],
                   'saccade_end': [],
                   'saccade_duration': [],
                   'saccade_start_X': [],
                   'saccade_start_Y': [],
                   'saccade_end_X': [],
                   'saccade_end_Y': [],
                   'saccade_amplitude': [],
                   'saccade_velocity': [],
                   'text ID': [],
                   'person ID': []}
    # Create a saccade event for every pair of consecutive fixation events
    for fix_index, row in allFixations[['text ID', 'person ID']].drop_duplicates().iterrows():
        fixations = allFixations[(allFixations['text ID'] == row['text ID']) & (allFixations['person ID'] == row['person ID'])]
        fixations = fixations.reset_index(drop=True)

        for index in range(fixations.shape[0]-1):
            # Add a saccade event to the list of all saccade events
            saccades['saccade_start'].append(fixations.iloc[index]['fixation_end'] + 1)
            saccades['saccade_end'].append(fixations.iloc[index + 1]['fixation_start'] - 1)
            saccades['saccade_duration'].append((fixations.iloc[index + 1]['fixation_start'] - 1) - (fixations.iloc[index]['fixation_end'] + 1))
            saccades['saccade_start_X'].append(fixations.iloc[index]['fixation_x'])
            saccades['saccade_start_Y'].append(fixations.iloc[index]['fixation_y'])
            saccades['saccade_end_X'].append(fixations.iloc[index + 1]['fixation_x'])
            saccades['saccade_end_Y'].append(fixations.iloc[index + 1]['fixation_y'])
            saccades['saccade_amplitude'].append(dist((fixations.iloc[index]['fixation_x'], fixations.iloc[index]['fixation_y']),
                                                 (fixations.iloc[index + 1]['fixation_x'], fixations.iloc[index + 1]['fixation_y'])))
            saccades['saccade_velocity'].append(
                (
                    dist(
                        (fixations.iloc[index]['fixation_x'], fixations.iloc[index]['fixation_y']),
                        (fixations.iloc[index + 1]['fixation_x'], fixations.iloc[index + 1]['fixation_y'])
                     )
                ) / ((fixations.iloc[index + 1]['fixation_start'] - 1) - (fixations.iloc[index]['fixation_end'] + 1)))
            saccades['text ID'].append(row['text ID'])
            saccades['person ID'].append(row['person ID'])

    return pd.DataFrame(saccades)
'''
# Stores all saccade events
    saccadeData = {'Start': [],
                   'End': [],
                   'Duration': [],
                   'Start_X': [],
                   'Start_Y': [],
                   'End_X': [],
                   'End_Y': [],
                   'Amplitude': [],
                   'Velocity': []}
    # Create a saccade event for every pair of consecutive fixation events

    fixations = fixations.reset_index(drop=True)

    for index in range(fixations.shape[0]-1):
        # Add a saccade event to the list of all saccade events
        saccadeData['Start'].append(fixations.iloc[index]['end'] + 1)
        saccadeData['End'].append(fixations.iloc[index + 1]['start'] - 1)
        saccadeData['Duration'].append((fixations.iloc[index + 1]['start'] - 1) - (fixations.iloc[index]['end'] + 1))
        saccadeData['Start_X'].append(fixations.iloc[index]['x'])
        saccadeData['Start_Y'].append(fixations.iloc[index]['y'])
        saccadeData['End_X'].append(fixations.iloc[index + 1]['x'])
        saccadeData['End_Y'].append(fixations.iloc[index + 1]['y'])
        saccadeData['Amplitude'].append(dist((fixations.iloc[index]['x'], fixations.iloc[index]['y']),
                                             (fixations.iloc[index + 1]['x'], fixations.iloc[index + 1]['y'])))
        saccadeData['Velocity'].append(
            (
                dist(
                    (fixations.iloc[index]['x'], fixations.iloc[index]['y']),
                    (fixations.iloc[index + 1]['x'], fixations.iloc[index + 1]['y'])
                 )
            ) / ((fixations.iloc[index + 1]['start'] - 1) - (fixations.iloc[index]['end'] + 1)))

    return pd.DataFrame(saccadeData)
'''


def blinkData(data):
    '''
    Retrieves blink event data from raw Data
    :param data:  DataFrame containing raw Data
    :return: DataFrame containing blink event Data
    '''
    print('Processing Blink Data')
    data = filterData(data, filterNegativeEyePositions=False)
    # Stores all blink events
    blinkEvents = {'blink_start': [], 'blink_end': [], 'blink_duration': [], 'text ID': [], 'person ID': []}

    # The first sample in a sequence of consecutive blink samples
    blinkStartSample = None

    for slice_index, slice_row in data[['stimulus_id', 'person ID']].drop_duplicates().iterrows():
        # Loop over all samples to determine sequences of blink events
        for index, row in data[(data['stimulus_id'] == slice_row['stimulus_id']) & (data['person ID'] == slice_row['person ID'])].iterrows():
            # We should have registered a negative position for at least one of the eyes
            if row['L POR X [px]'] <= 0 or row['L POR Y [px]'] <= 0 or row['R POR X [px]'] <= 0 or row['R POR Y [px]'] <= 0:
                # Check whether or not this is the first sample in the sequence of blinks currently under investigation
                if blinkStartSample is None: #or blinkStartSample.isna()
                    # Register that this is the first sample in the sequence of blinks currently under investigation
                    blinkStartSample = row
            # We have not registered a negative position for at least one of the eyes
            else:
                # Check whether or not a sequence of blink events was currently under construction
                if blinkStartSample is not None:
                    # Add the blink event to the list of blink events
                    blinkEvents['blink_start'].append(blinkStartSample['Time'])
                    blinkEvents['blink_end'].append(row['Time'] - 1)
                    blinkEvents['blink_duration'].append((row['Time'] - 1) - blinkStartSample['Time'])
                    blinkEvents['text ID'].append(slice_row['stimulus_id'])
                    blinkEvents['person ID'].append(slice_row['person ID'])
                    # Reset the first sample in the sequence of consecutive blink samples
                    blinkStartSample = None
        #blinkEvents = pd.DataFrame(blinkEvents)
    # Remove all blinks that have a duration of less than 70 milliseconds
    blinkEvents = pd.DataFrame(blinkEvents)
    blinkEvents = blinkEvents[(blinkEvents['blink_duration'] >= 70000)]

    return blinkEvents
'''
# Loop over all samples to determine sequences of blink events
    for index, row in data.iterrows():
        # We should have registered a negative position for at least one of the eyes
        if row['L POR X [px]'] <= 0 or row['L POR Y [px]'] <= 0 or row['R POR X [px]'] <= 0 or row['R POR Y [px]'] <= 0:
            # Check whether or not this is the first sample in the sequence of blinks currently under investigation
            if blinkStartSample is None: #or blinkStartSample.isna()
                # Register that this is the first sample in the sequence of blinks currently under investigation
                blinkStartSample = row
        # We have not registered a negative position for at least one of the eyes
        else:
            # Check whether or not a sequence of blink events was currently under construction
            if blinkStartSample is not None:
                # Add the blink event to the list of blink events
                blinkEvents['Start'].append(blinkStartSample['Time'])
                blinkEvents['End'].append(row['Time'] - 1)
                blinkEvents['Duration'].append((row['Time'] - 1) - blinkStartSample['Time'])
                # Reset the first sample in the sequence of consecutive blink samples
                blinkStartSample = None
    blinkEvents = pd.DataFrame(blinkEvents)

    # Remove all blinks that have a duration of less than 70 milliseconds
    blinkEvents = blinkEvents[(blinkEvents['Duration'] >= 70000)]

    return blinkEvents

'''

def idt(t, x, y, dispersion, duration):
    '''
    Implementation of a dispersion-based algorithm (I-DT) proposed by Salvucci & Goldberg(2000)
    :param t: array of all time stamps
    :param x: array of all x coordinates
    :param y: array of all y coordinates
    :param dispersion: float maximum dispersion
    :param duration: float maximum duration
    :return:
    '''

    t = t.reset_index(drop=True)
    x = x.reset_index(drop=True)
    y = y.reset_index(drop=True)

    # init variables
    fix_start = []
    fix_end = []
    fix_x = []
    fix_y = []

    start = 0  # window start position

    while start < (x.shape[0] - duration):
        # while: we move window by 1, if D > threshold
        end = start + duration  # window end position
        # create window
        x_win = x[start:end]
        y_win = y[start:end]
        # dispersion
        # na.rm = T , means exclude the ones that are NA
        D = (x_win.max(skipna=True) - x_win.min(skipna=True)) + (y_win.max(skipna=True) - y_win.min(skipna=True))
        j = 0  # window expander

        while D < dispersion and end + j < x.shape[0]:
            # while: we expand window by 1 using j
            x_win = x[start:(end + j)]
            y_win = y[start:(end + j)]
            D = (x_win.max(skipna=True) - x_win.min(skipna=True)) + (y_win.max(skipna=True) - y_win.min(skipna=True))

            if D > dispersion:
                # select window (j - 1) as fixation
                fix_start.append(t[start])
                fix_end.append(t[end + j - 1])  # j - 1 is previous window
                fix_x.append(x_win.mean(skipna=True))
                fix_y.append(y_win.mean(skipna=True))
                start = end + j  # skip window points
                break
            elif end + j == x.shape[0]:
                # handle last window if data ends during a fixation
                fix_start.append(t[start])
                fix_end.append(t[end])
                fix_x.append(x_win.mean(skipna=True))
                fix_y.append(y_win.mean(skipna=True))
                start = end + j
                break
            j += 1
        start += 1
    dict = {'fixation_start': fix_start, 'fixation_end': fix_end, 'fixation_x': fix_x, 'fixation_y': fix_y}
    dict['fixation_duration'] = [end-start for end, start in zip(fix_end, fix_start)]
    return pd.DataFrame(dict)