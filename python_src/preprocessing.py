import pandas as pd
import numpy as np
import scipy.stats as stats
from scipy.spatial.distance import euclidean as dist
import warnings

def preprocess(data, labels, dispersion=100):
    '''
    :param data: DataFrame containing csv data
    :param labels: DataFrame participants ratings
    :param dispersion: float max dispersion, default=100
    :return:
    '''
    # Remove irrelevant data
    data = clean_data(data)
    data.set_index(['person ID', 'text ID'], inplace=True)
    data.sort_index(level=[0, 1], inplace=True)
    data = transformData(data, dispersion)
    metrics = computeMetrics(data, labels, dispersion)
    return metrics

def clean_data(data):
    data = data.dropna(subset=['text ID', 'person ID'])
    return data[data['stimulus_name'].str.contains('stimulus')]

def computeMetrics(data, ratings, dispersion):
    all_metrics = []
    for id_pair, measurements in data.items():
        person, text = id_pair
        metrics = {'person ID': person, 'text ID': text}
        metrics.update(ratings.loc[person, text].to_dict())
        metrics.update(computeFixationMetrics(measurements, dispersion))
        metrics.update(computeSaccadeMetrics(measurements))
        metrics.update(computeBlinkMetrics(measurements))
        all_metrics.append(metrics)
    return pd.DataFrame(all_metrics).set_index(['person ID', 'text ID'])

def isProgressiveFixation(all_fixations, index1, index2, dispersion):
    '''
    :param: fixation1 = {x, y}
    :param: fixation2 = {x, y}
    :param: dispersion = float
    :returns: bool
    '''
    # A fixation is classified as progressive if it either has a larger x-coordinate and roughly the same
    # y-coordinate(on the same line) or when it has a larger y-coordinate(on a lower line)
    if (all_fixations['fixation_x'][index2] >= all_fixations['fixation_x'][index1] and
            all_fixations['fixation_y'][index2] >= all_fixations['fixation_y'][index1] - (dispersion / 2)) \
            or (all_fixations['fixation_y'][index2] >= all_fixations['fixation_y'][index1] + (dispersion / 2)):
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
    n_fixations = len(fixations['fixation_duration'])
    if n_fixations == 0:
        warnings.warn('No fixations found!')
        return {}
    # The first fixation is always a progressive fixation
    progressiveFixations = {
        #'start': [fixations['fixation_start'][0]],
        #'end': [fixations['fixation_end'][0]],
        #'x': [fixations['fixation_x'][0]],
        #'y': [fixations['fixation_y'][0]],
        'dur': [fixations['fixation_duration'][0]]
    }
    regressiveFixations = {
        #'start': [],
        #'end': [],
        #'x': [],
        #'y': [],
        'dur': []
    }

    # Loop over all but the first fixation event to determine which fixations are progressive and which fixations
    # are regressive
    for index in range(1, n_fixations):
        # Check if the fixation is progressive
        if isProgressiveFixation(fixations, index-1, index, dispersion):
            #progressiveFixations['start'].append(fixations['fixation_start'][index])
            #progressiveFixations['end'].append(fixations['fixation_end'][index])
            #progressiveFixations['x'].append(fixations['fixation_x'][index])
            #progressiveFixations['y'].append(fixations['fixation_y'][index])
            progressiveFixations['dur'].append(fixations['fixation_duration'][index])
        # The fixation is regressive
        else:
            #regressiveFixations['start'].append(fixations['fixation_start'][index])
            #regressiveFixations['end'].append(fixations['fixation_end'][index])
            #regressiveFixations['x'].append(fixations['fixation_x'][index])
            #regressiveFixations['y'].append(fixations['fixation_y'][index])
            regressiveFixations['dur'].append(fixations['fixation_duration'][index])

    check = len(regressiveFixations['dur']) != 0
    progressiveFixations['dur'] = np.asarray(progressiveFixations['dur'])
    regressiveFixations['dur'] = np.asarray(regressiveFixations['dur'])
    return {
            'Fixations': n_fixations,
            'Fixations_Progressive_Fraction'            : len(progressiveFixations['dur']),
            'Fixations_Regressive_Fraction'             : len(regressiveFixations['dur']),
            'Fixation_Progressive_Duration_Mean'        : np.mean(progressiveFixations['dur']),
            'Fixation_Regressive_Duration_Mean'         : np.mean(regressiveFixations['dur']) if check else 0.0,
            'Fixation_Progressive_Duration_SD'          : np.std(progressiveFixations['dur']),
            'Fixation_Regressive_Duration_SD'           : np.std(regressiveFixations['dur']) if check else 0.0,
            'Fixation_Progressive_Duration_Skewness'    : stats.skew(progressiveFixations['dur']),
            'Fixation_Regressive_Duration_Skewness'     : stats.skew(regressiveFixations['dur']) if check else 0.0,
            'Fixation_Progressive_Duration_Kurtosis'    : stats.kurtosis(progressiveFixations['dur']),
            'Fixation_Regressive_Duration_Kurtosis'     : stats.kurtosis(regressiveFixations['dur']) if check else 0.0,
            'Fixation_Progressive_Duration_Variation'   : stats.variation(progressiveFixations['dur']),
            'Fixation_Regressive_Duration_Variation'    : stats.variation(regressiveFixations['dur']) if check else 0.0
            }



def computeSaccadeMetrics(saccades):
    '''
    Description: this function computes the aggregate metrics of the saccade events
    :param saccades: DataFrame containing the aggregate metrics of the saccade events
    :return: DataFrame
    '''
    n_saccades = len(saccades['saccade_duration'])
    if n_saccades == 0:
        warnings.warn('No Saccades found!')
        return {}
    return {
        'Saccades'                    : n_saccades,
        'Saccade_Duration_Mean'       : np.mean(saccades['saccade_duration']),
        'Saccade_Amplitude_Mean'      : np.mean(saccades['saccade_amplitude']),
        'Saccade_Velocity_Mean'       : np.mean(saccades['saccade_velocity']),
        'Saccade_Duration_SD'         : np.std(saccades['saccade_duration']),
        'Saccade_Amplitude_SD'        : np.std(saccades['saccade_amplitude']),
        'Saccade_Velocity_SD'         : np.std(saccades['saccade_velocity']),
        'Saccade_Duration_Skewness'   : stats.skew(saccades['saccade_duration']),
        'Saccade_Amplitude_Skewness'  : stats.skew(saccades['saccade_amplitude']),
        'Saccade_Velocity_Skewness'   : stats.skew(saccades['saccade_velocity']),
        'Saccade_Duration_Kurtosis'   : stats.kurtosis(saccades['saccade_duration']),
        'Saccade_Amplitude_Kurtosis'  : stats.kurtosis(saccades['saccade_amplitude']),
        'Saccade_Velocity_Kurtosis'   : stats.kurtosis(saccades['saccade_velocity']),
        'Saccade_Duration_Variation'  : stats.variation(saccades['saccade_duration']),
        'Saccade_Amplitude_Variation' : stats.variation(saccades['saccade_amplitude']),
        'Saccade_Velocity_Variation'  : stats.variation(saccades['saccade_velocity'])
    }


def computeBlinkMetrics(blinks):
    '''
    Description: this function computes the aggregate metrics of the blink events
    :param blinks: DataFrame
    :return: DataFrame: this function returns a DataFrame of aggregate metrics of the blink events
    '''
    n_blinks = len(blinks['blink_duration'])
    if n_blinks == 0:
        warnings.warn('No blinks found!')
        return {}
    # Make sure that we do not calculate NA for the standard deviation if we have only 1 blink or less
    return {
        'Blinks'                    : n_blinks,
        'Blink_Duration_Mean'       : np.mean(blinks['blink_duration']),
        'Blink_Duration_SD'         : np.std(blinks['blink_duration']),
        'Blink_Duration_Skewness'   : stats.skew(blinks['blink_duration']),
        'Blink_Duration_Kurtosis'   : stats.kurtosis(blinks['blink_duration']),
        'Blink_Duration_Variation'  : stats.variation(blinks['blink_duration'])
    }


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
    data = fixationData(data, dispersion, 5)
    data = saccadeData(data)
    data = blinkData(data)

    return data


def fixationData(data, dispersion, minSamples):
    print('Processing Fixations')
    # Compute all fixation events
    fixations = {}
    levels = data.index.unique().levels
    for person in levels[0]:
        for text in levels[1]:
            slice = data.loc[person, text]
            fixations[(person, text)] = {'raw': slice}
            idtResults = idt(
                    filterData(slice),
                    dispersion,
                    minSamples
                )
            fixations[(person, text)].update(idtResults)
    return fixations


def saccadeData(allFixations):
    '''
    Calculates all the saccade data from the fixations
    :param fixations: DataFrame containing all the fixations
    :return: DataFrame containing all the saccades
    '''
    print('Processing SaccadeData')
    # Stores all saccade events
    for id, fixation in allFixations.items():
        # id = (personid, textid)
        # fixations = {fixation_start:[] etc}
        saccades = {
            #'saccade_start': [],
            #'saccade_end': [],
            #'saccade_start_x': [],
            #'saccade_start_y': [],
            #'saccade_end_x': [],
            #'saccade_end_y': [],
            'saccade_duration': [],
            'saccade_amplitude': [],
            'saccade_velocity': []
        }
        for index in range(len(fixation['fixation_start']) - 1):
            # Add a saccade event to the list of all saccade events
            saccade_end = fixation['fixation_start'][index + 1] - 1
            saccade_start = fixation['fixation_end'][index] + 1
            duration = saccade_end - saccade_start

            saccade_start_x = fixation['fixation_x'][index]
            saccade_start_y = fixation['fixation_y'][index]
            saccade_end_x = fixation['fixation_x'][index + 1]
            saccade_end_y = fixation['fixation_y'][index + 1]
            distance = dist( (saccade_start_x, saccade_start_y), (saccade_end_x, saccade_end_y) )

            #saccades['saccade_start'].append(saccade_start)
            #saccades['saccade_end'].append(saccade_end)
            #saccades['saccade_start_x'].append(saccade_start_x)
            #saccades['saccade_start_y'].append(saccade_start_y)
            #saccades['saccade_end_x'].append(saccade_end_x)
            #saccades['saccade_end_y'].append(saccade_end_y)

            saccades['saccade_duration'].append(duration)
            saccades['saccade_amplitude'].append(distance)
            saccades['saccade_velocity'].append(distance / duration)
        allFixations[id].update(saccades)
    return allFixations


def blinkData(all_data):
    '''
    Retrieves blink event data from raw Data
    :param data:  DataFrame containing raw Data
    :return: DataFrame containing blink event Data
    '''
    print('Processing Blink Data')
    for id, data in all_data.items():
        filtered_data = filterData(data['raw'], filterNegativeEyePositions=False)
        # Stores all blink events

        # The first sample in a sequence of consecutive blink samples
        blinkStartSample = None

        blinkEvents = {
            #'blink_start': [],
            #'blink_end': [],
            'blink_duration': []
        }
        # Loop over all samples to determine sequences of blink events
        for index, row in filtered_data.iterrows():
            # We should have registered a negative position for at least one of the eyes
            if row['L POR X [px]'] <= 0 or row['L POR Y [px]'] <= 0 or row['R POR X [px]'] <= 0 or row['R POR Y [px]'] <= 0:
                # Check whether or not this is the first sample in the sequence of blinks currently under investigation
                if blinkStartSample is None: #or blinkStartSample.isna()
                    # Register that this is the first sample in the sequence of blinks currently under investigation
                    blinkStartSample = row
            # We have not registered a negative position for at least one of the eyes
            elif blinkStartSample is not None:
                # Check whether or not a sequence of blink events was currently under construction
                duration = (row['Time'] - 1) - blinkStartSample['Time']
                # Remove all blinks that have a duration of less than 70 milliseconds
                if duration >= 70000:
                    # Add the blink event to the list of blink events
                    #blinkEvents['blink_start'].append(blinkStartSample['Time'])
                    #blinkEvents['blink_end'].append(row['Time'] - 1)
                    blinkEvents['blink_duration'].append(duration)

                    # Reset the first sample in the sequence of consecutive blink samples
                    blinkStartSample = None
        all_data[id].update(blinkEvents)
    return all_data


def idt(data, dispersion, duration):
    '''
    Implementation of a dispersion-based algorithm (I-DT) proposed by Salvucci & Goldberg(2000)
    :param t: array of all time stamps
    :param x: array of all x coordinates
    :param y: array of all y coordinates
    :param dispersion: float maximum dispersion
    :param duration: float maximum duration
    :return:
    '''
    # init variables
    fix_start = []
    fix_end = []
    fix_x = []
    fix_y = []

    start = 0  # window start position
    n_points = data.shape[0]
    while start < (n_points - duration):
        # while: we move window by 1, if D > threshold
        end = start + duration  # window end position
        # create window
        x_win = data.iloc[start:end]['L POR X [px]']
        y_win = data.iloc[start:end]['L POR Y [px]']
        # dispersion
        # na.rm = T , means exclude the ones that are NA
        D = (x_win.max(skipna=True) - x_win.min(skipna=True)) + (y_win.max(skipna=True) - y_win.min(skipna=True))
        j = 0  # window expander

        while D < dispersion and end + j < n_points:
            # while: we expand window by 1 using j
            x_win = data.iloc[start:end+j]['L POR X [px]']
            y_win = data.iloc[start:end+j]['L POR Y [px]']
            D = (x_win.max(skipna=True) - x_win.min(skipna=True)) + (y_win.max(skipna=True) - y_win.min(skipna=True))
            x_mean = x_win.mean(skipna=True)
            y_mean = y_win.mean(skipna=True)
            if D > dispersion:
                # select window (j - 1) as fixation
                if x_mean > 0 and y_mean > 0:
                    fix_start.append(data.iloc[start]['Time'])
                    fix_end.append(data.iloc[end + j - 1]['Time'])  # j - 1 is previous window
                    fix_x.append(x_mean)
                    fix_y.append(y_mean)
                start = end + j  # skip window points
                break
            elif end + j == n_points:
                # handle last window if data ends during a fixation
                if x_mean > 0 and y_mean > 0:
                    fix_start.append(data.iloc[start]['Time'])
                    fix_end.append(data.iloc[end]['Time'])
                    fix_x.append(x_mean)
                    fix_y.append(y_mean)
                start = end + j
                break
            j += 1
        start += 1
    return {
        'fixation_start': fix_start,
        'fixation_end': fix_end,
        'fixation_x': fix_x,
        'fixation_y': fix_y,
        'fixation_duration': [end - start for end, start in zip(fix_end, fix_start)]
    }