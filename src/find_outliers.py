import pandas as pd

################################################################################
#                                                                              #
#                               Global variables                               #
#                                                                              #
################################################################################

to_test = {
    'track_length_ms': 'z_score',
    'track_acousticness': 'z_score',
    'track_danceability': 'z_score',
    'track_energy': 'z_score',
    'track_key': 'prop',
    'track_loudness': 'z_score',
    'track_mode': 'prop',
    'track_tempo': 'z_score',
    'track_time_signature': 'prop',
    'track_valence': 'z_score'
}

################################################################################
#                                                                              #
#                                    Functions                                 #
#                                                                              #
################################################################################

def get_outliers_z_score(series):
    z = (series - series.mean())/series.std()
    outliers = (z[(z <= -2) | (z >= 2)]).to_frame(name = 'metric_value')
    return(outliers)

def get_outliers_prop(series):
    prop = pd.merge(
        left = series,
        right = series.value_counts(normalize = True).rename('metric_value'),
        left_on = series.name,
        right_index = True
    ).drop(series.name, axis = 1)
    outliers = prop[prop['metric_value'] < 0.05]
    return(outliers)

def main(tracks_fp, outliers_fp, verbose):

    track_df = pd.read_csv(
        tracks_fp, 
        index_col = 'track_name'
    )

    outliers = []

    if verbose: print('Finding outliers')

    for key, value in to_test.items():

        if value == 'z_score':
            outliers_to_add = get_outliers_z_score(track_df[key])
            outliers_to_add['metric'] = value
            outliers_to_add['variable'] = key
        
        elif value == 'prop':
            outliers_to_add = get_outliers_prop(track_df[key])
            outliers_to_add['metric'] = value
            outliers_to_add['variable'] = key
        
        outliers.append(outliers_to_add)

    outliers_df = pd.concat(outliers)

    if verbose: print('Found outliers')

    outliers_df.to_csv(outliers_fp)

################################################################################
#                                                                              #
#                                       Main                                   #
#                                                                              #
################################################################################

if __name__ == "__main__":

    import argparse

    parser = argparse.ArgumentParser(description = 'Find TSwift outliers')
    parser.add_argument('--tracks', required = True, help = 'Path to get tracks data')
    parser.add_argument('--outliers', required = True, help = 'Path to save outliers data')
    parser.add_argument('--verbose', required = True, help = 'Print statements')
    args = parser.parse_args()

    main(
        tracks_fp = args.tracks, 
        outliers_fp = args.outliers,
        verbose = args.verbose
    )