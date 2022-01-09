import yaml
import requests
import pandas as pd

################################################################################
#                                                                              #
#                               Global variables                               #
#                                                                              #
################################################################################

TSWIFT_ALBUM_IDS = ['7mzrIsaAjnXihW3InKjlC3','2dqn5yOQWdyGwOpOIi9O4x',
                    '5MfAxS5zz8MlfROjGQVXhy','1EoDsNmgTLtmwe1BDAVxV5',
                    '2QJmrSgbdM35R67eoGQo4j','6DEjYFkNZh67HP7R9PSZvv',
                    '1NAmidJlEaVgA3MpcPFYGq','2fenSS68JI1h4Fo296JfGr',
                    '2Xoteh7uEpea4TohMxjtaq']

################################################################################
#                                                                              #
#                                    Functions                                 #
#                                                                              #
################################################################################

def read_yaml(file_path):

    with open(file_path, "r") as f:
        return yaml.safe_load(f)

def get_access_token(auth_url, client_id, client_secret):

    auth_response = requests.post(
        auth_url,
        {'grant_type': 'client_credentials',
        'client_id': client_id,
        'client_secret': client_secret
        }
    )

    auth_response_data = auth_response.json()

    access_token = auth_response_data['access_token']

    return access_token

def make_headers(access_token):

    headers = {'Authorization': 'Bearer {token}'.format(token=access_token)}

    return headers

def get_album_data(base_url, album_id, headers):

    r = requests.get(
        base_url + 'albums/' + album_id,
        headers = headers
    )

    d = r.json()

    album_data = {
        'album_name': d['name'],
        'release_date': d['release_date'],
        'total_tracks': d['total_tracks']
    }

    return album_data

def get_album_tracks(base_url, album_id, headers):

    r = requests.get(
        base_url + 'albums/' + album_id + '/tracks',
        headers = headers
    )
        
    d = r.json()

    track_ids = [track['id'] for track in d['items']]

    return track_ids

def get_track_data(base_url, track_id, headers):

    r_track = requests.get(
        base_url + 'tracks/' + track_id,
        headers = headers
    )
    
    d_track = r_track.json()

    r_features = requests.get(
        base_url + 'audio-features/' + track_id,
        headers = headers
    )

    d_features = r_features.json()

    track_data = {
        'track_name': d_track['name'],
        'track_length_ms': d_track['duration_ms'],
        'track_number': d_track['track_number'],
        'track_acousticness': d_features['acousticness'],
        'track_danceability': d_features['danceability'],
        'track_energy': d_features['energy'],
        'track_instrumentalness': d_features['instrumentalness'],
        'track_key': d_features['key'],
        'track_liveness': d_features['liveness'],
        'track_loudness': d_features['loudness'],
        'track_mode': d_features['mode'],
        'track_speechiness': d_features['speechiness'],
        'track_tempo': d_features['tempo'],
        'track_time_signature': d_features['time_signature'],
        'track_valence': d_features['valence']
    }

    return track_data

def build_dfs(album_ids, base_url, headers, verbose):

    albums_data = []

    tracks_data = []

    for album_id in album_ids:

        if verbose: print('Getting album ', album_id)

        album_data = get_album_data(base_url, album_id, headers)

        if verbose: print('Got ', album_data['album_name'])

        albums_data.append(album_data)

        if verbose: print('Getting tracks')

        track_ids = get_album_tracks(base_url, album_id, headers)

        for track_id in track_ids:
            
            track_data = get_track_data(base_url, track_id, headers)

            track_data['album_name'] = album_data['album_name']
            
            tracks_data.append(track_data)
    
        if verbose: print('Got tracks')
    
    albums_df = pd.DataFrame(albums_data)

    tracks_df = pd.DataFrame(tracks_data)

    return albums_df, tracks_df

def main(config_fp, album_fp, tracks_fp, verbose):

    config = read_yaml(config_fp)

    access_token = get_access_token(
        config['SPOTIFY']['AUTH_URL'],
        config['SPOTIFY']['CLIENT_ID'], 
        config['SPOTIFY']['CLIENT_SECRET']
    )

    headers = make_headers(access_token)

    album_df, track_df = build_dfs(
        TSWIFT_ALBUM_IDS, 
        config['SPOTIFY']['BASE_URL'],
        headers, 
        verbose
    )

    album_df.to_csv(album_fp, index = False)

    track_df.to_csv(tracks_fp, index = False)

################################################################################
#                                                                              #
#                                       Main                                   #
#                                                                              #
################################################################################

if __name__ == "__main__":

    import argparse

    parser = argparse.ArgumentParser(description = 'Build TSwift datasets')
    parser.add_argument('--config', required = True, help = 'Path to config file')
    parser.add_argument('--album', required = True, help = 'Path to save album data')
    parser.add_argument('--tracks', required = True, help = 'Path to save tracks data')
    parser.add_argument('--verbose', required = True, help = 'Print statements')
    args = parser.parse_args()

    main(
        config_fp = args.config, 
        album_fp = args.album, 
        tracks_fp = args.tracks, 
        verbose = args.verbose
    )