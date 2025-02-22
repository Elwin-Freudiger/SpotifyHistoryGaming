from flask import Flask, render_template, request, jsonify
import os
import json
from collections import defaultdict
import random
import spotipy
from spotipy.oauth2 import SpotifyClientCredentials

app = Flask(__name__)

# Spotify API credentials (replace with your own)
SPOTIPY_CLIENT_ID = '69bcdc7b172e46c99226d49e2017ecb8'
SPOTIPY_CLIENT_SECRET = '7dc154279f054c16b62bd3f7894ce289'

sp = spotipy.Spotify(auth_manager=SpotifyClientCredentials(client_id=SPOTIPY_CLIENT_ID,
                                                           client_secret=SPOTIPY_CLIENT_SECRET))

players_data = {}
aggregated_songs = defaultdict(lambda: {'time_played': 0, 'top_listener': ''})

# Helper function to get Spotify song ID
def get_spotify_id(song_name, artist):
    results = sp.search(q=f'track:{song_name} artist:{artist}', limit=1)
    tracks = results.get('tracks', {}).get('items', [])
    if tracks:
        return tracks[0]['id']
    return None

# Aggregation function
def process_player_data(player_name, folder_path):
    total_time = defaultdict(int)
    for filename in os.listdir(folder_path):
        if filename.startswith('Streaming_History') and filename.endswith('.json'):
            with open(os.path.join(folder_path, filename), 'r', encoding='utf-8') as f:
                data = json.load(f)
                for entry in data:
                    song = entry['trackName']
                    artist = entry['artistName']
                    time_played = entry['msPlayed']
                    total_time[(song, artist)] += time_played

    for (song, artist), time in total_time.items():
        if time >= 180000:  # Only keep songs with more than 3 minutes listening
            if time > aggregated_songs[(song, artist)]['time_played']:
                aggregated_songs[(song, artist)] = {'time_played': time, 'top_listener': player_name}

@app.route('/')
def home():
    return render_template('index.html')

@app.route('/submit', methods=['POST'])
def submit():
    player_name = request.form['player_name']
    folder = request.files.getlist('spotify_data')

    player_folder = f'temp_data/{player_name}'
    os.makedirs(player_folder, exist_ok=True)

    for file in folder:
        file.save(os.path.join(player_folder, file.filename))

    process_player_data(player_name, player_folder)
    return jsonify({'status': 'success'})

@app.route('/get_round')
def get_round():
    song, data = random.choice(list(aggregated_songs.items()))
    song_name, artist = song
    song_id = get_spotify_id(song_name, artist)
    return jsonify({'song_name': song_name, 'artist': artist, 'spotify_id': song_id})

@app.route('/check_answer', methods=['POST'])
def check_answer():
    song_name = request.form['song_name']
    artist = request.form['artist']
    guess = request.form['player_guess']

    correct_player = aggregated_songs[(song_name, artist)]['top_listener']
    time_played = aggregated_songs[(song_name, artist)]['time_played'] / 60000  # Convert to minutes
    return jsonify({'correct': guess == correct_player, 'time_played': f'{time_played:.2f} minutes'})

if __name__ == '__main__':
    app.run(debug=True)