from flask import Flask, render_template, request, jsonify
import os
import pandas as pd
import io
import webbrowser
from threading import Timer

app = Flask(__name__)

OUTPUT_FOLDER = "outputs"
os.makedirs(OUTPUT_FOLDER, exist_ok=True)

@app.route("/")
def index():
    return render_template("index.html")

@app.route("/upload", methods=["POST"])
def upload_folder():
    # Get the player name
    player_name = request.form.get("player_name", "Anonymous")
    print(f"Received player name: {player_name}")

    # Check if "files[]" exists in the request
    if "files[]" not in request.files:
        print("No 'files[]' key in request.files")
        return jsonify({"error": "No folder uploaded"}), 400

    # Retrieve the files
    files = request.files.getlist("files[]")
    if not files:
        print("Files list is empty")
        return jsonify({"error": "No files found in folder"}), 400

    # Log the received file names
    filenames = [file.filename for file in files]
    print(f"Uploaded file names: {filenames}")

    # Return success response
    return jsonify({"message": "Files uploaded successfully", "files": filenames})


if __name__ == "__main__":
    app.run(debug=True)
