document.addEventListener("DOMContentLoaded", () => {
    const startGameBtn = document.getElementById("start-game-btn");
    const playerSetup = document.getElementById("player-setup");
    const startScreen = document.getElementById("start-screen");
    const playerCountEl = document.getElementById("player-count");
    const playerForm = document.getElementById("player-form");
    const addPlayerBtn = document.getElementById("add-player-btn");
  
    let playerCount = 1;
    const maxPlayers = 5;
    const songData = {}; // To store songs with player details
  
    const CLIENT_ID = '69bcdc7b172e46c99226d49e2017ecb8';
    const CLIENT_SECRET = '7dc154279f054c16b62bd3f7894ce289';
    let accessToken = null;

    const REDIRECT_URI = 'http://localhost:1234/callback';

    const SCOPES = 'playlist-read-private playlist-read-collaborative';
  
    // Create an instance of SpotifyWebApi
    var spotifyApi = new SpotifyWebApi();

    function authorizeUser() {
        const url = `https://accounts.spotify.com/authorize?client_id=${CLIENT_ID}&response_type=token&scope=${encodeURIComponent(SCOPES)}&redirect_uri=${encodeURIComponent(REDIRECT_URI)}`;
        document.location = url;
    }    
    
    // Function to parse the access token from URL hash
    function parseAccessTokenFromURL() {
        const hash = location.hash.substring(1); // Remove the # character
        const params = new URLSearchParams(hash);
        accessToken = params.get('access_token');
    
        if (accessToken) {
            spotifyApi.setAccessToken(accessToken);
            console.log("Access token set successfully.");
        } else {
            console.error("Access token not found. Redirecting to authorize...");
            authorizeUser(); // Redirect to authorize if token is not found
        }
    }    

    // Function to initialize Spotify API with token
    async function initializeSpotifyApi() {
      try {
        const tokenResponse = await fetch('https://accounts.spotify.com/api/token', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded',
            Authorization: `Basic ${btoa(`${CLIENT_ID}:${CLIENT_SECRET}`)}`,
          },
          body: 'grant_type=client_credentials',
        });
  
        const tokenData = await tokenResponse.json();
        spotifyApi.setAccessToken(tokenData.access_token);
        console.log('Spotify API initialized.');
      } catch (error) {
        console.error('Error initializing Spotify API:', error);
      }
    }
  
    // Start game button
    startGameBtn.addEventListener("click", () => {
      startScreen.classList.add("hidden");
      playerSetup.classList.remove("hidden");
    });
  
    // Add player button
    addPlayerBtn.addEventListener("click", () => {
      if (playerCount >= maxPlayers) {
        alert("Maximum players reached!");
        return;
      }
  
      playerCount++;
      updatePlayerCount();
  
      const newPlayerEntry = document.createElement("div");
      newPlayerEntry.classList.add("player-entry");
  
      newPlayerEntry.innerHTML = `
        <input type="text" placeholder="Player Name" name="playerName" required>
        <input type="file" id="playerFile" name="playerFile" required>
        <button type="button" class="validate-btn">Validate</button>
      `;
  
      playerForm.appendChild(newPlayerEntry);
  
      const validateBtn = newPlayerEntry.querySelector('.validate-btn');
      validateBtn.addEventListener('click', () => {
        const username = newPlayerEntry.querySelector('input[name="username"]').value.trim();
        const playlistName = newPlayerEntry.querySelector('input[name="playlist"]').value.trim();
  
        validatePlayer(username, playlistName);
      });
  
      if (playerCount === maxPlayers) {
        addPlayerBtn.style.display = "none";
      }
    });
  
    // Update the player count display
    const updatePlayerCount = () => {
      playerCountEl.textContent = playerCount;
    };
  });  