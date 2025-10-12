library(shiny)
library(shinyjs)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(purrr)
library(lubridate)
library(spotifyr) #for API queries
library(httr)
library(stringr)
library(shinybusy) #loading screens

#lowkey we future proof the year
current_year <- as.integer(year(Sys.Date()))

broad_genres <- c("All Genres", "Pop", "Rap", "Rock", "Indie", "Country", "Niche songs")

# Increase max file size for uploads
options(shiny.maxRequestSize = 100*1024^2)

# Function to unzip and process JSON files
clean_player <- function(zip_path, userName,
                         past_data, custom_past_data,
                         pattern = "^(Streaming_History_Audio_).*\\.json$") {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  unzip(zip_path, exdir = temp_dir)
  
  json_files <- list.files(temp_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
  
  if (length(json_files) == 0) {
    return(data.frame())
  }
  
  combined_df <- do.call(rbind, lapply(json_files, function(file) {
    tryCatch(fromJSON(file) %>% as.data.frame(), error = function(e) NULL)
  }))
  
  date_df <- combined_df %>% 
    mutate(date_time = as_datetime(ts))
  
  #filter for 1 year, or custom
  if (past_data == "Past Year") {
    filter_df <- date_df %>%
      filter(date_time >= (today() - years(1)))
  } else if (past_data == "Custom") {
    req(custom_past_data)
    filter_df <- date_df %>% filter(
      date_time >= ymd(paste0(custom_past_data[1], "-01-01")),
      date_time <= ymd(paste0(custom_past_data[2], "-12-31")))
  } else {
    filter_df <- date_df
  }
  
  
  lookup <- c(trackName = 'master_metadata_track_name',
              artistName =  'master_metadata_album_artist_name',
              msPlayed = 'ms_played',
              trackID = 'spotify_track_uri')
  
  clean_df <- filter_df %>% 
    select(any_of(c('master_metadata_track_name', 'trackName',
                    'master_metadata_album_artist_name', 'artistName',
                    'ms_played', 'msPlayed', 'spotify_track_uri'))) %>%
    rename(any_of(lookup)) %>% 
    drop_na(trackName) %>% 
    group_by(trackName, artistName) %>% 
    summarise(msPlayed = sum(msPlayed), trackID = first(trackID), .groups = "drop") %>% 
    filter(msPlayed >= 180000) %>%  # filter so that at least 3 minutes of total listening exist
    mutate(listener = userName)
  
  return(clean_df)
}

# Function to concatenate player data
concat_clean <- function(player_data) {
  valid_dfs <- Filter(Negate(is.null), player_data$dfs)
  
  if (length(valid_dfs) > 0) {
    concat_df <- bind_rows(valid_dfs)
  } else {
    return(data.frame())  
  }
  
  song_id_list <- concat_df %>% 
    filter(!is.na(trackID)) %>%
    select(trackName, artistName, trackID) %>% 
    group_by(trackName, artistName) %>% 
    slice_sample(n = 1) %>% 
    ungroup()
  
  concat_df <- concat_df %>%
    mutate(trackID = song_id_list$trackID[match(paste(trackName, artistName), 
                                                paste(song_id_list$trackName, song_id_list$artistName))])
  
  final_dataset <- concat_df %>% 
    group_by(trackName, artistName) %>% 
    slice_max(msPlayed, n = 1, with_ties = FALSE) %>% 
    ungroup() %>% 
    mutate(trackName = iconv(trackName, from = "UTF-8", to = "UTF-8"),
           artistName = iconv(artistName, from = "UTF-8", to = "UTF-8"),
           trackID = substr(trackID, 15, 1000))
  
  return(final_dataset)
}

#batching function
batch_ids <- function(ids, size = 50) {
  split(ids, ceiling(seq_along(ids) / size))
}

get_genres <- function(dataset_raw) {
  
  # --- Load caches or init empty ---
  tracks_cache <- if (file.exists("song_cache/tracks.csv")) read_csv("song_cache/tracks.csv",
                                                                     show_col_types = FALSE) 
  else tibble(
    trackID = character(),
    track_duration = integer(),
    album_release_date = integer(),
    popularity = numeric(),
    artist_id = character()
  )
  
  artists_cache <- if (file.exists("song_cache/artists.csv")) read_csv("song_cache/artists.csv",
                                                                       show_col_types = FALSE) 
  else tibble(
    artist_id = character(),
    genres = character()
  )
  
  # --- Find which songs are missing in cache ---
  missing_tracks <- setdiff(dataset_raw$trackID, tracks_cache$trackID)
  
  # --- Query new tracks if needed ---
  if (length(missing_tracks) > 0) {
    track_info <- batch_ids(missing_tracks) %>% 
      map_df(~{
        Sys.sleep(0.1)
        tracks <- get_tracks(.x)
        tibble(
          trackID = tracks$id,
          track_duration = tracks$duration_ms,
          album_release_date = as.integer(substr(tracks$album.release_date, start=1, stop=4)),
          popularity = tracks$popularity,
          artist_id = sapply(tracks$album.artists, function(df) df$id[1])
        )
      })
    
    # --- Find which artists are missing ---
    missing_artists <- setdiff(track_info$artist_id, artists_cache$artist_id)
    
    if (length(missing_artists) > 0) {
      artist_info <- batch_ids(missing_artists) %>%
        map_df(~{
          Sys.sleep(0.1)
          artists <- get_artists(.x)
          tibble(
            artist_id = artists$id,
            genres = sapply(artists$genres, function(line) paste(line, collapse = ", "))
          )
        })
      
      # Update artist cache
      artists_cache <- bind_rows(artists_cache, artist_info) %>%
        distinct(artist_id, .keep_all = TRUE)
      write_csv(artists_cache, "song_cache/artists.csv")
    }
    
    # Update track cache
    tracks_cache <- bind_rows(tracks_cache, track_info) %>%
      distinct(trackID, .keep_all = TRUE)
    write_csv(tracks_cache, "song_cache/tracks.csv")
  }
  
  # final dataset
  richer <- dataset_raw %>% 
    left_join(tracks_cache, by = "trackID") %>% 
    left_join(artists_cache, by = "artist_id")
  
  return(richer)
}


songs_filtered <- function(track_list, genres_choice, start_year = 1900, end_year = current_year) {
  track_list <- track_list %>% filter(!is.na(album_release_date))
  genres_choice <- tolower(genres_choice)
  track_list_time <- track_list %>% filter(album_release_date >= start_year, album_release_date <= end_year)
  if ("All Genres" %in% genres_choice || is.null(genres_choice) || length(genres_choice) == 0) {
    return(track_list_time)
  } else if ("Niche songs" %in% genres_choice) {
    track_list_na <- track_list_time %>% filter(is.na(genres))
    track_list_filter <- track_list_time %>% filter(str_detect(genres, paste0(genres_choice, collapse = "|")))
    return(rbind(track_list_na, track_list_filter))
  } else {
    track_list_filter <- track_list_time %>% filter(str_detect(genres, paste0(genres_choice, collapse = "|")))
    return(track_list_filter)
  }
}


############################################## UI
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Shiny Player App"),
  
  #sidebars
  sidebarLayout(
    sidebarPanel(
      div(
        id = "game_setup",
        radioButtons("past_data", "Use data since:",
                     c("Past Year", "All time", "Custom"),
                     selected = "Past Year",
                     inline=TRUE),
        conditionalPanel(
          condition = "input.past_data == 'Custom'",
          sliderInput("custom_past_data", "Choose which years to include:",
                      min=2010, max = current_year, value = c(2020, 2023),
                      step = 1,ticks = FALSE, sep = "", pre="")),
        h3("Players"), 
        uiOutput("player_inputs"),
        actionButton("launch_game", "Launch Game")
      ),
      div(
        id = "game_play",
        radioButtons("inf_rounds", "Number of rounds",
                     c("Endless", "Custom"),
                     selected = "Endless",
                     inline = TRUE),
        conditionalPanel(
          condition = "input.inf_rounds == 'Custom'",
          sliderInput("round_number", "Choose number of rounds:",
                      min=1, max = 50, value = 5,
                      step = 1,ticks = FALSE, sep = "", pre="")),
        selectizeInput("genre_input", "Genres:",
                       choices = broad_genres, selected = "All Genres",
                       multiple = TRUE, options = list(placeholder = 'Choose genres', create = TRUE, showAddOptionOnCreate = TRUE)),
        sliderInput("years", "Select Release Year Range:",
                    min = 1900, max = current_year, value = c(2000,  current_year),
                    step= 1, ticks = FALSE, sep = "", pre=""),
        br(), br(),
        actionButton("start_game", "Start Game"),
        textOutput("warning_pool")
      )
    ),
    #main panel
    mainPanel(
      uiOutput("game_ui"),
      textOutput("final_score")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  #hide sidebar at the beginning
  shinyjs::hide("game_play")
  
  player_data <- reactiveValues(dfs = list(), names = list())
  player_counter <- reactiveVal(0)  # Start at 0 and increase when players submit data
  
  # Dynamic UI for player inputs (always show all 5 players)
  output$player_inputs <- renderUI({
    player_ui <- lapply(1:5, function(i) {
      tagList(
        h4(paste("Player", i)),
        textInput(paste0("player_name_", i), "Username", placeholder = "Enter Player Name"),
        fileInput(paste0("file_", i), "Upload ZIP File", accept = ".zip")
      )
    })
    
    do.call(tagList, player_ui)
  })
  
  observe({
    lapply(1:5, function(i) {
      observe({
        name <- input[[paste0("player_name_", i)]]
        file <- input[[paste0("file_", i)]]
        
        # Only process if both are provided
        if (!is.null(file) && nzchar(name)) {
          df <- clean_player(file$datapath, name, input$past_data, input$custom_past_data)
          player_data$dfs[[i]] <- df
          player_data$names[[i]] <- name
        } else {
          player_data$dfs[[i]] <- NULL
          player_data$names[[i]] <- NULL
        }
      })
    })
  })
  
  # Launch Game
  observeEvent(input$launch_game, {
    
    showModal(modalDialog(
      title = "Loading Songs",
      tagList(
        div(
          style = "text-align:center;",
          # CSS spinner
          div(style = "
              border: 8px solid #f3f3f3;
              border-top: 8px solid #3498db;
              border-radius: 50%;
              width: 60px;
              height: 60px;
              animation: spin 1s linear infinite;
              margin: 20px auto;
            "),
          p("Unknown tracks are being researched on spotify"),
          p("Fun Fact: This project helps store information for future players")
        ),
        # Define keyframes for spinner
        tags$style("
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
      ")
      ),
      footer = NULL, easyClose = FALSE
    ))
    
    #clean and concat the data
    final_data <- player_data %>% 
      concat_clean() %>% 
      get_genres()
    
    #hide modal
    removeModal()
    
    #hide sidebar
    shinyjs::hide("game_setup")
    shinyjs::show("game_play")
    
    
    ####Start of the game once everything is loaded
    
    #set the number of rounds
    round_settings <- reactiveValues(
      inf_rounds = TRUE,
      round_nums = Inf,
      min_needed = 1
    )
    
    observe({
      if (input$inf_rounds == "Custom") {
        req(input$round_number)
        round_settings$inf_rounds <- FALSE
        round_settings$round_nums <- input$round_number
        round_settings$min_needed <- input$round_number*4
      } else {
        round_settings$inf_rounds <- TRUE
        round_settings$round_nums <- Inf
        round_settings$min_needed <- 1
      }
    })
    
    #reactive filter pool
    filtered_pool <- reactive({
      req(input$genre_input, input$years)
      songs_filtered(final_data, input$genre_input, input$years[1], input$years[2])
    })
    #make a warning appear if there aren't enough songs
    output$warning_pool <- renderText({
      pool <- filtered_pool()
      if (nrow(pool) < round_settings$min_needed) {
        paste0("Warning: Your pool has fewer than ",
               round_settings$min_needed,
               " songs. For optimal play, broaden your choices.")
      } else {
        ""
      }
    })
    #game data
    game_state <- reactiveValues(
      player_list = unique(final_data$listener),
      song_data = NULL,
      current_round = 1,
      correct_guesses = 0,
      is_game_over = FALSE
    )
    
    observeEvent(input$start_game, {
      pool <- filtered_pool()
      if (nrow(pool) < round_settings$min_needed) {
        showModal(modalDialog(
          title = "Pool Too Small",
          "Your song pool is too small! Please broaden your options.",
          easyClose = TRUE
        ))
        return()
      }
      
      if (round_settings$inf_rounds) {
        game_state$song_data <- pool
      } else if (!round_settings$inf_rounds) {
        game_state$song_data <- dplyr::slice_sample(pool, n = min(nrow(pool), round_settings$round_nums))
      }
      
      game_state$current_round <- 1
      game_state$correct_guesses <- 0
      game_state$is_game_over <- FALSE
    })
    
    output$game_ui <- renderUI({
      req(game_state$song_data, !game_state$is_game_over)
      
      current_song <- game_state$song_data[game_state$current_round, ]
      
      tagList(
        h3(current_song$trackName),
        h4(paste("Artist: ", current_song$artistName)),
        HTML(paste0(
          '<iframe style="border-radius:12px" src="https://open.spotify.com/embed/track/',
          current_song$trackID,
          '?utm_source=generator&theme=0" width="100%" height="352" frameBorder="0" allowfullscreen="" allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture" loading="lazy"></iframe>'
        )),
        h4("Who listened to it the most?"),
        selectInput("guess", "Select Listener:", choices = game_state$player_list),
        actionButton("submit_guess", "Submit Guess"),
        actionButton("next_song", "Next Song", style = "display: none;")
      )
    })
    
    observeEvent(input$submit_guess, {
      req(game_state$song_data, input$guess)
      
      current_song <- game_state$song_data[game_state$current_round, ]
      user_guess <- input$guess
      
      correct_listener <- current_song$listener
      listen_time <- current_song$msPlayed
      song_duration <- current_song$track_duration
      
      if (user_guess == correct_listener) {
        game_state$correct_guesses <- game_state$correct_guesses + 1
      }
      
      showModal(modalDialog(
        title = "Result",
        HTML(paste0(
          "<strong>Correct Listener:</strong> ", correct_listener, "<br>",
          "<strong>Listening Time:</strong> ", round(listen_time / 60000, 2), " minutes<br>",
          "<strong>Replay Count:</strong> ", ceiling(listen_time / song_duration), " times<br>",
          if (user_guess == correct_listener) "🎉 Correct!" else "❌ Wrong Guess."
        )),
        easyClose = TRUE
      ))
      
      shinyjs::show("next_song")
      shinyjs::disable("submit_guess")
    })
    
    observeEvent(input$next_song, {
      if (game_state$current_round < nrow(game_state$song_data)) {
        game_state$current_round <- game_state$current_round + 1
        shinyjs::hide("next_song")
        shinyjs::enable("submit_guess")
      } else {
        game_state$is_game_over <- TRUE
      }
    })
    
    output$final_score <- renderText({
      if (!game_state$is_game_over) return(NULL)
      paste0("Done! Your Total Score: ", game_state$correct_guesses, " / ", nrow(game_state$song_data))
    })
  })
}

# Run the app
shinyApp(ui, server)