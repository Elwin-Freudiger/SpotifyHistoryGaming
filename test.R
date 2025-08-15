library(shiny)
library(shinyjs)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(purrr)
library(spotifyr) #for API queries
library(httr)
library(stringr)


# Increase max file size for uploads
options(shiny.maxRequestSize = 100*1024^2)

# Function to unzip and process JSON files
clean_player <- function(zip_path, userName, 
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
  
  lookup <- c(trackName = 'master_metadata_track_name',
              artistName =  'master_metadata_album_artist_name',
              msPlayed = 'ms_played',
              trackID = 'spotify_track_uri')
  
  clean_df <- combined_df %>% 
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

get_genres <- function(dataset) {
  trackID <- dataset$trackID
  
  track_info <- batch_ids(trackID) %>% 
    map_df(~{
      Sys.sleep(0.1)
      tracks <- get_tracks(.x)
      tibble(
        trackID = tracks$id,
        track_duration = tracks$duration_ms,
        album_release_date = tracks$album.release_date,
        popularity = tracks$popularity,
        artist_id = sapply(tracks$album.artists, function(df) df$id[1])
      )
    })
  
  unique_artist <- unique(track_info$artist_id)
  
  artist_info <- batch_ids(unique_artist) %>%
    map_df(~{
      Sys.sleep(0.1)
      artists <- get_artists(.x)
      tibble(
        artist_id = artists$id,
        genres = sapply(artists$genres, function(line) paste(line, collapse = ", "))
      )
    })
  
  richer <- dataset %>% 
    left_join(track_info, by='trackID') %>% 
    left_join(artist_info, by='artist_id')
  
  return(richer)

  }


# UI
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Shiny Player App"),
  
  #sidebars
  sidebarLayout(
    sidebarPanel(
      div(
        id = "game_setup",
        h3("Players"),
        uiOutput("player_inputs"),
        actionButton("launch_game", "Launch Game")
      ),
      div(
        id = "game_play",
        selectizeInput("genre_input",
          "Select genres:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Select genres",
            maxItems = 10
          )
        ),
        sliderInput("years", "Select Year Range:",
                    min = 1945, max = 2024, value = c(1990, 2024), ticks = FALSE, sep = "", pre=""),
        br(), br(),
        actionButton("start_game", "Start Game"),
        textOutput("warning_pool")
      )
      ),
    #main panel
    mainPanel(
      textOutput("text")
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
          df <- clean_player(file$datapath, name)
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
    final_data <- player_data %>% 
      concat_clean() %>% 
      get_genres()
    
    #hide sidebar
    lapply(1:5, function(i) {
      shinyjs::hide("game_setup")
      shinyjs::show("game_play")
    })
    
    output$text <- renderPrint({
      if (nrow(final_data) > 0) {
        head(final_data, 10)
      } else {
        "No valid data available!"
      }
    })
  })
}

# Run the app
shinyApp(ui, server)