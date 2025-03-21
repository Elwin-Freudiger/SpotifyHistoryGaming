library(shiny)
library(shinyjs)
library(tidyverse)
library(jsonlite)
library(dplyr)

# Increase max file size for uploads
options(shiny.maxRequestSize = 100*1024^2)

# Function to unzip and process JSON files
clean_player <- function(zip_path, userName, 
                         pattern = "^(Streaming_History_Audio_|StreamingHistory_music_)\\d{1,4}.*\\.json$") {
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
    mutate(trackID = ifelse("trackID" %in% names(.), trackID, NA)) %>% 
    drop_na(trackName) %>% 
    group_by(trackName, artistName) %>% 
    summarise(msPlayed = sum(msPlayed), trackID = first(trackID), .groups = "drop") %>% 
    filter(msPlayed >= 180000) %>% 
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
           artistName = iconv(artistName, from = "UTF-8", to = "UTF-8"))
  
  return(final_dataset)
}

# UI
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Shiny Player App"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Players"),
      uiOutput("player_inputs"),
      hr(),
      actionButton("launch_game", "Launch Game")
    ),
    
    mainPanel(
      verbatimTextOutput("text")
    )
  )
)

# Server
server <- function(input, output, session) {
  player_data <- reactiveValues(dfs = list(), names = list())
  player_counter <- reactiveVal(0)  # Start at 0 and increase when players submit data
  
  # Dynamic UI for player inputs (always show all 5 players)
  output$player_inputs <- renderUI({
    player_ui <- lapply(1:5, function(i) {
      tagList(
        h4(paste("Player", i)),
        textInput(paste0("player_name_", i), "Username", placeholder = "Enter Player Name"),
        fileInput(paste0("file_", i), "Upload ZIP File", accept = ".zip"),
        actionButton(paste0("clean_button_", i), "Submit")
      )
    })
    
    do.call(tagList, player_ui)
  })
  
  # Observe "Submit" button clicks for each player
  observe({
    lapply(1:5, function(i) {
      observeEvent(input[[paste0("clean_button_", i)]], {
        req(input[[paste0("file_", i)]], input[[paste0("player_name_", i)]])
        
        df <- clean_player(input[[paste0("file_", i)]]$datapath, input[[paste0("player_name_", i)]])
        
        player_data$dfs[[i]] <- df
        player_data$names[[i]] <- input[[paste0("player_name_", i)]]
        
        # Increase counter only if it's the first time this player submits data
        if (is.null(player_data$dfs[[i]]) || nrow(player_data$dfs[[i]]) == 0) {
          player_counter(player_counter() + 1)
        }
      })
    })
  })
  
  # Launch Game
  observeEvent(input$launch_game, {
    final_data <- concat_clean(player_data)
    
    output$text <- renderPrint({
      if (nrow(final_data) > 0) {
        head(final_data)
      } else {
        "No valid data available!"
      }
    })
  })
}

# Run the app
shinyApp(ui, server)
