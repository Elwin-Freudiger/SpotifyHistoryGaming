library("shiny")
library("shinyjs")
library("dplyr")
library("httr")
library("jsonlite")
library("stringr")
library("spotifyr")


get_users_songs <- function(filtered_songs) {
  if (nrow(filtered_songs) == 0) {
    stop("No songs available to process.")
  }
  
  # Randomly sample up to 5 songs
  sampled_songs <- filtered_songs %>%
    dplyr::slice_sample(n = min(5, nrow(filtered_songs))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      search_result = list(search_songs(trackName, artistName)),
      ID = search_result$ID,
      duration = search_result$duration
    ) %>%
    dplyr::ungroup()
  
  # Select relevant columns
  return(
    sampled_songs %>%
      dplyr::select(trackName, artistName, listener, msPlayed, ID, duration)
  )
}


  
  
##The shiny app itself
songPlayerGameUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600&display=swap",
        rel = "stylesheet"
      )
    ),
    titlePanel("Song Player Game"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(ns("playlist_input"),
                       "Select the style:",
                       choices = playlists$title,
                       selected = NULL,
                       multiple = TRUE,
                       options = list(placeholder = "Select genres", maxItems = 1)
        ),
        actionButton(ns("start"), "Start Game")
      ),
      mainPanel(
        uiOutput(ns("game_ui")),
        textOutput(ns("final_score"))
      )
    )
  )
}

# Define module for "Who Among Us?"
whoAmongUsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    titlePanel("Who Among Us?"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          ns("genre_input"),
          "Select genres:",
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Select genres",
            maxItems = 10
          )
        ),
        sliderInput(ns("years"), "Select Year Range:",
                    min = 1945, max = 2024, value = c(1990, 2024), ticks = FALSE, sep = "", pre=""),
        br(), br(),
        actionButton(ns("start_game"), "Start Game"),
        textOutput(ns("warning_pool"))
      ),
      mainPanel(
        uiOutput(ns("game_ui")),
        textOutput(ns("final_score"))
      )
    )
  )
}

whoAmongUsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    updateSelectizeInput(
      session, "genre_input",
      choices = genre_list,
      selected = "All genres",
      server = TRUE
    )
    filtered_pool <- reactive({
      req(input$genre_input, input$years)
      songs_filtered(input$genre_input, input$years[1], input$years[2])
    })
    
    output$warning_pool <- renderText({
      pool <- filtered_pool()
      if (nrow(pool) < 30) {
        "Warning: Your pool has fewer than 30 songs. For optimal play, broaden your choices."
      } else {
        ""
      }
    })
    
    game_state <- reactiveValues(
      song_data = NULL,
      current_song = 1,
      correct_guesses = 0,
      is_game_over = FALSE
    )
    
    observeEvent(input$start_game, {
      pool <- filtered_pool()
      if (nrow(pool) < 5) {
        showModal(modalDialog(
          title = "Pool Too Small",
          "Your song pool is too small! Please broaden your options.",
          easyClose = TRUE
        ))
        return(NULL)
      }
      
      game_state$song_data <- get_users_songs(pool)
      game_state$current_song <- 1
      game_state$correct_guesses <- 0
      game_state$is_game_over <- FALSE
    })
    
    output$game_ui <- renderUI({
      req(game_state$song_data, !game_state$is_game_over)
      
      current_song <- game_state$song_data[game_state$current_song, ]
      
      tagList(
        h3(current_song$trackName),
        h4(paste("Artist: ", current_song$artistName)),
        HTML(paste0(
          '<iframe style="border-radius:12px" src="https://open.spotify.com/embed/track/',
          current_song$ID,
          '?utm_source=generator&theme=0" width="100%" height="352" frameBorder="0" allowfullscreen="" allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture" loading="lazy"></iframe>'
        )),
        h4("Who listened to it the most?"),
        selectInput(ns("guess"), "Select Listener:", choices = c("Ambre", "Arnaud", "Elwin", "Fanny", "Olivier")),
        actionButton(ns("submit_guess"), "Submit Guess"),
        actionButton(ns("next_song"), "Next Song", style = "display: none;")
      )
    })
    
    observeEvent(input$submit_guess, {
      req(game_state$song_data)
      current_song <- game_state$song_data[game_state$current_song, ]
      user_guess <- input$guess
      
      correct_listener <- current_song$listener
      listen_time <- current_song$msPlayed
      song_duration <- current_song$duration
      
      if (user_guess == correct_listener) {
        game_state$correct_guesses <- game_state$correct_guesses + 1
      }
      
      showModal(modalDialog(
        title = "Result",
        HTML(paste0(
          "<strong>Correct Listener:</strong> ", correct_listener, "<br>",
          "<strong>Listening Time:</strong> ", round(listen_time / 60000, 2), " minutes<br>",
          "<strong>Replay Count:</strong> ", round(listen_time / song_duration, 1), " times<br>",
          if (user_guess == correct_listener) "🎉 Correct!" else "❌ Wrong Guess."
        )),
        easyClose = TRUE
      ))
      
      shinyjs::show("next_song")
      shinyjs::disable("submit_guess")
    })
    
    observeEvent(input$next_song, {
      if (game_state$current_song < nrow(game_state$song_data)) {
        game_state$current_song <- game_state$current_song + 1
        shinyjs::hide("next_song")
        shinyjs::enable("submit_guess")
      } else {
        game_state$is_game_over <- TRUE
      }
    })
    
    output$final_score <- renderText({
      if (!game_state$is_game_over) return(NULL)
      paste("Done! Your Total Score: ", game_state$correct_guesses, "/ 5")
    })
  })
}

# Main App UI
ui <- navbarPage(
  "SPOTIFY GAMES",
  tabPanel("Song Player Game", songPlayerGameUI("songPlayerGame")),
  tabPanel("Who Among Us?", whoAmongUsUI("whoAmongUs"))
)

# Main App Server
server <- function(input, output, session) {
  songPlayerGameServer("songPlayerGame")
  whoAmongUsServer("whoAmongUs")
}

# Run the app
shinyApp(ui, server)