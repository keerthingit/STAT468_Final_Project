library(shiny)
library(bslib)
library(ggplot2)
library(DBI)
library(duckdb)
library(vetiver)
library(tibble)

{
  con <- dbConnect(duckdb::duckdb())
  dbExecute(con, "INSTALL httpfs;")
  dbExecute(con, "LOAD httpfs;")
  dbExecute(con, sprintf("SET s3_region='%s';", Sys.getenv("AWS_DEFAULT_REGION")))
  
  final_stroke_data <- dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_csv_auto('s3://%s/%s/final_stroke_data.csv')",
      Sys.getenv("S3_BUCKET"),
      Sys.getenv("S3_PREFIX")
    )
  )
  
  opponents     <- sort(unique(final_stroke_data$opponent))
  rally_lengths <- suppressWarnings(as.integer(final_stroke_data$rally_length))
  rl_min        <- min(rally_lengths, na.rm = TRUE)
  rl_max        <- max(rally_lengths, na.rm = TRUE)
  shot_types    <- sort(unique(na.omit(as.character(final_stroke_data$shot_type))))
  
  dbDisconnect(con, shutdown = TRUE)
}


ui <- page_sidebar(
  theme = bs_theme(bootswatch = "darkly"),
  title = "WinningRally",
  
  sidebar = sidebar(
    collapsible = FALSE,
    width = 680,
    
    # Player name
    tags$h5(
      HTML('Player <span style="margin: 0 8px;">></span> Chou Tien Chen'),
      style = "color:white; font-size: 16px; padding-left: 15px;"
    ),
    # Player profile photo
    tags$img(
      src = "chou_profile_pic.jpg",
      width = "60px",
      style = "padding-left: 15px;"
    ),
    
    # Opponent dropdown
    div(
      style = "padding-left: 15px; padding-right: 15px;",
      selectInput(
        inputId = "opponent",
        label   = "Opponent",
        choices = opponents,
        width   = "100%"
      )
    ),
    
    # Rally length slider
    div(
      style = "padding-left: 15px; padding-right: 15px;",
      sliderInput(
        inputId = "rally_length",
        label   = tags$span("Rally length", style = "color: #fff;"),
        min     = rl_min,
        max     = rl_max,
        value   = rl_min,
        step    = 1,
        width   = "100%"
      )
    ),
    
    # Final shot type
    div(
      style = "padding-left: 15px; padding-right: 15px;",
      selectInput(
        inputId = "shot_type",
        label   = tags$span("Final shot type", style = "color: #fff;"),
        choices = shot_types,
        width   = "100%"
      )
    ),
    
    # Dynamic position sliders (CHOU + OPP)
    div(
      style = "padding-left: 15px; padding-right: 15px;",
      uiOutput("player_ui")
    ),
    
    # Selection summary
    tags$hr(),
    div(
      style = "padding-left: 15px; padding-right: 15px; color: #ddd;",
      h5("Current selection"),
      verbatimTextOutput("sel_summary", placeholder = TRUE)
    ),
    
    # Predictions
    # tags$hr(),
    # div(
    #   style = "padding-left: 15px; padding-right: 15px; color: #ddd;",
    #   h5("Model prediction"),
    #   verbatimTextOutput("pred_out", placeholder = TRUE),
    #   verbatimTextOutput("pred_prob", placeholder = TRUE)
    # )
  ),
  
  tags$head(
    tags$style(HTML("
      .navbar-brand { padding-left: 15px; }
    "))
  ),
  
  plotOutput("badminton_court", height = "70vh")
)


server <- function(input, output, session) {
  
  
  con <- dbConnect(duckdb::duckdb())
  onStop(function() dbDisconnect(con, shutdown = TRUE))
  dbExecute(con, "INSTALL httpfs;")
  dbExecute(con, "LOAD httpfs;")
  dbExecute(con, sprintf("SET s3_region='%s';", Sys.getenv("AWS_DEFAULT_REGION")))
  
  # court coordinates from S3
  court_coordinates <- dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM read_csv_auto('s3://%s/%s/court_coordinates.csv')",
      Sys.getenv("S3_BUCKET"),
      Sys.getenv("S3_PREFIX")
    )
  )
  
  court_x_min <- court_coordinates$downleft_x
  court_x_max <- court_coordinates$downright_x
  court_y_min <- court_coordinates$upleft_y
  court_y_max <- court_coordinates$downleft_y
  
  # Fractions per official dimensions
  court_frac_net_y            <- 0.50
  court_frac_short_service1_y <- 118/335
  court_frac_short_service2_y <- 217/335
  court_frac_singles_left_x   <- 23/305
  court_frac_singles_right_x  <- 282/305
  
  # (fraction -> coordinate)
  court_x_at_fraction <- function(fw) court_x_min + fw * (court_x_max - court_x_min)
  court_y_at_fraction <- function(fl) court_y_min + fl * (court_y_max - court_y_min)
  
  # bounds inside singles sidelines
  x_min <- court_x_at_fraction(court_frac_singles_left_x)
  x_max <- court_x_at_fraction(court_frac_singles_right_x)
  y_net <- court_y_at_fraction(court_frac_net_y)
  
  x_min_i <- ceiling(x_min)
  x_max_i <- floor(x_max)
  
  # CHOU y from bottom to (net - 1)
  y_min_i <- ceiling(court_y_min)
  y_max_i <- floor(y_net - 1)
  if (y_max_i < y_min_i) y_max_i <- y_min_i
  
  # OPP y from (net + 1) to top
  y_top_min_i <- ceiling(y_net + 1)
  y_top_max_i <- floor(court_y_max)
  if (y_top_min_i > y_top_max_i) y_top_min_i <- y_top_max_i
  
  # Default
  x_ctr     <- round((x_min_i + x_max_i) / 2)
  y_ctr     <- round((y_min_i + y_max_i) / 2)
  x_ctr_top <- x_ctr
  y_ctr_top <- round((y_top_min_i + y_top_max_i) / 2)
  
  # overlay
  output$player_ui <- renderUI({
    tagList(
      tags$hr(),
      tags$h5(tags$span("Player (CHOU) position", style = "color: #fff;")),
      sliderInput(
        "player_x", "X (left → right)",
        min = x_min_i, max = x_max_i, value = x_ctr, step = 1, width = "100%"
      ),
      sliderInput(
        "player_y", "Y (bottom → net)",
        min = y_min_i, max = y_max_i, value = y_ctr, step = 1, width = "100%"
      ),
      tags$hr(),
      tags$h5(tags$span("Opponent (OPP) position", style = "color: #fff;")),
      sliderInput(
        "opp_x", "X (left → right)",
        min = x_min_i, max = x_max_i, value = x_ctr_top, step = 1, width = "100%"
      ),
      sliderInput(
        "opp_y", "Y (net → top)",
        min = y_top_min_i, max = y_top_max_i, value = y_ctr_top, step = 1, width = "100%"
      )
    )
  })
  
  # built court with CHOU nd OPP
  build_court <- function(px = NULL, py = NULL, ox = NULL, oy = NULL) {
    base <- ggplot() +
      # Outer court
      geom_rect(
        aes(xmin = court_x_min, xmax = court_x_max,
            ymin = court_y_min, ymax = court_y_max),
        fill = NA, colour = "white", linewidth = 0.5
      ) +
      # Net
      geom_segment(
        aes(x = court_x_min, xend = court_x_max,
            y = y_net, yend = y_net),
        linewidth = 0.5, linetype = "longdash", colour = "white"
      ) +
      # Short service lines
      geom_segment(
        aes(x = court_x_min, xend = court_x_max,
            y = court_y_at_fraction(court_frac_short_service1_y),
            yend = court_y_at_fraction(court_frac_short_service1_y)),
        linewidth = 0.5, colour = "white"
      ) +
      geom_segment(
        aes(x = court_x_min, xend = court_x_max,
            y = court_y_at_fraction(court_frac_short_service2_y),
            yend = court_y_at_fraction(court_frac_short_service2_y)),
        linewidth = 0.5, colour = "white"
      ) +
      # Singles sidelines
      geom_segment(
        aes(x = court_x_at_fraction(court_frac_singles_left_x),
            xend = court_x_at_fraction(court_frac_singles_left_x),
            y = court_y_min, yend = court_y_max),
        linewidth = 0.5, colour = "white"
      ) +
      geom_segment(
        aes(x = court_x_at_fraction(court_frac_singles_right_x),
            xend = court_x_at_fraction(court_frac_singles_right_x),
            y = court_y_min, yend = court_y_max),
        linewidth = 0.5, colour = "white"
      ) +
      coord_fixed() +
      theme_void()
    
    # CHOU 
    if (!is.null(px) && !is.null(py)) {
      base <- base +
        geom_point(aes(x = px, y = py), size = 3.5, shape = 21,
                   stroke = 1, fill = "cyan", colour = "black") +
        geom_text(aes(x = px, y = py, label = "CHOU"),
                  vjust = -1.1, colour = "cyan", size = 4)
    }
    
    # OPP 
    if (!is.null(ox) && !is.null(oy)) {
      base <- base +
        geom_point(aes(x = ox, y = oy), size = 3.5, shape = 21,
                   stroke = 1, fill = "magenta", colour = "black") +
        geom_text(aes(x = ox, y = oy, label = "OPP"),
                  vjust = -1.1, colour = "magenta", size = 4)
    }
    
    base
  }
  
  output$badminton_court <- renderPlot({
    build_court(input$player_x, input$player_y, input$opp_x, input$opp_y)
  }, bg = "transparent")
  
  
  output$sel_summary <- renderText({
    paste0(
      "Opponent: ", input$opponent, "\n",
      "Rally length: ", input$rally_length, "\n",
      "Final shot type: ", input$shot_type, "\n",
      "CHOU (x, y): (", input$player_x, ", ", input$player_y, ")\n",
      "OPP  (x, y): (", input$opp_x, ", ", input$opp_y, ")"
    )
  })
  
  # api_base <- Sys.getenv("PREDICT_API_URL", unset = "http://3.139.64.118:8000")
  # ep <- vetiver_endpoint(api_base)
  # 
  # newdata <- reactive({
  #   req(input$shot_type, input$opponent, input$rally_length)
  #   tibble(
  #     shot_type          = as.character(input$shot_type),
  #     opponent           = as.character(input$opponent),
  #     chou_shuttle_paral = as.numeric(473),   # fixed value per requirement
  #     rally_length       = as.integer(input$rally_length)
  #   )
  # })
  # 
  # # idk why its not workin
  # output$pred_out <- renderPrint({
  #   tryCatch(
  #     predict(ep, newdata()),
  #     error = function(e) paste("Prediction error:", conditionMessage(e))
  #   )
  # })
  # 
  # output$pred_prob <- renderPrint({
  #   tryCatch(
  #     predict(ep, newdata(), type = "prob"),
  #     error = function(e) "(probabilities unavailable or model is regression)"
  #   )
  # })
}

shinyApp(ui, server)
