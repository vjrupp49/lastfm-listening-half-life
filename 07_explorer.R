# ==========================================================
# 07_explorer.R  --  click-to-drill-down Shiny app
# Run with:  source("07_explorer.R")
# Click any point in the top chart to see that artist's real
# monthly history with the peak and half-life marked.
# ==========================================================

source("00_config.R")
library(shiny)
library(plotly)
library(ggplot2)

bundle       <- readRDS(MONTHLY_FILE)
artist_month <- bundle$artist_month
hl           <- readRDS(file.path(RESULTS_DIR, "half_lives.rds"))
series       <- split(artist_month, artist_month$artist)

roll_mean <- function(x, w = SMOOTH_WIDTH) {
  n <- length(x); half <- (w - 1) %/% 2
  vapply(seq_len(n), function(i) mean(x[max(1, i - half):min(n, i + half)]), numeric(1))
}

pal <- c("Supernova" = "#E74C3C", "Steady" = "#E39A2B",
         "Slow burn" = "#2E86C1", "Evergreen" = "#1E8449")

pool <- hl[hl$status %in% c("decayed", "still burning"), ]
pool$plot_hl <- ifelse(is.na(pool$half_life_months),
                       pool$months_after_peak, pool$half_life_months)

ui <- fluidPage(
  titlePanel("Listening Half-Life Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("minplays", "Minimum lifetime plays",
                  min = MIN_PLAYS, max = max(pool$total_plays),
                  value = MIN_PLAYS, step = 10),
      checkboxGroupInput("chars", "Show",
                         choices  = names(pal),
                         selected = names(pal)),
      selectizeInput("artist", "Or jump straight to an artist",
                     choices = sort(pool$artist), selected = NULL,
                     options = list(placeholder = "type a name...")),
      hr(),
      helpText("Click any bubble above to inspect that artist below."),
      hr(),
      htmlOutput("stats")
    ),
    mainPanel(
      width = 9,
      plotlyOutput("map", height = "420px"),
      hr(),
      plotOutput("detail", height = "330px"),
      htmlOutput("detail_text")
    )
  )
)

server <- function(input, output, session) {
  
  filtered <- reactive({
    d <- pool[pool$total_plays >= input$minplays & pool$character %in% input$chars, ]
    d$hover <- paste0("<b>", d$artist, "</b><br>peaked ",
                      format(d$peak_month, "%b %Y"), "<br>",
                      d$character, ": ", d$plot_hl, " months<br>",
                      d$total_plays, " plays")
    d
  })
  
  output$map <- renderPlotly({
    d <- filtered()
    validate(need(nrow(d) > 0, "No artists match those filters."))
    plot_ly(d, x = ~peak_intensity, y = ~plot_hl, customdata = ~artist,
            color = ~character, colors = pal, size = ~total_plays,
            sizes = c(60, 1000), type = "scatter", mode = "markers",
            marker = list(opacity = 0.75, line = list(width = 0.5, color = "white")),
            hoverinfo = "text", text = ~hover, source = "map") |>
      layout(xaxis = list(title = "Peak intensity (plays/month)", type = "log"),
             yaxis = list(title = "Months held above half"),
             hovermode = "closest") |>
      event_register("plotly_click")
  })
  
  selected <- reactiveVal(NULL)
  
  observeEvent(event_data("plotly_click", source = "map"), {
    ev <- event_data("plotly_click", source = "map")
    if (!is.null(ev$customdata)) selected(as.character(ev$customdata))
  })
  observeEvent(input$artist, {
    if (nzchar(input$artist)) selected(input$artist)
  })
  
  output$detail <- renderPlot({
    a <- selected()
    validate(need(!is.null(a), "Click a bubble, or pick an artist from the sidebar."))
    d  <- series[[a]][order(series[[a]]$month), ]
    d$smooth <- roll_mean(d$plays)
    pk  <- which.max(d$smooth)
    row <- hl[hl$artist == a, ][1, ]
    thr <- 0.5 * d$smooth[pk]
    
    g <- ggplot(d, aes(month, plays)) +
      geom_col(fill = "grey85") +
      geom_line(aes(y = smooth), colour = "#2C3E50", linewidth = 1) +
      geom_vline(xintercept = as.numeric(d$month[pk]),
                 colour = "#E74C3C", linetype = "dashed") +
      geom_hline(yintercept = thr, colour = "grey40", linetype = "dotted") +
      annotate("text", x = d$month[pk], y = max(d$plays),
               label = " peak", hjust = 0, colour = "#E74C3C", size = 3.5) +
      labs(title = a, x = NULL, y = "Plays per month") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank())
    
    if (!is.na(row$half_life_months)) {
      hm <- d$month[min(nrow(d), pk + row$half_life_months)]
      g <- g + geom_vline(xintercept = as.numeric(hm),
                          colour = "#2E86C1", linewidth = 1) +
        annotate("text", x = hm, y = max(d$plays) * 0.9,
                 label = " half-life", hjust = 0, colour = "#2E86C1", size = 3.5)
    }
    g
  })
  
  output$detail_text <- renderUI({
    a <- selected(); if (is.null(a)) return(NULL)
    r <- hl[hl$artist == a, ][1, ]
    HTML(paste0(
      "<p><b>", a, "</b> &mdash; ", r$total_plays, " plays since ",
      format(r$first_play, "%B %Y"), ". Peaked ", format(r$peak_month, "%B %Y"),
      " at ", r$peak_intensity, " plays/month. Classified <b>", r$character, "</b>",
      ifelse(is.na(r$half_life_months),
             paste0(", still above half after ", r$months_after_peak, " months."),
             paste0(", half-life ", r$half_life_months, " months.")), "</p>"))
  })
  
  output$stats <- renderUI({
    d <- filtered()
    sc <- d[d$status == "decayed", ]
    HTML(paste0("<b>", nrow(d), "</b> artists shown<br>",
                "median half-life: <b>",
                ifelse(nrow(sc) > 0, median(sc$half_life_months), "-"),
                "</b> months<br>evergreens: <b>",
                sum(d$character == "Evergreen"), "</b>"))
  })
}

shiny::runApp(shiny::shinyApp(ui, server), launch.browser = TRUE)

