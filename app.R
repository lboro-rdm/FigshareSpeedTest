# Figshare download speed monitor - Shiny app
#
# Runs a FIXED test (same article, files, runs and size cap every time),
# appends every result to a CSV log, and charts speed over time and by
# time of day. Settings live in R/core.R.
#
#   install.packages(c("shiny", "httr2", "curl"))
#   shiny::runApp("figshare_speedtest_app")
#
# Speeds are measured from the machine running the app.

library(shiny)
if (!exists("run_standard_test")) source("R/core.R")   # older Shiny versions

ui <- fluidPage(
  titlePanel("Figshare download speed monitor"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("tester", "Your name / location",
                placeholder = "e.g. Sam - office wifi"),
      actionButton("run", "Run standard test", class = "btn-success"),
      hr(),
      h5("Fixed test settings"),
      tags$small(
        tags$div("Article ID: ", tags$b(CONFIG$article_id)),
        tags$div("File filter: ", tags$b(if (nzchar(CONFIG$file_match)) CONFIG$file_match else "all files")),
        tags$div("Runs per file: ", tags$b(CONFIG$runs)),
        tags$div("Cap per run: ", tags$b(if (is.na(CONFIG$max_mb)) "whole file" else paste(CONFIG$max_mb, "MB"))),
        tags$div("Log: ", tags$code(CONFIG$log_path))
      ),
      hr(),
      downloadButton("download_log", "Download log (CSV)")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Latest test", br(),
          h4(textOutput("verdict")),
          tableOutput("latest")
        ),
        tabPanel(
          "History", br(),
          selectInput("who", "Show results from", choices = "All"),
          textOutput("health"),
          plotOutput("time_plot", height = "280px"),
          plotOutput("hour_plot", height = "280px"),
          h5("Recent tests"),
          tableOutput("sessions")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Re-reads the log whenever the file changes (including scheduled runs).
  log_data <- reactivePoll(
    3000, session,
    checkFunc = function() {
      if (file.exists(CONFIG$log_path)) as.numeric(file.mtime(CONFIG$log_path)) else 0
    },
    valueFunc = function() read_log(CONFIG$log_path)
  )
  
  latest  <- reactiveVal(NULL)
  verdict <- reactiveVal("Click 'Run standard test' to begin.")
  
  observeEvent(input$run, {
    prog <- Progress$new(session, min = 0, max = 1)
    prog$set(message = "Running standard test...", value = 0)
    on.exit(prog$close(), add = TRUE)
    
    res <- run_standard_test(
      tester = trimws(input$tester),
      progress = function(i, n, detail) prog$set(value = (i - 1) / n, detail = detail)
    )
    append_log(res)
    latest(res)
    verdict(describe_run(res, read_log(CONFIG$log_path)))
  })
  
  output$verdict <- renderText(verdict())
  
  output$latest <- renderTable({
    req(latest())
    latest()[, c("local_time", "file", "run", "status", "MB_per_s",
                 "Mbit_per_s", "ttfb_ms", "seconds", "note")]
  }, digits = 2)
  
  # Filter dropdown follows whoever appears in the log
  observe({
    lg <- log_data()
    testers <- unique(lg$tester)
    testers <- sort(testers[nzchar(testers)])
    updateSelectInput(session, "who", choices = c("All", testers),
                      selected = isolate(input$who))
  })
  
  filtered <- reactive({
    lg <- log_data()
    if (!is.null(input$who) && input$who != "All") lg <- lg[lg$tester == input$who, ]
    lg
  })
  
  sessions <- reactive(session_summary(filtered()))
  
  output$health <- renderText({
    lg <- filtered()
    n_tests <- nrow(unique(lg[, c("timestamp_utc", "tester")]))
    n_fail  <- sum(lg$status == "error")
    sprintf("%d test(s) logged, %d failed download(s).", n_tests, n_fail)
  })
  
  output$time_plot <- renderPlot({
    s <- sessions()
    validate(need(NROW(s) > 0, "No successful tests logged yet."))
    testers <- unique(s$tester)
    palette <- c("#4C78A8", "#F58518", "#54A24B", "#E45756", "#B279A2", "#72B7B2")
    cols <- palette[((seq_along(testers) - 1) %% length(palette)) + 1]
    plot(s$time, s$MB_per_s, type = "n", xlab = "", ylab = "MB/s",
         ylim = c(0, max(s$MB_per_s) * 1.1),
         main = "Download speed over time (dashed = median)")
    for (j in seq_along(testers)) {
      d <- s[s$tester == testers[j], ]
      points(d$time, d$MB_per_s, pch = 19, col = cols[j])
      if (length(testers) == 1) lines(d$time, d$MB_per_s, col = cols[j])
    }
    abline(h = median(s$MB_per_s), lty = 2, col = "grey40")
    if (length(testers) > 1) {
      legend("topright", legend = ifelse(nzchar(testers), testers, "(no name)"),
             col = cols, pch = 19, bty = "n")
    }
  })
  
  output$hour_plot <- renderPlot({
    s <- sessions()
    validate(need(NROW(s) > 0, "No successful tests logged yet."))
    boxplot(MB_per_s ~ local_hour, data = s, col = "#c6dbef", border = "grey40",
            xlab = "Hour of day (local time of whoever ran the test)", ylab = "MB/s",
            main = "Download speed by time of day")
    stripchart(MB_per_s ~ local_hour, data = s, vertical = TRUE, add = TRUE,
               method = "jitter", pch = 19, col = "#4C78A8")
  })
  
  output$sessions <- renderTable({
    s <- sessions()
    req(NROW(s) > 0)
    s <- s[order(s$time, decreasing = TRUE), ]
    s <- head(s, 20)
    s[, c("local_time", "tester", "file", "n_runs", "MB_per_s", "Mbit_per_s", "ttfb_ms")]
  }, digits = 2)
  
  output$download_log <- downloadHandler(
    filename = function() paste0("figshare_speedtest_log_", Sys.Date(), ".csv"),
    content = function(file) {
      if (file.exists(CONFIG$log_path)) {
        file.copy(CONFIG$log_path, file)
      } else {
        writeLines(paste(LOG_COLUMNS, collapse = ","), file)
      }
    }
  )
}

shinyApp(ui, server)