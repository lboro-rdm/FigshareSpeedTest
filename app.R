# Figshare item-creation speed monitor - Shiny app
#
# Runs a FIXED test (creates a draft titled "Speed Test", times it, then
# deletes it). Results for the current session are shown in a table laid out
# exactly like the log CSV, with a button that copies the rows (tab-separated)
# so they can be pasted straight into Excel.
#
# The History tab reads a shared Excel file on OneDrive/SharePoint
# (CONFIG$log_url), or the local CSV at CONFIG$log_path if no URL is set. The
# app only reads it; the team pastes the copied rows into the file themselves.
#
#   install.packages(c("shiny", "curl", "jsonlite", "readxl"))
#   Sys.setenv(APIkey = "your-personal-token")
#   shiny::runApp("figshare_speedtest_app")
#
# Times are measured from the machine running the app. Lower = faster.

library(shiny)
if (!exists("run_standard_test")) source("R/core.R")   # older Shiny versions

copy_js <- "
Shiny.addCustomMessageHandler('copy_text', function(text) {
  function fallback() {
    var ta = document.createElement('textarea');
    ta.value = text; document.body.appendChild(ta); ta.select();
    try { document.execCommand('copy'); } catch (e) {}
    document.body.removeChild(ta);
  }
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(text).catch(fallback);
  } else { fallback(); }
});
"

sheet_url <- "https://lunet-my.sharepoint.com/:x:/r/personal/lbls2_lunet_lboro_ac_uk/Documents/304_RShiny/Lookups/FigshareSpeedTest.xlsx?d=wd2bc3756d9ee429ab038c63eebf1dca7&csf=1&web=1&e=xk6SGF"

ui <- fluidPage(
  tags$head(tags$script(HTML(copy_js))),
  titlePanel("Figshare item-creation speed monitor"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      p("The information you include here is not secure. Do not include your real name or detailed location."),
      textInput("name", "Alias", placeholder = "e.g. Octopus"),
      textInput("location", "Location", placeholder = "e.g. Library, Not library"),
      actionButton("run", "Run standard test", class = "btn-success"),
      if (is.null(CONFIG$token)) {
        tags$div(class = "text-danger", style = "margin-top:8px;",
                 tags$small("No token found (APIkey) - tests will fail."))
      },
      hr(),
      h5("Fixed test settings"),
      tags$small(
        tags$div("Draft title: ", tags$b(CONFIG$title)),
        tags$div("Drafts per test: ", tags$b(CONFIG$runs)),
        tags$div("Delete after timing: ", tags$b(if (CONFIG$cleanup) "yes" else "no"))
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Results", br(),
          h4(textOutput("verdict")),
          tags$style(".copy-bar .form-group { margin-bottom: 0; }"),
          div(
            class = "copy-bar",
            style = "display:flex; flex-wrap:wrap; align-items:center; gap:10px; margin-bottom:8px;",
            actionButton("copy", "Copy rows for Excel", icon = icon("copy"), class = "btn-primary"),
            actionButton("clear", "Clear table", icon = icon("trash")),
            checkboxInput("header", "Include header row", value = FALSE, width = "auto")
          ),
          tags$p(
            class = "text-muted",
            style = "margin-bottom:16px;",
            "Columns match the log file. Paste the copied rows into the ",
            tags$a("shared results spreadsheet", href = sheet_url,
                   target = "_blank", rel = "noopener"),
            " (team access only)."
          ),
          tableOutput("results")
        ),
        tabPanel(
          "History", br(),
          tags$p(class = "text-muted",
                 if (nzchar(CONFIG$log_url)) "Based on the shared Excel file, not on tests run in this session."
                 else "Based on the bundled log file, not on tests run in this session."),
          actionButton("refresh", "Refresh history"),
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
  
  # Bundled log, read once per session.
  log_data <- reactive({
    input$refresh   # re-read when the Refresh button is clicked
    withCallingHandlers(
      read_log(),
      warning = function(w) {
        showNotification(conditionMessage(w), type = "warning", duration = 10)
        invokeRestart("muffleWarning")
      }
    )
  })
  
  results <- reactiveVal(NULL)   # every row from tests run in this session
  verdict <- reactiveVal("Enter your name and location, then click 'Run standard test'.")
  
  observeEvent(input$run, {
    prog <- Progress$new(session, min = 0, max = 1)
    prog$set(message = "Running standard test...", value = 0)
    on.exit(prog$close(), add = TRUE)
    
    res <- run_standard_test(
      name = trimws(input$name),
      location = trimws(input$location),
      progress = function(i, n, detail) prog$set(value = (i - 1) / n, detail = detail)
    )
    results(rbind(results(), res))
    verdict(describe_run(res, isolate(log_data())))
  })
  
  observeEvent(input$clear, {
    results(NULL)
    verdict("Table cleared.")
  })
  
  output$verdict <- renderText(verdict())
  
  # Same columns and order as the log CSV; oldest first so pasting keeps order.
  output$results <- renderTable({
    shiny::validate(shiny::need(!is.null(results()), "No tests run in this session yet."))
    results()[, LOG_COLUMNS]
  }, digits = 2, na = "")
  
  observeEvent(input$copy, {
    req(results())
    lines <- capture.output(
      write.table(results()[, LOG_COLUMNS], sep = "\t", quote = FALSE,
                  row.names = FALSE, col.names = isTRUE(input$header), na = "")
    )
    session$sendCustomMessage("copy_text", paste(lines, collapse = "\n"))
    showNotification(sprintf("Copied %d row(s) - paste into Excel.", nrow(results())),
                     type = "message", duration = 4)
  })
  
  # ---- History (bundled log) -------------------------------------------------
  
  observe({
    lg <- log_data()
    people <- sort(unique(lg$who[nzchar(lg$who)]))
    updateSelectInput(session, "who", choices = c("All", people),
                      selected = isolate(input$who))
  })
  
  filtered <- reactive({
    lg <- log_data()
    if (!is.null(input$who) && input$who != "All") lg <- lg[lg$who == input$who, ]
    lg
  })
  
  sessions <- reactive(session_summary(filtered()))
  
  output$health <- renderText({
    lg <- filtered()
    n_tests <- nrow(unique(lg[, c("timestamp_utc", "who")]))
    n_fail  <- sum(lg$status == "error")
    sprintf("%d test(s) logged, %d failed creation(s).", n_tests, n_fail)
  })
  
  output$time_plot <- renderPlot({
    s <- sessions()
    shiny::validate(shiny::need(NROW(s) > 0, "No successful tests logged yet."))
    people <- unique(s$who)
    palette <- c("#4C78A8", "#F58518", "#54A24B", "#E45756", "#B279A2", "#72B7B2")
    cols <- palette[((seq_along(people) - 1) %% length(palette)) + 1]
    plot(s$time, s$create_s, type = "n", xlab = "", ylab = "Seconds to create draft",
         ylim = c(0, max(s$create_s) * 1.1),
         main = "Item creation time over time (dashed = median; lower is faster)")
    for (j in seq_along(people)) {
      d <- s[s$who == people[j], ]
      points(d$time, d$create_s, pch = 19, col = cols[j])
      if (length(people) == 1) lines(d$time, d$create_s, col = cols[j])
    }
    abline(h = median(s$create_s), lty = 2, col = "grey40")
    if (length(people) > 1) {
      legend("topright", legend = ifelse(nzchar(people), people, "(no name)"),
             col = cols, pch = 19, bty = "n")
    }
  })
  
  output$hour_plot <- renderPlot({
    s <- sessions()
    shiny::validate(shiny::need(NROW(s) > 0, "No successful tests logged yet."))
    boxplot(create_s ~ local_hour, data = s, col = "#c6dbef", border = "grey40",
            xlab = paste0("Hour of day (", CONFIG$timezone, ")"),
            ylab = "Seconds to create draft",
            main = "Item creation time by time of day")
    stripchart(create_s ~ local_hour, data = s, vertical = TRUE, add = TRUE,
               method = "jitter", pch = 19, col = "#4C78A8")
  })
  
  output$sessions <- renderTable({
    s <- sessions()
    req(NROW(s) > 0)
    s <- s[order(s$time, decreasing = TRUE), ]
    s <- head(s, 20)
    s[, c("local_time", "who", "n_runs", "create_s", "ttfb_ms", "delete_s")]
  }, digits = 2)
}

shinyApp(ui, server)