# Shared code for the Figshare speed monitor (used by app.R and scheduled_test.R).
# Shiny sources everything in R/ automatically.
#
# Requires: install.packages(c("shiny", "httr2", "curl"))

library(httr2)

FIGSHARE_API <- "https://api.figshare.com/v2"

# ---- Fixed test settings ---------------------------------------------------
# Keep these constant so results stay comparable over time. Each logged row
# records the article, file and max_mb used, so any change is visible later.
# Override without editing this file via environment variables.
CONFIG <- list(
  article_id = Sys.getenv("FIGSHARE_ARTICLE_ID", "34544658"),
  file_match = Sys.getenv("FIGSHARE_FILE_MATCH", ""),  # regex on file name; "" = all files
  runs       = 3L,                                      # downloads per file per test
  max_mb     = 50,                                      # cap per download; NA_real_ = whole file
  log_path   = Sys.getenv("FIGSHARE_LOG", "speedtest_log.csv"),
  timezone   = Sys.getenv("FIGSHARE_TZ", "Europe/London"),  # used for local_time / hour of day
  token      = {
    t <- Sys.getenv("FIGSHARE_TOKEN", "")
    if (nzchar(t)) t else NULL
  }
)

LOG_COLUMNS <- c(
  "timestamp_utc", "local_time", "local_hour", "weekday", "tz", "tester",
  "article_id", "file", "file_size_mb", "max_mb", "run", "status",
  "bytes", "seconds", "ttfb_ms", "MB_per_s", "Mbit_per_s", "note"
)

# ---- Figshare access -------------------------------------------------------

get_article <- function(article_id, token = NULL) {
  endpoints <- sprintf("%s/articles/%s", FIGSHARE_API, article_id)
  if (!is.null(token)) {
    endpoints <- c(endpoints, sprintf("%s/account/articles/%s", FIGSHARE_API, article_id))
  }
  for (endpoint in endpoints) {
    req <- request(endpoint) |>
      req_timeout(30) |>
      req_error(is_error = function(resp) FALSE)
    if (!is.null(token)) req <- req_headers(req, Authorization = paste("token", token))
    resp <- req_perform(req)
    if (resp_status(resp) == 200) {
      data <- resp_body_json(resp)
      return(list(title = data$title, files = data$files))
    }
  }
  stop("Figshare returned no article for ID ", article_id, call. = FALSE)
}

# Stream a file and discard the data. Returns list(ttfb, total, bytes).
time_download <- function(url, token = NULL, max_bytes = NULL) {
  h <- curl::new_handle(followlocation = TRUE, connecttimeout = 30)
  hdrs <- list()
  if (!is.null(token)) hdrs$Authorization <- paste("token", token)
  if (!is.null(max_bytes)) hdrs$Range <- sprintf("bytes=0-%.0f", max_bytes - 1)
  if (length(hdrs)) do.call(curl::handle_setheaders, c(list(h), hdrs))
  
  start <- proc.time()[["elapsed"]]
  con <- curl::curl(url, handle = h)
  open(con, "rb")
  on.exit(close(con), add = TRUE)
  
  first <- NULL
  total <- 0
  repeat {
    chunk <- readBin(con, "raw", 256 * 1024)
    if (length(chunk) == 0) break
    if (is.null(first)) first <- proc.time()[["elapsed"]]
    total <- total + length(chunk)
    if (!is.null(max_bytes) && total >= max_bytes) break
  }
  end <- proc.time()[["elapsed"]]
  
  list(ttfb = (if (is.null(first)) end else first) - start,
       total = end - start,
       bytes = total)
}

# ---- Running the standard test ---------------------------------------------

# Runs the fixed test and returns one row per download (failures included).
# `progress(i, n, detail)` is an optional callback for UIs.
run_standard_test <- function(tester = "", progress = function(i, n, detail) NULL) {
  now     <- Sys.time()
  ts_utc  <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ts_loc  <- format(now, "%Y-%m-%d %H:%M:%S", tz = CONFIG$timezone)
  hr      <- as.integer(format(now, "%H", tz = CONFIG$timezone))
  wd      <- format(now, "%a", tz = CONFIG$timezone)
  tzn     <- format(now, "%Z", tz = CONFIG$timezone)
  
  make_row <- function(file = NA_character_, size_mb = NA_real_, run = NA_integer_,
                       status = "ok", bytes = NA_real_, seconds = NA_real_,
                       ttfb_ms = NA_real_, note = "") {
    mbs <- if (!is.na(seconds) && seconds > 0) (bytes / 1024^2) / seconds else NA_real_
    data.frame(
      timestamp_utc = ts_utc, local_time = ts_loc, local_hour = hr, weekday = wd,
      tz = tzn, tester = tester, article_id = CONFIG$article_id, file = file,
      file_size_mb = round(size_mb, 3), max_mb = CONFIG$max_mb, run = run,
      status = status, bytes = bytes, seconds = round(seconds, 3),
      ttfb_ms = round(ttfb_ms), MB_per_s = round(mbs, 3),
      Mbit_per_s = round(mbs * 8 * 1024^2 / 1e6, 2), note = note,
      stringsAsFactors = FALSE
    )[, LOG_COLUMNS]
  }
  
  art <- tryCatch(get_article(CONFIG$article_id, CONFIG$token), error = function(e) e)
  if (inherits(art, "error")) {
    return(make_row(status = "error", note = paste("Article lookup failed:", conditionMessage(art))))
  }
  
  files <- art$files
  if (nzchar(CONFIG$file_match)) {
    keep <- vapply(files, function(f) grepl(CONFIG$file_match, f$name, ignore.case = TRUE), logical(1))
    files <- files[keep]
  }
  if (length(files) == 0) {
    return(make_row(status = "error", note = "No matching files in article"))
  }
  
  max_bytes <- if (is.na(CONFIG$max_mb)) NULL else CONFIG$max_mb * 1024^2
  n <- length(files) * CONFIG$runs
  k <- 0
  rows <- list()
  
  for (f in files) {
    size_mb <- if (is.null(f$size)) NA_real_ else f$size / 1024^2
    for (i in seq_len(CONFIG$runs)) {
      k <- k + 1
      progress(k, n, sprintf("%s (run %d/%d)", f$name, i, CONFIG$runs))
      r <- tryCatch(time_download(f$download_url, CONFIG$token, max_bytes),
                    error = function(e) e)
      rows[[k]] <- if (inherits(r, "error")) {
        make_row(f$name, size_mb, i, status = "error", note = conditionMessage(r))
      } else {
        make_row(f$name, size_mb, i, bytes = r$bytes, seconds = r$total,
                 ttfb_ms = r$ttfb * 1000)
      }
    }
  }
  do.call(rbind, rows)
}

# ---- Log -------------------------------------------------------------------

append_log <- function(df, path = CONFIG$log_path) {
  exists <- file.exists(path)
  write.table(df[, LOG_COLUMNS], path, sep = ",", row.names = FALSE,
              col.names = !exists, append = exists, qmethod = "double")
  invisible(path)
}

read_log <- function(path = CONFIG$log_path) {
  if (!file.exists(path)) {
    log <- setNames(as.data.frame(matrix(ncol = length(LOG_COLUMNS), nrow = 0)), LOG_COLUMNS)
  } else {
    log <- read.csv(path, stringsAsFactors = FALSE)
  }
  # Columns that are entirely blank get read as logical NA; make them text.
  for (col in c("tester", "note")) {
    log[[col]] <- ifelse(is.na(log[[col]]), "", as.character(log[[col]]))
  }
  t_utc <- as.POSIXct(as.character(log$timestamp_utc),
                      format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  # Always derive local time from the UTC timestamp, so rows logged with the
  # wrong time zone are shown correctly too.
  log$local_time <- format(t_utc, "%Y-%m-%d %H:%M:%S", tz = CONFIG$timezone)
  log$local_hour <- as.integer(format(t_utc, "%H", tz = CONFIG$timezone))
  log$weekday    <- format(t_utc, "%a", tz = CONFIG$timezone)
  log$tz         <- format(t_utc, "%Z", tz = CONFIG$timezone)
  log$time <- t_utc
  attr(log$time, "tzone") <- CONFIG$timezone
  log
}

# Optional one-off: rewrite the local_time / local_hour / weekday / tz columns
# stored in the CSV itself (a backup is saved alongside). The app already
# displays corrected values without this.
repair_log <- function(path = CONFIG$log_path) {
  stopifnot(file.exists(path))
  file.copy(path, paste0(path, ".bak"), overwrite = TRUE)
  log <- read_log(path)
  write.table(log[, LOG_COLUMNS], path, sep = ",", row.names = FALSE,
              col.names = TRUE, qmethod = "double")
  invisible(path)
}

# One row per test x file x tester (average of the runs in that test).
session_summary <- function(log) {
  ok <- log[log$status == "ok" & !is.na(log$MB_per_s), , drop = FALSE]
  if (nrow(ok) == 0) return(NULL)
  key <- paste(ok$timestamp_utc, ok$tester, ok$file, sep = "|")
  parts <- lapply(split(ok, key), function(d) {
    data.frame(
      time = d$time[1], local_time = d$local_time[1], local_hour = d$local_hour[1],
      tester = d$tester[1], file = d$file[1], n_runs = nrow(d),
      MB_per_s = mean(d$MB_per_s), Mbit_per_s = mean(d$Mbit_per_s),
      ttfb_ms = mean(d$ttfb_ms), stringsAsFactors = FALSE
    )
  })
  s <- do.call(rbind, parts)
  s[order(s$time), ]
}

# Plain-English comparison of a just-finished test against the log history.
describe_run <- function(new_rows, log) {
  ok <- new_rows[new_rows$status == "ok" & !is.na(new_rows$MB_per_s), ]
  if (nrow(ok) == 0) {
    return("Every download in this test failed - see the table for details.")
  }
  cur <- mean(ok$MB_per_s)
  
  mm <- log$max_mb
  same_mb <- (is.na(mm) & is.na(CONFIG$max_mb)) |
    (!is.na(mm) & !is.na(CONFIG$max_mb) & mm == CONFIG$max_mb)
  prev <- log[as.character(log$article_id) == CONFIG$article_id & same_mb &
                log$timestamp_utc != new_rows$timestamp_utc[1], , drop = FALSE]
  s <- session_summary(prev)
  if (NROW(s) == 0) {
    return(sprintf("This test: %.2f MB/s. First recorded test, so there is nothing to compare against yet.", cur))
  }
  
  pct_text <- function(now, ref) {
    d <- (now / ref - 1) * 100
    sprintf("%.0f%% %s", abs(d), if (d >= 0) "faster" else "slower")
  }
  med <- median(s$MB_per_s)
  msg <- sprintf("This test: %.2f MB/s - %s than the typical %.2f MB/s (median of %d earlier tests).",
                 cur, pct_text(cur, med), med, nrow(s))
  
  hr <- new_rows$local_hour[1]
  same_hr <- s[s$local_hour %in% c(hr - 1, hr, hr + 1), ]
  if (nrow(same_hr) >= 3) {
    msg <- paste(msg, sprintf("Around this time of day (%02d:00 +/- 1h): %s than the typical %.2f MB/s (%d tests).",
                              hr, pct_text(cur, median(same_hr$MB_per_s)),
                              median(same_hr$MB_per_s), nrow(same_hr)))
  }
  msg
}