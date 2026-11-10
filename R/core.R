# Shared code for the Figshare speed monitor (used by app.R and scheduled_test.R).
# Shiny sources everything in R/ automatically.
#
# This version times ITEM CREATION: it creates a draft metadata record titled
# "Speed Test" via the Figshare API, records how long that took, then deletes
# the draft again so nothing accumulates in the account.
#
# Requires: install.packages(c("shiny", "curl", "jsonlite"))
# Requires a personal token: set APIkey (or FIGSHARE_TOKEN) as an environment
# variable, e.g. in .Renviron (Figshare > Applications).

# curl and jsonlite are called with :: rather than attached, because jsonlite
# exports its own validate() which would mask shiny::validate().

FIGSHARE_API <- "https://api.figshare.com/v2"

# ---- Fixed test settings ---------------------------------------------------
# Keep these constant so results stay comparable over time.
# shinyapps.io cannot set environment variables, so if no token is set, load
# one from an optional secrets.R in the app folder, which is deployed with the
# app. It should contain one line:  Sys.setenv(APIkey = "your-figshare-token")
if (!nzchar(Sys.getenv("FIGSHARE_TOKEN")) && !nzchar(Sys.getenv("APIkey")) &&
    file.exists("secrets.R")) {
  source("secrets.R")
}

CONFIG <- list(
  title    = Sys.getenv("FIGSHARE_TEST_TITLE", "Speed Test"),
  runs     = 3L,   # drafts created per test
  cleanup  = TRUE, # delete each draft after timing it
  pause_s  = 1,    # polite pause between API calls (Figshare rate-limits)
  # History source. If log_url is set (a OneDrive/SharePoint sharing link to an
  # Excel file, set to "Anyone with the link can view") it is used; otherwise
  # the local CSV at log_path is read. The link only gives READ access, so it
  # can be pasted in as the default below. Write access stays with the people
  # you add as editors on the file.
  log_url   = Sys.getenv("FIGSHARE_LOG_URL", "https://lunet-my.sharepoint.com/:x:/g/personal/lbls2_lunet_lboro_ac_uk/IQBWN7zS7tmaQrA4xj7r8dynAdmqXIXHgaE6qGaC6el22KM?e=dwx3RF"),
  log_sheet = 1,   # which worksheet holds the log (name or number)
  log_path = Sys.getenv("FIGSHARE_LOG", "create_log.csv"),  # new file: columns differ from the download log
  timezone = Sys.getenv("FIGSHARE_TZ", "Europe/London"),
  token    = {
    t <- Sys.getenv("FIGSHARE_TOKEN", Sys.getenv("APIkey", ""))
    if (nzchar(t)) t else NULL
  }
)

LOG_COLUMNS <- c(
  "timestamp_utc", "local_time", "local_hour", "weekday", "tz", "name", "location",
  "title", "run", "status", "http_status", "article_id",
  "create_s", "ttfb_ms", "delete_s", "note"
)

# ---- Figshare access -------------------------------------------------------

# One timed API call. Returns list(status, body, total, ttfb) (times in seconds).
fig_request <- function(method, url, token, body = NULL) {
  h <- curl::new_handle(customrequest = method, connecttimeout = 30, timeout = 60)
  hdrs <- list(Authorization = paste("token", token))
  if (!is.null(body)) {
    hdrs[["Content-Type"]] <- "application/json"
    curl::handle_setopt(h, postfields = body)
  }
  do.call(curl::handle_setheaders, c(list(h), hdrs))
  
  res <- curl::curl_fetch_memory(url, handle = h)
  list(
    status = res$status_code,
    body   = rawToChar(res$content),
    total  = unname(res$times[["total"]]),
    ttfb   = unname(res$times[["starttransfer"]])
  )
}

# Create a draft with only a title. Returns list(ok, status, article_id, total, ttfb, note).
create_draft <- function(title, token) {
  payload <- as.character(jsonlite::toJSON(list(title = title), auto_unbox = TRUE))
  r <- fig_request("POST", sprintf("%s/account/articles", FIGSHARE_API), token, payload)
  
  if (r$status != 201) {
    return(list(ok = FALSE, status = r$status, article_id = NA_character_,
                total = r$total, ttfb = r$ttfb,
                note = paste("Unexpected response:", substr(r$body, 1, 200))))
  }
  loc <- tryCatch(jsonlite::fromJSON(r$body)$location, error = function(e) NA_character_)
  id  <- sub(".*/", "", loc)
  list(ok = TRUE, status = r$status, article_id = id,
       total = r$total, ttfb = r$ttfb, note = "")
}

# Delete a draft. Returns list(ok, seconds, note).
delete_draft <- function(article_id, token) {
  r <- fig_request("DELETE",
                   sprintf("%s/account/articles/%s", FIGSHARE_API, article_id), token)
  list(ok = r$status %in% c(200, 204), seconds = r$total,
       note = if (r$status %in% c(200, 204)) "" else
         paste("Delete failed, HTTP", r$status, "- remove draft", article_id, "manually"))
}

# ---- Running the standard test ---------------------------------------------

# Runs the fixed test and returns one row per draft creation (failures included).
# `progress(i, n, detail)` is an optional callback for UIs.
run_standard_test <- function(name = "", location = "", progress = function(i, n, detail) NULL) {
  now     <- Sys.time()
  ts_utc  <- format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ts_loc  <- format(now, "%Y-%m-%d %H:%M:%S", tz = CONFIG$timezone)
  hr      <- as.integer(format(now, "%H", tz = CONFIG$timezone))
  wd      <- format(now, "%a", tz = CONFIG$timezone)
  tzn     <- format(now, "%Z", tz = CONFIG$timezone)
  
  make_row <- function(run = NA_integer_, status = "ok", http_status = NA_integer_,
                       article_id = NA_character_, create_s = NA_real_,
                       ttfb_ms = NA_real_, delete_s = NA_real_, note = "") {
    data.frame(
      timestamp_utc = ts_utc, local_time = ts_loc, local_hour = hr, weekday = wd,
      tz = tzn, name = name, location = location, title = CONFIG$title, run = run,
      status = status, http_status = http_status, article_id = article_id,
      create_s = round(create_s, 3), ttfb_ms = round(ttfb_ms),
      delete_s = round(delete_s, 3), note = note,
      stringsAsFactors = FALSE
    )[, LOG_COLUMNS]
  }
  
  if (is.null(CONFIG$token)) {
    return(make_row(status = "error",
                    note = "No token: set APIkey or FIGSHARE_TOKEN (needed to create drafts)"))
  }
  
  rows <- vector("list", CONFIG$runs)
  for (i in seq_len(CONFIG$runs)) {
    progress(i, CONFIG$runs, sprintf("Creating draft (run %d/%d)", i, CONFIG$runs))
    if (i > 1) Sys.sleep(CONFIG$pause_s)
    
    r <- tryCatch(create_draft(CONFIG$title, CONFIG$token), error = function(e) e)
    if (inherits(r, "error")) {
      rows[[i]] <- make_row(i, status = "error", note = conditionMessage(r))
      next
    }
    if (!r$ok) {
      rows[[i]] <- make_row(i, status = "error", http_status = r$status,
                            create_s = r$total, ttfb_ms = r$ttfb * 1000, note = r$note)
      next
    }
    
    del_s <- NA_real_
    note  <- ""
    if (CONFIG$cleanup) {
      Sys.sleep(CONFIG$pause_s)
      d <- tryCatch(delete_draft(r$article_id, CONFIG$token), error = function(e) e)
      if (inherits(d, "error")) {
        note <- paste("Delete failed:", conditionMessage(d), "- remove draft", r$article_id)
      } else {
        del_s <- d$seconds
        note  <- d$note
      }
    }
    rows[[i]] <- make_row(i, http_status = r$status, article_id = r$article_id,
                          create_s = r$total, ttfb_ms = r$ttfb * 1000,
                          delete_s = del_s, note = note)
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

# Parse timestamps written by this app (ISO, UTC) or re-typed by a spreadsheet
# (Google Sheets / Excel often turn them into date-times and drop the "T"/"Z").
parse_timestamp_utc <- function(x) {
  x <- trimws(as.character(x))
  out <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  # Excel date-times read as text arrive as serial numbers (days since 1899-12-30).
  serial <- !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?$", x)
  if (any(serial)) {
    out[serial] <- as.POSIXct(as.numeric(x[serial]) * 86400,
                              origin = "1899-12-30", tz = "UTC")
  }
  for (fmt in c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
                "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M")) {
    miss <- is.na(out) & !is.na(x) & nzchar(x)
    if (!any(miss)) break
    out[miss] <- as.POSIXct(x[miss], format = fmt, tz = "UTC")
  }
  out
}

# Turn a OneDrive/SharePoint sharing link into a direct-download link.
# Other URLs are left unchanged.
direct_download_url <- function(url) {
  if (grepl("1drv\\.ms|onedrive\\.live\\.com", url)) {
    # Personal OneDrive: use the public "shares" API with the encoded link.
    enc <- gsub("\\s", "", jsonlite::base64_enc(charToRaw(url)))
    enc <- gsub("=+$", "", chartr("+/", "-_", enc))
    return(paste0("https://api.onedrive.com/v1.0/shares/u!", enc, "/root/content"))
  }
  if (grepl("sharepoint\\.com", url) && !grepl("download=1", url, fixed = TRUE)) {
    return(paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", "download=1"))
  }
  url
}

# Load the raw log from the shared Excel file (if log_url is set) or the local
# CSV. Returns NULL if there is nothing to load; warns if the download fails.
fetch_log_csv <- function(path, url) {
  csv_args <- list(stringsAsFactors = FALSE, colClasses = c(article_id = "character"))
  if (nzchar(url)) {
    res <- tryCatch(
      curl::curl_fetch_memory(direct_download_url(url),
                              handle = curl::new_handle(followlocation = TRUE, timeout = 30)),
      error = function(e) e
    )
    if (inherits(res, "error")) {
      warning("Could not load the history file (", conditionMessage(res),
              "). Showing no history.", call. = FALSE)
      return(NULL)
    }
    if (res$status_code != 200) {
      warning("Could not load the history file (HTTP ", res$status_code,
              "). Check the link is set to 'Anyone with the link can view'. Showing no history.",
              call. = FALSE)
      return(NULL)
    }
    bytes <- res$content
    
    if (length(bytes) >= 2 && identical(bytes[1:2], as.raw(c(0x50, 0x4b)))) {  # "PK" = xlsx
      if (!requireNamespace("readxl", quietly = TRUE)) {
        warning("Install the 'readxl' package to read the Excel history file.", call. = FALSE)
        return(NULL)
      }
      tmp <- tempfile(fileext = ".xlsx")
      on.exit(unlink(tmp), add = TRUE)
      writeBin(bytes, tmp)
      return(tryCatch(
        as.data.frame(readxl::read_excel(tmp, sheet = CONFIG$log_sheet, col_types = "text"),
                      stringsAsFactors = FALSE),
        error = function(e) {
          warning("Could not read the Excel file: ", conditionMessage(e), call. = FALSE)
          NULL
        }
      ))
    }
    
    head_txt <- tryCatch(rawToChar(bytes[seq_len(min(500L, length(bytes)))]),
                         error = function(e) "")
    if (grepl("<html|<!doctype", head_txt, ignore.case = TRUE)) {
      warning("The history link returned a web page instead of the file. It probably needs ",
              "'Anyone with the link can view' (your organisation may block this). ",
              "Showing no history.", call. = FALSE)
      return(NULL)
    }
    txt <- rawToChar(bytes)
    Encoding(txt) <- "UTF-8"
    return(tryCatch(do.call(read.csv, c(list(text = txt), csv_args)), error = function(e) NULL))
  }
  if (file.exists(path)) do.call(read.csv, c(list(file = path), csv_args)) else NULL
}

read_log <- function(path = CONFIG$log_path, url = CONFIG$log_url) {
  log <- fetch_log_csv(path, url)
  if (is.null(log)) {
    log <- setNames(as.data.frame(matrix(ncol = length(LOG_COLUMNS), nrow = 0)), LOG_COLUMNS)
  } else {
    # Upgrade older logs: the single "tester" column became "name" + "location",
    # and any other missing column is added blank.
    if (!"name" %in% names(log) && "tester" %in% names(log)) log$name <- log$tester
    for (m in setdiff(LOG_COLUMNS, names(log))) log[[m]] <- rep(NA, nrow(log))
  }
  # Excel is read as text, so convert the numeric columns back.
  for (col in intersect(c("run", "http_status", "create_s", "ttfb_ms", "delete_s"), names(log))) {
    log[[col]] <- suppressWarnings(as.numeric(log[[col]]))
  }
  # Columns that are entirely blank get read as logical NA; make them text.
  for (col in c("name", "location", "note")) {
    log[[col]] <- ifelse(is.na(log[[col]]), "", as.character(log[[col]]))
  }
  # Parse timestamps tolerantly, drop blank/unreadable rows, and normalise the
  # text form so grouping and comparisons behave the same however it was pasted.
  t_utc <- parse_timestamp_utc(log$timestamp_utc)
  log   <- log[!is.na(t_utc), , drop = FALSE]
  t_utc <- t_utc[!is.na(t_utc)]
  log$timestamp_utc <- format(t_utc, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  # Always derive local time from the UTC timestamp.
  log$local_time <- format(t_utc, "%Y-%m-%d %H:%M:%S", tz = CONFIG$timezone)
  log$local_hour <- as.integer(format(t_utc, "%H", tz = CONFIG$timezone))
  log$weekday    <- format(t_utc, "%a", tz = CONFIG$timezone)
  log$tz         <- format(t_utc, "%Z", tz = CONFIG$timezone)
  log$time <- t_utc
  attr(log$time, "tzone") <- CONFIG$timezone
  # Display label combining name and location, used for filtering and plots.
  log$who <- trimws(ifelse(nzchar(log$location),
                           paste0(log$name, " (", log$location, ")"), log$name))
  log
}

# Optional one-off: rewrite the derived time columns stored in the CSV itself
# (a backup is saved alongside).
repair_log <- function(path = CONFIG$log_path) {
  stopifnot(file.exists(path))
  file.copy(path, paste0(path, ".bak"), overwrite = TRUE)
  log <- read_log(path, url = "")
  write.table(log[, LOG_COLUMNS], path, sep = ",", row.names = FALSE,
              col.names = TRUE, qmethod = "double")
  invisible(path)
}

# One row per test x person (average of the runs in that test).
# Lower create_s = faster.
session_summary <- function(log) {
  ok <- log[log$status == "ok" & !is.na(log$create_s), , drop = FALSE]
  if (nrow(ok) == 0) return(NULL)
  key <- paste(ok$timestamp_utc, ok$who, sep = "|")
  parts <- lapply(split(ok, key), function(d) {
    data.frame(
      time = d$time[1], local_time = d$local_time[1], local_hour = d$local_hour[1],
      who = d$who[1], n_runs = nrow(d),
      create_s = mean(d$create_s), ttfb_ms = mean(d$ttfb_ms),
      delete_s = if (all(is.na(d$delete_s))) NA_real_ else mean(d$delete_s, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  s <- do.call(rbind, parts)
  s[order(s$time), ]
}

# Plain-English comparison of a just-finished test against the log history.
describe_run <- function(new_rows, log) {
  ok <- new_rows[new_rows$status == "ok" & !is.na(new_rows$create_s), ]
  if (nrow(ok) == 0) {
    return("Every draft creation in this test failed - see the table for details.")
  }
  cur <- mean(ok$create_s)
  
  prev <- log[log$title == CONFIG$title &
                log$timestamp_utc != new_rows$timestamp_utc[1], , drop = FALSE]
  s <- session_summary(prev)
  if (NROW(s) == 0) {
    return(sprintf("This test: %.2f s to create a draft. First recorded test, so there is nothing to compare against yet.", cur))
  }
  
  # For timings, a bigger number is SLOWER.
  pct_text <- function(now, ref) {
    d <- (now / ref - 1) * 100
    sprintf("%.0f%% %s", abs(d), if (d >= 0) "slower" else "faster")
  }
  med <- median(s$create_s)
  msg <- sprintf("This test: %.2f s per draft - %s than the typical %.2f s (median of %d earlier tests).",
                 cur, pct_text(cur, med), med, nrow(s))
  
  hr <- new_rows$local_hour[1]
  same_hr <- s[s$local_hour %in% c(hr - 1, hr, hr + 1), ]
  if (nrow(same_hr) >= 3) {
    msg <- paste(msg, sprintf("Around this time of day (%02d:00 +/- 1h): %s than the typical %.2f s (%d tests).",
                              hr, pct_text(cur, median(same_hr$create_s)),
                              median(same_hr$create_s), nrow(same_hr)))
  }
  msg
}