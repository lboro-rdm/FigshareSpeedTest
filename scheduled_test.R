# Runs the standard test once and appends the result to the log, no UI.
# Use this to collect data automatically at set times of day.
#
#   Rscript scheduled_test.R
#
# Optional environment variables:
#   FIGSHARE_TESTER   label stored in the log (default: this computer's name)
#   FIGSHARE_LOG      path to the shared CSV log (default: speedtest_log.csv next to this script)
#
# Linux/macOS cron example (8am, noon, 4pm, 8pm):
#   0 8,12,16,20 * * * /usr/bin/Rscript /path/to/figshare_speedtest_app/scheduled_test.R
#
# Windows Task Scheduler: create a task whose action runs
#   "C:\Program Files\R\R-4.x.x\bin\Rscript.exe" "C:\path\to\scheduled_test.R"

args <- commandArgs(FALSE)
script <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(script)) setwd(dirname(normalizePath(script)))

source("R/core.R")

tester <- Sys.getenv("FIGSHARE_TESTER", unname(Sys.info()[["nodename"]]))
res <- run_standard_test(tester = tester)
append_log(res)

cat(describe_run(res, read_log(CONFIG$log_path)), "\n")
cat("Logged", nrow(res), "row(s) to", normalizePath(CONFIG$log_path), "\n")