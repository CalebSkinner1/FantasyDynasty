# Workflow Support
# run_step() runs one pipeline script in a fresh R process (the same R installation) from the
# project root, so memory is cleared between steps and objects from one script can't leak
# into the next. If the script fails, the workflow stops there (so nothing is deployed).
#
# Script settings that read getOption() can be set for one run, e.g.
#   run_step("Modeling/Player Total Value.R", options = list(fantasy.train_models = TRUE))

run_step <- function(script, options = list()) {
  root <- here::here()
  path <- file.path(root, script)
  if (!file.exists(path)) {
    stop("can't find ", script, call. = FALSE)
  }

  set_options <- if (length(options) > 0) {
    values <- vapply(options, \(x) paste(deparse(x), collapse = ""), character(1))
    sprintf("options(%s); ", paste(names(options), values, sep = " = ", collapse = ", "))
  } else {
    ""
  }
  expr <- sprintf("%ssource(%s)", set_options, deparse(path))

  message("\n== ", script, " (started ", format(Sys.time(), "%H:%M:%S"), ")")
  start <- Sys.time()

  old_wd <- setwd(root)
  on.exit(setwd(old_wd))
  status <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(expr)))

  minutes <- as.numeric(difftime(Sys.time(), start, units = "mins"))
  if (status != 0) {
    stop(script, " failed after ", round(minutes, 1), " minutes", call. = FALSE)
  }
  message("== ", script, " finished in ", round(minutes, 1), " minutes")
  invisible(minutes)
}
