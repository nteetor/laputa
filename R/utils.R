verify <- function(...) {
  statements <- eval(substitute(alist(...)))
  env <- parent.frame()

  msg <- ''
  for (s in statements) {
    seval <- tryCatch(
      eval(s, env),
      error = function(e) {
        stop('could not evaluate ', s, call. = FALSE)
      }
    )

    if (!is.logical(seval)) {
      stop(s, ' does not evalutate to TRUE/FALSE', call. = FALSE)
    }

    if (!seval) {
      msg <- c(msg, sprintf('  %s is not TRUE', deparse(s)))
    }
  }

  if (length(msg) != 1) {
    stop(paste(msg, collapse = '\n'), call. = FALSE)
  }

  invisible(TRUE)
}

set_names <- function(values, names) {
  names(values) <- names
  values
}
