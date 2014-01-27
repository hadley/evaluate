#' Evaluate input and return all details of evaluation.
#'
#' Compare to \code{\link{eval}}, \code{evaluate} captures all of the
#' information necessary to recreate the output as if you had copied and
#' pasted the code into a R terminal. It captures messages, warnings, errors
#' and output, all correctly interleaved in the order in which they occured.
#' It stores the final result, whether or not it should be visible, and the
#' contents of the current graphics device.
#'
#' @export
#' @param input input object to be parsed an evaluated.  Maybe a string,
#'   file connection or function.
#' @param envir environment in which to evaluate expressions
#' @param enclos when \code{envir} is a list or data frame, this is treated
#'   as the parent environment to \code{envir}.
#' @param debug if \code{TRUE}, displays information useful for debugging,
#'   including all output that evaluate captures
#' @param stop_on_error if \code{2}, evaluation will stop on first error and you
#'   will get no results back. If \code{1}, evaluation will stop on first error,
#'   but you will get back all results up to that point. If \code{0} will
#'   continue running all code, just as if you'd pasted the code into the
#'   command line.
#' @param keep_warning,keep_message whether to record warnings and messages
#' @param new_device if \code{TRUE}, will open a new graphics device and
#'   automatically close it after completion. This prevents evaluation from
#'   interfering with your existing graphics environment.
#' @param output_handler an instance of \code{\link{output_handler}}
#'   that processes the output from the evaluation. The default simply
#'   prints the visible return values.
#' @import stringr
evaluate <- function(input, envir = parent.frame(), enclos = NULL, debug = FALSE,
                     stop_on_error = 0L, keep_warning = TRUE, keep_message = TRUE,
                     new_device = TRUE, output_handler = default_output_handler) {
  parsed <- parse_all(input)

  stop_on_error <- as.integer(stop_on_error)
  stopifnot(length(stop_on_error) == 1)

  if (is.null(enclos)) {
    enclos <- if (is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv()
  }

  if (new_device) {
    # Start new graphics device and clean up afterwards
    dev.new()
    dev.control(displaylist = "enable")
    dev <- dev.cur()
    on.exit(dev.off(dev))
  }

  out <- vector("list", nrow(parsed))
  for (i in seq_along(out)) {
    expr <- parsed$expr[[i]]
    if (!is.null(expr))
      expr <- as.expression(expr)
    out[[i]] <- evaluate_call(
      expr, parsed$src[[i]],
      envir = envir, enclos = enclos, debug = debug, last = i == length(out),
      use_try = stop_on_error != 2L,
      keep_warning = keep_warning, keep_message = keep_message,
      output_handler = output_handler)

    if (stop_on_error > 0L) {
      errs <- vapply(out[[i]], is.error, logical(1))

      if (!any(errs)) next
      if (stop_on_error == 1L) break
    }
  }

  structure(unlist(out, recursive = FALSE, use.names = FALSE), class="evaluation")
}

evaluate_call <- function(call, src = NULL,
                          envir = parent.frame(), enclos = NULL,
                          debug = FALSE, last = FALSE, use_try = FALSE,
                          keep_warning = TRUE, keep_message = TRUE,
                          output_handler = new_output_handler()) {
  if (debug) message(src)

  if (is.null(call) && !last) {
    return(list(new_source(src)))
  }
  stopifnot(is.call(call) || is.language(call) || is.atomic(call))

  # Capture output
  w <- watchout(debug)
  on.exit(w$close())
  source <- new_source(src)
  output_handler$source(source)
  output <- list(source)

  handle_output <- function(plot = FALSE, incomplete_plots = FALSE) {
    out <- w$get_new(plot, incomplete_plots)
    if (!is.null(out$text))
      output_handler$text(out$text)
    if (!is.null(out$graphics))
      output_handler$graphics(out$graphics)
    output <<- c(output, out)
  }

  # Hooks to capture plot creation
  capture_plot <- function() {
    handle_output(TRUE)
  }
  old_hooks <- set_hooks(list(
    persp = capture_plot,
    before.plot.new = capture_plot,
    before.grid.newpage = capture_plot))
  on.exit(set_hooks(old_hooks, "replace"), add = TRUE)

  handle_condition <- function(cond) {
    handle_output()
    output <<- c(output, list(cond))
  }

  # Handlers for warnings, errors and messages
  wHandler <- if (keep_warning) function(wn) {
    if (getOption("warn") >= 0) {
      handle_condition(wn)
      output_handler$warning(wn)
    }
    invokeRestart("muffleWarning")
  } else identity
  eHandler <- if (use_try) function(e) {
    handle_condition(e)
    output_handler$error(e)
  } else identity
  mHandler <- if (keep_message) function(m) {
    handle_condition(m)
    output_handler$message(m)
    invokeRestart("muffleMessage")
  } else identity

  ev <- list(value = NULL, visible = FALSE)

  if (use_try) {
    handle <- function(f) try(f, silent = TRUE)
  } else {
    handle <- force
  }
  if (!is.null(call)) {
    handle(ev <- withCallingHandlers(
      withVisible(eval(call, envir, enclos)),
      warning = wHandler, error = eHandler, message = mHandler))
    handle_output(TRUE)
  }

  value_handler <- output_handler$value
  multi_args <- length(formals(value_handler)) > 1
  # If visible or the value handler has multi args, process and capture output
  if (ev$visible || multi_args) {
    pv <- list(value = NULL, visible = FALSE)
    value_fun <- if (multi_args) value_handler else {
      function(x, visible) value_handler(x)
    }
    handle(pv <- withCallingHandlers(withVisible(value_fun(ev$value, ev$visible)),
      warning = wHandler, error = eHandler, message = mHandler))
    handle_output(TRUE)
    # If the return value is visible, save the value to the output
    if (pv$visible) output <- c(output, list(pv$value))
  }

  # Always capture last plot, even if incomplete
  if (last) {
    handle_output(TRUE, TRUE)
  }

  output
}

