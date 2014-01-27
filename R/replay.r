#' Replay a list of evaluated results.
#'
#' Replay a list of evaluated results, as if you'd run them in an R
#' terminal.
#'
#' @param x result from \code{\link{evaluate}}
#' @export
#' @examples
#' samples <- system.file("tests", "testthat", package = "evaluate")
#' if (file_test("-d", samples)) {
#' replay(evaluate(file(file.path(samples, "order.r"))))
#' replay(evaluate(file(file.path(samples, "plot.r"))))
#' replay(evaluate(file(file.path(samples, "data.r"))))
#' }
#' @S3method replay list
#' @S3method replay evaluation
#' @S3method replay character
#' @S3method replay source
#' @S3method replay warning
#' @S3method replay message
#' @S3method replay error
#' @S3method replay value
#' @S3method replay recordedplot
replay <- function(x) UseMethod("replay", x)

replay.list <- function(x) {
  invisible(lapply(x, replay))
}

replay.evaluation <- function(x){
  replay(unclass(x))
}

replay.character <- function(x) {
  cat(x)
}

replay.source <- function(x) {
  cat(line_prompt(x$src))
}

replay.warning <- function(x) {
  message("Warning message:\n", x$message)
}

replay.message <- function(x) {
  message(str_replace(x$message, "\n$", ""))
}

replay.error <- function(x) {
  if (is.null(x$call)) {
    message("Error: ", x$message)
  } else {
    call <- deparse(x$call)
    message("Error in ", call, ": ", x$message)
  }
}

replay.value <- function(x) {
  if (x$visible) print(x$value)
}

replay.recordedplot <- function(x) {
  print(x)
}

#' Line prompt.
#'
#' Format a single expression as if it had been entered at the command prompt.
#'
#' @param x string representing a single expression
#' @param prompt prompt for first line
#' @param continue prompt for subsequent lines
#' @return a string
line_prompt <- function(x, prompt = getOption("prompt"), continue = getOption("continue")) {
  lines <- strsplit(x, "\n")[[1]]
  n <- length(lines)

  lines[1] <- str_c(prompt, lines[1])
  if (n > 1)
    lines[2:n] <- str_c(continue, lines[2:n])

  str_c(lines, "\n", collapse = "")
}
