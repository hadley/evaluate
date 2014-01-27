#' @S3method print evaluation
print.evaluation <- function(x, ...){
  cat(paste(" *", vapply(lapply(x, is), head, character(1), 1), collapse="\n"), "\n")
  invisible()
}


