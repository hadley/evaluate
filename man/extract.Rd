\name{extract}
\alias{extract}
\title{Extract output from an evaluation.}
\usage{
  extract(evaluation,
    what = c("source", "text", "graphics", "message", "warning", "error", "value"))
}
\arguments{
  \item{evaluation}{object returned by evaluate()}

  \item{what}{type of output to extract. Must be string.}
}
\description{
  This function helps extracting certain output types (e.g.
  graphics or messages) from an evaluation object that has
  been returned by evaluate()
}

