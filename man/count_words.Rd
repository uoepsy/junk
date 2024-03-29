% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_words.R
\name{count_words}
\alias{count_words}
\title{A function for counting words in an Rmarkdown/Quarto document.
taken from https://github.com/jvcasillas/academicWriteR/blob/master/R/count_words.R}
\usage{
count_words(file)
}
\arguments{
\item{file}{A path to a .Rmd/.qmd file}
}
\description{
This function takes a path to a .Rmd file and returns a word count. For best
results each sentence should start on a new line. Inline code chunks should
also start on a new line.
}
\examples{
# count_words("test.Rmd")
}
\keyword{Word}
\keyword{count}
