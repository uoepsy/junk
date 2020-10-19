#' get a quick list of all functions used in Rmd. Will exclude echo=FALSE chunks on premise that students aren't presented with these, so we often put more complicated plotting stuff in there. Will also auto exclude include=FALSE.
#' @param filepath rmd document for lab/lecture
#' @param excludechunks knitr options to exclude
#' @export
#' @examples
#' uoepsy_rmd_funcs("01-slr.Rmd", excludechunks=c("echo=FALSE","include=FALSE"))
uoepsy_rmd_funcs <- function(filepath, excludechunks=NULL){

  dropchunks <- function(scriptname, what.to.drop){
    script <- readLines(scriptname)
    script <- do.call(paste, list(script, collapse = "\n") )
    subpattern = paste0("(", do.call(paste, list(what.to.drop, collapse="|")), ")")
    mainpattern <- paste('(?s)## ((?!##).)*?', subpattern, '.*?((?=##)|$)', sep="")
    mainpattern <- paste('(?s)##((?!##).)*?', subpattern, '.*?((?=##)|$)', sep="")
    script<- gsub(pattern = mainpattern, replacement = "", x = script, perl=TRUE)
    writeLines(text = script, con= scriptname)
  }

  knitr::purl(filepath, output="tmp.R")
  if(!is.null(excludechunks)){
    for(i in seq_along(excludechunks)){
      print(i)
      dropchunks("tmp.R",excludechunks[i])
    }
  }
  code <- parse("tmp.R")
  tokens <- as.list(code)
  calls <- c()
  while (TRUE) {
    any_unpacked <- FALSE
    for (ii in seq_along(tokens)) {
      part <- tokens[[ii]]
      # Calls always have the function name as the first element
      if (is.call(part)) {
        fun_token <- part[[1]]
        calls <- c(calls, deparse(fun_token))
      }
      # Expressions have a length
      if (length(part) > 1) {
        tokens[[ii]] <- as.list(part)
        any_unpacked <- TRUE
      }
    }
    tokens <- unlist(tokens)
    tokens <- tokens[nchar(tokens)>=1]
    if (!any_unpacked) break
  }
  #file.remove("tmp.R")
  unique(calls)
}

