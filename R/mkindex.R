mkindex <- function(path, ftypes=c("*.html")){
    d<-unlist(purrr::map(ftypes, ~list.files(path, .)))
    newindex<-file(paste0(path,"index.md"))
    writeLines(unlist(purrr::map(d, ~paste0("[",.,"](",.,")  "))), newindex)
    close(newindex)
}

