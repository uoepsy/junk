#' make quick marks for turnitin.
#' csv should have two columns, the first is the name of the quickmark
#' the second is the content.
#' should be able to use html in the content if you need it for bold/ital etc.
#'
#' @export
makeqms = function(csvfile, setname="myquickmarkset"){
  qms = read.csv(csvfile)
  outfile = paste0(setname,".qms")
  sink(outfile)
  cat('{"QuickMarkTemplate":[')
  qm = paste0('{"symbol":"',gsub("[[:punct:]]", "", qms[,1]),'","description":"<p>',
              gsub('"','\\\\"',qms[,2]), # ESCAPE ALL OF THE QUOTES
              '</p>"}')
  cat(paste0(qm, collapse=","))
  cat(paste0('],"QuickMarkTemplateSet":[{"name":"',setname,'"}]}'))
  sink()
}
