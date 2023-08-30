#' render xaringan slides without presenter notes
#' @param fpath path to .Rmd file
#' @param outdir directory to place rendered html
#' @export
xaringan_nonote_render <- function(fpath=NULL, outdir=NULL){
  sc = readLines(fpath)
  notes = which(sc=="???")
  to_rm = c()
  for(i in 1:length(notes)){
    to_rm = c(to_rm, notes[i] : (notes[i] + min(which(sc[notes[i]:length(sc)]%in%c("---","--"))) - 2))
  }
  #to_rm
  fileConn<-file(gsub(".Rmd","_tmp_nonotes.Rmd",fpath))
  writeLines(sc[-to_rm], fileConn)
  close(fileConn)
  if(is.null(outdir)){
    rmarkdown::render(gsub(".Rmd","_tmp_nonotes.Rmd",fpath),
                      output_file = gsub(".Rmd",".html", fpath))
  }else{
    rmarkdown::render(gsub(".Rmd","_tmp_nonotes.Rmd",fpath),
                      output_dir = outdir)
  }
  file.remove(gsub(".Rmd","_tmp_nonotes.Rmd",fpath))
}
