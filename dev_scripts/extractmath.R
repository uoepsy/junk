#' extracts math from the rmd/qmd etc files
#' @examples
#' extractmath("../dapr3/dapr3_labs/01_regressionrefresh.qmd","../../test.rmd")
extractmath <- function(infile=NULL, outfile=NULL){
  sc = readLines(infile)
  knitr::purl(infile,"temporarycodefile.r")
  sc_r = readLines("temporarycodefile.r")
  eqs = which(unlist(lapply(sc, function(x) grepl("\\$\\$",x))))
  sngl_eqs = which(unlist(lapply(sc, function(x) grepl("\\$",x))))
  sngl_eqs = sngl_eqs[!sngl_eqs %in% eqs]
  sngl_eqs = sc[sngl_eqs]
  sngl_eqs = sngl_eqs[!unlist(lapply(sngl_eqs, function(x) grepl("params\\$",x)))]
  sngl_eqs = sngl_eqs[!unlist(lapply(sngl_eqs, function(x) x %in% sc_r))]



  sink(outfile)
  cat("# DOUBLE DOLLAR EQUATIONS  \n")
  if(length(eqs)>0){
    for(i in 1:length(eqs)){
      if(i%%2!=0){
        cat(sc[eqs[i]:eqs[i+1]])
        cat("\n   \n   \n")
      }
    }
  }
  cat("\n   \n   \n# OTHER BITS OF MATH\n   \n")
  if(length(sngl_eqs)>1){
    for(i in sngl_eqs){
      cat(i)
      cat("\n   \n")
    }
  }
  sink()

  file.remove("temporarycodefile.r")
}

# extractmath("../dapr3/dapr3_labs/01_regressionrefresh.qmd","../../lab1.rmd")
# extractmath("../dapr3/dapr3_labs/02_intromlm.qmd","../../lab2.rmd")
# extractmath("../dapr3/dapr3_labs/03_assumptranef.qmd","../../lab3.rmd")
# extractmath("../dapr3/dapr3_labs/04_centerglmer.qmd","../../lab4.rmd")
# extractmath("../dapr3/dapr3_labs/05_recap.qmd","../../lab5.rmd")
#
# extractmath("../dapr3/dapr3_lectures/dapr3_mlm_lectures/01_lmcluster.Rmd","../../lec1.rmd")
# extractmath("../dapr3/dapr3_lectures/dapr3_mlm_lectures/02p_intromlm.Rmd","../../lec2.rmd")
# extractmath("../dapr3/dapr3_lectures/dapr3_mlm_lectures/03_assumptdiag.Rmd","../../lec3.rmd")
# extractmath("../dapr3/dapr3_lectures/dapr3_mlm_lectures/04_centeringglmer.Rmd","../../lec4.rmd")
# extractmath("../dapr3/dapr3_lectures/dapr3_mlm_lectures/05_mlmresearch.Rmd","../../lec5.rmd")
#
# extractmath("../dapr3/dapr3_labs/todo/07_path1.Rmd","../../lab7.rmd")
# extractmath("../dapr3/dapr3_labs/todo/08_path2.Rmd","../../lab8.rmd")
# extractmath("../dapr3/dapr3_labs/todo/09_pca.Rmd","../../lab9.rmd")
# extractmath("../dapr3/dapr3_labs/todo/10_efa.Rmd","../../lab10.rmd")
# extractmath("../dapr3/dapr3_labs/todo/11_efa2.Rmd","../../lab11.rmd")
#
# extractmath("../dapr3/dapr3_lectures/pathfa_lectures/week1_pathintro.Rmd","../../lec7.rmd")
# extractmath("../dapr3/dapr3_lectures/pathfa_lectures/week2_pathmediation.Rmd","../../lec8.rmd")
# extractmath("../dapr3/dapr3_lectures/pathfa_lectures/week3_pca.Rmd","../../lec9.rmd")
# extractmath("../dapr3/dapr3_lectures/pathfa_lectures/week4_efa1.Rmd","../../lec10.rmd")
# extractmath("../dapr3/dapr3_lectures/pathfa_lectures/week5_efa2.Rmd","../../lec11.rmd")
