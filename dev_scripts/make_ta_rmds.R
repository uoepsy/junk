dropchunks <- function(scriptname, what.to.drop){
  script <- readLines(scriptname)
  script <- do.call(paste, list(script, collapse = "\n") )
  subpattern = paste0("(", do.call(paste, list(what.to.drop, collapse="|")), ")")
  mainpattern <- paste('(?s)## ((?!##).)*?', subpattern, '.*?((?=##)|$)', sep="")
  mainpattern <- paste('(?s)##((?!##).)*?', subpattern, '.*?((?=##)|$)', sep="")
  script<- gsub(pattern = mainpattern, replacement = "", x = script, perl=TRUE)
  writeLines(text = script, con= scriptname)
}

excludechunks=c("echo=FALSE","include=FALSE")

options(knitr.purl.inline = TRUE)

courses = c("dapr1","dapr2","rms2","usmr")

for(coursei in courses){
  labs = list.files(glue::glue("~/Desktop/uoepsy/{course}/{course}_labs",course=coursei),".Rmd", full.names=T)
  for(labi in labs){
    knitr::purl(labi, output="~/Downloads/tmp.R", documentation = 1)
    for(i in seq_along(excludechunks)){
      dropchunks("~/Downloads/tmp.R",excludechunks[i])
    }
    code <- readLines("~/Downloads/tmp.R")
    code[substr(code,1,5)=="## --"] <- "```\n\n```{r}"
    code <- code[!(grepl("include_graphics",code))]
    code <- code[!(grepl("source\\(",code))]
    code<-code[-c(1:2)]
    code[1]<-"```{r}"
      
      # fix the question solution bits
      # turn them into main text section headings
    code[grepl("qbegin\\(",code)] <- paste0("```\n\n## Question ", gsub('qbegin\\(|\\)|\\"|\\,|qlabel|=|FALSE|F','',code[grepl("qbegin\\(",code)]),"\n```{r}")
    code<-code[!grepl("qend\\(\\)",code)]
    code<-code[!grepl("solend\\(\\)",code)]
    code<-code[!grepl("optbegin\\(",code)]
    code<-code[!grepl("optend\\(",code)]
    code[grepl("solbegin\\(",code)] <- paste0("```\n\n### Solution ", gsub('solbegin\\(|\\)|\\"|\\,|slabel|show|SHOW_SOLS|TOGGLE|TRUE|T|toggle|params|\\$|=|FALSE|F','',code[grepl("solbegin\\(",code)]),"\n```{r}")
      
    code <- code[!code==""]
    if(length(code)>1){
      for(l in 1:(length(code)-1)){
        if(substr(code[l], nchar(code[l])-5,nchar(code[l]))=="```{r}" & substr(code[l+1],1,4)=="```\n"){
          code[l]<-gsub('```\\{r\\}','',code[l])
          code[l+1]<-gsub('```\\\n','',code[l+1])
        }
      }
    }
      
      
    title = toupper(paste0(coursei," - ", gsub(".Rmd","",strsplit(labi, split="_labs/")[[1]][2])))
    headeryaml = glue::glue('
    ---
    title: "{title}"
    output:
      html_document:
      theme: flatly
    ---
    ')
    outfilename = paste0(coursei,"_",strsplit(labi, split="_labs/")[[1]][2])
    writeLines(c(headeryaml,code,'```\n'), glue::glue("~/Desktop/uoepsy_admin/ta_rmds/{outfilename}"))
  }
}
