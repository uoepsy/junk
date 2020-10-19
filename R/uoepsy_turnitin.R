#' make spreadsheet with turnitin links from set of files
#' @param submission_dir directory holding all and only the submitted files
#' @param output_file filepath to output csv
#' @param seed search script for seed? (looks for x<-, based on dapr labtests)
#' @param rename_files if TRUE will rename files to just exam numbers (where possible)
#' @export
#' @examples
#' uoepsy_turnitin("~/Desktop/dapr1reps/","~/Desktop/dapr_marks.csv")
uoepsy_turnitin<-function(submission_dir,output_file,seed=FALSE,rename_files=TRUE){
  require(tidyverse)

  #read script
  script_extract <- function(dir,file){
    script = readLines(paste0(dir,file))[-c(1:13)] #remove first 13 lines of script
  }

  #extract seed from script
  extract_seed <- function(dir, file){
    script = readLines(paste0(dir,file))
    seedset = script[grepl("X <-|x<-|x <-",script) & !grepl("PLEASE ASSIGN",script)]
    gsub("x|<|-| ","",seedset)
  }

  #extract questions from script
  q_extract <- function(scriptlst,qnum){
    script = scriptlst
    idx1 = map_dbl(script, ~min(which(grepl(paste0("Question ",qnum),.)))+1)
    idx2 = map_dbl(script, ~min(which(grepl(paste0("Question ",qnum+1),.)))-1)
    if(is.infinite(idx2)){idx2 = length(script[[1]])}
    script[[1]][idx1:idx2]
  }

  #get original filenames
  submissions =
    tibble(
      filename = list.files(path=submission_dir)
    )

  submissions %>%
    mutate(
      filetype = substr(filename, start = regexpr("\\.", filename)+1, stop=nchar(filename)),
      exam_number = gsub(".*?(B\\d{6}).*", "\\1", filename),
      turnitin_id = substr(filename, start = 1, stop=regexpr("-", filename)-1),
      turnitin_link = paste0('=HYPERLINK("https://ev.turnitinuk.com/app/carta/en_us/?o=', turnitin_id,'","',turnitin_id,'")')
    ) -> submissions

    if(seed){
      submissions %>%
         mutate(
           seed = map(filename, ~extract_seed(dir,.)),
           seed = map_chr(seed, ~ifelse(length(.)>1,"CHECK",.[1])),
           seed = substr(seed, 1, 6),
           # script = map(filename, ~script_extract("submissions/",.)),
           # q1 = map(filename, ~q_extract(script,1)),
           # q2 = map(filename, ~q_extract(script,2)),
           # q3 = map(filename, ~q_extract(script,3)),
           # q4 = map(filename, ~q_extract(script,4)),
           # q5 = map(filename, ~q_extract(script,5)),
         )
    }


  # finally, rename the files to just exam numbers
  if(rename_files){
    #make new file name
    submissions %>% mutate(
      original_filename = filename,
      file_sanitise = gsub("O", "0", toupper(substr(filename, start = 1, stop=regexpr("\\.", filename)))),
      filename = paste0(gsub(".*?(B\\d{6}).*", "\\1.", file_sanitise), filetype)
    ) %>% select(-file_sanitise) -> submissions

    #rename files
    file.rename(paste0(submission_dir,submissions$original_filename),
                paste0(submission_dir,submissions$filename))

    #remove old filename
    submissions %>% select(-original_filename) -> submissions
  }

  # and write out the marking spreadsheet
  write.csv(arrange(submissions,exam_number), output_file, row.names=F)
}


