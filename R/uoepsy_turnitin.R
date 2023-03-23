#' Create a spreadsheet with turnitin links from a set of files.
#' @param submission_dir directory holding all and only the submitted files
#' @param output_file filepath to output csv
#' @param rename_files if TRUE will rename files to just the exam numbers (where possible)
#' @export
#' @examples
#' # from turnitin submission box, download all files, and unzip to a folder, e.g. dapr1reps/
#' uoepsy_turnitin(submission_dir = "~/Desktop/dapr1reps/", output_file = "~/Desktop/dapr_marks.csv")
uoepsy_turnitin<-function(submission_dir,output_file,rename_files=FALSE){
  require(tidyverse)

  #get original filenames
  submissions =
    tibble(
      filename = list.files(path=submission_dir)
    )

  submissions %>%
    mutate(
      filetype = substr(filename, start = regexpr("\\.", filename)+1, stop=nchar(filename)),
      exam_number = gsub(".*?(B\\d{6}).*", "\\1", toupper(filename)),
      turnitin_id = substr(filename, start = 1, stop=regexpr("-", filename)-2),
      turnitin_link = paste0('=HYPERLINK("https://ev.turnitinuk.com/app/carta/en_us/?o=', turnitin_id,'","',turnitin_id,'")')
    ) -> submissions

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


