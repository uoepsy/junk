require(tidyverse)
require(readxl)

# function for tidying a single students data
# just crops it down to the relevant bit (i.e., the table of the courses)
tidy_student <- function(df){
  #rm everything above course code
  df <- df[-c(1:(which(df$...1 == "Course Code")-1)),]
  #rm everything after first NA
  df <- df[-c(min(which(is.na(df$...1))):nrow(df)),]
  #rm cols where NA in first row
  all_na <- function(x) any(!is.na(x))
  df <- df %>% select_if(all_na) %>% as.data.frame
  names(df) <- as.character(df[1,])
  df <- df[-1,] %>% as_tibble(.,.name_repair="universal") %>% janitor::clean_names(.)
  df %>%
    mutate(
      affected = ifelse(academic_year == "2019/0" & grepl("SEM2|YR|FLEX|SB23",x5), 1, 0),
      coursemarks_num = as.numeric(gsub("[^0-9.-]","",course_marks))
    )
}

# calculates the weighted mean for nested df
wgtmean_nested <- function(df){
  df %>% summarise(
    avg = weighted.mean(coursemarks_num, w=as.numeric(credits), na.rm=T)
  ) %>% pull(avg)
}

coursecombinations <- function(df){
  affected_courses = df$course_code[df$affected==1]
  # find all combinations of (Y3 and Y4 sem1) + any n of Y4 sem2
  comb_rows = map(0:sum(df$affected), ~combn(which(df$affected==1), ., simplify = F)) %>% 
    unlist(.,recursive = FALSE)
  
  # for each combination of courses, pull the data back in:
  comb_data = map_dfr(comb_rows, ~rbind(df[df$affected==0,],df[unlist(.),]), .id="combination") %>%
    nest(-combination)
  
  # for each combination, calc wgted average
  comb_data %>% mutate(
    avg = map_dbl(data,~wgtmean_nested(.)),
    aff_courses_incl = map(data, . %>% filter(affected==1) %>% pull(course_code)),
    aff_courses_excl = map(aff_courses_incl, ~affected_courses[!(affected_courses %in% .)])
  ) %>% select(-data,-combination) %>% arrange(desc(avg))
}

no_det <- function(df){
  uav = wgtmean_nested(df %>% filter(affected==0))
  av = wgtmean_nested(df)
  rm = c()
  while(av < uav){
    # incrementally remove most influential courses which are lower than sem1 av
    rm = c(rm, 
           df %>% filter(!(course_code %in% rm) & affected==1 & coursemarks_num<uav) %>%
             mutate(infl = (uav-coursemarks_num)*as.numeric(credits)) %>%
             filter(infl == max(infl)) %>% filter(row_number()==1) %>% 
             pull(course_code)
    )
    av = wgtmean_nested(df %>% filter(!(course_code %in% rm)))
  }
  return(list(av, rm))
}



fileinout<-function(filename){
  fileout = paste0(gsub(" |\\.xlsx","",filename),".csv")
  # read in data, nest by exam number
  # then tidy, then calculate means
  orig <- read_xlsx(filename, col_names = FALSE)
  
  orig %>% mutate(
    exam_number = ifelse(grepl("Exam Number", ...1), ...1, NA)
  ) %>% fill(exam_number) %>%
    group_by(exam_number) %>%
    nest() %>%
    mutate(
      # tidy up
      courseinfo = map(data, ~tidy_student(.)),
      
      # unnaffected average 
      unaffected_avg = map_dbl(courseinfo, . %>% filter(affected==0) %>% wgtmean_nested()),
      # full year average
      year_avg = map_dbl(courseinfo, ~wgtmean_nested(.)),
      # number of affected courses 
      n_affected_courses = map_dbl(courseinfo, ~sum(.$affected==1)),
      
      nd = map(courseinfo, ~no_det(.)),
      no_detriment_avg = map_dbl(nd, 1),
      no_detriment_excluded_courses = map(nd, 2) %>% unlist %>% paste(collapse=", "),
      
      
      #get the avgs for the different course combinations
      avgs = map(courseinfo, ~coursecombinations(.)),
      # pull out the best avg
      best_avg = map_dbl(avgs, ~max(.$avg)),
      
      # pull out the combo of excluded courses with best avg
      excl_courses = map(avgs, ~.$aff_courses_excl[.$avg == best_avg]),
      
      # occasionally, multiple combos will have same best avg,
      excl_opts = map_dbl(excl_courses, ~length(.)),
      # this will show preference for excluding no courses, or for excluding fewest additional:
      min_excl_course = map(excl_courses, ~.[lengths(.)==min(lengths(.))]),
      #make strings of courses
      excl_course_chr0 = map(min_excl_course, ~map(.,~unlist(.x) %>% paste(collapse=", "))),
      #collapse with OR if multiple
      excl_course_chr = ifelse(excl_opts>1, paste(unlist(excl_course_chr0),collapse=" OR "), unlist(excl_course_chr0))
    ) -> nested
  
  
  #If acceptable, we should just write the list of students and additional courses to a new spreadsheet:
  nested %>% 
    select(exam_number, unaffected_avg, best_avg, excl_course_chr, no_detriment_avg, no_detriment_excluded_courses) %>%
    mutate_if(is.numeric,~round(.,2)) %>%
    rename(
    `Y3_Y4S1_avg` = unaffected_avg,
    `Best_avg` = best_avg,
    `Best_avg - Exclude these courses` = excl_course_chr,
    `MCE_avg` = no_detriment_avg,
    `MCE_avg - Exclude these courses` = no_detriment_excluded_courses
  ) -> write_data
  
  write.csv(write_data, fileout, row.names=FALSE)
}


map(list.files(path = "lel_in/t",pattern="\\.xlsx",full.names = TRUE), ~fileinout(.))


fileinout("late/TO SEND TO JOSIAH PART THREE Psychology (MA Hons).xlsx")







#####
## If we have to edit the existing .xlsx workbook.

## okay, let's try to add to the xlsx while preserving the layout..
#these are the averages
uavgs <- nested$unaffected_avg
bavgs <- nested$best_avg 
bcourses <- ifelse(nested$excl_course_chr=="","None",nested$excl_course_chr)
mavgs <- nested$no_detriment_avg
mcourses <- ifelse(nested$no_detriment_excluded_courses=="","None",nested$excl_course_chr)

# these are the rows i want to add to:
startrows <- which(grepl("Y3 & Y4", orig$...1))+1

require(XLConnect)
wb <- loadWorkbook("~/Downloads/For josiah Progression and Awards Board Report_UTPHILY1.xlsx")
setStyleAction(wb,XLC$"STYLE_ACTION.NONE")

for(i in 1:length(startrows)){
  
  d1 = data.frame(
    Y3_Y4_Sem1 = uavgs[i],
    Best_avg = bavgs[i],
    Best_avg_excluded_courses = bcourses[i],
    Minimum_course_exclusion_avg = mavgs[i],
    Minimum_course_exclusion_courses = mcourses[i]
  )
  
  writeWorksheet(wb,d1,getSheets(wb)[1],startRow=startrows[i],startCol=39)
}
saveWorkbook(wb)
  






