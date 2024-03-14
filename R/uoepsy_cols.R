#' colours for courses
#'
#' @export
uoepsy_col <- function(course=NULL){
  coursecols = c(
    `dapr1` =  "#0F4C81",
    `dapr2` =  "#BF1932",
    `dapr3` =  "#88B04B",
    `usmr` =   "#FCBB06",
    `msmr` =   "#a41ae4",
    `ap` = "#670E36"
  )
  if(is.null(course)){coursecols} else {coursecols[course]}
}
