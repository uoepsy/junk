#' round to 20 point psychology marking scale.
#' will round to nearest point on scale. if mid-point, will round up.
#' @param x numeric vector
#' @param lgrade logical specifying whether letter grades are desired (default = FALSE)
#' @export
#' @examples
#' pscale(c(46.5,34,76.6))
#' [1] 48 32 78
pscale <- function(x,lgrade=FALSE){
  ps = data.frame(
    pscale = c(0, 15, 25,32,38,42,45,48, 52,55,58,62,65,68,72,75,78,85,92,100),
    pgrade = c("H","G","F","E","E","D","D","D","C","C","C","B","B","B","A3","A3","A3","A2","A1","A1")
  )
  if(lgrade){
    g = lapply(x, FUN = function(y) ps$pgrade[which.min(abs(ps$pscale-ifelse(y%%.5==0,y+.01,y)))])
  } else {
    g = lapply(x, FUN = function(y) ps$pscale[which.min(abs(ps$pscale-ifelse(y%%.5==0,y+.01,y)))])
  }
  g[lengths(g) == 0] <- NA
  unlist(g)
}
