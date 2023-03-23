#'  Quickly sim data for a basic model y~1+x+(1+x|g)
#' @param seed random seed to use
#' @param b0 fixed intercept
#' @param b1 fixed slope
#' @param z0 random intercept sd
#' @param z1 random slope sd
#' @param e residual var
#' @export
#' @examples
#' sim_basicrs()
#' library(lme4)
#' summary(lmer(y~1+x+(1+x|g),data=sim_basicrs()))
sim_basicrs <- function(seed=NULL,b0=0,b1=1,z0=1,z1=1,e=1){
  if(!is.null(seed)){
    set.seed(seed)
  }
  N = 200
  n_groups = 20
  g = rep(1:n_groups, e = N/n_groups)      # the group identifier
  x = rnorm(N)                             # an observation level continuous variable
  re0 = rnorm(n_groups, sd = z0)  # random intercepts
  re  = re0[g]
  rex = rnorm(n_groups, sd = z1)  # random effects
  re_x  = rex[g]
  lp = (b0 + re) + (b1 + re_x)*x
  y = rnorm(N, mean = lp, sd = e) # create a continuous target variable
  y_bin = rbinom(N, size = 1, prob = plogis(lp)) # create a binary target variable
  data.frame(x, g=factor(g), y, y_bin)
}
