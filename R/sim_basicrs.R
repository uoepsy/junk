#'  Quickly sim data for a basic model y~1+x+(1+x|g)
#' @param seed random seed to use
#' @param b0 fixed intercept
#' @param b1 fixed slope
#' @param z0 random intercept va
#' @param z1 random slope var
#' @param zz int-slope covar
#' @param e residual var
#' @param eq equality of x between groups
#' @export
#' @examples
#' sim_basicrs()
#' library(lme4)
#' summary(lmer(y~1+x+(1+x|g),data=sim_basicrs()))
sim_basicrs <- function(seed=NULL,b0=0,b1=1,z0=1,z1=1,zz=0,e=1,eq=TRUE){
  if(!is.null(seed)){
    set.seed(seed)
  }

  N = 200
  n_groups = 20
  g = rep(1:n_groups, e = N/n_groups)

  if(eq){
    x = rep(1:(N/n_groups), n_groups)
  } else {
    x = rnorm(N)
  }

  Gcov = Matrix::nearPD(matrix(c(z0,zz,zz,z1),nrow=2))$mat
  res = MASS::mvrnorm(n_groups, mu=c(0,0), Sigma=Gcov)
  re  = res[,1][g] # random intercepts
  re_x  = res[,2][g] # random slopes
  lp = (b0 + re) + (b1 + re_x)*x
  y = rnorm(N, mean = lp, sd = e) # create a continuous target variable
  y_bin = rbinom(N, size = 1, prob = plogis(lp)) # create a binary target variable
  data.frame(x, g=factor(g), y, y_bin)
}
