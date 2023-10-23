#' @title odds ratio with 95 percent confidence interval
#' @description a function that returns the odds ratio with a 95 percent confidence interval
#' @param coef beta coefficient from logistic regression
#' @param se standard error of the beta coefficient
#' @param siglevel alpha/significance level you wish to use (ex. 0.95)
#' @param roundto the multiple to which the number will be rounded (default is 1)
#' @return an odds ratio with an upper and lower 95 percent confidence interval
#' @author Kayla Esser
#' @examples
#' library(rms)
#' toy.lrm <- rms::lrm(y ~ x1 + x2, data = toydata, x = TRUE, y = TRUE)
#' toy.lrm
#' OR_95CI(1.0055, 0.2534, 0.95, 1)
#' OR_95CI(0.8937, 0.5948, 0.95, 2)
#' @export


OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}
