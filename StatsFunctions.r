
wald.test <- function (R, r, lmdl)
{
  # Arguments
  # object
  # an object. See below for details.
  #
  # …
  # further object specifications passed to methods. See below for details.
  #
  # vcov
  # a function for estimating the covariance matrix of the regression coefficients, e.g., vcovHC. If only two models are compared it can also be the covariance matrix of the more general model.
  #
  # test
  # character specifying whether to compute the large sample Chi-squared statistic (with asymptotic Chi-squared distribution) or the finite sample F statistic (with approximate F distribution).
  #
  # name
  # a function for extracting a suitable name/description from a fitted model object. By default the name is queried by calling formula.
  #
  # data
  # a data frame containing the variables in the model.
  
  validationMessage = ""
  if (class(R) != "matrix") {
    validationMessage = validationMessage + sprintf("\nR variable must be a matrix.")
  }
  if (class(r) != "matrix") {
    validationMessage = validationMessage + sprintf("\nr variable must be a matrix.")
  }
  if (class(lmdl) != "lm") {
    validationMessage = validationMessage + sprintf("\nlml variable must be a linear model")
  }
  
  if (validationMessage != "") {
    stop(validationMessage)
  }
  
  nCoef = length(lmdl$coefficients)
  nRest = nrow(R)
  n = length(lmdl$residuals)
  
  Q = t((R %*% lmdl$coefficients - r)) %*% solve(R %*% (vcov(lmdl) / n) %*% t(R)) %*% (R %*% lmdl$coefficients - r)
  
  pval = pchisq(Q, df = nRest, lower.tail = F)
  
  colnames(R) = names(lmdl$coefficients)
  cat(sprintf("H0: R theta -r = 0 \n"))
  cat(sprintf("test statistic: %0.4f\tp-value: %0.4f", Q, pval))
  cat(R)
}

