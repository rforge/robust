leverage <- function(object, ...)
  UseMethod("leverage")


leverage.default <- function(object, ...)
{
  p <- length(coef(object))
  n <- length(fitted(object))
  rep(p/n, n)
}


leverage.lm <- function(object, ...)
  lm.influence(object, do.coef = FALSE)$hat



