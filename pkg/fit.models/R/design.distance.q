design.distance <- function(object, ...)
  UseMethod("design.distance")


design.distance.default <- function(object, ...)
{
  m <- model.frame(object)

  if(is.null(m))
    return(rep(as.numeric(NA), length(fitted(object))))

  m.terms <- terms(m)
  attr(m.terms, "intercept") <- 0
  X <- model.matrix(m.terms, m)
  sqrt(mahalanobis(X, center = apply(X, 2, mean), cov = var(X)))
}

