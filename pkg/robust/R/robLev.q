robMD2 <- function(object, scale = c("md2", "hat"), X = NULL)
{
  scale <- match.arg(scale)
  m <- model.frame(object)

  if(is.null(m))
    return(rep(as.numeric(NA), length(fitted(object))))

  m.terms <- terms(m)

  if(is.null(X))
    X <- model.matrix(m.terms, m)

  dc <- attr(m.terms, "dataClasses")
  tl <- attr(m.terms, "term.labels")
  numeric.vars <- intersect(tl, names(dc)[dc == "numeric"])

  if(length(numeric.vars)) {
    X2 <- as.matrix(m[numeric.vars])
    mcd <- covMcd(X2)
    w <- mcd$mcd.wt
    sum.w <- sum(w)

    mu <- apply(X2, 2, weighted.mean, w = w)

    X2.tilde <- sweep(X2, 2, mu)
    X2.tilde <- sqrt(prod(mcd$cnp) * (nrow(X2.tilde) - 1)/(sum.w - 1)) * w * X2.tilde

    m[numeric.vars] <- sweep(X2.tilde, 2, mu, FUN = "+")
  }

  D <- model.matrix(m.terms, m)

  if(scale == "hat") {
    R <- qr.R(qr(D))
    U <- backsolve(R, t(X), transpose = TRUE)
    ans <- colSums(U^2)
  }
  
  else {
    if(attr(m.terms, "intercept")) {
      X <- X[, -1, drop = FALSE]
      D <- D[, -1, drop = FALSE]
    }
    ans <- mahalanobis(X, apply(D, 2, mean), var(D))
  }

  ans
}


hatvalues.lmRob <- function(model, ...)
  robMD2(model, scale = "hat", X = model$x)


## Methods for other packages ##


hatvalues.lmrob <- function(model, ...)
  robMD2(model, scale = "hat")


hatvalues.glmrob <- function(model, ...)
  robMD2(model, scale = "hat")


hatvalues.rlm <- function(model, ...)
  robMD2(model, scale = "hat")








