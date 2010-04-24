#----------------------------------------------------------------#
# Robust Linear Regression                                       #
# Author: Jeffrey Wang & Kjell Konis                             #
# Date:   02/06/2002                                             #
# Insightful Corporation                                         #
#----------------------------------------------------------------#

lmRob <- function(formula, data, weights, subset,
		na.action, model = TRUE, x = FALSE, y = FALSE, contrasts = NULL,
		nrep = NULL, control = lmRob.control(...),
		genetic.control = NULL, ...)
{
  the.call <- match.call()
  m <- match.call(expand = FALSE)
  m$model <- m$x <- m$y <- m$contrasts <- m$nrep <- NULL
  m$control <- m$genetic.control <- m$... <- NULL
  m[[1]] <- as.name("model.frame")
  m <- eval(m, sys.parent())
  Terms <- attr(m, "terms")
  weights <- model.extract(m, weights)
  Y <- model.extract(m, "response")
  X <- model.matrix(Terms, m, contrasts)

  if(nrow(X) <= ncol(X))
    stop("Robust method is inappropriate: there are not enough observations")

  ##	In this section we make the model.matrix X and the sub
  ##	model.matices X1 and X2 such that X1 contains all of the
  ##	columns of X that correspond to factor variables and X2
  ##	contains all the columns of X that correspond to numeric
  ##	variables.
  ##  x1.idx[] are the indices of columns of X that are in X1.

  asgn <- attr(X, "assign")
  factor.vars <- names(m)[sapply(m, is.factor)]
  factors <- attr(Terms, "factors")

  factors <- if(length(factors))
    factors[factor.vars, , drop = FALSE] else matrix(0., nrow = 0, ncol = 0)

  order <- attr(Terms, "order")
  intercept <- attr(Terms, "intercept")

  all.factors <- if(nrow(factors) > 0)
    apply(factors > 0, 2, sum) == order else logical(0)

  x1.idx <- which(asgn %in% which(all.factors))
  x2.idx <- setdiff(1:(dim(X)[2]), x1.idx)

  if(intercept == 1) {
    ##	If there are both factor and numeric variables
    ##	then put the intercept term in X1

    if(length(x1.idx) > 0 && length(x2.idx) > 1) {
      x1.idx <- c(1, x1.idx)
      x2.idx <- x2.idx[-1]
    }

    ##	If the only column in X2 is the intercept
    ##	move it to X1 and set X2 to NULL.

    else if(length(x2.idx) == 1) {
      x1.idx <- c(1, x1.idx)
      x2.idx <- x2.idx[-1]
    }
  }

  ##	If X1 is empty then set it to NULL.

  if(length(x1.idx))
    X1 <- X[, x1.idx, drop = FALSE]
  else {
    x1.idx <- NULL
    X1 <- NULL
  }

  ##	If X2 is empty then set it to NULL.

  if(length(x2.idx))
    X2 <- X[, x2.idx, drop = FALSE]
  else
    X2 <- NULL

  if(length(weights))
    fit <- lmRob.wfit(X, Y, weights, x1 = X1, x2 = X2, x1.idx = x1.idx,
                      nrep = nrep, robust.control = control,
                      genetic.control = genetic.control)
  else
    fit <- lmRob.fit(X, Y, x1 = X1, x2 = X2, x1.idx = x1.idx, nrep = nrep,
                     robust.control = control,
                     genetic.control = genetic.control)

  if(is.null(fit))
    return(NULL)

  fit$terms <- Terms
  fit$call <- the.call

  effects <- X * matrix(fit$coefficients, nrow(X), ncol(X), byrow = TRUE)
  fit$effects <- sqrt(colSums(effects^2))
  fit$assign <- asgn
  if(model)
    fit$model <- m
  if(x)
    fit$x <- X
  if(y)
    fit$y <- Y
  attr(fit, "na.message") <- attr(m, "na.message")
  if(!is.null(attr(m, "na.action")))
    fit$na.action <- attr(m, "na.action")

  fit
}


