print.summary.glmfm <- function(x, digits = max(3, getOption("digits") - 3),
                                ...)
{
    stopifnot((n.models <- length(x)) >= 1, sapply(x, is.list))
    i.Mod <- seq_len(n.models)

    mod.names <- names(x)
    fancy.names <- format(mod.names, justify = "right")
    model.list <- attr(x, "model.list")

    cat("\nCalls: \n")
    for(i in i.Mod) {
  cat(fancy.names[i], ": ", sep = "")
  print(x[[i]]$call, ...)
    }

    resid.qrtls <- t(sapply(x, function(u)
          quantile(u$deviance.resid,
             na.rm = TRUE, names = FALSE)))
    dimnames(resid.qrtls) <-
  list(paste(fancy.names, ":", sep = ""),
       c("Min", "1Q", "Median", "3Q", "Max"))

    cat("\nDeviance Residuals:\n")
    print(resid.qrtls, digits = digits, ...)

    coefs <- lapply(x, coef)
    coef.names <- format(rownames(coefs[[1]]), justify = "right")
    p <- length(coef.names)
    coef.matrix <- matrix(as.numeric(NA), n.models * p, 4)
    dimnames(coef.matrix) <-
  list(paste(fancy.names,
       if(n.models > 1) rep(coef.names, each = 2) else coef.names),
       c("Value", "Std. Error", "t value", "Pr(>|t|)"))

  for(i in i.Mod) {
    row.indices <- seq(from = i, by = n.models, length = p)
    coef.matrix[row.indices, 1:(dim(coefs[[i]])[2])] <- coefs[[i]]
  }

    cat("\nCoefficients:\n")
    print(coef.matrix, digits = digits, ...)

  cat("\nResidual Deviance:\n")
  for(i in i.Mod)
    cat(fancy.names[i], ": ", format(x[[i]]$deviance, digits = digits, ...),
        " on ", x[[i]]$df[2], " degrees of freedom\n", sep = "")

  if(all(sapply(x, function(u) !is.null(u$correlation)))) {
    correlations <- lapply(x, function(u) u$correlation)
    if(dim(correlations[[1]])[1] > 1) {
      cat("\nCorrelation of Coefficients:\n")
      for(i in i.Mod) {
        cormat <- format(correlations[[i]][2:p, 1:(p-1)], digits = digits, ...)
        cormat[row(cormat) < col(cormat)] <- ""
        cat(mod.names[i], ":\n", sep = "")
        print(cormat, quote = FALSE)
        cat("\n")
      }
    }
  }

  invisible(x)
}

