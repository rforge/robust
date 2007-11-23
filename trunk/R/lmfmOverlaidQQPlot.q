lmfmOverlaidQQPlot <- function(x, main, xlab, ylab, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  if(missing(main))
    main <- "Normal QQ Plot of Residuals"

  if(missing(xlab))
    xlab <- "Quantiles of Standard Normal"

  if(missing(ylab))
    ylab <- "Residuals"

  model <- sapply(x, function(u) !is.null(u$model))

	res <- na.omit(sapply(x, residuals))
  n <- length(res)
	px <- py <- matrix(0, n, n.models)

	for(i in 1:n.models) {
		tmp <- qqnorm(res[, i], plot.it = FALSE)
		px[, i] <- tmp$x
		py[, i] <- tmp$y
	}

  matplot(px, py,
    pch = 1:n.models,
    col = 1:n.models,
    xlab = xlab,
    ylab = ylab,
    main = main,
    ...)

  key(min(px), max(py),
    text = list(mod.names),
    lines = list(type = "p", col = 1:n.models, pch = 1:n.models),
    transparent = TRUE)

  invisible(x)
}


