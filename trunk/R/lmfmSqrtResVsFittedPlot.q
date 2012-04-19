lmfmSqrtResVsFittedPlot <- function(x, type = "response", smooths = FALSE,
  rugplot = FALSE, id.n = 3, ...)
{
  n.models <- length(x)
  mod.names <- names(x)

  panel.special <- function(x, y, smooths, rugplot, id.n = 3, ...)
  {
    panel.xyplot(x, y, ...)
    panel.addons(x, y, smooths, rugplot, id.n = id.n)
    invisible()
  }

  res <- as.matrix(sqrt(abs(sapply(x, resid, type = type))))
  fit <- as.matrix(sapply(x, fitted))
  mod <- factor(rep(mod.names, each = nrow(res)), levels = mod.names)

  df <- data.frame(res = as.vector(res),
                   fit = as.vector(fit),
                   mod = mod)

  p <- xyplot(res ~ fit | mod,
              data = df,
              panel = panel.special,
              smooths = smooths,
              rugplot = rugplot,
              id.n = id.n,
              strip = function(...) strip.default(..., style = 1),
              layout = c(n.models, 1, 1),
              ...)

  print(p)
  invisible(p)
}


