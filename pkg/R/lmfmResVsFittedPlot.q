lmfmResVsFittedPlot <- function(x, type = "response", smooths = FALSE,
                                rugplot = FALSE, id.n = 3, ...)
{
  n.models <- length(x)
  mod.names <- names(x)
  n <- length(residuals(x[[1]]))

  panel.special <- function(x, y, smooths, rugplot, id.n, ...)
  {
    panel.xyplot(x, y, ...)
    panel.addons(x, y, smooths = smooths, rugplot = rugplot, id.n = id.n)
    panel.abline(h = 0, lty = 2)
    invisible()
  }

  mod <- factor(rep(mod.names, each = n), levels = mod.names)

  df <- data.frame(r = as.vector(sapply(x, residuals, type = type)),
                   f = as.vector(sapply(x, fitted)),
                   mod = mod)

  p <- xyplot(r ~ f | mod,
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


