lmRob.lar <- function(x, y, tol=1e-6)
{
  x <- as.matrix(x)
  p <- ncol(x)
  n <- nrow(x)
  storage.mode(x) <- "double"
  storage.mode(y) <- "double"
  bet0 <- 0.773372647623  ## bet0 = pnorm(0.75)
  tmpn <- double(n)
  tmpp <- double(p)
  z1 <- .Fortran("rllarsbi",
                 x,
                 y,
                 as.integer(n),
                 as.integer(p),
                 as.integer(n),
                 as.integer(n),
                 as.double(tol),
                 NIT=integer(1),
                 K=integer(1),
                 KODE=integer(1),
                 SIGMA=double(1),
                 THETA=tmpn,
                 RS=tmpn,
                 SC1=tmpn,
                 SC2=tmpp,
                 SC3=tmpp,
                 SC4=tmpp,
                 BET0=as.double(bet0),
                 PACKAGE = "robust")
  list(coef=z1$THETA[1:p], scale=z1$SIGMA, resid=z1$RS)
}


