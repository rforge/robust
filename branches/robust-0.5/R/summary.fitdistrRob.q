summary.fitdistrRob <- function(object, ...)
{
  object <- object[c("call", "estimate", "sd", "vcov", "mu", "V.mu")] 
	oldClass(object) <- "summary.fitdistrRob"
	object
}


