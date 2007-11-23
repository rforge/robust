get.fit.models.database <- function()
{

	fmdb <- list()

	##
	##	This function returns a list containing one element for
	##	each set of comparable models.  These elements are in
	##	turn lists with three components: (1) a character vector
	##	containing the classes in each set, (2) a character vector
	##	with one element containing the virtual class whose methods
	##	should be used, and (3) a validation function.
	##

	##	Add linear models

	classes <- c("lmRob", "lm")
	object.class <- "lmfm"
	validation.function <- NULL

	fmdb[["lmfm"]] <- list(	classes = classes,
													object.class = object.class,
													validation.function = validation.function)

	##	Add generalized linear models

	classes <- c("glmRob", "glm")
	object.class <- "glmfm"
	validation.function <- NULL

	fmdb[["glmfm"]] <- list(classes = classes,
													object.class = object.class,
													validation.function = validation.function)

	##	Add AOV models

	classes <- c("aovRob", "aov")
	object.class <- "aovfm"
	validation.function <- NULL

	fmdb[["aovfm"]] <- list(classes = classes,
													object.class = object.class,
													validation.function = validation.function)

	##	Add AOV models

	classes <- c("discRob", "discrim")
	object.class <- "discfm"
	validation.function <- NULL

	fmdb[["discfm"]] <- list(classes = classes,
													object.class = object.class,
													validation.function = validation.function)

	##	Add covariance models

	classes <- c("covRob", "cov")
	object.class <- "covfm"
	validation.function <- NULL

	fmdb[["covfm"]] <- list(classes = classes,
													object.class = object.class,
													validation.function = validation.function)

	##	Add asymmetric models

	classes <- c(	"gammaRob", "gammaMLE", "weibullRob", "weibullMLE",
								"lognormRob", "lognormMLE")
	object.class <- "asymfm"
	validation.function <- NULL

	fmdb[["asymfm"]] <- list(classes = classes,
													object.class = object.class,
													validation.function = validation.function)

	##	Add principal components models

	classes <- c(	"princompRob", "princomp")
	object.class <- "pcompfm"
	validation.function <- NULL

	fmdb[["pcompfm"]] <- list(classes = classes,
													object.class = object.class,
													validation.function = validation.function)

	fmdb
}


