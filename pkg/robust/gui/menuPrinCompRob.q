menuPrinCompRob <- function(
		data,
		variables = names(data),
		na.method = "fail",
		rob.rad.p = "Both",
		corr = F,
		control = NULL,
		short.p = F,
		importance = T,
		loadings = F,
		cutoff = 0.1,
		sp.plot = T,
		ld.plot = T,
		var.plot = T,
		spcomps = "All",
		method = "Auto",
		quan = "Auto",
		trial = 500,
		autoOne = "Auto",
		autoTwo = "Auto",
		random.sample = F,
		strOne = .45,
		strTwo = .05,
		strThree = 1e-006,
		strFour = .001,
		rob.int = 150)

{
	control.call <- call("covRob.control")
	switch(method,
		"Donoho-Stahel" = {
			method <- "donostah"
			control.call$estim <- "donostah"

			if(autoOne != "Auto") {
				if(autoOne == "All")
					control.call$nresamp <- 0
				else
					control.call$nresamp <- as.integer(autoOne)
			}

			if(autoTwo != "Auto")
				control.call$maxres <- as.integer(autoTwo)
			if(random.sample)
				control.call$random.sample <- T
			if(strOne != .95)
				control.call$tune <- strOne
			if(strTwo != .99)
				control.call$prob <- strTwo
			if(strThree != .5)
				control.call$eps <- strThree
		},
		
		"M" = {
			control.call$estim <- "M"
			if(quan != "Auto")
				control.call$quan <- as.numeric(quan)
			if(trial != 500)
				control.call$ntrial <- trial
			if(strOne != .45)
				control.call$r <- strOne
			if(strTwo != .05)
				control.call$alpha <- strTwo
			if(rob.int != 150)
				control.call$maxit <- rob.int
			if(strThree != 1e-006)
				control.call$tau <- strThree
			if(strFour != .001)
				control.call$tol <- strFour
		},
		
		"MCD" = {
			control.call$estim <- "MCD"
			if(quan != "Auto")
				control.call$quan <- as.numeric(quan)
			if(trial != 500)
				control.call$ntrial <- trial
		}
	)

	data <- as.data.frame(data)
	data.name <- deparse(substitute(data))

	var.call <- NULL
	if(!missing(variables))
		variables <- sapply(unpaste(variables, sep = ","), strip.blanks)

	if(!is.element(variables[[1]], c("<ALL>", "(All Variables)"))) {
		if(!length(variables))
			stop("You must select at least one variable\n")
		data <- data[, variables, drop = F]
		var.call <- vector("list", length(variables) + 1)
		var.call[[1]] <- as.name("c")
		var.call[2:(length(variables) + 1)] <- variables
	}

	dropped.cols <- !sapply(data, is.numeric) | sapply(data, is.dates)

	if(all(dropped.cols))
		stop("No numeric columns specified.")

	if(any(dropped.cols)) {
		warning(paste("Dropping non-numeric column(s) ", paste(names(
			data)[dropped.cols], collapse = ", "), ".", sep = ""))

		if(is.null(var.call))
			variables <- names(data)[!dropped.cols]
		else
			variables <- variables[!dropped.cols]

		data <- data[, !dropped.cols, drop = F]
		var.call <- vector("list", length(variables) + 1)
		var.call[[1]] <- as.name("c")
		var.call[2:(length(variables) + 1)] <- variables
	}

	
	fun.call <- match.call()

	tmp.call <- function(x) {
		match.call()[[2]]
	}

	if (is.null(var.call))
		fun.call$data <- as.name(data.name)
	else {
		data.call <- tmp.call(data[,variables,drop=F])
		data.call[[2]] <- as.name(data.name)
		data.call[[4]] <- as.call(var.call)
		fun.call$data <- as.call(data.call)
	}
	
	fun.call$na.method <- NULL

	if(rob.rad.p == "Classical" || rob.rad.p == "Both")
	{
		fun.args <- is.element(arg.names(fun.call), arg.names(princomp))
		classic.call <- fun.call[c(T, fun.args)]
		classic.call[[1]] <- as.name("princomp")
		if(na.method == "omit")
			classic.call$na.action <- substitute(na.omit)
		if(corr == "Correlations")
			classic.call$cor <- T
	}
	
	if(rob.rad.p == "Robust" || rob.rad.p == "Both")
	{
		fun.args <- is.element(arg.names(fun.call), arg.names(princompRob))
		robust.call <- fun.call[c(T, fun.args)]
		robust.call[[1]] <- as.name("princompRob")
		if(method != "Auto")
			robust.call$estim <- method
		if(length(control.call) > 2)
			robust.call$control <- substitute(control.call)
		if(na.method == "omit")
			robust.call$na.action <- substitute(na.omit)
		if(corr == "Correlations")
			robust.call$corr <- T
		else
			robust.call$corr <- NULL
	}

	if(rob.rad.p == "Both") {
		fun.call <- call("fit.models", model.list = list(Robust = robust.call,
											Classical = classic.call))
		header.txt <- paste("\n\t***  Classical and Robust Principal Component Analyses for data in: ",
			data.name, "  ***\n\n")
	}

	else if(rob.rad.p == "Classical") {
		fun.call <- call("fit.models", model.list = list(Classical = classic.call))
		header.txt <- paste("\n\t***  Classical Principal Component Analysis for data in: ",
			data.name, "  ***\n\n")
	}

	else {
		fun.call <- call("fit.models", model.list = list(Robust = robust.call))
		header.txt <- paste("\n\t***  Robust Principal Component Analysis for data in: ",
			data.name, "  ***\n\n")
	}

	coeff <- eval(fun.call)

	if(short.p) {
		print(coeff)
	}

	if(importance) {
		cat(header.txt)
		print(summary(coeff, loadings = loadings, cutoff = cutoff))
	}

	if(sp.plot || ld.plot || var.plot) {
		if(spcomps != "Auto")
			spcomps <- try(eval(parse(text = spcomps)))
		
		if(class(spcomps) == "Error") {
			warning("\"Which Components\" not a valid S expression, using default.")
			spcomps <- "Auto"
		}

		plot.pcompfm(	x = coeff,
									which.plots = (2:4)[c(sp.plot,ld.plot, var.plot)],
									spcomps = spcomps)
	}

	invisible(coeff)
}








