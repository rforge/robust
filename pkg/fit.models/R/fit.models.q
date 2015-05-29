fit.models <- function(model.list, ...)
{
  fm.call <- match.call()
  fm.call$attributes <- NULL

  dots <- list(...)
  dots.names <- names(dots)

  if(is.null(dots.names))
    dots.names <- character(length(dots))

  supported.classes <- unlist(sapply(e$fmreg, function(u) u$classes))


  ## The only way model list can be missing is if all the arguments in the call
  ## are named. In this case, assume dots is a collection of comparable fitted
  ## models.

  if(missing(model.list)) {
    model.list <- dots
    model.names <- dots.names
  }


  ## Otherwise model.list is either an unnamed (in the call) fitted model or
  ## a list of function names.

  else if(class(model.list)[1] %in% supported.classes) {
    model.list <- c(list(model.list), dots)
    model.names <- c("", dots.names)

    object.names <- as.character(fm.call[-1])
    empty.names <- (nchar(model.names) == 0)
    model.names[empty.names] <- object.names[empty.names]
  }

  else if(is.character(model.list) || class(model.list)[1] == "list") {
    model.list <- as.list(model.list)

    ## Little hack for robust package backward compatibility
    model.list[model.list == "cov"] <- "covMLE"

    n.models <- length(model.list)
    model.funs <- unlist(model.list)

    if(is.null(model.names <- names(model.list)))
      model.names <- model.funs

    model.call <- fm.call
    model.call$model.list <- NULL

    for(i in 1:n.models) {
      model.call[[1]] <- as.name(model.list[[i]])
      model.list[[i]] <- eval(model.call, sys.parent())
    }

    empty.names <- (nchar(model.names) == 0)
    model.names[empty.names] <- model.funs[empty.names]
  }

  else
    stop("impossible error: this should never happen!")

  if(any(nchar(model.names) == 0))
    stop("All models should be named")


  ## Each element of model.list should have a unique name.

  if(length(unames <- unique(model.names)) < length(model.list)) {
    for(n in unames) {
      idx <- (model.names == n)
      if(sum(idx) > 1)
        model.names[idx] <- paste(n, 1:sum(idx), sep = ".")
    }
  }

  names(model.list) <- model.names


  ## Now we should have a properly named list of fitted models.  Have to
  ## set the appropriate attributes.

  candidates <- lapply(e$fmreg, getElement, name = "classes")
  classes <- sapply(model.list, function(u) class(u)[1])

  ## First, the fm class must beable to compare all the classes.

  idx <- sapply(candidates, function(u) all(classes %in% u))
  candidates <- candidates[idx]

  if(!length(candidates)) {
    warning("fit.models cannot compare the provided models")
    return(invisible(model.list))
  }


  ## Try to choose the best comparable class.

  idx <- sapply(candidates, function(u) length(intersect(u, classes)))
  fmclass <- names(which(idx == max(idx)))[1]

  oldClass(model.list) <- fmclass
  model.list
}

