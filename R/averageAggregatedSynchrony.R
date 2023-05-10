
#' A function to compute an aggregate (parameter 'fun2') of a function (parameter 'fun1') of
#' the synchrony values per time points.
#' 
#' @returns a number between 0 and 1 with higher values
#           indicating more synchrony
#' @export
averageAggregatedSynchrony <- function(x, fun1=min, fun2=mean) {
  if (!inherits(x,"pdcsync")) {
    stop("Unknown data structure")
  }
  tp_vals <- apply(x$rr,2, function(y) { fun1(y,na.rm = TRUE)})
  1-fun2(tp_vals, na.rm=TRUE) #invert scale, such that higher values mean more synchrony
}

