#' @title Get list of synchronised blocks
#' @export
getListOfSynchronisedBlocks <- function(x, type="sync") {
  rr <- x$rr
  
  bool_ts <-
    apply(rr, 2, function(xelem) {
      min(xelem, na.rm = TRUE)
    }) < x$lag_threshold
  
  leadlag_ts <-
    apply(rr, 2, function(xelem) {
      which.min(xelem)
    }) - (((nrow(rr)-1)/2)+1)
  
  if (type=="lead") {
    ts <- leadlag_ts>0
    ts[!bool_ts] <- FALSE
  } else if (type=="lag") {
      ts <- leadlag_ts<0
      ts[!bool_ts] <- FALSE
  } else if (type=="sync") {
    ts <- bool_ts
  } else {
    stop("Not implemented")
    return(NA)
  }
  
  get_consecutive_blocks <- function(bool_list) {
    blocks <- list()
    start <- NULL
    for (i in seq_along(bool_list)) {
      if (bool_list[i] && is.null(start)) {
        # start of a new block
        start <- i
      } else if (!bool_list[i] && !is.null(start)) {
        # end of a block
        blocks <- c(blocks, list(c(start, i - 1)))
        start <- NULL
      }
    }
    # If the last element of the list is TRUE, include it in the last block
    if (bool_list[length(bool_list)] && !is.null(start)) {
      blocks <- c(blocks, list(c(start, length(bool_list))))
    }
    return(blocks)
  }
  
  get_consecutive_blocks(ts)
}