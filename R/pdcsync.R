

#' @title pdc_synchrony
#'
#' @param t1 first time series; must be a numeric vector
#' @param t2 second time series; must be a numeric vector
#' @param segment_width width of the segment
#' @param search_width search width 
#'
#'
#' @importFrom pdc hellingerDistance
#' @export
pdcsync <- function(t1,
                          t2,
                          segment_width = 100,
                          search_width = 25,
                          lag_threshold = NULL,
                          m = NULL,
                          t = NULL) {
  
  if (length(t1) != length(t2)) {
    stop("Time series not of identical length")
  }
    
  len <- length(t1)
  
  if (is.null(m) | is.null(t)) {
    if (is.null(m)) {
      m_min <- 3
      m_max <- 7
    } else {
      m_min <- m
      m_max <- m
    }
    if (is.null(t)) {
      t_min <- 1
      t_max <- 1
    } else {
      t_min <- t
      t_max <- t
    }
    
    mine <- pdc::entropyHeuristic(
      t1,
      t.min = t_min,
      t.max = t_max,
      m.min = m_min,
      m.max = m_max
    )
    m <- mine$m
    t <- mine$t
  }
  
  if (is.null(lag_threshold)) {
    lag_threshold <- samplingBasedThreshold(t1, t2, window_size=segment_width, m=m, t=t)
  }
  
  max_len <- (len - search_width - 1)

  rr <- sapply(1:(len - segment_width - 1), function(pos1) {
    t1_seg <- t1[pos1:(pos1 + segment_width)]
    cb1 <- pdc::codebook(t1_seg, m = m, t = t)
    
    # searchlight (all start positions to search the t2 series;
    # between pos1-search_width and pos1+search_width
    t2_start <- max(1, pos1 - search_width)
    t2_end <- min(length(t2), pos1 + search_width)
    t2_rng <- t2_start:t2_end
    
    search_window <- sapply(
      t2_start:t2_end,
      FUN = function(pos2) {
        # cat("  |- Pos 2 from",pos2," to ", (pos2+segment_width),"\n")
        t2_seg <- t2[pos2:(pos2 + segment_width)]
        cb2 <- pdc::codebook(t2_seg, m = m, t = t)
        pdc::hellingerDistance(cb1, cb2)
      }
    )
    
    if (length(search_window) < search_width*2+1) {
      #search_window <- rep(NA, search_width * 2 + 1)
      full_len <- search_width*2+1
      # prefill with NA
      search_window <- c(rep(NA,full_len-length(search_window)),search_window)
    }
    
    search_window
    
    
  })
  
  result <- list(
    t1 = t1,
    t2 = t2,
    rr = rr,
    lag_threshold = lag_threshold,
    search_width = search_width,
    m = m,
    t = t
  )
  class(result) <- "pdcsync"
  
  return(result)
  
}


#' @exportS3Method print pdcsync
print.pdcsync <- function(x, ...) {
  cat(
    "PDC Synchrony object of two time series; embedding dimension m =",
    x$m,
    " and t = ",
    x$t,
    ". Use plot() command to display synchrony profile.\n"
  )
}