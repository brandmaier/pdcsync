library(pdc)
library(tidyverse)
library(ggplot2)

#' @title pdc_synchrony
#'
#' @param t1
#' @param t2
#' @param segment_width
#' @param search_width
#'
#' @export
pdc_synchrony <- function(t1,
                          t2,
                          segment_width = 100,
                          search_width = 25,
                          lag_threshold = 0.05,
                          m = NULL,
                          t = NULL) {
  if (length(t1) != length(t2))
    stop("Time series not of identical length")
  
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
  
  
  
  max_len <- (len - segment_width - 1)
  
  # first time point of return object corresponds to
  #

  rr <- sapply(1:(len - segment_width - 1), function(pos1) {
    t1_seg <- t1[pos1:(pos1 + segment_width)]
    cb1 <- pdc::codebook(t1_seg, m = m, t = t)
    
    # searchlight (all start positions to search the t2 series;
    # between pos1-search_width and pos1+search_width
    t2_start <- max(1, pos1 - search_width)
    t2_end <- min(length(t2), pos1 + search_width)
    t2_rng <- t2_start:t2_end
    
    #if (length(t2_rng) < (search_width * 2 + 1)) {
    #  return(rep(NA, search_width * 2 + 1))
    #}
    
    search_window <- sapply(
      t2_start:t2_end,
      FUN = function(pos2) {
        # cat("  |- Pos 2 from",pos2," to ", (pos2+segment_width),"\n")
        t2_seg <- t2[pos2:(pos2 + segment_width)]
        cb2 <- pdc::codebook(t2_seg, m = m, t = t)
        hellingerDistance(cb1, cb2)
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
    rr = rr,
    lag_threshold = lag_threshold,
    search_width = search_width,
    m = m,
    t = t
  )
  class(result) <- "pdcsync"
  
  return(result)
  
}

plot.pdcsync <- function(x, lag_threshold=NULL, ...) {
  
  if (is.null(lag_threshold))
    lag_threshold <- x$lag_threshold
  
  result <- x$rr %>%
    as_tibble() %>%
    rowid_to_column(var = "X") %>%
    gather(key = "Y", value = "Z",-1) %>%
    
    # Change Y to numeric
    mutate(Y = as.numeric(gsub("V", "", Y)))
  
  # determine minimum curve fit
  min_vals <- unlist(apply(x$rr, 2, min))
  min_lag <- unlist(apply(x$rr, 2, which.min))
  min_lag[abs(min_vals) > lag_threshold] <- NA
  
  lag_df <- data.frame(time = 1:length(min_lag) + x$search_width, min_lag)
  
  result %>% #na.omit() %>%
    ggplot(aes(x = Y, y = X, fill = Z)) + geom_raster(interpolate = FALSE) +
    #geom_tile()+
    geom_hline(
      yintercept = 26,
      alpha = .7,
      lwd = 2,
      color = "black"
    ) +
    xlab("Time") + ylab("Synchrony Offset\nLower-half values indicate that T1 leads") +
    #scale_fill_gradient(low = "blue", high = "white") +
    scale_fill_gradient(
      trans = "log",
      low = "blue",
      high = "white",
      breaks = c(0, 0.0005, 0.005, 0.05, 0.05, 0.5)
    ) +
    labs(fill='Sync.\nStrength')+
      geom_line(data=lag_df,aes(x=time,y=min_lag,fill=NULL),lwd=2,color="red")+
    NULL
}

print.pdcsync <- function(x, ...) {
  cat(
    "PDC Synchrony object of two time series; embedding dimension m =",
    x$m,
    " and t = ",
    x$t,
    ". Use plot() command to display synchrony profile.\n"
  )
}