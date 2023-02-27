#' @alias pdcsync
#'
#' @importFrom dplyr `%>%`
#' @import ggplot2
#' @exportS3Method plot pdcsync
plot.pdcsync <- function(x, lag_threshold=NULL, 
                         show_sync_line=TRUE,
                         sync_lwd=2,
                         sync_col="red",
                         show_center_line=TRUE,
                         ...) {
  
  if (is.null(lag_threshold))
    lag_threshold <- x$lag_threshold
  
  colnames(x$rr) <- paste0("V",1:dim(x$rr)[2])
  
  result <- x$rr %>%
    dplyr::as_tibble() %>%
    tibble::rowid_to_column(var = "X") %>%
    tidyr::gather(key = "Y", value = "Z",-1) %>%
    
    # Change Y to numeric
    dplyr::mutate(Y = as.numeric(gsub("V", "", Y)))
  
  # determine minimum curve fit
  min_vals <- unlist(apply(x$rr, 2, min))
  min_lag <- unlist(apply(x$rr, 2, which.min))
  min_lag[abs(min_vals) > lag_threshold] <- NA
  
  lag_df <- data.frame(time = 1:length(min_lag), min_lag)

  
  ycenter <- (dim(x$rr)[1]-1)/2
  
  plt <- result %>% #na.omit() %>%
    ggplot(aes(x = Y, y = X, fill = Z)) + 
    geom_raster(interpolate = FALSE) +
    xlab("Time") + 
    ylab("Synchrony Offset\nLower-half values indicate that T1 leads") +
    #scale_fill_gradient(low = "blue", high = "white") +
    scale_fill_gradient(
      trans = "log",
      low = "blue",
      high = "white",
      breaks = c(0, 0.0005, 0.005, 0.05, 0.05, 0.5)
    ) +
    labs(fill='Sync.\nStrength')

    if (show_center_line)
  plt <- plt + geom_hline(
    yintercept = ycenter,
    alpha = .7,
    lwd = 2,
    color = "black"
  ) 
  
  if (show_sync_line) {
    plt <- plt + geom_line(data=lag_df,aes(x=time,y=min_lag,fill=NULL),
                           lwd=sync_lwd,
                           color=sync_col)
  }
    
  plt
}