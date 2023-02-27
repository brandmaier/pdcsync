

#' @import ggplot2
#' @importFrom dplyr `%>%`
#' @export
syncplot <- function(x, annotated_segments = FALSE, ...) {
  t1 <- x$t1
  t2 <- x$t2
  
  pp <- plot(x, lag_threshold = 0.3) +
    geom_vline(xintercept = 200, alpha = .4) +
    geom_vline(xintercept = 400, alpha = .4) +
    theme(legend.position = "none") + xlim(0, 1000)
  
  t1p <-
    ggplot(data.frame(time = 1:length(t1), val = t1), aes(x = time, y = val)) +
    geom_line() +
    ylab(" \n ") + xlab("")
  t2p <-
    ggplot(data.frame(time = 1:length(t2), val = t2), aes(x = time, y = val)) +
    geom_line() +
    ylab(" \n ") + xlab("")
  
  # determine minimum curve fit (DUPLICATED CODE!)
  min_vals <- unlist(apply(x$rr, 2, min))
  min_lag <- unlist(apply(x$rr, 2, which.min))
  min_lag[abs(min_vals) > lag_threshold] <- NA
  
  if (annotaged_segments) {
    # center lag time series
    min_lag <- min_lag - (dim(x$rr)[1] - 1) / 2
    min_lag[is.na(min_lag)] <- 0
    min_lag <- sign(min_lag)
    
    ymin_t1 <- min(t1)
    ymax_t1 <- max(t1)
    
    ymin_t2 <- min(t2)
    ymax_t2 <- max(t2)
    
    # find coherent areas for annotation
    cur_start_pos <- 1
    cur_area <- min_lag[1]
    for (i in 2:length(min_lag)) {
      # switch?!
      if (cur_area != min_lag[i]) {
        if (cur_area == 1) {
          t1p <-
            t1p + annotate(
              "rect",
              xmin = cur_start_pos,
              xmax = i,
              ymin = ymin_t1,
              ymax = ymax_t1,
              alpha = 0.2,
              color = "green"
            )
        } else if (cur_area == -1) {
          t2p <-
            t2p + annotate(
              "rect",
              xmin = cur_start_pos,
              xmax = i,
              ymin = ymin_t2,
              ymax = ymax_t2,
              alpha = 0.2,
              color = "green"
            )
        }
        cur_start_pos <- i
        cur_area <- min_lag[i]
      }
    }
  }
  cowplot::plot_grid(t1p, pp, t2p, ncol = 1)
  
}