#' @title Heuristically determine a threshold for synchronisation
#' @name samplingBasedThreshold
#'
#' @param m embedding dimension
#' @param t time-delayed embedding
#' @param window_size integer; size of the window
#' @param num_samples integer; number of samples to determine empirical distribution
#' @param alpha numeric; alpha level to determine threshold based on empirical null distribution
#' 
#' @export
#' @importFrom pdc hellingerDistance codebook
samplingBasedThreshold <-
  function(t1,
           t2,
           window_size,
           m,
           t,
           num_samples = 5000,
           alpha = 0.01) {
    
    if (alpha<=0 | alpha>=1) {
      stop("Alpha must be larger than 0 and smaller than 1")
    }
    
    emp_dist <- replicate(n = num_samples, expr = {
      pos1 <- round(runif(1, 1, length(t1) - window_size))
      pos2 <- round(runif(1, 1, length(t2) - window_size))
      
      t1seg <- t1[pos1:(pos1 + window_size)]
      t2seg <- t2[pos2:(pos2 + window_size)]
      
      cb1 <- pdc::codebook(t1seg, m = m, t = t)
      cb2 <- pdc::codebook(t2seg, m = m, t = t)
      
      pdc::hellingerDistance(cb1, cb2)
    })
    
    # normal approximation
    #max(0.001, mean(emp_dist)-sqrt(var(emp_dist))*2)
    
    # non-parametric
    cutoff_id <- round(num_samples * alpha)
    sort(emp_dist)[cutoff_id]
  }