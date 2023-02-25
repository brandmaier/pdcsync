#' @export
#' 
samplingBasedThreshold <- function(t1, t2, window_size, m, t, num_samples = 2000) {
  
  emp_dist <- replicate(n=num_samples, expr={
  pos1 <- round(runif(1, 1, length(t1)-window_size))
  pos2 <- round(runif(1, 1, length(t2)-window_size))
  
  t1seg <- t1[pos1:(pos1+window_size)]
  t2seg <- t2[pos2:(pos2+window_size)]
  
  cb1 <- codebook(t1seg, m = m, t = t)
  cb2 <- codebook(t2seg, m = m, t = t)
  
  hellingerDistance(cb1, cb2)
  })
  
  max(0.001, mean(emp_dist)-sqrt(var(emp_dist))*2)
}