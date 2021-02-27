changing <- function(t1, t2) {
  
  n_new = sum(t2$abund) - sum(t1$abund)
  
  n_increase = sum((t2$abund - t1$abund)[ t2$abund > t1$abund])
  n_decrease = sum(abs(t2$abund - t1$abund)[ t2$abund < t1$abund])
  n_change = (sum(abs(t2$abund - t1$abund)) - n_new) / 2
  
  
  
  
}