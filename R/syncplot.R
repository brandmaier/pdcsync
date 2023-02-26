
#' @import gpgplot2
#' @import tidyverse
#' @export
syncplot <- function(x, ...) {
  
  t1 <- x$t1
  t2 <- x$t2

pp<-plot(x, lag_threshold = 0.3) + 
  geom_vline(xintercept=200, alpha=.4)+
  geom_vline(xintercept=400, alpha=.4)+
  theme(legend.position = "none") + xlim(0,1000)

t1p <- ggplot(data.frame(time=1:length(t1),val=t1),aes(x=time,y=val))+geom_line()+
  ylab(" \n ")+xlab("")
t2p <- ggplot(data.frame(time=1:length(t2),val=t2),aes(x=time,y=val))+geom_line()+
  ylab(" \n ")+xlab("")

cowplot::plot_grid(t1p, pp,t2p,ncol=1)

}