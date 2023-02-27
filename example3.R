source("pdc_synchrony.R")

#
# (I) generate two time series according to ARIMA
#       and break their synchrony in-between
#


# length of time series
N <- 1000

t1 <- arima.sim(model = list(ar = 0.2), n = N)

# t1 is later than t2; t2 leads
t2 <- t1

# no synchrony between 200 and 400
t2[1:50] <- runif(50, 0, 1)
t2[200:250] <- runif(51, 0, 1)

#
t2[500:700] <- t1[510:710]

#
# (II) Call the synchrony detection algorithm
#


sync <- pdcsync(t1, t2)

pp<-plot(sync, lag_threshold = 0.3) + 
  geom_vline(xintercept=50, alpha=.4)+
  geom_vline(xintercept=200, alpha=.4)+
  geom_vline(xintercept=500, alpha=.4)+
  geom_vline(xintercept=700, alpha=.4)+
  theme(legend.position = "none") + xlim(0,1000)

t1p <- ggplot(data.frame(time=1:length(t1),val=t1),aes(x=time,y=val))+geom_line()+
  ylab(" \n ")+xlab("")
t2p <- ggplot(data.frame(time=1:length(t2),val=t2),aes(x=time,y=val))+geom_line()+
  ylab(" \n ")+xlab("")
cowplot::plot_grid(t1p, pp,t2p,ncol=1)


