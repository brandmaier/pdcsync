source("pdc_synchrony.R")

#
# (I) generate two time series according to ARIMA
#       and break their synchrony in-between
#


# length of time series
N <- 1000

t1 < arima.sim(model = list(ar = 0.2), n = N)

# t1 is later than t2; t2 leads
#t2 <- c(0,0,0,t1[1:(length(t1)-3)])+ rnorm(length(t1),0,0.01)
# t2 is later than t1; t1 leads
t2 <- c(t1[4:(length(t1))], 0, 0, 0) + rnorm(N, 0, 0.001)


# no synchrony between 200 and 400
t2[200:400] <- runif(201, 0, 1)

#
# (II) Call the synchrony detection algorithm
#


sync <- pdc_synchrony(t1, t2)

plot(sync) + 
  geom_vline(xintercept=200, alpha=.4)+
  geom_vline(xintercept=400, alpha=.4)


