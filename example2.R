
#
# (I) generate two time series according to ARIMA
#       and break their synchrony in-between
#


# length of time series
N <- 1000

t1 < arima.sim(model = list(ar = 0.2), n = N)

# t1 is later than t2; t2 leads
t2 <- c(0,0,0,t1[1:(length(t1)-3)])+ rnorm(length(t1),0,0.01)

# no synchrony between 200 and 400
t2[200:400] <- runif(201, 0, 1)

#
# (II) Call the synchrony detection algorithm
#


sync <- pdc_synchrony(t1, t2,segment_width = 50)


