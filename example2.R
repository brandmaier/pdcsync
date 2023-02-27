
#
# (I) generate two time series according to ARIMA
#       and break their synchrony in-between
#
library(pdcsync)

# length of time series
N <- 1000

ta1 <- arima.sim(model = list(ar = 0.2), n = N)

# t1 is later than t2; t2 leads
ta2 <- c(0,0,0,ta1[1:(length(ta1)-3)])+ rnorm(length(ta1),0,0.01)

# no synchrony between 200 and 400
ta2[200:400] <- runif(201, 0, 1)

#
# (II) Call the synchrony detection algorithm
#


sync <- pdcsync(ta1, ta2,segment_width = 50)

syncplot(sync)

syncplot(sync, annotated_segments = TRUE, show_sync_line = FALSE)

plot(sync, show_sync_line = FALSE)

plot(sync, show_sync_line = TRUE, sync_col="green")
