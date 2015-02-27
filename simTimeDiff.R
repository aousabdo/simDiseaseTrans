# We'll simulate a large population using the log-normal distribution
# we will then use this distribution to sample our 160 participants from

N = 1e5 # number to simulate for our population 
meanLog = 2 # the mean of the log-normal distribution
sdLog = 0.5 # the standard deviation of the log-normal distribution


# simulate the distribution with the given parameteres
tDelta <- rlnorm(n = N, meanlog = meanLog, sdlog = sdLog)

# plot the distribution
png('parent_log_normal_dist.png', width = 700, height = 550) # opening png graphic device to save image
hist(tDelta, col = 'blue', breaks = 100, probability = TRUE, 
                xlab = 'Time Difference (Hours)', main = 'Log-Normal Distribution (Parent Distribution)', 
                xlim = c(0, 50*sdLog))
dev.off() # closing graphic device


# notice that the log of the log normal distribution is normally distributed 
hist(log(tDelta), col = "green", probability = TRUE, xlab = 'Log of Time Difference', main = NULL)


# now we will use our distribution to sample our participants
tDeltaSample <- sample(tDelta, 160)

# let's plot these and see how they look like
png('sample_log_normal_dist.png', width = 700, height = 550)# opening png graphic device to save image
hist(tDeltaSample, col = 'green', breaks = 10, probability = TRUE, 
                xlab = 'Time Difference (Hours)', main = 'Log-Normal Distribution (Sample Distribution)', 
                xlim = c(0, 50*sdLog))
dev.off() # closing graphic device
