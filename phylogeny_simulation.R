## Install the packages
library( geiger )
library( TreeSim )

# Generate a phylogeny (sample size is 1000), 
       #n=Number of extant sampled tips.
       #numbsim=Number of trees to simulate.
       #lambda=Speciation rate
       #mu=Extinction rate
       #use true for faster critical processing
phy1 <- sim.bd.taxa(n = 1000, numbsim = 1, lambda = 0.2, mu = 0.1
                   , complete = FALSE)
phy1 <- phy1[[1]] # Take the phylogeny out of the list.
## Make a plot of the simulated phylogeny.
plot( phy1, direction = "upwards")

# Define parameters:
# variance (here named "par")
# mean (here named "root")
sim1 <- sim.char(phy = phy1, par = (0.5)^2, model = "BM", root = 10)
sim1 <- sim1[,,1]
hist( sim1 )

# Estimate parameters:
# We use parameter estimates to get the value of the parameters given the observed data.
fit <- fitContinuous(phy = phy1, dat = sim1, model = "BM")
class( fit )
fit$opt$sigsq # Variance
fit$opt$z0 # Mean

## Problem: Reduce sample size (number of species in the phylogeny) and show how the parameters change.

#simulation 2-> n=500
phy2 <- sim.bd.taxa(n = 500, numbsim = 1, lambda = 0.2, mu = 0.1, complete = FALSE)
phy2 <- phy2[[1]] # Take the phylogeny out of the list.
plot( phy2, direction = "upwards")
sim2 <- sim.char(phy = phy2, par = 0.25, model = "BM", root = 10)
               #root=mean, par= variance->sd^2
sim2 <- sim2[,,1]
hist( sim2 )
fit2 <- fitContinuous(phy = phy2, dat = sim2, model = "BM")
class( fit )
fit2$opt$sigsq 
fit2$opt$z0
# variance and mean increased and became closer to inputed values

#simulation 3-> n=250
phy3 <- sim.bd.taxa(n = 250, numbsim = 1, lambda = 0.2, mu = 0.1, complete = FALSE)
phy3 <- phy3[[1]] # Take the phylogeny out of the list.
plot( phy3, direction = "upwards")
sim3 <- sim.char(phy = phy3, par = 0.25, model = "BM", root = 10)
#root=mean, par= variance->sd^2
sim3 <- sim3[,,1]
hist( sim3 )
fit3 <- fitContinuous(phy = phy3, dat = sim3, model = "BM")
class( fit3 )
fit3$opt$sigsq 
fit3$opt$z0
#variance still within anticipated range, mean has decreased a bit off -1.5

#simulation 4-> n=125
phy4 <- sim.bd.taxa(n = 125, numbsim = 1, lambda = 0.2, mu = 0.1, complete = FALSE)
phy4 <- phy4[[1]] # Take the phylogeny out of the list.
plot( phy4, direction = "upwards")
sim4 <- sim.char(phy = phy4, par = 0.25, model = "BM", root = 10)
#root=mean, par= variance->sd^2
sim4 <- sim4[,,1]
hist( sim4 )
fit4 <- fitContinuous(phy = phy4, dat = sim4, model = "BM")
class( fit4 )
fit4$opt$sigsq 
fit4$opt$z0
#variance still within anticipated range -.01,
#mean increased from last simulation by +.4 and -1.5 from anticipated


#simulation 5-> n=63
phy5 <- sim.bd.taxa(n = 63, numbsim = 1, lambda = 0.2, mu = 0.1, complete = FALSE)
phy5 <- phy5[[1]] # Take the phylogeny out of the list.
plot( phy5, direction = "upwards")
sim5 <- sim.char(phy = phy5, par = 0.25, model = "BM", root = 10)
#root=mean, par= variance->sd^2
sim5 <- sim5[,,1]
hist( sim5 )
fit5 <- fitContinuous(phy = phy5, dat = sim5, model = "BM")
class( fit5 )
fit5$opt$sigsq 
fit5$opt$z0
#variance has significantly varied off,
#mean decreased from last simulation by -2.4 and -.4 from anticipated

combined_data <- rbind(
  data.frame(dataset = "fit 1", sigsq = fit$opt$sigsq, z0 = fit$opt$z0),
  data.frame(dataset = "fit 2", sigsq = fit2$opt$sigsq, z0 = fit2$opt$z0),
  data.frame(dataset = "fit 3", sigsq = fit3$opt$sigsq, z0 = fit3$opt$z0),
  data.frame(dataset = "fit 4", sigsq = fit4$opt$sigsq, z0 = fit4$opt$z0),
  data.frame(dataset = "fit 5", sigsq = fit5$opt$sigsq, z0 = fit5$opt$z0)
)

#overall graph
library(ggplot2)
simulation_plot<- ggplot(combined_data, aes(x = sigsq, y = z0, color = dataset)) +
  geom_point() +
  labs(x = "Variance", y = "Mean", title = "Comparison of variance and mean for simulations decreasing sample size ")

print(simulation_plot)

#graph showing variance
barplot(combined_data$sigsq, names.arg = combined_data$dataset, col = "blue", main = "Comparison of Variance for Datasets", xlab = "Datasets", ylab = "Variance")
#graph showing variance
barplot(combined_data$z0, names.arg = combined_data$dataset, col = "Purple", main = "Comparison of Mean for Datasets", xlab = "Datasets", ylab = "Mean")


# do a for loop to repeat each simulation multiple times to make box plots showing variance, look into apply function
# do smaller sample size in the tens

