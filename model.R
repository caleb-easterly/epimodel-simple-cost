library(EpiModel)

# Initialize the network
nw <- network.initialize(n = 500, directed = FALSE)

# Define the formation model: edges,
#                             number concurrent (degree > 1),
#                             number with degree 3+
formation <- ~edges + concurrent + degrange(from = 3)

# Input the appropriate target statistics for each term
target.stats <- c(175, 110, 0)

# Parameterize the dissolution model
coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 50)
coef.diss

# Fit the model
est <- netest(nw, formation, target.stats, coef.diss)

# Model diagnostics
dx <- netdx(est, nsims = 8, ncores = 8, nsteps = 500)
print(dx)
plot(dx, plots.joined = FALSE)


# Epidemic model simulation -----------------------------------------------

# Parameterizing an SIS epidemic
inf.prob <- 0.4
act.rate <- 2
rec.rate <- 0.05
param <- param.net(inf.prob = inf.prob, act.rate = act.rate,
                   rec.rate = rec.rate)

# Initial conditions
init <- init.net(i.num = 10)

source('cost.R')
# Control settings
max_time <- 300
control <- control.net(type="SIS",
                       nsims = 10,
                       ncores = 4,
                       nsteps = max_time,
                       cost.FUN = cost)

# Run the network model simulation with netsim
sim <- netsim(est, param, init, control)

# with intervention
inter_cost_million <- 1
param_inter <- param.net(inf.prob = inf.prob, act.rate = act.rate,
                         rec.rate = rec.rate,
                         inter.eff = 0.7, inter.start = 50)

# Run the network model simulation with netsim
sim_inter <- netsim(est, param_inter, init, control)

# Plot outcomes
par(mfrow = c(2, 2))
steps <- 1:max_time
plot(rowMeans(sim$epi$i.num), x = steps,
     main = "Number infected",
     xlab = "Time (weeks)", ylab = "Number of people",
     type = "l", lwd = 2)
lines(rowMeans(sim_inter$epi$i.num), x = steps,
      type = "l", col = "purple", lwd = 2)
plot(rowMeans(sim$epi$costPerCapita), x = steps,
     main = "Weekly Cost Per Capita",
     xlab = "Time (weeks)", ylab = "Cost (dollars)",
     type = "l", lwd = 2)
lines(rowMeans(sim_inter$epi$costPerCapita), x = steps,
      type = "l", col = "purple", lwd = 2)
plot(rowMeans(sim$epi$cumuCost), x = steps,
     main = "Cumulative cost",
     xlab = "Time (weeks)", ylab = "Cost (dollars)",
     type = "l", lwd = 2)
lines(rowMeans(sim_inter$epi$cumuCost), x = steps,
      type = "l", col = "purple", lwd = 2)

# difference in cumulative cost
no_inter_cumu_cost_million <- c(t(sim$epi$cumuCost[max_time, ]))/10^6
inter_cumu_cost_million <- c(t(sim_inter$epi$cumuCost[max_time, ]))/10^6

dev.off()
plot(density(no_inter_cumu_cost_million), xlim = c(10, 22), xlab = "Cumulative cost (Millions of dollars)")
lines(density(inter_cumu_cost_million), col = "purple")

# benefit-cost ratio
(mean(no_inter_cumu_cost_million) - mean(inter_cumu_cost_million))/inter_cost_million

# how often does benefit exceed cost? go a sampling route
mean(replicate(1000, (sample(no_inter_cumu_cost_million, 1) - sample(inter_cumu_cost_million, 1) > inter_cost_million)))
