#install.packages("dcm")
#install.packages("EpiModel", dependencies = TRUE)

require(EpiModel)
#require(dcm)

#################################################################################
#### Deterministic compartmental models
#################################################################################

######## Basic SI model 

param <- param.dcm(inf.prob = 0.5, act.rate = c(2))
init <- init.dcm(s.num = 999, i.num = 1)
control <- control.dcm(type = "SI", nsteps = 365)


mod <- dcm(param, init, control)
mod
plot(mod)
summary(mod, at=100)

comp_plot(mod, at = 2, digits = 1)


########### SIS model 

param <- param.dcm(inf.prob = 0.3, act.rate = 1, rec.rate = 0.05)

init <- init.dcm(s.num = 999, i.num = 4)
control <- control.dcm(type = "SIS", nsteps = 500, dt = 0.5)
mod <- dcm(param, init, control)

plot(mod)

par(mar = c(3.2, 3, 2, 1), mgp = c(2, 1, 0), mfrow = c(1, 2))

plot(mod, popfrac = FALSE, alpha = 0.5,
     lwd = 4, main = "Compartment Sizes")

mod

plot(mod, y = "si.flow", lwd = 4, col = "firebrick",
     main = "Disease Incidence", legend = "n")

par(mfrow = c(1, 1))
comp_plot(mod, at = 2, digits = 1)




########### SIR model 

param <- param.dcm(inf.prob = 0.3, act.rate = 0.1, rec.rate = 0.005)

init <- init.dcm(s.num = 999, i.num = 4,r.num = 0)
control <- control.dcm(type = "SIR", nsteps = 500, dt = 0.5)
mod <- dcm(param, init, control)

plot(mod)

par(mar = c(3.2, 3, 2, 1), mgp = c(2, 1, 0), mfrow = c(1, 2))

plot(mod, popfrac = FALSE, alpha = 0.5,
     lwd = 4, main = "Compartment Sizes")

mod

plot(mod, y = "si.flow", lwd = 4, col = "firebrick",
     main = "Disease Incidence", legend = "n")

par(mfrow = c(1, 1))
comp_plot(mod, at = 2, digits = 1)


########### SIR model with demography 

param <- param.dcm(inf.prob = 0.3, act.rate = 1, rec.rate = 1/20,
                   a.rate = 1/95, ds.rate = 1/100, di.rate = 1/80, dr.rate = 1/100)

init <- init.dcm(s.num = 999, i.num = 1, r.num = 0)
control <- control.dcm(type = "SIR", nsteps = 500, dt = 0.5)
mod <- dcm(param, init, control)

plot(mod)

par(mar = c(3.2, 3, 2, 1), mgp = c(2, 1, 0), mfrow = c(1, 2))

plot(mod, popfrac = FALSE, alpha = 0.5,
     lwd = 4, main = "Compartment Sizes")

mod

plot(mod, y = "si.flow", lwd = 4, col = "firebrick",
     main = "Disease Incidence", legend = "n")

par(mfrow = c(1, 1))
comp_plot(mod, at = 2, digits = 1)


############# Sensitivity analysis 
## Specify multiple inf.prob and act.rate
param <- param.dcm(inf.prob = , act.rate = seq(), rec.rate = 0.02)
param <- param.dcm(inf.prob = seq(), act.rate = , rec.rate = 0.02)

init <- init.dcm(s.num = 500, i.num = 1,r.num=0)
control <- control.dcm(type = "SIR", nsteps = 350)
mod <- dcm(param, init, control)


plot(mod)

par(mfrow = c(1,2), mar = c(3.2,3,2.5,1))
plot(mod, alpha = 1, main = "Disease Prevalence")
plot(mod, y = "si.flow", col = "Greens", alpha = 0.8, main = "Disease Incidence")



#################################################################################
#### Individual contact models (and comparison to DCM)
#################################################################################

param.dcm <- param.dcm(inf.prob = c(0.1), act.rate = 0.5, rec.rate = 0.01)
init.dcm <- init.dcm(s.num = 500, i.num = 1,r.num = 0)
control.dcm <- control.dcm(type = "SIR", nsims = 10, nsteps = 300)
mod.dcm <- dcm(param.dcm, init.dcm, control.dcm)
plot(mod.dcm)


param.icm <- param.icm(inf.prob = c(0.1), act.rate = 0.5, rec.rate = 0.01)
init.icm <- init.icm(s.num = 500, i.num = 1,r.num = 0)
control.icm <- control.icm(type = "SIR", nsims = 100, nsteps = 300)
mod.icm <- icm(param.icm, init.icm, control.icm)
plot(mod.icm)



plot(mod.dcm, alpha = 0.75, lwd = 4, main = "DCM and ICM Comparison")
plot(mod.icm, qnts = FALSE, sim.lines = FALSE, add = TRUE, mean.lty = 2, legend = FALSE)


dev.off()
