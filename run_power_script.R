

# f2 ≥ 0.02, small
# f2 ≥ 0.15, medium
# f2 ≥ 0.35, large

#### Small Effect ####
rm(list = ls())
number_of_participants <- 400
n1 <- 10
n2 <- number_of_participants/2
f <- .02

file.remove("N_power.RData")
save(n1,n2,f, file = "N_power.RData")

source("power_script.R")
test
pwr::pwr.f2.test(u = test$NumDF, v = test$DenDF, f2 = f, sig.level =  .05)
f_original <- MuMIn::r.squaredGLMM(model1)[1]/(1-MuMIn::r.squaredGLMM(model1)[1])
f_original


#### Medium Effect ####

rm(list = ls())
number_of_participants <- 54

n1 <- 10
n2 <- number_of_participants/2
f <- .15

file.remove("N_power.RData")
save(n1,n2,f, file = "N_power.RData")

source("power_script.R")
test
pwr::pwr.f2.test(u = test$NumDF, v = test$DenDF, f2 = f, sig.level =  .05)
f_original <- MuMIn::r.squaredGLMM(model1)[1]/(1-MuMIn::r.squaredGLMM(model1)[1])
f_original


#### Medium Effect ####

rm(list = ls())
number_of_participants <- 24

n1 <- 10
n2 <- number_of_participants/2
f <- .35

file.remove("N_power.RData")
save(n1,n2,f, file = "N_power.RData")

source("power_script.R")
test
pwr::pwr.f2.test(u = test$NumDF, v = test$DenDF, f2 = f, sig.level =  .05)
f_original <- MuMIn::r.squaredGLMM(model1)[1]/(1-MuMIn::r.squaredGLMM(model1)[1])
f_original
