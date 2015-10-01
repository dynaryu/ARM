# ARM ch3

library(foreign)
library(rstan)

mcmc_sampling <- function(stanDso, dataList, nChains=4, burnInSteps=1000, 
  addSteps=5000){

  #nChains = 4
  #burnInSteps = 1000
  #iterSteps = burnInSteps + 5000

  # Get MC sample of posterior:
  stanFit <- sampling(object=stanDso, 
                      data = dataList, 
                      #pars = parameters , # optional
                      chains = nChains,
                      iter = burnInSteps + addSteps, 
                      warmup = burnInSteps, 
                      #init = initsList , # optional
                      thin = 1)

  stanSamples <- extract(stanFit)
  paramNames <- names(stanSamples)
  paramNames <- paramNames[-length(paramNames)] # remove the last

  print(stanFit, digits_summary=3, pars=paramNames, probs = c(.025, .5, .975))

  return(list(paramNames, stanFit))
}


code_path = paste(path.expand("~"), 'Projects/ARM/code', sep="/")
data_path = paste(path.expand("~"),'Projects/ARM/data/child.iq', sep="/")

# read data
mydata <- read.dta(paste(data_path,"kidiq.dta", sep="/"))

dataList = list(
  mom_hs = mydata$mom_hs, 
  mom_iq = mydata$mom_iq,
  kid_score = mydata$kid_score,
  N = nrow(mydata)
)

# linear regression
fit4 <- lm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=mydata)

# load stan models
source(paste(code_path,"stan_models.R",sep="/"))

stanDso <- stan_model(model_code=model_ch3ex)

tmp <- mcmc_sampling(stanDso, dataList)
paramNames <- tmp[[1]]
stanFit <- tmp[[2]]

# diagplot 
traceplot(stanFit, pars=paramNames, nrow=2, ncol=3)

# lm for standarised data
mydata$z_mom_hs <- (mydata$mom_hs - mean(mydata$mom_hs))/(2*sd(mydata$mom_hs))
mydata$z_mom_iq <- (mydata$mom_iq - mean(mydata$mom_iq))/(2*sd(mydata$mom_iq))

fit5 <- lm(kid_score ~ z_mom_hs + z_mom_iq + z_mom_hs:z_mom_iq, data=mydata)

# load stan models
stanDso_z <- stan_model(model_code=model_ch3ex_z)

tmp <- mcmc_sampling(stanDso_z, dataList)
paramNames <- tmp[[1]]
stanFit_z <- tmp[[2]]

# diagplot 
traceplot(stanFit_z, pars=paramNames, nrow=2, ncol=3)

# ggplot() +
# geom_point(data=mydata, aes(mom_iq, kid_score, color=mom_hs)) +
# geom_line(data=newdata, aes(mom_iq, kid_score), color='red') 

# ggplot() +
# geom_point(data=mydata, aes(mom_iq, kid_score, color=mom_hs)) +
# geom_line(data=newdata1, aes(mom_iq, kid_score), color='red') + 
# geom_line(data=newdata0, aes(mom_iq, kid_score), color='blue') 

