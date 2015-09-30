# ARM ch2

library(foreign)
library(rstan)

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

fit4 <- lm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=mydata)

# load stan models
source(paste(code_path,"stan_models.R",sep="/"))
#stanDso1 <- stan_model( model_code = model_linreg)
stanDso <- stan_model(model_code=model_ch2)

nChains = 4
burnInSteps = 1000
iterSteps = burnInSteps + 5000

# Get MC sample of posterior:
stanFit <- sampling( object=stanDso , 
                     data = dataList , 
                     #pars = parameters , # optional
                     chains = nChains ,
                     iter = iterSteps , 
                     warmup = burnInSteps , 
                     #init = initsList , # optional
                     thin = 1 )

stanSamples <- extract(stanFit)
paramNames <- names(stanSamples)
paramNames <- paramNames[-length(paramNames)] # remove the last

# diagplot 
traceplot(stanFit, pars=paramNames, nrow=2, ncol=3)

print(stanFit, digits_summary=3, pars=paramNames, probs = c(.025, .5, .975))

# Get MC sample of posterior:
stanDso1 <- stan_model( model_code = model_linreg)
stanFit1 <- sampling( object=stanDso1 , 
                     data = dataList , 
                     #pars = parameters , # optional
                     chains = nChains ,
                     iter = iterSteps , 
                     warmup = burnInSteps , 
                     #init = initsList , # optional
                     thin = 1 )

print(stanFit1, digits_summary=3, pars=paramNames, probs = c(.025, .5, .975))

# ggplot() +
# geom_point(data=mydata, aes(mom_iq, kid_score, color=mom_hs)) +
# geom_line(data=newdata, aes(mom_iq, kid_score), color='red') 

# ggplot() +
# geom_point(data=mydata, aes(mom_iq, kid_score, color=mom_hs)) +
# geom_line(data=newdata1, aes(mom_iq, kid_score), color='red') + 
# geom_line(data=newdata0, aes(mom_iq, kid_score), color='blue') 

