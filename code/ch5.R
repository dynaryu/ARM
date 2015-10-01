# ch5 logistic regression

# logistic regression for binary outcomes

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
data_path = paste(path.expand("~"),'Projects/ARM/data/nes', sep="/")

# read data
mydata <- read.dta(paste(data_path,"2009 nes5200_processed_voters_realideo.dta", 
	sep="/"))

dataList = list(
  mom_hs = mydata$mom_hs, 
  mom_iq = mydata$mom_iq,
  kid_score = mydata$kid_score,
  N = nrow(mydata)
)

