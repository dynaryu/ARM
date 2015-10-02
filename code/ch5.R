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
data_path = paste(path.expand("~"),'Projects/ARM/data', sep="/")

# read data
mydata <- readRDS(paste(data_path,"nes/nes_data.Rda", 
	sep="/"))

yr <- 1992
ok <- mydata$year==yr & mydata$presvote < 3
vote <- mydata$presvote[ok] - 1
income <- mydata$income[ok]

# remove NA
ndata <- data.frame(vote, income)
ndata <- ndata[complete.cases(ndata),]

# Pr(y=1) = invlogit(x*beta)
fit.1 <- glm (vote ~ income, data=ndata, family=binomial(link="logit"))

dataList = list(
  vote = ndata$vote, 
  income = ndata$income,
  N = nrow(ndata)
)

# load stan models
source(paste(code_path,"stan_models.R",sep="/"))

stanDso <- stan_model(model_code=model_ch5ex)

tmp <- mcmc_sampling(stanDso, dataList)
paramNames <- tmp[[1]]
stanFit <- tmp[[2]]

# BUGS example beeltes
# http://www.openbugs.net/Examples/Beetles.html
dataList <- list( x = c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839),
      n = c(59, 60, 62, 56, 63, 59, 62, 60),
      r = c(6, 13, 18, 28, 52, 53, 61, 60), N = 8)

stanDso <- stan_model(model_code=model_beetles)

tmp <- mcmc_sampling(stanDso, dataList)
paramNames <- tmp[[1]]
stanFit <- tmp[[2]]
