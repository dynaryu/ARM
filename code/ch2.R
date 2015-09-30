# ARM ch2

library(foreign)
code_path = '/home/hyeuk/Projects/ARM/code'
working_path = '/home/hyeuk/Projects/ARM/data/child.iq'

# read data
mydata <- read.dta(paste(working_path,"/kidiq.dta", sep=""))

dataList = list(
  mom_hs = mydata$mom_hs, 
  mom_iq = mydata$mom_iq,
  kid_score = mydata$kid_score,
  Ndata = nrow(mydata)
)

# load stan models
source(paste(codePath,"stan_models.R",sep=""))
stanDso <- stan_model( model_code = model_linreg)

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



ggplot() +
geom_point(data=mydata, aes(mom_iq, kid_score, color=mom_hs)) +
geom_line(data=newdata, aes(mom_iq, kid_score), color='red') 

ggplot() +
geom_point(data=mydata, aes(mom_iq, kid_score, color=mom_hs)) +
geom_line(data=newdata1, aes(mom_iq, kid_score), color='red') + 
geom_line(data=newdata0, aes(mom_iq, kid_score), color='blue') 

