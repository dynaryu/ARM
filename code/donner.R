# https://onlinecourses.science.psu.edu/stat504/node/159

code_path = paste(path.expand("~"), 'Projects/ARM/code', sep="/")
data_path = paste(path.expand("~"),'Projects/ARM/data', sep="/")

# read data
mydata <- read.csv(paste(data_path,"donner.dat", sep="/"), sep=" ")
# age, gender, surviorship

#P(y=1) = invlogit(a+b*age + gender + age|gender)

fit1 <- glm(survivorship ~ age + gender + age:gender, data=mydata, 
	family=binomial(link="logit"))


fit.1 <- glm (vote ~ income, data=ndata, family=binomial(link="logit"))

# https://onlinecourses.science.psu.edu/stat504/node/161
mydata <- read.csv('')