library(MuMIn)
library(glmmTMB)
library(tidyverse)
library(DHARMa)
library(car)
library(performance)
library(emmeans)
options(scipen=999)
base_breaks <- function(n = 10){
  function(x) {axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#setwd("./dataRaw/")

#tax2019 <- read.csv("19instateshares.csv")
#tax2019$Year <- 2019

#tax2018 <- read.csv("18instateshares.csv")
#tax2018$Year <- 2018


#tax2017 <- read.csv("17instateshares.csv")
#tax2017$Year <- 2017


#tax2016 <- read.csv("16instateshares.csv")
#tax2016$Year <- 2016

#tax2015 <- read.csv("15instateshares.csv")
#tax2015$Year <- 2015

#tax2014 <- read.csv("14instateshares.csv")
#tax2014$Year <- 2014

#taxData <- rbind(tax2019,tax2017,tax2016,tax2015,tax2014)

#setwd("../")
taxData <- read.csv("taxData.csv")
#taxData <- taxData[!taxData$Year=="2017",]

taxData$avg_inc_1 <- (taxData$sum_agi_01/taxData$top_01) * 1000

taxData$avg_inc_5 <- ((taxData$sum_agi_05-taxData$sum_agi_01)/
  (taxData$top_05-taxData$top_01)) * 1000

taxData$avg_inc_10 <- ((taxData$sum_agi_10-taxData$sum_agi_05)/
                           (taxData$top_10-taxData$top_05)) * 1000

taxData$avg_inc_25 <- ((taxData$sum_agi_25-taxData$sum_agi_10)/
                            (taxData$top_25-taxData$top_10)) * 1000

taxData$avg_inc_50 <- ((taxData$sum_agi_50-taxData$sum_agi_25)/
                            (taxData$top_50-taxData$top_25)) * 1000

taxData$avg_inc_75 <- ((taxData$sum_agi_75-taxData$sum_agi_50)/
                            (taxData$top_75-taxData$top_50)) * 1000


taxData$avg_tax_rate_01 <- taxData$sum_tax_01/taxData$sum_agi_01

taxData$avg_tax_rate_5 <- ((taxData$sum_tax_05-taxData$sum_tax_01)/
                          (taxData$sum_agi_05-taxData$sum_agi_01)) 

taxData$avg_tax_rate_10 <- ((taxData$sum_tax_10-taxData$sum_tax_05)/
                           (taxData$sum_agi_10-taxData$sum_agi_05)) 

taxData$avg_tax_rate_25 <- ((taxData$sum_tax_25-taxData$sum_tax_10)/
                            (taxData$sum_agi_25-taxData$sum_agi_10))

taxData$avg_tax_rate_50 <- ((taxData$sum_tax_50-taxData$sum_tax_25)/
                            (taxData$sum_agi_50-taxData$sum_agi_25)) 

taxData$avg_tax_rate_75 <- ((taxData$sum_tax_75-taxData$sum_tax_50)/
                            (taxData$sum_agi_75-taxData$sum_agi_50)) 





write.csv(taxData,"taxDataAnalyzed.csv")
###Does number of returns increase each year?##



fit<- glmmTMB(total~as.factor(Year)+(1|state), data=taxData, family = "nbinom2")
summary(fit)

r.squaredGLMM(fit)

Anova(fit)





#Yes, it does, very low R squared but with numbers this high it makes sense
#Suspect that if done with just total US returns would be higher R2
#The random effect of states is likely overshadowing pop growth


fit<- glmmTMB(total~as.factor(Year), data=taxData[taxData$state=="US",], 
              family = "nbinom2")
pTotUSA <- Anova(fit)

r.squaredGLMM(fit)

#And that suspicion is correct, with a .99 R-squared. Population growth? 

ggplot(data=taxData[taxData$state=="US",],aes(x=(Year), y=total))+
  geom_smooth(method = "lm") + geom_point() + xlab("Year")+ylab("Number of Returns in US")+
  ggtitle("Total USA Returns Over Time", 
          paste0("R2 = ",round(r2totUSA[1,1],5), " p = ", pTotUSA[1,3]))


#Does the 1% of each state have similar tax rates? 

#fitRate <- glmmTMB(taxData$)


#Is the effective tax rate of the one percent different at different states?


fitETR <- glmmTMB(avg_tax_rate_01~state, data=taxData, family = "beta_family")
plot(simulateResiduals(fitETR))


Anova(fitETR)

diffETR<-emmeans(fitETR, pairwise~state, type="response")

diffETR <- as.data.frame(diffETR$contrasts)

#Only Vermont and Other Areas are stat different than the national 
#effective tax rate of the USA

#Vermont pays 3 percent less than the rest of the country. Why?


fitAGI <- glmmTMB(avg_inc_1~state, data=taxData)
plot(simulateResiduals(fitAGI))


Anova(fitAGI)

fitAGI<-emmeans(fitAGI, pairwise~state, type="response")

fitAGI <- as.data.frame(diffETR$contrasts)



#####Maybe see what the avg income tax rate is at 1,000,000#######

taxData_NO_OA <- taxData[taxData$Year=="2019",]

taxDataLongInc <- taxData_NO_OA[142:147]
taxDataLongETR <- taxData_NO_OA[148:153]

taxDataLongInc <- data.frame(y=unlist(taxDataLongInc))
taxDataLongETR <- data.frame(y=unlist(taxDataLongETR))

taxDataLongIncETR <- NULL

taxDataLongIncETR$AvgInc <- taxDataLongInc$y

taxDataLongIncETR$ETR <- taxDataLongETR$y

taxDataLongIncETR <- as.data.frame(taxDataLongIncETR)

fitMilTaxRate <- lm(ETR~AvgInc, data=taxDataLongIncETR)
plot(simulateResiduals(fitMilTaxRate))

r.squaredGLMM(fitMilTaxRate)

summary(fitMilTaxRate)



efrMil <- 0.000000111465*1000000+0.085264967671

efrMil #The model shows that an generally a person makes 1000000 is taxed 
#at a rate of .265



ggplot(data=taxData, aes(y=avg_tax_rate_01))
