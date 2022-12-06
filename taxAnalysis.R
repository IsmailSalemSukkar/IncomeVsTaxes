######Preamble####

library(MuMIn)
library(glmmTMB)
library(tidyverse)
library(DHARMa)
library(car)
library(performance)
library(emmeans)
library(viridis)
library(MASS)
options(scipen=99999)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

setwd("./dataRaw/")

tax2019 <- read.csv("19instateshares.csv")
tax2019$Year <- 2019

tax2018 <- read.csv("18instateshares.csv")
tax2018$Year <- 2018


tax2017 <- read.csv("17instateshares.csv")
tax2017$Year <- 2017

tax2017[grepl("sum_",names(tax2017))] <- (1/1000)* tax2017[grepl("sum_",names(tax2017))]


tax2016 <- read.csv("16instateshares.csv")
tax2016$Year <- 2016

tax2016[grepl("sum_",names(tax2016))] <- (1/1000)* tax2016[grepl("sum_",names(tax2016))]



tax2015 <- read.csv("15instateshares.csv")
tax2015$Year <- 2015



tax2014 <- read.csv("14instateshares.csv")
tax2014$Year <- 2014
tax2014[grepl("sum_",names(tax2014))] <- (1/1000)* tax2014[grepl("sum_",names(tax2014))]




common.names <- intersect(colnames(tax2014),
                          colnames(tax2018))

taxData <- rbind(tax2019[, common.names], tax2018[, common.names],
                 tax2017[, common.names],tax2016[, common.names],
                 tax2015[, common.names],tax2014[, common.names])

rm(list = ls(pattern = "tax20*"))

setwd("../")
#taxData <- read.csv("taxData.csv")
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

taxData$true_sum_sal_10 <- taxData$sum_sal_10 - taxData$sum_sal_05
taxData$true_sum_int_10 <- taxData$sum_int_10 - taxData$sum_int_05
taxData$true_sum_div_10 <- taxData$sum_div_10- taxData$sum_div_05
taxData$true_sum_businc_10 <- taxData$sum_businc_10 - taxData$sum_businc_05
taxData$true_sum_cpgain_10 <- taxData$sum_cpgain_10 - taxData$sum_cpgain_05
taxData$true_sum_scorp_10 <- taxData$sum_scorp_10 - taxData$sum_scorp_05

taxData$true_sum_sal_05 <- taxData$sum_sal_05 - taxData$sum_sal_01
taxData$true_sum_int_05 <- taxData$sum_int_05 - taxData$sum_int_01
taxData$true_sum_div_05 <- taxData$sum_div_05- taxData$sum_div_01
taxData$true_sum_businc_05 <- taxData$sum_businc_05 - taxData$sum_businc_01
taxData$true_sum_cpgain_05 <- taxData$sum_cpgain_05 - taxData$sum_cpgain_01
taxData$true_sum_scorp_05 <- taxData$sum_scorp_05 - taxData$sum_scorp_01

taxData$true_sum_sal_25 <- taxData$sum_sal_25 - taxData$sum_sal_10
taxData$true_sum_int_25 <- taxData$sum_int_25 - taxData$sum_int_10
taxData$true_sum_div_25 <- taxData$sum_div_25- taxData$sum_div_10
taxData$true_sum_businc_25 <- taxData$sum_businc_25 - taxData$sum_businc_10
taxData$true_sum_cpgain_25 <- taxData$sum_cpgain_25 - taxData$sum_cpgain_10
taxData$true_sum_scorp_25 <- taxData$sum_scorp_25 - taxData$sum_scorp_10

taxData$true_sum_sal_50 <- taxData$sum_sal_50 - taxData$sum_sal_25
taxData$true_sum_int_50 <- taxData$sum_int_50 - taxData$sum_int_25
taxData$true_sum_div_50 <- taxData$sum_div_50- taxData$sum_div_25
taxData$true_sum_businc_50 <- taxData$sum_businc_50 - taxData$sum_businc_25
taxData$true_sum_cpgain_50 <- taxData$sum_cpgain_50 - taxData$sum_cpgain_25
taxData$true_sum_scorp_50 <- taxData$sum_scorp_50 - taxData$sum_scorp_25


taxData$true_sum_sal_75 <- taxData$sum_sal_75 - taxData$sum_sal_50
taxData$true_sum_int_75 <- taxData$sum_int_75 - taxData$sum_int_50
taxData$true_sum_div_75 <- taxData$sum_div_75- taxData$sum_div_50
taxData$true_sum_businc_75 <- taxData$sum_businc_75 - taxData$sum_businc_50
taxData$true_sum_cpgain_75 <- taxData$sum_cpgain_75 - taxData$sum_cpgain_50
taxData$true_sum_scorp_75 <- taxData$sum_scorp_75 - taxData$sum_scorp_50

taxData$true_sum_agi_05 <- taxData$sum_agi_05 - taxData$sum_agi_01
taxData$true_sum_agi_10 <- taxData$sum_agi_10 - taxData$sum_agi_05
taxData$true_sum_agi_25 <- taxData$sum_agi_25 - taxData$sum_agi_10
taxData$true_sum_agi_50 <- taxData$sum_agi_50 - taxData$sum_agi_25
taxData$true_sum_agi_75 <- taxData$sum_agi_75 - taxData$sum_agi_50



taxData$SalPerc01 <- taxData$sum_sal_01/taxData$sum_agi_01
taxData$SalPerc05<- taxData$true_sum_sal_05/taxData$true_sum_agi_05
taxData$SalPerc10<- taxData$true_sum_sal_10/taxData$true_sum_agi_10
taxData$SalPerc25<- taxData$true_sum_sal_25/taxData$true_sum_agi_25
taxData$SalPerc50<- taxData$true_sum_sal_50/taxData$true_sum_agi_50
taxData$SalPerc75<- taxData$true_sum_sal_75/taxData$true_sum_agi_75

taxData$BusPerc01 <- taxData$sum_businc_01/taxData$sum_agi_01
taxData$BusPerc05<- taxData$true_sum_businc_05/taxData$true_sum_agi_05
taxData$BusPerc10<- taxData$true_sum_businc_10/taxData$true_sum_agi_10
taxData$BusPerc25<- taxData$true_sum_businc_25/taxData$true_sum_agi_25
taxData$BusPerc50<- taxData$true_sum_businc_50/taxData$true_sum_agi_50
taxData$BusPerc75<- taxData$true_sum_businc_75/taxData$true_sum_agi_75

taxData$DivPerc01 <- taxData$sum_div_01/taxData$sum_agi_01
taxData$DivPerc05<- taxData$true_sum_div_05/taxData$true_sum_agi_05
taxData$DivPerc10<- taxData$true_sum_div_10/taxData$true_sum_agi_10
taxData$DivPerc25<- taxData$true_sum_div_25/taxData$true_sum_agi_25
taxData$DivPerc50<- taxData$true_sum_div_50/taxData$true_sum_agi_50
taxData$DivPerc75<- taxData$true_sum_div_75/taxData$true_sum_agi_75

taxData$CapPerc01 <- taxData$sum_cpgain_01/taxData$sum_agi_01
taxData$CapPerc05<- taxData$true_sum_cpgain_05/taxData$true_sum_agi_05
taxData$CapPerc10<- taxData$true_sum_cpgain_10/taxData$true_sum_agi_10
taxData$CapPerc25<- taxData$true_sum_cpgain_25/taxData$true_sum_agi_25
taxData$CapPerc50<- taxData$true_sum_cpgain_50/taxData$true_sum_agi_50
taxData$CapPerc75<- taxData$true_sum_cpgain_75/taxData$true_sum_agi_75

taxData$IntPerc01 <- taxData$sum_int_01/taxData$sum_agi_01
taxData$IntPerc05<- taxData$true_sum_int_05/taxData$true_sum_agi_05
taxData$IntPerc10<- taxData$true_sum_int_10/taxData$true_sum_agi_10
taxData$IntPerc25<- taxData$true_sum_int_25/taxData$true_sum_agi_25
taxData$IntPerc50<- taxData$true_sum_int_50/taxData$true_sum_agi_50
taxData$IntPerc75<- taxData$true_sum_int_75/taxData$true_sum_agi_75

taxData$SCorpPerc01 <- taxData$sum_scorp_01/taxData$sum_agi_01
taxData$ScorpPerc05<- taxData$true_sum_scorp_05/taxData$true_sum_agi_05
taxData$ScorpPerc10<- taxData$true_sum_scorp_10/taxData$true_sum_agi_10
taxData$ScorpPerc25<- taxData$true_sum_scorp_25/taxData$true_sum_agi_25
taxData$ScorpPerc50<- taxData$true_sum_scorp_50/taxData$true_sum_agi_50
taxData$ScorpPerc75<- taxData$true_sum_scorp_75/taxData$true_sum_agi_75



taxData <- taxData[!taxData$state=="OA",]


taxDataLongInc <- taxData[116:121]
taxDataLongETR <- taxData[122:127]
taxDataLongState <- taxData[2]
taxDataLongSalPerc <- taxData[163:168]

taxDataLongSalPerc <-   rename(taxDataLongSalPerc,
  "01_Perc" = SalPerc01,
  "05_Perc" = SalPerc05,
  "10_Perc" = SalPerc10,
  "25_Perc" = SalPerc25,
  "50_Perc" = SalPerc50,
  "75_Perc" = SalPerc75
)

taxDataLongSalPerc <- pivot_longer(taxDataLongSalPerc, cols = "01_Perc":"75_Perc")

taxDataLongSalPerc <-   rename(taxDataLongSalPerc,
                               Perc = name,
                               SalPerc = value)


taxDataLongSalPerc <- taxDataLongSalPerc[order(taxDataLongSalPerc$Perc),]

taxDataLongInc <- data.frame(y=unlist(taxDataLongInc))
taxDataLongETR <- data.frame(y=unlist(taxDataLongETR))
taxDataLongState <- data.frame(y=unlist(taxDataLongState))



taxDataLongIncETR <- NULL

taxDataLongIncETR$AvgInc <- taxDataLongInc$y
taxDataLongIncETR$SalPerc <- taxDataLongSalPerc$SalPerc
taxDataLongIncETR$ETR <- taxDataLongETR$y
taxDataLongIncETR$State <- taxDataLongState$y
taxDataLongIncETR$Perc <- taxDataLongSalPerc$Perc


taxDataLongIncETR <- as.data.frame(taxDataLongIncETR)

taxDataLongIncETR_NO_US <- taxDataLongIncETR[!taxDataLongIncETR$State=="US",]


write.csv(taxData,"taxDataAnalyzed.csv")

############
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

r2totUSA<- r.squaredGLMM(fit)

#And that suspicion is correct, with a .99 R-squared. Population growth? 

ggplot(data=taxData[taxData$state=="US",],aes(x=(Year), y=total))+
  geom_smooth(method = "lm") + geom_point() + xlab("Year")+ylab("Number of Returns in US")+
  ggtitle("Total USA Returns Over Time", 
          paste0("R2 = ",round(r2totUSA[1,1],5), " p = ", pTotUSA[1,3]))


#Does the 1% of each state have similar tax rates? 




#####First lets see what the avg income tax rate is at 1,000,000, may be more useful 
#Than looking at the 1%#######






fitMilTaxRate <- glmmTMB(ETR~AvgInc, 
                         data=taxDataLongIncETR_NO_US)
plot(simulateResiduals(fitMilTaxRate))

r.squaredGLMM(fitMilTaxRate)

summary(fitMilTaxRate)



efrMil <- 0.000000121919*1000000+0.092524298146

efrMil #The model shows that an generally a person makes 1000000 is taxed 
#at a rate of 21.44%





#Lets compare that to the US

taxDataLongIncETR_US <- taxDataLongIncETR[taxDataLongIncETR$State=="US",]

fitMilTaxRateUS <- glmmTMB(ETR~AvgInc, data=taxDataLongIncETR_US)
plot(simulateResiduals(fitMilTaxRateUS))

r.squaredGLMM(fitMilTaxRateUS)

summary(fitMilTaxRateUS)



efrMilUS <- 0.00000012060*1000000+0.09074009753

efrMilUS

#US is 21.13%, which is likely stat close enough to 21.44


fitStateTaxRate <- glmmTMB(ETR~State + (1|AvgInc), data=taxDataLongIncETR)
plot(simulateResiduals(fitStateTaxRate))

r.squaredGLMM(fitStateTaxRate)

summary(fitStateTaxRate)

diffStateTaxRate <- emmeans(fitStateTaxRate, pairwise~State, type="response")

diffStateTaxRate <- as.data.frame(diffStateTaxRate$contrasts)

#As a whole, tax rates per state do not differ from the US average



#Is the effective tax rate of the one percent different at different states?


fitETR1 <- glmmTMB(avg_tax_rate_01~state+(1|avg_inc_1), data=taxData)
plot(simulateResiduals(fitETR1))

summary(fitETR1)
Anova(fitETR1)

diffETR1 <- emmeans(fitETR1, pairwise~state, type="response")

diffETR1 <- as.data.frame(diffETR1$contrasts)

#Only Vermont Areas are even close to stat different than the national 
#effective tax rate of the USA

#How about the top 5% (2-5%)

fitETR5 <- glmmTMB(avg_tax_rate_5~state+(1|avg_inc_5), data=taxData)
plot(simulateResiduals(fitETR5))

summary(fitETR5)
Anova(fitETR5)

diffETR5 <- emmeans(fitETR5, pairwise~state, type="response")

diffETR5 <- as.data.frame(diffETR5$contrasts)

#Interesting! Why does the 2-5% get taxed differently based on state?
#Perhaps different income sources?

#Lets see what type of income is the largest driver of tax rate
#I assume salary.
baseFitTaxVsInc1 <- lm(avg_tax_rate_01~SalPerc01+BusPerc01+
                         CapPerc01+DivPerc01+IntPerc01+
                         SCorpPerc01, data=taxData)

summary(lm(avg_tax_rate_01~SalPerc01, data=taxData))


selectedTaxVsInc1 <- stepAIC(baseFitTaxVsInc1, direction = "backward",
        scope= list(upper = ~SalPerc01+BusPerc01+
            CapPerc01+DivPerc01+IntPerc01+
             SCorpPerc01, lower = ~1))


plot(simulateResiduals(selectedTaxVsInc1))

summary(selectedTaxVsInc1)

#So we can see that the largest drivers of tax for the 1% is their salaries

mean(taxData$SalPerc01)
mean(taxData$BusPerc01)
mean(taxData$SCorpPerc01)
mean(taxData$CapPerc01)
mean(taxData$DivPerc01)
mean(taxData$IntPerc01)


EnvStats::geoMean(taxData$SalPerc01)

#despite their salaries being ~35% of their income. The largest percentage for sure, 
#but the fact that salaries are taxed at higher percentages than capital gains 
#and Scorp.business income is concerning

#Salary is .16 slope







baseFitTaxVsInc5 <- lm(avg_tax_rate_5~SalPerc05+BusPerc05+
                         CapPerc05+DivPerc05+IntPerc05+
                         ScorpPerc05, data=taxData)


selectedTaxVsInc5 <- stepAIC(baseFitTaxVsInc5, direction = "backward",
                            scope= list(upper = ~SalPerc05+BusPerc05+
                                          CapPerc05+DivPerc05+IntPerc05+
                                          ScorpPerc05, lower = ~1))

summary(lm(avg_tax_rate_5~SalPerc05, data=taxData))



plot(simulateResiduals(selectedTaxVsInc5))

summary(selectedTaxVsInc5)

avPlots(selectedTaxVsInc5)

mean(taxData$SalPerc05)


#Salary is higher relevant to their taxes (.28 slope)

baseFitTaxVsInc10 <- lm(avg_tax_rate_10~SalPerc10+BusPerc10+
                          CapPerc10+DivPerc10+IntPerc10+
                          ScorpPerc10, data=taxData)

summary(lm(avg_tax_rate_10~SalPerc10, data=taxData))


selectedTaxVsInc10 <- stepAIC(baseFitTaxVsInc10, direction = "backward",
                             scope= list(upper = ~SalPerc10+BusPerc10+
                                           CapPerc10+DivPerc10+IntPerc10+
                                           ScorpPerc10, lower = ~1))

summary(selectedTaxVsInc10)

avPlots(selectedTaxVsInc10)

mean(taxData$SalPerc10)


#The 10% is more balanced, with salary as relevant (.16)

baseFitTaxVsInc25 <- lm(avg_tax_rate_25~SalPerc25+BusPerc25+
                          CapPerc25+DivPerc25+IntPerc25+
                          ScorpPerc25, data=taxData)


selectedTaxVsInc25 <- stepAIC(baseFitTaxVsInc25, direction = "backward",
                              scope= list(upper = ~SalPerc25+BusPerc25+
                                            CapPerc25+DivPerc25+IntPerc25+
                                            ScorpPerc25, lower = ~1))

summary(selectedTaxVsInc25)

avPlots(selectedTaxVsInc25)

mean(taxData$SalPerc25)


#25% has salary as relevant (slope .2)


baseFitTaxVsInc50 <- lm(avg_tax_rate_50~SalPerc50+BusPerc50+
                          CapPerc50+DivPerc50+IntPerc50+
                          ScorpPerc50, data=taxData)


selectedTaxVsInc50 <- stepAIC(baseFitTaxVsInc50, direction = "backward",
                              scope= list(upper = ~SalPerc50+BusPerc50+
                                            CapPerc50+DivPerc50+IntPerc50+
                                            ScorpPerc50, lower = ~1))

summary(selectedTaxVsInc50)

avPlots(selectedTaxVsInc50)

mean(taxData$SalPerc50)



#50% has salary as relevant, but notably lower (slope .11)

baseFitTaxVsInc75 <- lm(avg_tax_rate_75~SalPerc75+BusPerc75+
                          CapPerc75+DivPerc75+IntPerc75+
                          ScorpPerc75, data=taxData)


selectedTaxVsInc75 <- stepAIC(baseFitTaxVsInc75, direction = "backward",
                              scope= list(upper = ~SalPerc75+BusPerc75+
                                            CapPerc75+DivPerc75+IntPerc75+
                                            ScorpPerc75, lower = ~1))

summary(selectedTaxVsInc75)

avPlots(selectedTaxVsInc75)

#75% has salary as irrelevant to their income!

#Time to wrap this up. 

#The 1% likely derive a higher percentage
#of their taxable income from other holdings, which is confirmed since 
#they derive 35% of their income from taxes. The slope is .16. 

#the 5% derive a larger percentage of their taxes from their incomes, 
#which is confirmed from the fact that 65% of their income comes from salaries. 
#The slope is .28

#the 10% derive most of their taxes from their incomes, which is confirmed from the 
#fact that 72%% of their income comes from salaries. The slope is .16, similar to the 1%


#the 25% derive most of their taxes from their incomes, which is confirmed from the 
#fact that 74%% of their income comes from salaries. The slope is .20


#the 50% derive most of their taxes from their incomes, which is confirmed from the 
#fact that 77%% of their income comes from salaries. The slope is .11. 

#The 75% also derives most of their taxes from their income, but interestingly their salaries
#are not correlated wit their tax rate?

#Lets do a visualization!


taxDataLongIncETR_NO_US$Perc <- as.factor(taxDataLongIncETR_NO_US$Perc)

summary(taxDataLongIncETR_NO_US$Perc)


ggplot(data=taxDataLongIncETR_NO_US, aes(y=ETR,x=AvgInc))+
  geom_smooth() + 
  geom_point(aes(color=Perc)) +
  xlab("Average Income Per Bracket") +
  ylab("Effective Tax Rate") +
  ggtitle("Effective Tax Rate vs Average Income")


ggplot(data=taxDataLongIncETR_NO_US, aes(y=ETR, x=SalPerc)) + 
  geom_point(aes(color=Perc))+
  geom_smooth()+
  ylab("Effective Tax Rate per Bracket") +
  xlab("Salary % of Income") +
  ggtitle("Effective Tax Rate vs Salary % of Income")

ggplot(data=taxDataLongIncETR_NO_US, aes(y=SalPerc , x=AvgInc)) + 
  geom_point(aes(color=Perc)) +
  geom_smooth() +
  xlab("Average Income per Bracket") +
  ylab("Percent of Income that is Salary") +
  ggtitle("Percent of Income that is Salary vs Average Income")

summary(taxDataLongIncETR_NO_US$Perc)

#In other words, as the income increases, the percentage of income will slowly switch from
#mostly salary to lower levels of salary

#Additionally, these increases of income are taxed at a lower rate than salary, since the 
#effective tax rate gradually plateus once total income reaches 1000000

taxDataLongIncETR_NO_US