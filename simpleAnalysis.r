# reading the simulated set and running simple analysis

# begin with survival analysis
library(ggfortify)
library(tidyverse)
library(ggplot2)
library(survey)
library(survival)
library(survminer)


## 
# set censoring decision
dfBoth$month <- case_when(dfBoth$sickDay <= 299 ~ 1, TRUE ~ 0) # currently for 299 days survival
# set reference groups
dfBoth$ageGroup <- relevel(dfBoth$ageGroup, ref='41-60')
dfBoth$firstDose <- relevel(dfBoth$firstDose, ref='1')
dfBoth$gender <- factor(dfBoth$gender, levels=c(0,1))
surv <- Surv(time = dfBoth$sickDay, event = dfBoth$month)  
summary(surv)
k_fit <- survfit(surv ~ ageGroup, data=dfBoth)
autoplot(k_fit) + theme_minimal() + ggtitle("Age Group")


k_fitD <- survfit(surv ~ firstDose, data=dfBoth)
autoplot(k_fitD) + theme_minimal() + ggtitle("Vaccination")

# test effect of time of vaccination
k_fitT <- survfit(surv ~ timeOfVac, data=dfBoth)
autoplot(k_fitT) + theme_minimal() + ggtitle("Vaccination Time")


# run Cox-Hazard Ratio (I haveremoved the vac(yes/no) as timeOfVac includes that as well)
fit.cox <- coxph(surv ~  gender + ageGroup + timeOfVac, 
                 data = dfBoth)
ggforest(fit.cox, data = dfBoth)


## Calculating effectiveness (OR or HR)
# need to take all unvaccinated and compare to those who were vaccinated in the morning. 
# Should adjust and create the relevant dataset (probably also create a new variable)
dfMorning <- dplyr::filter(dfBoth, timeOfVac=='morning'| timeOfVac=='notVacc')
dfMorning$timeOfVac <- factor(dfMorning$timeOfVac, levels = c('notVacc','morning'))
survM <- Surv(time = dfMorning$sickDay, event = dfMorning$month)
fit.coxM <- coxph(survM ~  gender + ageGroup + timeOfVac, 
                  data = dfMorning)
ggforest(fit.coxM, data = dfMorning) # here its 0.22 HR for morning

## Calculating for evening
dfEvening <- dplyr::filter(dfBoth, timeOfVac=='evening'| timeOfVac=='notVacc')
dfEvening$timeOfVac <- factor(dfEvening$timeOfVac, levels = c('notVacc','evening'))
survM <- Surv(time = dfEvening$sickDay, event = dfEvening$month)
fit.coxE <- coxph(survM ~  gender + ageGroup + timeOfVac, 
                  data = dfEvening)
ggforest(fit.coxE, data = dfEvening) # here its 0.12 for evening
