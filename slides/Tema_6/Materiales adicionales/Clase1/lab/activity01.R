# Analysis of Experiment from Opening Activity 1
setwd("~/Dropbox/Research/Teaching/SurveyExperiments/Clase1/lab")


# Packages
# install.packages("gsheet")
library(gsheet)


# import data
dat <- rio::import("https://docs.google.com/spreadsheets/d/1SKWljS1EeNkAV5V0NZUwrKOu3LQFILVMB37xfTxyrPM/edit#gid=778386445")
# dat <- read.delim("activity01.tsv")
dat


# cleanup data slightly
dat[["Guess"]] <- as.numeric(gsub("[[:punct:]]", "", dat[["Guess"]]))



# anayze experiment #

# plot
plot(Guess~as.factor(Group), dat)


## t-test
t.test(Guess ~ Group, data = dat) # unequal variances assumed
t.test(Guess ~ Group, data = dat, var.equal = TRUE)


## OLS regression
s <- summary(m <- lm(Guess ~ Group, data = dat))

### OLS /w heteroskedasticity consistent standard errors
#install.packages("sandwich")
sandwich::vcovHC(m)
s$coefficients[,2] <- sqrt(diag(sandwich::vcovHC(m)))
s$coefficients[,3] <- s$coefficients[,1]/s$coefficients[,2]
s$coefficients[,4] <- 2*(1-pt(s$coef[,3], df = nrow(dat)-2L))
s

## ANOVA
summary(aov(Guess ~ Group, data = dat))

### F-test equivalent to t-test when only two conditions
### F = t^2



####### datos de estudiantes Uruguayos
library(gsheet)
dat <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1JFhZ3vrgMMBWy7Ln6vfgaeYIgeg_TgEh6ah2ztyGaOk/edit?usp=sharing")
# rename variables
names(dat) <- c('Timestamp', 'Group', 'Guess')
dat


# anayze experiment #

# plot
plot(Guess~as.factor(Group), dat)


## t-test
t.test(Guess ~ Group, data = dat) # unequal variances assumed


## OLS regression
s <- summary(m <- lm(Guess ~ Group, data = dat))

### OLS /w heteroskedasticity consistent standard errors
#install.packages("sandwich")
sandwich::vcovHC(m)
s$coefficients[,2] <- sqrt(diag(sandwich::vcovHC(m)))
s$coefficients[,3] <- s$coefficients[,1]/s$coefficients[,2]
s$coefficients[,4] <- 2*(1-pt(s$coef[,3], df = nrow(dat)-2L))
s
