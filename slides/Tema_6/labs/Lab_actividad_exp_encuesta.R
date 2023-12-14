# Analysis of Experiment from Opening Activity 1
setwd("~/Dropbox/Research/Teaching/SurveyExperiments/Clase1/lab")

---
    title: |
    | Taller 8: Lab 8: Actividad inicial experimentos de encuesta
output:
    html_document:
    toc: true
number_sections: true
toc_depth: 3
toc_float: true
theme: spacelab
highlight: tango
urlcolor: BrickRed
lang : es-AR
header-includes: \usepackage[spanish]{babel}
\usepackage{graphicx}
\usepackage{subfig}

---


# Packages
# install.packages("gsheet")
library(gsheet)
# install.packages("ri2")
library(ri2)
# install.packages("tidyverse") # Instalar en caso de ser necesario
library(tidyverse)



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


#Crear Z - nueva variable de Tratamiento
dat <- mutate(dat, Z = case_when(
    Group == "Group 1" ~ 0,
    TRUE ~ 1)
)

# declarar el procedimiento de aleatorización
declaration <- randomizr::declare_ra(N = 153, m = 2)

declaration

ri2_out <- conduct_ri(
    formula = Guess ~ Z,
    declaration = declaration,
    sharp_hypothesis = 0,
    data = dat,
    sims = 1000,
    p = "two-tailed"
)

summary(ri2_out)

plot(ri2_out)

ri2_out <- conduct_ri(
    formula = Guess ~ Z,
    declaration = declaration,
    sharp_hypothesis = 0,
    data = dat,
    sims = 1000,
    p = "upper"
)

summary(ri2_out)

plot(ri2_out)


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
