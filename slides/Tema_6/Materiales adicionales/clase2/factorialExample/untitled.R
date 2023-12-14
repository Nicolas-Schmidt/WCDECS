
# block randomization
 ID <- 1:N
country <- c("ar", "bra", "chi")
country <- rep(country, each=400)
class <- c("poor", "middle", "rich")
class <- rep(class, length=N)
df<- data.frame(ID,country,class)

# install.packages("dplyr")
library(dplyr)

# install.packages("randomizr")
library(randomizr)

blockvar <- paste0(class, country)
table(blockvar)

Z <- block_ra(blockvar, condition_names=c("informal", "poor", "control"))

e <- rnorm(1200)
e.ar <- rnorm(1)
e.chile <- rnorm(1, .2, sd=2)
e.brazil <- rnorm(1, .3, sd= 4)

Y <- 3 + 2*(Z=="informal") + 1*(Z=="poor") + e+ 
	e.ar*(country=="ar") + e.chile*(country=="chi") + e.brazil*(country=="bra")

df <- data.frame(ID, class, country, blockvar, Z, Y)































