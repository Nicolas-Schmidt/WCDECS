stopifnot(require(randomizr))
stopifnot(require(devtools))
stopifnot(require(DeclareDesign))
stopifnot(require(plyr))

## DGP # 1 -- No DeclareDesign

# DGP 1—No DeclareDesign --------------------------------------------------


N <- 1206
countries <- rep(c("Argentina", "Brazil", "Chile"), each = 402)
class <- rep(c("poor", "poor", "poor", "middle", "middle", "rich"), length.out = 1206)
block <- paste0(countries, class)
e_ind <- rnorm(1206)
e_country <- as.numeric(as.character(mapvalues(countries, 
                                               from = unique(countries), 
                                               to = rnorm(3))))
e_class <- as.numeric(as.character(mapvalues(class, 
                                               from = unique(class), 
                                               to = c(0, -.1, -.3))))

# treatment conditions: informal, poor, control--block on class and country
Z <- block_ra(block_var = block, condition_names = c("informal", "poor", "control"))

# create potential outcomes
Y <- 3 + 1 * (Z == 'informal') - .5 * (Z == 'poor') + e_ind + e_country + e_class

# likard-scale (7 point)
Y_lik <- round(Y)
Y_lik[Y_lik < 1] <- 1
Y_lik[Y_lik > 7] <- 7

# make dataset
data_frame <- data.frame(countries, class, block, e_ind, e_class, e_country, Z, Y, Y_lik)

# to save data uncomment
#save(data_frame, file = "mock_data.Rdata")


# DeclareDesign -----------------------------------------------------------
install_github("DeclareDesign/DeclareDesign")

population <- declare_population(
  country = list(e_country = declare_variable()),
  individual = list(e_individual = declare_variable(),
                    class = declare_variable(type = "multinomial", 
                                             probabilities = c(3/6, 2/6, 1/6), 
                                             outcome_categories = c("poor", "middle", "rich"))),
  size = c(1206, 3))

draw_population(population)

sampling <- declare_sampling(sampling = F)

assignment <- declare_assignment(condition_names = c("informal", "poor", "control"),
                                  block_variable_name = "class")

pos <- declare_potential_outcomes(formula = Y ~ 3 + 1 * 1 * (Z == 'informal') - 
                                  .5 * (Z == 'poor') + e_ind + e_country,
                                  condition_names = c("Zinformal", "Zcontrol", "Zpoor"))

design <- declare_design(population = population,
                         sampling   = sampling,
                         assignment = assignment,
                         potential_outcomes = pos)

data <- draw_data(design)

#save(data, file = "mock_data_b.Rdata")
