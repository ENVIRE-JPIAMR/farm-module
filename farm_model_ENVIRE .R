library(tidyverse) #for data cleaning and plotting
library(mc2d) #for pert distribution
library(future)
library(furrr)
library(scales)
set.seed(123)

# introduce random scenarios

##Production parameters model - water consumption (Avian advice, 2009. University of Arkansas) - other parameters (Ross308 performance objectives, 2022)
min_water <- c(
  14.384558, 21.1604419, 35.5449999, 40.503887, 45.046379, 50.4973694,
  54.7370286, 47.5068955, 73.3990999, 73.3612458, 87.1022841, 98.5720764,
  107.4677899, 113.2594672, 112.4645311, 116.0606706, 123.0636791, 131.5429975,
  140.8929602, 146.495367, 134.6470337, 147.8959687, 143.6941636, 163.7568366,
  160.0849889, 175.3780453, 185.6743605, 201.8759153, 200.3996054, 181.0561603,
  212.5507715, 225.4211655, 209.4467353, 223.7934392, 212.5507715,210.3552337
)  #minimum water consumption
max_water <- c(
  29.8668849, 42.6615707, 53.6392597, 62.6485355, 64.1626995, 73.2476835,
  81.9541265, 87.7079497, 110.3447015, 113.8651328, 118.861874, 137.5239453,
  143.6184554, 153.8390624, 153.8390624, 174.6588174, 185.7500687, 196.0085298,
  206.6455319, 212.2479387, 207.0997811, 224.9669163, 238.0644349, 248.2471878,
  245.1431516, 262.7453081, 271.5274593, 287.0097862, 290.8330503, 300.0315966,
  298.1388916, 319.7535827, 333.5703292, 323.6147009, 330.7691258, 331.1855209	
)  #max water consumption
mean_water <- c(
  20.2519435, 29.147657, 41.5638018, 48.6046644, 38.0055164, 60.4151436,
  66.9639029, 73.8533491, 85.3231414, 97.3228911, 105.3479603, 114.2815279,
  124.0857398, 131.6565598, 135.1769911, 147.4038654, 156.5645576, 163.0376087,
  166.8608728, 174.8480879, 178.7849143, 187.8698983, 201.6866448, 206.6076778,
  205.6991794, 217.8881996, 226.9731836, 238.7836628, 238.7836628, 248.5500206,
  258.5056489, 265.357241, 265.8114902, 274.7829119, 277.1298661, 281.4452335
) #mean water consumption
weight <- c(
  81, 102, 125, 151, 181, 213, 249, 288, 330, 376, 425, 477, 533, 592, 655,
  720, 789, 860, 935, 1012, 1092, 1174, 1258, 1345, 1434, 1524, 1616, 1710,
  1805, 1901, 1999, 2097, 2196, 2296, 2396, 2496
) #broilers weight
daily_gain <- c(
  19, 21, 23, 26, 29, 32, 36, 39, 42, 46, 49, 52, 56, 59, 62, 66, 69, 72,
  74, 77, 80, 82, 85, 87, 89, 91, 92, 94, 95, 96, 97, 98, 99, 100, 100,
  100
)
daily_intake <- c(
  16, 20, 24, 27, 31, 35, 39, 44, 48, 52, 57, 62, 67, 72, 77, 83, 88, 94,
  100, 105, 111, 117, 122, 128, 134, 139, 145, 150, 156, 161, 166, 171,
  176, 180, 185, 189
) #broilers feed intake

####################################

 ##create dataframe             
df_conf <- data.frame()

#force of infection input, Dame-Korevaar 2020
# these parameters are used to calculate the force of infenction 
bconcentration_est = 0.31 # cfu * day^-1
bconcentration_ci_lower = 0.10 # lower limit of the 95% CI for bconcentration
bconcentration_ci_upper = 0.57 # upper limit of the 95% CI for bconcentration
bconcentration_sd = (bconcentration_ci_upper - bconcentration_est) / 1.96 # standard deviation for bconcentration





#initial animals, density function
# this function defines the initial flock. 
# prevalence is the starting prevalence of ESBL E. coli, expressed in %
# target_weight is the maximum weight of the broilers, expressed in kg
# density is the number of broilers per m2
# size is the size of the farm, expressed in m2


initial_animals_density <- function(prevalence,target_weight, density, size) {
  healthy <- tribble(
    ~days_since_infection, ~age,
    -1, 1
  )
  
  sick <- tribble(
    ~days_since_infection, ~age,
    1, 1
  )
  
  # Number of animals depending on broiler density and farm size
  n_animals <- density/target_weight*size
  
  animals <- rbind(
    sick[rep(1,round(n_animals*prevalence)),],
    healthy[rep(1,round(n_animals*(1-prevalence))),]
  ) %>% mutate(content = 0,
               sum_feces = 0,
               esbl = ifelse(days_since_infection == -1, 0, 10),
               sum_environment=0,
               density = density,
               infected = days_since_infection != -1,
               ingested_feces =0,
               cfu_environment =0)
  
  
  
  return(animals)
  
 
  
  
}



## bacteria logistic growth function. Growth rate from Becker 2022
# K is the maximum amount of bacteria in the substrate (the intestinal content in this case)
# r is the growth rate of the bacteria

logistic_growth <- function(animals, K, r) {
  animals %>%
    mutate(esbl = ifelse(days_since_infection != -1,
                         K / (1 + ((K - esbl) / esbl) * exp(-r * days_since_infection)),
                         esbl))}

#force of infection
force_of_infection_model3 <- function(animals, b_concentration) {
  
  sum_excretion_concentration <- animals %>%
    filter(days_since_infection != -1) %>%
    summarise(
      environment = sum(sum_environment),
      feces = sum(sum_feces),
      env_fec = log10(environment / feces)
    ) %>% pull(env_fec)
  
  foi <- b_concentration * sum_excretion_concentration 
  #in the study of dame korevaar the density was blabla and in this simulation...
  #100/8 m2, factor my density/density study
  return(foi)
}


#infection model 3, based on bacteria cfu in the environment. Dame-Korevaar 2020
# b_concentration is the concentration of ESBL E. coli in the environment, expressed in cfu * day^-1
# Dt is the time step, expressed in days

infection_animals2_model3 <- function(animals, b_concentration,Dt) {
  
  foi <- force_of_infection_model3(animals, b_concentration)
  
  num_negatives <- sum(animals$days_since_infection == -1)
  number_new_infected <- round(num_negatives * (1 - exp(-foi * Dt)))
  number_new_infected <- max(0, number_new_infected)
  
  
  if (number_new_infected >= num_negatives ) {
    number_new_infected <- num_negatives
  }
  
  
  
  animals$days_since_infection[sample(which(animals$days_since_infection == -1), number_new_infected, replace = FALSE)] <- 0
  
  animals$age <- animals$age + 1
  animals <- animals %>%
    mutate(days_since_infection = 
             ifelse(days_since_infection != -1, days_since_infection + 1, -1),
           infected = days_since_infection != -1,
           esbl = ifelse(days_since_infection == 1,  sum(sum_environment)/sum(sum_feces) * ingested_feces, esbl))
  
  return(animals)
}

#quantity of feces produced by a broiler per day. 

feces_function <- function(day, animals) {
  feces_amount <- runif(nrow(animals), min= min_water[day] , max= max_water[day]) * runif(nrow(animals), 0.7, 0.8) + (daily_intake[day] * rnorm(nrow(animals), 1, 0.1)) - daily_gain[day]
  animals$content <- feces_amount
  
  animals$sum_feces <- animals$sum_feces + feces_amount
  
  return(animals)
  
}



#amount of feces ingested per day
ingested_feces <- function(day, animals) {
  
  ingested <- rpert(nrow(animals), 0 ,0.05, 2) #+ log(animals$age)
  animals$ingested_feces <- ingested
  
  return(animals)
}


#excretion function
# e_rate is the excretion rate of ESBL E. coli, expressed in % of the total amount of ESBL E. coli in the intestinal content
excretion <- function(animals, e_rate) {
  
  content<- animals$content
  esbl<- animals$esbl
  
  excretion_cfu <- animals %>% mutate(cfu_environment = ifelse(days_since_infection!=-1,esbl*e_rate*runif(n = 1, min = 0.8, max = 1.2),0),
                                      sum_environment = sum_environment + cfu_environment - (animals$ingested_feces * sum(sum_environment)/sum(sum_feces)),
                                      esbl = esbl - cfu_environment  + (animals$ingested_feces * sum(sum_environment)/sum(sum_feces)) )
  }


# bacteria environmental decay  
# ed_rate is the environmental decay rate of ESBL E. coli, expressed in % of the total amount of ESBL E. coli in the environment
environmental_decay <- function(animals, ed_rate) {
  animals %>%
    mutate(sum_environment = ifelse(days_since_infection != -1,
                                    sum_environment * (1-ed_rate),
                                    sum_environment))}



farm <- 100
litter <- farm * 1000
animals <- initial_animals_density(0.01,2.5,39, farm)
initial_animals <- animals

#this function simulates a production day
simulate_day <- function(animals, day, until) {
  
  
 
  animals <- feces_function (day,animals)
  animals <- ingested_feces(day,animals)
  animals <- excretion(animals,e_rate=0.3)
  animals <- logistic_growth(animals, K=10^6*animals$content, r= 10^runif(1,0,5))
  animals <- infection_animals2_model3(animals, rnorm(1, bconcentration_est, bconcentration_sd),1)
  animals <- environmental_decay(animals, 0.5)
  

  
  if (day < until) 
    c(list(animals), simulate_day(animals, day = day + 1, until = until)) 
  else return(list(animals))
}

#montecarlo but also including the initial dataframe "animals" as day 1
# the map function is used to run the simulation in parallel
montecarlo <- map(1:1000, .progress = TRUE, function(x) {
  simulated_days <- simulate_day(animals = animals, day = 1, until = 36)
  c(list(initial_animals), simulated_days)
})



# data frame with the results of the montecarlo simulation
df_montecarlo <-montecarlo |> map(bind_rows, .id = "day",  .progress = TRUE) |> bind_rows(.id = "groups") |>  mutate(day = as.numeric(day)) |> group_by(groups)

#summarise the results of the montecarlo simulation
result <- df_montecarlo |> 
  group_by(day, groups) |>
  summarise(sum_environment = sum(sum_environment, na.rm = TRUE),
            sum_feces= sum(sum_feces),
            esbl = sum(esbl)
  )




##plot 
ggplot(prova, aes(x=day, y=sum_environment / (sum_feces + litter), color=factor(density))) +
  geom_line() + geom_point() + 
  labs(#title="Concentration ESBL producing E.coli in broiler manure",
       x="Day",
       y="CFU/g",
       color="Density") + theme_classic() + scale_y_continuous(labels = function(x) formatC(x, format = "e", digits = 1))


##calculate mean and sd 
result_last_day <- result %>% filter(day==36)
result_last_day <- result_last_day %>% mutate(cfu_g = sum_environment / (sum_feces+litter))
mean(result_last_day$cfu_g)
sd(result_last_day$cfu_g)


result <- result %>%
  filter(day <= 36)


result

#cfu/g manure  

max_value <- max(mean(result$sum_environment / (result$sum_feces + litter), na.rm = TRUE))
max_day <- result$day[which.max(mean(result$sum_environment / (result$sum_feces + litter)))]

ggplot(result, aes(day, sum_environment/(sum_feces + litter), color = as.factor(groups))) +
  geom_line(alpha= 0.1)  + 
  scale_x_continuous(breaks = seq(min(1), max(36), by = 1)) +
  theme_classic()  + guides(color = "none")  +
  ylab("CFU/g") + scale_y_continuous(labels = function(x) formatC(x, format = "e", digits = 1)) +
  stat_summary(fun.ymin = min, fun.ymax = max, fun.y = mean, geom = "line", linetype = "dashed", size = 0.5, color = "black") +
  theme(
    axis.title = element_text(size = 16)  # Adjust the size as needed
  )


ggplot(result, aes(day, sum_environment/(sum_feces + litter), color = as.factor(groups))) +
  geom_line(alpha= 0.1)  + 
  geom_point(aes(x = max_day, y = max_value), color = "red", size = 3) +  # add this line
  scale_x_continuous(breaks = seq(min(1), max(36), by = 1)) +
  theme_classic()  + guides(color = "none")  +  labs(title = "CFU per gram of feces") +
  ylab("CFU/g") + scale_y_continuous(labels = function(x) formatC(x, format = "e", digits = 1)) +
  stat_summary(fun.ymin = min, fun.ymax = max, fun.y = mean, geom = "line", linetype = "dashed", size = 0.5, color = "black") +
  theme(
    axis.title = element_text(size = 16)  # Adjust the size as needed
  )

ggplot(result, aes(day, sum_feces + litter, color = as.factor(groups))) +
  geom_line(alpha= 0.1)  + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), minor_breaks = NULL) +
  theme_classic()  + guides(color = "none")  +
  ylab("CFU/g") + scale_y_continuous(labels = function(x) formatC(x, format = "e", digits = 1)) +
  stat_summary(fun.ymin = min, fun.ymax = max, fun.y = mean, geom = "line", linetype = "dashed", size = 0.5, color = "black") +
  theme(
    axis.title = element_text(size = 16)  # Adjust the size as needed
  )



###cfu environment
ggplot(result, aes(day, sum_environment, color = as.factor(groups))) +
  geom_line(alpha= 0.1) +
  scale_x_continuous(breaks = seq(min(1), max(36), by = 1)) +
  theme_classic()  + guides(color = "none") + scale_y_continuous(labels = function(x) formatC(x, format = "e", digits = 1)) +
  ylab("CFU") + labs(title = "Total CFU of ESBL E. coli excreted") +
  stat_summary(fun.ymin = min, fun.ymax = max, fun.y = mean, geom = "line", linetype = "dashed", size = 0.5, color = "black") + 
  theme(
    axis.title = element_text(size = 16)  # Adjust the size as needed
  )

ggplot(result, aes(day, sum_feces, color = as.factor(groups))) +
  geom_line(alpha= 0.1) +
  scale_x_continuous(breaks = seq(min(1), max(36), by = 1)) + scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  theme_classic()  + guides(color = "none") +
  ylab("feces") + 
  stat_summary(fun.ymin = min, fun.ymax = max, fun.y = mean, geom = "line", linetype = "dashed", size = 0.5, color = "black") +
  theme(
    axis.title = element_text(size = 16)  # Adjust the size as needed
  )


##esbl gut
ggplot(result, aes(day, esbl, color = as.factor(groups))) +
  geom_line(alpha= 0.1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), minor_breaks = NULL) +
  theme_classic()  + guides(color = "none") + scale_y_continuous(labels = function(x) formatC(x, format = "e", digits = 1)) +
  ylab("CFU") + 
  stat_summary(fun.ymin = min, fun.ymax = max, fun.y = mean, geom = "point", linetype = "dashed", size = 0.8, color = "black")

cacca <- result %>% select(day, groups, sum_feces) %>% filter(day==37)




###infected
infected<-  df_montecarlo |> 
  group_by(day, groups) |> 
  select(infected)

daily_infected <- infected %>%
  filter(infected) %>%
  group_by(day, groups) %>%
  summarise(num_infected = n()) %>%
  ungroup()


# First, calculate the total number of animals per day for each group
total_animals <- df_montecarlo %>%
  group_by(day, groups) %>%
  summarise(total = n()) %>%
  ungroup()

# Now, join the total animals dataframe with daily_infected to add the total column
daily_infected <- daily_infected %>%
  left_join(total_animals, by = c("day", "groups")) 

# Calculate the proportion of infected animals
daily_infected <- daily_infected %>%
  mutate(prop_infected = num_infected / total)


daily_infected <- daily_infected %>% filter(day<=36)

daily_infected %>% filter(day > 2 & day < 3 & prop_infected != 1)


daily_infected %>% filter(day==9 & prop_infected < 0.99)

# plot the proportion of infected animals
ggplot(daily_infected, aes(x = day, y = prop_infected, color = as.factor(groups))) + 
  geom_line(alpha = 0.3) +  scale_x_continuous(breaks = seq(min(0), max(36), by = 1)) +
  guides(color = "none") +
  stat_summary(fun.min = min, fun.max = max, fun = mean, geom = "line", 
               linetype = "dashed", size = 0.5, color = "black") + 
  theme_classic() + labs(
                         x="Day",
                         y="Positive animals",
                         color="Density") + scale_y_continuous(labels=scales::percent) + theme(
                           axis.title = element_text(size = 16)  # Adjust the size as needed
                         )

resulttt <- daily_infected %>%
  group_by(groups) %>%
  filter(prop_infected >= 1) %>%   summarize(first_day_to_100_percent = min(day))

mean(resulttt$first_day_to_100_percent)
median(resulttt$first_day_to_100_percent)
getmode(resulttt$first_day_to_100_percent)


day_100_percent_infected <- daily_infected %>%
  group_by(day) %>%
  summarize(average_infected = mean(prop_infected, na.rm = TRUE)) %>%
  filter(average_infected >= 0.999) %>%
  slice(1) %>%
  pull(day)


ggplot(daily_infected, aes(x = day, y = prop_infected, color = as.factor(groups))) + 
  geom_line(alpha = 0.3) +
  scale_x_continuous(breaks = seq(min(0), max(36), by = 1)) +
  guides(color = "none") +
  stat_summary(fun.min = min, fun.max = max, fun = mean, geom = "line", 
               linetype = "dashed", size = 0.5, color = "black") +
  theme_classic() +
  labs(x="Day", y="Positive animals", color="Density") +
  scale_y_continuous(labels=scales::percent) +
  theme(axis.title = element_text(size = 16)) +
  geom_point(aes(x = day_100_percent_infected, y = 1), color = "red", size = 4)  # Add a point at the day when average prop_infected reaches 1



df_montecarlo


# Calculate the average values for each day
avg_values <- aggregate(result$sum_environment / (result$sum_feces + litter), 
                        by = list(day = result$day), 
                        FUN = mean)

# Identify the day and value for the maximum average
max_avg_value <- max(avg_values$x, na.rm = TRUE)
max_avg_day <- avg_values$day[which.max(avg_values$x)]

ggplot(result, aes(day, sum_environment/(sum_feces + litter), color = as.factor(groups))) +
  geom_line(alpha= 0.1)  + 
  geom_point(aes(x = max_avg_day, y = max_avg_value), color = "blue", size = 3) +  # add this line
  scale_x_continuous(breaks = seq(min(1), max(36), by = 1)) +
  theme_classic()  + guides(color = "none")  +  labs(title = " CFU of ESBL E. coli per gram of manure") +
  ylab("CFU/g") + scale_y_continuous(labels = function(x) formatC(x, format = "e", digits = 1)) +
  stat_summary(fun.ymin = min, fun.ymax = max, fun.y = mean, geom = "line", linetype = "dashed", size = 0.5, color = "black") +
  theme(
    axis.title = element_text(size = 16)  # Adjust the size as needed
  )
