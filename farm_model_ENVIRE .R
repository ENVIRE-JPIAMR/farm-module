library(tidyverse) #for data cleaning and plotting
library(mc2d) #for pert distribution
library(future)
library(furrr)
library(scales)
library(readxl)
set.seed(123)

# read input variables
df_read <- read.csv("inputs.csv", header = TRUE, sep = ';')

# parsing objects from input list
input_objects = list(
  water_consum.min    = eval(parse(text = df_read$Value[df_read$Variable == "water_consum.min"])),
  water_consum.max    = eval(parse(text = df_read$Value[df_read$Variable == "water_consum.max"])),
  water_consum.mean   = eval(parse(text = df_read$Value[df_read$Variable == "water_consum.mean"])),
  weight              = eval(parse(text = df_read$Value[df_read$Variable == "weight"])),
  daily_gain          = eval(parse(text = df_read$Value[df_read$Variable == "daily_gain"])),
  daily_intake        = eval(parse(text = df_read$Value[df_read$Variable == "daily_intake"]))
)

# parsing non-objects from input list
idx_double <- df_read$Type != "OBJECT"
df_double <-
  data.frame(id = df_read$Variable[idx_double],
             val = unlist(lapply(df_read$Value[idx_double], function(x)
               eval(parse(
                 text = x
               )))))

named_vector <- with(df_double, setNames(val, id))
input_doubles <- lapply(split(named_vector, names(named_vector)), unname)

input_list <- c(input_objects, input_doubles)  

#initial animals, density function
# this function defines the initial flock. 
# prevalence is the starting prevalence of ESBL E. coli, expressed in %
# target_weight is the maximum weight of the broilers, expressed in kg
# density is the number of broilers per m2
# size is the size of the farm, expressed in m2


initialize_df <- function() {
  healthy <- tribble(
    ~days_since_infection, ~age,
    -1, 1
  )
  
  sick <- tribble(
    ~days_since_infection, ~age,
    1, 1
  )
  
  # Number of animals depending on broiler density and farm size
  n_animals <- input_list$farm_density/input_list$target_weight*input_list$farm_size
  
  animals <- rbind(
    sick[rep(1,round(n_animals*input_list$prevalence)),],
    healthy[rep(1,round(n_animals*(1-input_list$prevalence))),]
  ) %>% mutate(content = 0,
               sum_feces = 0,
               esbl = ifelse(days_since_infection == -1, input_list$esbl.min, input_list$esbl.max),
               sum_environment=0,
               density = input_list$farm_density,
               infected = days_since_infection != -1,
               ingested_feces =0,
               cfu_environment =0)
  
  
  
  return(animals)
  
 
  
  
}



## bacteria logistic growth function. Growth rate from Becker 2022
logistic_growth <- function(animals) {
  
  K <- input_list$K*animals$content
  r <- 10^runif(1, input_list$r.min, input_list$r.max)
  
  animals %>%
    mutate(esbl = ifelse(days_since_infection != -1,
                         K / (1 + ((K - esbl) / esbl) * exp(-r * days_since_infection)),
                         esbl))}

#force of infection
force_of_infection_model3 <- function(animals) {
  
  sum_excretion_concentration <- animals %>%
    filter(days_since_infection != -1) %>%
    summarise(
      environment = sum(sum_environment),
      feces = sum(sum_feces),
      env_fec = log10(environment / feces)
    ) %>% pull(env_fec)
  
  foi <- input_list$beta.mean * sum_excretion_concentration 
  #in the study of dame korevaar the density was blabla and in this simulation...
  #100/8 m2, factor my density/density study
  return(foi)
}


#infection model 3, based on bacteria cfu in the environment. Dame-Korevaar 2020
infection_animals2_model3 <- function(animals) {
  
  foi <- force_of_infection_model3(animals)
  
  num_negatives <- sum(animals$days_since_infection == -1)
  number_new_infected <- round(num_negatives * (1 - exp(-foi * input_list$Dt)))
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
  feces_amount <- runif(nrow(animals), min= input_list$water_consum.min[day] , max= input_list$water_consum.max[day]) * runif(nrow(animals), input_list$water_reduction.min, input_list$water_reduction.max) + (input_list$daily_intake[day] * rnorm(nrow(animals), 1, input_list$daily_intake.sd)) - input_list$daily_gain[day]
  animals$content <- feces_amount
  
  animals$sum_feces <- animals$sum_feces + feces_amount
  
  return(animals)
  
}



#amount of feces ingested per day
ingested_feces <- function(animals) {
  
  ingested <- rpert(nrow(animals), input_list$ingested_feces.min ,input_list$ingested_feces.mode, input_list$ingested_feces.max) #+ log(animals$age)
  animals$ingested_feces <- ingested
  
  return(animals)
}


#excretion function
# e_rate is the excretion rate of ESBL E. coli, expressed in % of the total amount of ESBL E. coli in the intestinal content
excretion <- function(animals) {
  
  content<- animals$content
  esbl<- animals$esbl
  
  excretion_cfu <- animals %>% mutate(cfu_environment = ifelse(days_since_infection!=-1, esbl*input_list$e_rate, 0),
                                      sum_environment = sum_environment + cfu_environment - (animals$ingested_feces * sum(sum_environment)/sum(sum_feces)),
                                      esbl = esbl - cfu_environment  + (animals$ingested_feces * sum(sum_environment)/sum(sum_feces)) )
  }


# bacteria environmental decay  
# ed_rate is the environmental decay rate of ESBL E. coli, expressed in % of the total amount of ESBL E. coli in the environment
environmental_decay <- function(animals) {
  animals %>%
    mutate(sum_environment = ifelse(days_since_infection != -1,
                                    sum_environment * (1-input_list$ed_rate),
                                    sum_environment))}

litter <- input_list$farm_size * input_list$litter_mass
animals <- initialize_df()
initial_animals <- animals

#this function simulates a production day
simulate_day <- function(animals, day, until) {
  
  
 
  animals <- feces_function (day,animals)
  animals <- ingested_feces(animals)
  animals <- excretion(animals)
  animals <- logistic_growth(animals)
  animals <- infection_animals2_model3(animals)
  animals <- environmental_decay(animals)
  

  
  if (day < until) 
    c(list(animals), simulate_day(animals, day = day + 1, until = until)) 
  else return(list(animals))
}

#montecarlo but also including the initial dataframe "animals" as day 1
# the map function is used to run the simulation in parallel
montecarlo <- map(1:input_list$n_sim, .progress = TRUE, function(x) {
  simulated_days <- simulate_day(animals = animals, day = input_list$day.min, until = input_list$day.max)
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
