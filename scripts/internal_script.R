
# Introduction to Functional Programming with purrr -------------------------


library(purrr)
library(dplyr)
library(repurrrsive)
library(ggplot2)
library(modelr)
library(broom)
library(tidyr)


myList <- list(A = rnorm(100),
               B = sample(LETTERS, 10))


length(myList)
names(myList)

myList[[2]]
myList[2]

myList$A

addingFunction <- function(x, y = 0){
  x + y
}

addingFunction(1:10, 2)

str(gap_split)


# exercise 1-4 ------------------------------------------------------------

length(gap_split)
names(gap_split)

gap_uk <- gap_split$`United Kingdom`


mean_lifeExp <- function(data, name){
  data <- data[[name]]
  mean(data$lifeExp)
}

mean_lifeExp(gap_split, "United Kingdom")



gap_split[[1]] %>%
  filter(lifeExp == max(lifeExp)) %>%
  pull(year)

gap_split[[1]] %>%
  filter(lifeExp == max(lifeExp)) %>%
  magrittr::extract2(year)


maxYear <- function(data){
  data %>%
    filter(lifeExp == max(lifeExp)) %>%
    magrittr::extract2("year")
}

maxYear(gap_split[[4]])

years <- vector("numeric", length = length(gap_split))

for(i in seq_along(gap_split)){
  years[i] <- maxYear(gap_split[[i]])
}

map(gap_split, maxYear)



# Iteration in purrr ------------------------------------------------------


lifeExpectancy <- map(gap_split, "lifeExp")
map(lifeExpectancy, max)

map(gap_split, "lifeExp") %>% map(max)


# exercise 2-3 ------------------------------------------------------------


population <- map(gap_split, "pop")
map(population, min)

map(gap_split, "gdpPercap") %>% map(var)


ans <-  map(population, 1)
which.min(ans)
ans[[109]]



# Additional arguments ----------------------------------------------------


map(lifeExpectancy, quantile, probs = c(0.05, 0.95))

maxLife <- function(data){
  max(data$lifeExp)
}

map(gap_split, maxLife)

x <- gap_split[[1]]

max(x$lifeExp)

map(gap_split, ~max(.x$lifeExp))


# exercise 2-5 ------------------------------------------------------------


min_pop_year <- function(data){
  data %>%
    filter(pop == min(pop)) %>%
    magrittr::extract2("year")
}

ans <- map(gap_split, min_pop_year)

ans <- map(gap_split, ~magrittr::extract2(filter(., pop == min(pop)), "year"))

map(gap_split, ~{filter(., pop == min(pop)) %>%
                 magrittr::extract2("year")})

which.max(ans)
gap_split[[41]]



map_dbl(gap_split, ~max(.x$lifeExp))



#map_lgl(gap_split, ~max(.x$lifeExp))


# exercise 2-7 ------------------------------------------------------------

map_dbl(gap_split, ~mean(.x$lifeExp))


pluck(gap_split, "United Kingdom")
# gap_split$`United Kingdom`
# gap_split[['United Kingdom']]

pluck(gap_split, "United Kingdom", "lifeExp")

is.europe <- function(data){
  unique(data$continent) == "Europe"
}

europe <- keep(gap_split, is.europe)

not_europe <- discard(gap_split, is.europe)

uk <- pluck(gap_split, "United Kingdom")

gap_split_updated <- prepend(gap_split, values = list(UK = uk))

invert <- transpose(gap_split)
names(invert)

# exercise 3-4 ------------------------------------------------------------

is.maxLife <- function(data){
  data$lifeExp[data$year == max(data$year)] == max(data$lifeExp)
}

life_mostRec <- function(data){
  data$lifeExp[data$year == max(data$year)]
}

max_lifeExp <- function(data){
  max(data$lifeExp)
}

is.maxLife <- function(data){
  life_mostRec(data) == max_lifeExp(data)
}


gap_maxLife <- discard(gap_split, is.maxLife)


map(gap_maxLife, max_lifeExp)
map(gap_maxLife, life_mostRec)



# The wider map family ----------------------------------------------------


means <- rep(0:5, each = 2)
sds <- rep(c(1, 2), times = 6)

means <- set_names(means, nm = LETTERS[1:12])

normData <- map2_df(means, sds, rnorm, n = 100)

normData %>% gather(key = Simulation, value = Value) %>%
  qplot(Value, data = ., geom = "density", group = Simulation)


map2_df(means, sds, ~rnorm(mean = .x, sd = .y, n = 100))

n <- sample(c(5, 10, 100), 12, replace = TRUE)

pmap(list(means, sds, n), ~rnorm(n = ..3, mean = ..1, sd = ..2))



# Side effects ------------------------------------------------------------

plotLifeExpectancy <- function(data){
  country <- unique(data$country)
  p <- qplot(x = year, y = lifeExp, data = data,
             main = country, geom = "line")
  print(p)
}

pdf("lifeExpectancyPlots.pdf")
walk(gap_split, plotLifeExpectancy)
dev.off()



# exercise 4-4 ------------------------------------------------------------

max_life_print <- function(lifeExp, name){
  cat("The maximum life expectancy for", name, "was", max(lifeExp), "\n")
}

max_life_print(x$lifeExp, "Afghanistan")

lifeExp_list <- map(gap_split, "lifeExp")
country_names <- names(gap_split)

walk2(lifeExp_list, country_names, max_life_print)


plotLifeExpectancy <- function(data, country){
  p <- qplot(x = year, y = lifeExp, data = data,
             main = country, geom = "line")
  print(p)
}

pdf("lifeExpectancyPlotsWithCountry.pdf")
iwalk(gap_split, plotLifeExpectancy)
dev.off()


# exercise 4-5 ------------------------------------------------------------

map(gap_split, "lifeExp") %>% iwalk(max_life_print)



# Nested Data -------------------------------------------------------------

# How to create split list dataframes
iris %>% split(iris$Species)

gap_nested %>%
  filter(country == "United Kingdom") %>%
  select(data) %>%
  unnest()


map(gap_nested$data, "lifeExp")


gap_nested %>%
  mutate(maxLife = map_dbl(data, ~max(.$lifeExp)))


# exercise 5-4 ------------------------------------------------------------

gap_nested %>%
  mutate(minPop = map_dbl(data, ~min(.$pop)),
         varGPD = map_dbl(data, ~var(.$gdpPercap)))


gap_nested %>%
  mutate(pop1952 = map_int(data, ~magrittr::extract2(filter(., year == 1952), "pop")))


gap_model <- gap_nested %>%
             mutate(model = map(data, ~lm(lifeExp ~ year, data = .x)))


gap_model %>%
  transmute(country, fit = map(model, glance)) %>%
  unnest()
         

gap_fit <- gap_model %>%
           mutate(residuals = map2(data, model, add_residuals)) %>%
           unnest(residuals)

ggplot(data = gap_fit, aes(x = year, y = resid)) +
  geom_line(alpha = 0.5, aes(group = country)) + facet_wrap(~continent)


iris %>%
  group_by(Species) %>%
  nest()


# exercise 5-7 ------------------------------------------------------------

gap_simple %>%
  group_by(continent) %>%
  nest() %>%
  mutate(model = map(data, ~lm(lifeExp ~ pop, data = .x)),
         metrics = map(model, glance))
