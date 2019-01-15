
# Functional Programming with Purrr ---------------------------------------

# Notes and a neater version of this script
# https://github.com/MangoTheCat/Introduction-to-Functional-Programming-with-Purrr
# or short link
# https://bit.ly/mcpurrr

# Need the packages?
install.packages(c("tidyverse", "repurrrsive"))

# Lists

myList <- list(A = rnorm(100), B = sample(LETTERS, 10))
myList

length(myList)
names(myList)

myList[["B"]]
myList$B

myMass <- list(doug = 80, gary = 100)

rest_energy <- function(m, c = 299792458) {
  m * c^2
}

lapply(myMass, rest_energy)

# Exercise 1-4

library(repurrrsive)

# Using the gap_split data in the repurrrsive package:
#   a. How many elements are in the list?

View(gap_split)
length(gap_split)

#   b. Do the elements have names?
names(gap_split)

#   c. Extract the data from the United Kingdom. What type of data is it?

class(gap_split[["United Kingdom"]])
print.default(gap_split[["United Kingdom"]])

#   2. Write a function that, when given the data and a country name will
# calculate the mean life expectancy for that country

library(tidyverse)

meanLife <- function(data, country) {
  mean(data[[country]]$lifeExp)
}

View(myList)
View(gap_split)

data(package = "repurrrsive")

library(purrr)

maxYear <- function(data) {
  
  data %>%
    filter(lifeExp == max(lifeExp)) %>%
    pull("year")
  
}

maxYear(gap_split$Afghanistan)

# purrr map

map(gap_split, maxYear) # equivalent to an lapply


# Chapter 2

# Extracting elements
lifeExpectancy <- map(gap_split, "lifeExp")

names(lifeExpectancy)
length(142)


map(lifeExpectancy, max)


# Exercise working with map Page 2-3
# 
# 1. Using the split gapminder data:
#   a. Find the minimum value of the population for each country

population <- map(gap_split, "pop")
map(population, min)

#   b. Calculate the variance of the GDP per capita

gdpVar <- function(data) {
  gdp <- data[["gdpPercap"]]
  var(gdp)
}
map(gap_split, gdpVar)

# Alternative
map(gap_split, "gdpPercap") %>% map(var)


# Extension Questions
# 2. For each country, extract the value of the population in 1952.

# 3. Which country had the lowest population in 1952? (hint: take a look at
#                                                      which.min)

maxLife <- function(data) {
  max(data$lifeExp)
}

map(gap_split, maxLife)

map(gap_split, ~max(.$lifeExp))


lapply(gap_split, maxLife)
maxLifeVec <- sapply(gap_split, maxLife)

class(maxLifeVec)
maxLifeVec[3]

maxLifeVec <- vapply(gap_split, maxLife, numeric(1))

# Type-safe map functions
maxLifeDbl <- map_dbl(gap_split, maxLife)
maxLifeDbl

# Exercise
# Page 2-6
# 1. Find the average life expectancy for each country, storing 
# the output in a numeric vector

avLife <- map_dbl(gap_split, ~ mean(.$lifeExp))

# 2. Can you store the output in an integer vector?

avLife <- map_int(gap_split, ~ mean(.$lifeExp))


# Chapter 3 ---------------------------------------------------------------

# Extracting elements
pluck(gap_split, "United Kingdom", "lifeExp")

# Filtering
is.europe <- function(data) {
  unique(data$continent) == "Europe"
}

europe <- keep(gap_split, is.europe)

notEurope <- discard(gap_split, is.europe)

# Joining

uk <- pluck(gap_split, "United Kingdom")

updatedGap <- prepend(gap_split, values = list(UK = uk))
View(updatedGap)

# Merging lists
# purrr way
?list_merge
# Base R way 
?modifyList()

# Transposing / Inverting

myList <- list(firstname = list("Doug", "Gary"),
               lastname = list("Ashton", "Linekar"))

myTransposedList <- transpose(myList)
View(myTransposedList)

gap_inverted <- transpose(gap_split)
names(gap_inverted)

gap_inverted[["year"]]

# Exercise page 3-4
# 
# 1. Write a function to test if the life expentancy for the most recent year is
#   the maximum life expectancy. The function should return TRUE (when
#   life expectancy in 2007 is the maximum) or FALSE.
#
data <- pluck(gap_split, "Botswana")

life07 <- data$lifeExp[nrow(data)]
maxlife <- max(data$lifeExp[nrow(data)])

life07 == maxlife

maxLife07 <- function(data) {
  life07 <- data$lifeExp[nrow(data)]
  maxlife <- max(data$lifeExp)
  
  life07 == maxlife
}


# 2. Test your function on the data for Botswana and the data for Denmark.

maxLife07(gap_split[["Botswana"]])
maxLife07(gap_split[["Denmark"]])


# 3. Filter the split gapminder data to return only elements where the life
#   expectancy in 2007 is not it's highest life expectancy.

peakedLife <- discard(gap_split, maxLife07)

# Extension Questions
# 4. Use appropriate map functions to return the maximum life expectancy
#   for each of these countries and their life expectancy in 2007


# Chapter 4 ---------------------------------------------------------------


?map2

means <- rep(0:5, each = 2)
means <- set_names(means, nm = LETTERS[1:12])

sds <- rep(c(1,2), times = 6)

normData <- map2_df(means, sds, rnorm, n = 100)
View(normData)

gather(normData, Simulation, Value) %>%
  qplot(Value, data = ., geom = "density", group = Simulation)

?pmap



# Chapter 5 ---------------------------------------------------------------

# Nested Data

gap_nested

gap_nested %>%
  filter(country == "United Kingdom") %>%
  select(data) %>%
  unnest()


gap_simple %>%
  group_by(country, continent) %>%
  nest()


# Mutate and map together

map(gap_nested$data, ~max(.$lifeExp))

gap_nested %>%
  mutate(MaxLife = map_dbl(data, ~max(.$lifeExp)))


# Exercise 
# 1. Using the nested gapminder data:
#   a. Find the minimum value of the population for each
#     country


# b. Calculate the variance of the GDP per capita


gap_nested %>%
  mutate(MinPop = map_dbl(data, ~min(.$pop))) %>%
  mutate(GdpVar = map_dbl(data, ~var(.$gdpPercap))) %>%
  View()


ukModel <- lm(lifeExp ~ year, data = gap_split[["United Kingdom"]])


gap_model <- gap_nested %>%
  mutate(model = map(data, ~lm(lifeExp ~ year, data = .)))

View(gap_model)


library(broom)
glance(ukModel) %>% View()


gap_model %>%
  mutate(model_results = map(model, glance)) %>%
  select(country, model_results) %>%
  unnest() %>% View()
