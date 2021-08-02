# basic operators  <-- also note # for comments

2 + 2

10 - 7

6 * 3

5/2

2^5 # or 2**5

# assignment

x <- 2 + 3
x

# vectors

y <- c(1, 2, 3, 4, 5)
y

z <- seq(0, 1, by = .1)
z

## operating on vectors returns vectors
y + 5 

y < 4 

## indexing vectors

y[1] # 1-indexed

z[2:5] # grabbing a slice, from indices 2 to 5

y[y<4] # get elements of y less than 4

y[y%%2==1] # get odd elements of y

# functions

mean(z)

sd(z)
length(z)

sqrt(11)

## defining new functions

se <- function(x){sd(x)/sqrt(length(x))}

se(z)

## loading packages with library()

library(tidyverse) # ggplot2, readr, dplyr, etc.

## loading data

df <- read_csv("stroop-2014.csv")

## inspecting data

df

head(df)

glimpse(df) # dplyr

View(df)

## plotting

ggplot(df, aes(x = congruent, y = incongruent)) + 
  geom_point() +
  geom_smooth(method="lm", alpha=.2) +
  theme_bw()

## debugging warnings/errors

sqrt(-1) # Warning Message: NaNs produced

Head(df) # Error: could not find function "Head"

z - mean # Error: non-numeric argument to binary operator

data$congruent # Error: object of type 'closure' is not subsettable

### --------------

## some tricky stuff (the sharp bits)

mean(c(1,2,3,4,5,NA)) # returns NA

y[6] # returns NA (there is nothing in the 6th index)

sqrt(2)
print(sqrt(2), digits=20) # wikipedia says 1.4142135623730950488…

2 - sqrt(2)^2 # = 0?

my_float <- 2 - sqrt(2)^2

my_float == 0 # FALSE
all.equal(my_float, 0) # TRUE

