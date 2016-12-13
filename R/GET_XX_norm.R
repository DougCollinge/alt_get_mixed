####################################
### For context it is helpful to ###
### plot the profiles
####################################

## Load in libraries
library(tidyverse)

## Name the data file we want in
filename <- "97130187.t11" 

## Load in the data 
df <- read.table(filename, 
                 col.names = c("depth","temper","salinity","unk-var1","unk-var2","density"))


approx(y=df$depth, x=df$temper, method= "linear")



