# Script that produces a data file of the ISO currency codes and Datastream equivalents


currencyDS2ISO <- read.csv("./data-raw/currencyDS2ISO.csv", encoding = "UTF-8")
require(devtools)
devtools::use_data(currencyDS2ISO, overwrite = TRUE)