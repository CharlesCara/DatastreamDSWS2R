## code to prepare `currencyDS2ISO` dataset goes here

# Script that produces a data file of the ISO currency codes and Datastream equivalents
currencyDS2ISO <- read.csv("./data-raw/currencyDS2ISO.csv", encoding = "utf-8")
currencyDS2ISO$dsCode  <- iconv(currencyDS2ISO$dsCode, from="utf-8", to = "latin1")



usethis::use_data(currencyDS2ISO, overwrite = TRUE)
