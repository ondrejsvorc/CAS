setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

bike_sharing <- read.csv("bike_sharing.csv", nrows = 2000)
str(bike_sharing)
head(bike_sharing)