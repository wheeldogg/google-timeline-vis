# Description: An analysis of Google timeline data in R.
# @Author: SW
# date : 21-07-2018

# load packages
# if (packageVersion("devtools") < 1.6) {
  # install.packages("devtools")
# }
# devtools::install_github("hadley/lazyeval")
# devtools::install_github("hadley/dplyr")
install.packages('dplyr')
library(dplyr)
library(jsonlite)
library(rJava)
library(RMongo)
library(rmarkdown)
install.packages('rmarkdown')
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("shiny", "rstudio")

####
# Set the start and end times to query between.
startdate <- as.Date("2018-01-01")
enddate   <- as.Date("2018-01-10")
startts   <- as.character(as.numeric(startdate) * 86400000)
endts     <- as.character(as.numeric(enddate) * 86400000)

# Setup mongo connection
mongo    <- mongoDbConnect('google')
collection <- 'full'

# Query data to retreive between two dates.
x <- dbGetQuery(mongo, collection, sprintf('{"$and": [ {"timestampMs" : {"$gte" : "%s"}}, 
                                                       {"timestampMs" : {"$lte" : "%s"}}]}', startts, endts), skip = 0, limit = Inf)
