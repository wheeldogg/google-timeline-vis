# Description: An analysis of Google timeline data in R.
# @Author: SW
# date : 21-07-2018

# load packages
# load packages
library(dplyr)
library(jsonlite)
library(rJava)
library(RMongo)

####
# Set the start and end times to query between.
startdate <- 20180101
enddate   <- 20180110
startts  <- "1532168407867"
endts    <- "1532168407867"

as.numeric(as.POSIXct(startdate))
# Solution using your own example

a <- 1383293400000

# b <- as.POSIXct((a)/1000, origin = "1970-01-01")
# b
# b <- strftime(b, format="%Y-%m-%d %H:%M") #"2013-11-01 09:10"
# outputs
# > as.numeric(as.POSIXct(b))*1000 == a
# [1] TRUE
#   
"timestampMs" : "1532168407867"
# Connet to mongo, alternatively # raw <- fromJSON('/Users/ubuntu/location_small.json')
mongo    <- mongoDbConnect('google')
collection <- 'full'

# Query data to retrieve between two dates.
x <- dbGetQuery(mongo, collection, '{}', skip = 0 , limit = 10)
x <- dbGetQuery(mongo, collection, '{timestampMs: "1532168407867"}', skip = 0 , limit = 10)

x <- dbGetQuery(mongo, collection, '{timestampMs: {$gte : "1532168407867"}}', skip = 0 , limit = 1000)
x <- dbGetQuery(mongo, collection, sprintf('{timestampMs: {$gte : "%s"}}', startts, endts), skip = 0 , limit = 1000)
head(x)

x <- dbGetQuery(mongo, collection, sprintf('{"$and": [ {"timestampMs" : {"$gte" : "%s"}}, {"timestampMs" : {"$lte" : "%s"}}]}',
                                           startts, endts), skip = 0, limit = 1000)


x <- dbGetQuery(mongo, collection, sprintf('{"$and": [ {"timestampMs" : {"$gte" : %s}}]}',
                                           startts, endts), skip = 0, limit = 1000)
head(x)

dbGetQuery(mongo, collection, sprintf('{"$and": [ {"timestampMs" : {"$gte" : %s}}, {"ts_unix" : {"$lte" : %s}}]}',
                                      start_date, end_date), skip = intervalprices, limit = count_prices_timeframe)
head(x)
head(x$timestampMs)


# x <- dbGetQuery(mongo, collection, '{latitudeE7: 521034468}', skip = 0 , limit = 10)

# this query works in mongodb console
db.full.findOne({timestampMs : "1532168407867"})

x <- dbGetQuery(mongo, collection, sprintf('{}'), limit=10)




# Return all results.
result <- dbGetQuery(mongo, collection, query, skip=0, limit=Inf)
nrow(y)

nrow(x)
# returns as a dataframe.

class(x)
class(x$activity)
limit=Inf
head(x)
nrow(x)
# dbGetQuery(mg1, collectionNameEddie, sprintf('{"$and": [ { "r_id": %s }, {"ts_unix" : {"$gte" : %s}}, {"ts_unix" : {"$lte" : %s}}]}', 
#                                              r_id, start_date, end_date), skip = intervalprices, limit = count_prices_timeframe)  
cursor <- mongo.find(mongo, namespace, query= queryjson, fields=fieldjson, sort=sortjson, limit = 1)