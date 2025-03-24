# --------------------------------------------
# Script Name: Data retriever and databases
# Purpose: My class mainly focuses on exploring the Doubs River 
#          dataset. This script is to show how to get data from 
#          free public databases and save a database of SQlite 
#          or postgresql.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2024-03-2
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

#####################################################
# 01-import data
#####################################################

# A) download.file() or read_csv() from websites
# https://www.davidzeleny.net/anadat-r/doku.php/en:data:doubs

# Set the base URL for the datasets

base_url <- "https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/"

datasets <- c("DoubsSpe.csv","DoubsEnv.csv","DoubsSpa.csv")  # List of datasets 

# Download each dataset
for(dataset in datasets) {
  full_url <- paste0(base_url, dataset) # full URL of files
  dest_file <- file.path("data/rbasic_data", dataset) # the destination
  download.file(full_url, destfile = dest_file, mode = "wb") # Download
  
  cat("Downloaded:", dataset, "\n") # Print a message for complete
}

# if getting an error, check DNS (sudo vim /etc/resolv.conf)

# B) using rdataretriever to download data from databases

# Install rdataretriever in python environment 
# https://github.com/ropensci/rdataretriever
# https://rstudio.github.io/reticulate/

# install.packages('reticulate') # interface to Python
library(reticulate)
py_config()
# $pip install retriever # Install retriever package
# install.packages('rdataretriever') # install rdataretriever
library(rdataretriever)
get_updates() # Update the available datasets
# List the datasets available via the Retriever
datasets()

# install_csv('portal') # Install csv portal
download('portal', 'data/data_db/portal') # [219 dataset]
portal = fetch('portal')
names(portal)
head(portal$species)

plot <- read.csv("data/data_db/portal/3299474")
species <- read.csv("data/data_db/portal/3299483")
survey <- read.csv("data/data_db/portal/5603981")
library(tidyverse)
glimpse(survey)

# download('harvard-forest', 'data/data_db') # vector [162]
# unzip("data/data_db/hf110-01-gis.zip")
# 
# library(sf)
# sf <- st_read(unzip("data/data_db/hf110-01-gis.zip", #read .shp into R
#                     "Harvard_Forest_Properties_GIS_Layers/stands_1937.shp"))
# sf #view data 

# C) loading data from R package

data() # check built-in R
data(doubs, package = "ade4")

DoubsEnv <- readr::read_csv("data/rbasic_data/DoubsEnv.csv")
DoubsEnv

# Checking dataset

str(DoubsEnv)
tibble::glimpse(DoubsEnv)

#####################################################
# 02-Working on the SQLite with R
#####################################################
# A) working with RStudio Connections Pane

# https://www.youtube.com/watch?v=id0GX4sXnyI
# https://www.youtube.com/watch?v=0euy9b3CjuY
# https://staff.washington.edu/phurvitz/r_sql/

# // setup sqlite for rstudio's connections
# apt install unixodbc 
# apt install sqliteodbd
# vim /etc/odbcinst.ini
# vim /etc/odbc.ini

# B) working with RStudio codes (dplyr)

# several packages: DBI, RSQLite, tidyverse, dplyr, dbplyr
# https://rdbsql.rsquaredacademy.com/dbplyr
# https://datacarpentry.org/R-ecology-lesson/05-r-and-databases.html
# download from https://github.com/weecology/portal-teachingdb/blob/master/"

library(DBI)
doubs <- DBI::dbConnect(RSQLite::SQLite(), # create or connect
                        "data/data_db/doubs.sqlite")
dbListTables(doubs)
dbListFields(doubs, "DoubsEnv")

# creating tables and inserting data by dbplyr
library(dbplyr)
dbplyr::src_dbi(doubs) # view the database
env <- dplyr::tbl(doubs, "DoubsEnv") # Querying table
head(env)
library(tidyverse)
env_clean <- env %>%
  select(-field1) %>%
  collect() # load into the R session

dbDisconnect(doubs)
doubs

# C) import data to a sqlite database

library(tidyverse) # for the read_csv()
SPE <- read_csv("data/data_db/DoubsSpe.csv")
ENV <- read_csv("data/data_db/DoubsEnv.csv")
SPA <- read_csv("data/data_db/DoubsSpa.csv")

# create an empty SQLite database
library(dplyr) # link to a database by src_sqlite()
my_db_file <- "resutls/DOUBS.sqlite"
my_db <- src_sqlite("results/my_db_file", create = TRUE)
my_db

# copy the existing data.frames into the empty database
copy_to(my_db, SPE, temporary = FALSE)
copy_to(my_db, ENV, temporary = FALSE)
copy_to(my_db, SPA, temporary = FALSE)

dbDisconnect(my_db$con)
my_db

##################################################
# 01- Get data from an URL or a repository
##################################################

# A) using download.file() or read.csv()
# https://www.davidzeleny.net/anadat-r/doku.php/en:data:doubs
# Set the base URL for the datasets

base_url <- "https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/"

datasets <- c("DoubsSpe.csv","DoubsEnv.csv","DoubsSpa.csv") # List of datasets 

# Download each dataset
for(dataset in datasets) {
  full_url <- paste0(base_url, dataset) # a full URL for dataset
  dest_file <- file.path("data/data_db", dataset) # specify destination
  download.file(full_url, destfile = dest_file, mode = "wb") # Download

  cat("Downloaded:", dataset, "\n") # Print a message after complete
}

# if getting an error, check DNS (sudo vim /etc/resolv.conf)

# download.file(URL, destfile = "data/data_db/download_data.csv", 
#               method="curl")
# download.file(URL, destfile = "data/data_db/download_data.csv", 
#               method="libcurl")

# using read.csv() or read.table()

readcsv_data <- read.csv(full_url)
head(readcsv_data)
readtbl_data <- read.table(full_url)
saveRDS(object = readcsv_data, file = "data/data_db/readcsv_data.RDS") 
# readRDS("data/data_db/readcsv_data.RDS")

# using the RCurl package
library(RCurl)
get_url <- RCurl::getURL(full_url)
url_data <- read.csv(textConnection(get_url))
url_data

# self-defined function for automation
autoget_data <- function(url) { # may add the_sep = "\t"
  url <- getURL(url) 
  data <- read.csv(textConnection(url)) # may add sep = the_sep
  return(data)
}

autoget_data(URL)

spe <- read.csv("data/data_db/DoubsSpe.csv",
                row.names = 1) # remove X col using row.names

# B) using API to access data
## https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/API-data-access-r/

## accessing non-spatial data from web api

library(ggmap)
library(ggplot2)
library(dplyr)
library(rjson) # getJSON() to import data from api
library(jsonlite)
library(RCurl)

# create a API request (url)

base_url = "https://data.colorado.gov/resource/tv8u-hswn.json?"# Base URL path
full_url = paste0(base_url, "county=Boulder", # other arguments
                  "&$where=age between 20 and 40",
                  "&$select=year,age,femalepopulation")
full_url

# using fromJSON (full_url) and getting errors

# pop_proj_data_df <- rjson::fromJSON(full_url) # Convert JSON to data frame
# pop_proj_data_df <- RJSONIO::fromJSON(full_url)
# pop_proj_data_df <- jsonlite::fromJSON(full_url)

# encoding URL with characters and using fromJSON() to get
full_url_encoded <- URLencode(full_url)
full_url_encoded
pop_proj_data_df <- jsonlite::fromJSON(full_url_encoded)
head(pop_proj_data_df)

# pop_proj_data_df1 <- rjson::fromJSON(RCurl::getURL(full_url_encoded)) 
# head(pop_proj_data_df1, n = 2)
# pop_proj_df_convert1 <- do.call(rbind.data.frame, pop_proj_data_df1)
# pop_proj_df_convert1 <- do.call(rbind.data.frame, pop_proj_data_df1)

# pop_proj_data_df2 <- RJSONIO::fromJSON(full_url_encoded)
# head(pop_proj_data_df2, n = 2)

# turn columns to numeric 
pop_proj_data_df <- pop_proj_data_df %>%
  mutate_at(c( "age", "year", "femalepopulation"), as.numeric)
str(pop_proj_data_df)

# plot the data
ggplot(pop_proj_data_df, aes(x = year, y = femalepopulation,
                             group = factor(age), color = age)) + geom_line() +
  labs(x = "Year",
       y = "Female Population - Age 20-40",
       title = "Projected Female Population",
       subtitle = "Boulder, CO: 1990 - 2040")

## Accessing Geodata from web API
# https://kdvdecisions-blog.netlify.app/2020/04/18/obtaining-spatial-data-from-esri-rest-apis-in-r/

library(sf)
library(leaflet)
library(geojsonsf)
library(dplyr)
library(urltools)

# queried an Esri REST API and copy from browser

# navigate to Esri REST APIs → EDW/EDW_ForestSystemBoundaries_01
# → Administrative Forest Boundaries - National Extent (O) → Query
# →  HTML describing →  API

forest <- 
  geojson_sf("https://apps.fs.usda.gov/arcx/rest/services/EDW/EDW_ForestSystemBoundaries_01/MapServer/0/query?where=FORESTNAME+LIKE+%27%25Tahoe+National%25%27&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&having=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&queryByDistance=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson")

# take a look at the data

leaflet() %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addPolygons(data=forest, weight=2, color="blue")

target_link <- "https://commons.wikimedia.org/wiki/Data:Doubs_River.map"
arget_page <- read_html(target_link)

# C) using rdataretriever for databases

# Install rdataretriever in python environment 
# https://github.com/ropensci/rdataretriever
# https://rstudio.github.io/reticulate/

# install.packages('reticulate') # interface to Python
library(reticulate)
py_config()
# $pip install retriever # Install retriever package
# install.packages('rdataretriever') # install rdataretriever
library(rdataretriever)
get_updates() # Update the available datasets
# List the datasets available via the Retriever
datasets()

# install_csv('portal') # Install csv portal
download('portal', 'data/DBdata/portal') # .csv data
portal = fetch('portal')
names(portal)
head(portal$species)

download('harvard-forest', 'data/DBdata') # vector data


#####################################################
# 02-Working on the SQLite with R
#####################################################
# A) working with RStudio Connections Pane

# https://www.youtube.com/watch?v=id0GX4sXnyI
# https://www.youtube.com/watch?v=0euy9b3CjuY
# https://staff.washington.edu/phurvitz/r_sql/

# // setup sqlite for rstudio's connections
# apt install unixodbc 
# apt install sqliteodbd
# vim /etc/odbcinst.ini
# vim /etc/odbc.ini

# B) working with RStudio codes (dplyr)

# several packages: DBI, RSQLite, tidyverse, dplyr, dbplyr
# https://rdbsql.rsquaredacademy.com/dbplyr
# https://datacarpentry.org/R-ecology-lesson/05-r-and-databases.html
# download from https://github.com/weecology/portal-teachingdb/blob/master/"

library(DBI)
doubs <- DBI::dbConnect(RSQLite::SQLite(), # create or connect
                          "data/data_db/doubs.sqlite")
dbListTables(doubs)
dbListFields(doubs, "DoubsEnv")

# creating tables and inserting data by dbplyr
library(dbplyr)
dbplyr::src_dbi(doubs) # view the database
env <- dplyr::tbl(doubs, "DoubsEnv") # Querying table
head(env)
library(tidyverse)
env_clean <- env %>%
  select(-field1) %>%
  collect() # load into the R session

dbDisconnect(doubs)
doubs

# C) import data to a sqlite database

library(tidyverse) # for the read_csv()
SPE <- read_csv("data/data_db/DoubsSpe.csv")
ENV <- read_csv("data/data_db/DoubsEnv.csv")
SPA <- read_csv("data/data_db/DoubsSpa.csv")

# create an empty SQLite database
library(dplyr) # link to a database by src_sqlite()
my_db_file <- "resutls/DOUBS.sqlite"
my_db <- src_sqlite("results/my_db_file", create = TRUE)
my_db

# copy the existing data.frames into the empty database
copy_to(my_db, SPE, temporary = FALSE)
copy_to(my_db, ENV, temporary = FALSE)
copy_to(my_db, SPA, temporary = FALSE)

dbDisconnect(my_db$con)
my_db

#################################################
# 03-Working on PostgreSQL with R
#################################################
## A) Install PostgreSQL on local computer 

# Install and configure postgresql by following
# https://www.youtube.com/watch?v=OxIQ_xJ-yzI

# For ubuntu 22.04, install a default postgresql version by following the site 
# https://www.rosehosting.com/blog/how-to-install-postgresql-on-ubuntu-22-04/
# 
# Verify the installation
# $ dpkg --status postgresql
# $ whereis postgresql
# $ which psql # psql is an interactive PostgreSQL client
# $ ll /usr/bin/psql
# $ psql -V # check postgresql version

# Configure the postgresql
# Including client authentication methods,connecting to 
# PostgreSQL server, authenticating with users, etc. see
# https://ubuntu.com/server/docs/databases-postgresql

# Create a database and enable PostGIS extension
# https://staff.washington.edu/phurvitz/r_sql/

# CREATE EXTENSION postgis;
# CREATE EXTENSION postgis_topology;

# B) create a connect to PostgreSQL

library(DBI)
library(RPostgreSQL)
ecodata <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), # connecting
                          dbname = 'ecodata', 
                          host = 'localhost', 
                          port = 5432, 
                          user = 'ecosci', 
                          password = 'ecosci')
?install_postgres # Install datasets via Retriever
rdataretriever::install_postgres(dataset = "portal", # .csv file
                                 host = "localhost",
                                 port = "5432", 
                                 database = "ecodata",
                                 database_name = "census",
                                 user = "ecosci", 
                                 password = "ecosci")

rdataretriever::install_postgres(dataset = 'harvard-forest', # Vector data
                                 host= "localhost",
                                 port = "5432",
                                 database = "ecodata", 
                                 database_name = "postgis",
                                 user = "ecosci", 
                                 password = "ecosci")

rdataretriever::install_postgres(dataset = 'bioclim',
                                 host= "localhost",
                                 port = '5432',
                                 database = "ecodata", 
                                 database_name = "postgis",
                                 user = "ecosci", 
                                 password = "ecosci") # Raster data
dbDisconnect(ecodata)
dbGetInfo(ecodata)

# C) connect to postgresql and save to specific schemas

# Connect to postgresql
# using connection code
rm(list = ls())
library(DBI)
library(RPostgreSQL)
rsql <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), # connecting
                       dbname = 'rsql', 
                       host = 'localhost', 
                       port = 5432, 
                       user = 'csde', 
                       password = 'gis')
rsql
dbGetInfo(rsql)
dbExistsTable(rsql, c("rawdata")) # Checking if a table exists

# Upload data to specific schema
ah <- readstata13::read.dta13("data/21600-0001-Data.dta", # read the file
                              convert.factors = FALSE)
colnames(ah) <- tolower(colnames(ah)) # lowercase column names
ah <- ah %>% 
  mutate_at(., -1, as.integer) # convert to integers except 1st column
# make long
ah1 <- ah %>% 
  pivot_longer(-aid, names_to = "varname", values_to = "val")

dbWriteTable(conn = rsql, 
             name = c("addhealth", "data_21600_0001"), 
             value = ah1, 
             row.names = FALSE, 
             overwrite = TRUE)

dbGetQuery(rsql,# List tables in the schema
           "SELECT table_name FROM information_schema.tables
                   WHERE table_schema='addhealth'")

dbListFields(rsql, c("addhealth", 
                     "data_21600_0001")) # List fields of the table

dbDisconnect(rsql)
dbGetInfo(rsql)

# D) Connect to PostgreSQL as an ODBC Data Source
library(odbc)
# //open ubuntu terminal to edit /etc/odbc.ini like this
# [ecodata]
# Driver = CData ODBC Driver for PostgreSQL
# Description = My Description
# User = postgres
# Password = admin
# Database = postgres
# Server = 127.0.0.1
# Port = 5432

conn <- odbcConnect("ecodata")
library(RODBC)
library(dplyr)
library(dbplyr)
sqlTables(conn)
odbcClose(conn)
odbcCloseAll()

# run the "\copy" to upload data

gps <- read.csv("data/gpslogger.csv", 
                as.is = TRUE) # as string rather than a factor
head(gps)
gps$ageofdgpsdata <- gps$dgpsid <- gps$activity <- gps$annotation <- NULL
head(gps)
write.csv(x = gps, file = "data/gpslogger_dropped_cols.csv", row.names = FALSE, na = "")
# an SQL query to define the table
sql <- "
    --drop the table if it exists
    drop table if exists gps.tracks;
    --create the table
    create table gps.tracks
    (fname text
        , time_gps timestamptz
        , calendar_date date
        , lat float
        , lon float
        , elevation float
        , accuracy float
        , bearing float
        , speed float
        , satellites integer
        , provider text
        , hdop float
        , vdop float
        , pdop float
        , geoidheight float
        , battery integer);"

res <- dbGetQuery(conn = rsql, 
                  statement = sql) # run the query to make the table

# \copy table_schema.table_name from 'systemfile.csv' with csv header
mycmd <- "psql -U csde -c \"\\copy gps.tracks from 'data/gpslogger_dropped_cols.csv' with csv header\" rsql"
system(mycmd)
dbGetQuery(conn = rsql, statement = "select count(*) from gps.tracks")

