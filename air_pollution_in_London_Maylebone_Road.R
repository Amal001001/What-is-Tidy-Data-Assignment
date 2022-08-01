#----- GROUP Assignments-------
# Assignment 4: What is Tidy Data Assignment. 

# this scripts uses the data from the Automatic Urban and Rural Network (AURN) in London Marylebone Road
# from the openair package, and goes through the steps of tidying and preparing the dataset.
# Then, plotting the data to get insights on the effects of air pollution in this area.
# 
# Authors: NOUF ALJOHANI, Amal Almutairi, Salha Nasser, Rawan Alsudias, & Rahaf Alzaharani
# Last Update : Wed, 20 Jul, 2022, 
#------------------------------------------------------------------------------------------------


# Load and import all needed packages.

# You need to install Rtools package. 
install.packages("devtools")
library(devtools)
require(devtools)
install_github('davidcarslaw/openair') # install the dataset
install.packages("openair")
library(openair) # use the package.

# package for cleaning and tidying the data.
install.packages("tidyverse")
library(tidyverse)

# THIS SHOWS YOU ALL THE AVAILABLE DATASETS in this package.
?openair
library(help = "openair")

?importAURN #to get all the arguments of the dataset



# LOAD DATA and assign it to df_original
# here the chosen data are from 2020-2022

df_original <- importAURN(year= 2020:2022)
View(df_original)


# Exploring Dataset:


# function that helps in finding all missing data (NA).
# it returns all columns with the count of missing values.
missing_values <- function(df) {
  missing_val <- c()
  for (c in colnames(df)) {
    missing <- sum(is.na(df[[c]]))
    missing_val <- c(missing_val, missing)
  }
  tibble(colnames(df), missing_val)
} #End of function


# check the dataset, and get information

summary(df_original) # summary of dataset

missing_values(df_original) # get counts of missing values

#------End of Exploring Dataset------



# Tidying and Cleaning Dataset:

#----STEP 1 -----#
# Remove  all columns where number of missing values is more than 7000
# Remove all useless columns.
df<-select(df_original, -c(site, code, v10, nv10, v2.5, nv2.5))

# check if the changes are reflected. 
View(df)

#----STEP 2 -----#

# fill the missing values where the missing values are less than 1000 (since it is a small number)
df <- df %>% fill(c(nox, no2, no, wd, ws, air_temp,so2)
, .direction = "up") # fill up.

# check the counts of missing values after the filling.
missing_values(df)

#----STEP 3 -----#
# drop all columns with missing values to avoid issues when plotting the data.
df <- df %>% drop_na()

missing_values(df) # now that all counts of missing values should be ZERO! 

#----STEP 4 -----#
# add columns to spread the date column into year, month, and day.
library(lubridate)
df$year <- year(ymd_hms(df$date))
df$month <- month(ymd_hms(df$date)) 
df$day <- day(ymd_hms(df$date))
df
View(df)


#--------End of Tidying-----------#



# Plotting Data to show different Insights:

#----Graph 1 & 2-----#
# Calendar Plot for year 2020 and 2021 that shows the SO2 level per day.

options(repr.plot.width=5, repr.plot.height=4)
calendarPlot(df, pollutant = "so2", year=2020,main = "SO2 Level per Day (2020)",key.header = "PPB")

options(repr.plot.width=5, repr.plot.height=4)
calendarPlot(df, pollutant = "so2", year=2021,main = "SO2 Level per Day (2021)",key.header = "PPB")

#----Graph 3-----#

# Time plot to show the normalized levels for CO and SO2 from 2020-2022. 
options(repr.plot.width=8, repr.plot.height=3) 
timePlot(df, pollutant = c("co", "so2"), avg.time = "year",normalise = "mean",
          lwd = 2, lty = 1, group = TRUE, main=" Line Plot for CO & SO2 (2020-2022)",ylab="Normalised Level (PPB)")

#----Graph 4-----#
# Time plot to show the normalized levels for NO and NO2 from 2020-2022.
options(repr.plot.width=8, repr.plot.height=3) 
timePlot(df, pollutant = c("no","no2"), avg.time = "year",normalise = "mean",
         lwd = 2, lty = 1, group = TRUE, main=" Line Plot for NO & NO2 (2020-2022)",ylab="Normalised Level (PPB)")

#----Graph 5-----#
# Time plot to show the normalized levels for pm2.5 & pm10  from 2020-2022.
options(repr.plot.width=8, repr.plot.height=3) 
timePlot(df, pollutant = c("pm2.5","pm10"), avg.time = "year",normalise = "mean",
         lwd = 2, lty = 1, group = TRUE, main=" Line Plot for pm2.5 & pm10 (2020-2022)",ylab="Normalised Level (ug'/'m3)")


#----------End of Script----------#



#