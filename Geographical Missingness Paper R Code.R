
# Compiled code for 'Geographical Missingness' Journal Paper
# Written by Dominick Sutton
# University of Glasgow Geospatial Data Science Unit
# Code originally created in separate thematic files
# File created 1 July 2024

###  NB: Owing to data licensing conditions 
###  the data used in this analysis cannot be shared.  
###  Hence, the uploading scripts are blank.


# Loading Necessary packages

library(tidyverse)
library(sf)
library(mapview)
library(tmap)
library(rgdal)
library(spdep)
library(mltools)
library(spgwr)


#############################

# Set working directory

setwd()



# Import exploratory data analysis cut

dataEDA <- read_csv()

summary(dataEDA)
head(dataEDA)

# Changing gender D1 data to numeric
dataEDA1a <- dataEDA %>%
  mutate(D1=case_when(
    D1 == "Female" ~ 1,
    D1 == "Male" ~ 2,
    D1 == "Prefer not to say" ~ 3,
    D1 == "Prefer to self-define (write in)" ~ 4
  ))

head(dataEDA1a)

count(dataEDA1a, D1)

# Changing marital status to numeric

dataEDA1b <- dataEDA1a %>%
  mutate(D5=case_when(
    D5 == "Divorced" ~ 1,
    D5 == "Formerly in a civil partnership which is now legally dissolved" ~ 2,
    D5 == "Prefer not to say" ~ 3,
    D5 == "Don't know" ~ 4,
    D5 == "In a registered civil partnership" ~ 5,
    D5 == "Married" ~ 6,    
    D5 == "Separated, but still legally in a civil partnership" ~ 7,
    D5 == "Separated, but still legally married" ~ 8,
    D5 == "Single, that is never married and never registered in a civil partnership" ~ 9,
    D5 == "Surviving partner from a civil partnership" ~ 10,
    D5 == "Widowed" ~ 11
  ))

summary(dataEDA1b)
head(dataEDA1b)

count(dataEDA1b, D5)

# Changing working status to numeric

dataEDA1c <- dataEDA1b %>%
  mutate(D10=case_when(
    D10 == "Agency work/Zero hours contract" ~ 1,
    D10 == "Carer" ~ 2,
    D10 == "Don't know" ~ 3,
    D10 == "Employed, but hours not stated" ~ 4, # ' - include all mat leave/sick with unstated hour' removed
    D10 == "Looking after the home" ~ 5,
    D10 == "Permanently sick/ disabled" ~ 6,    
    D10 == "Retired" ~ 7,
    D10 == "Self-employed full-time (30 or more hours per week)" ~ 8,
    D10 == "Self-employed part-time (less than 30 hours per week)" ~ 9,
    D10 == "Semi-retired (drawing a pension or other income but still working)" ~ 10,
    D10 == "Student" ~ 11,
    D10 == "Temporarily sick (no job to go to)" ~ 12,
    D10 == "Unemployed and looking for work" ~ 13,
    D10 == "Unemployed and not looking for work" ~ 14,
    D10 == "Voluntary work" ~ 15,
    D10 == "Working for an employer(s) full-time (30 or more hours per week)" ~ 16,
    D10 == "Working for an employer(s) part-time (less than 30 hours per week)" ~ 17
  ))

summary(dataEDA1c)
head(dataEDA1c)

count(dataEDA1c, D10)

# Changing 'currently living in' to numeric

dataEDA1d <- dataEDA1c %>%
  mutate(D13d=case_when(
    D13d == "Detached house" ~ 1,
    D13d == "Don't know" ~ 2,
    D13d == "Flat or maisonette"~ 3,
    D13d == "Semi-detached house" ~ 4, 
    D13d == "Terraced house" ~ 5
  ))

summary(dataEDA1d)
head(dataEDA1d)

count(dataEDA1d, D13d)

# Changing working status to numeric

dataEDA1e <- dataEDA1d %>%
  mutate(D30=case_when(
    D30 == "A level, vocational level 3 and equivalents" ~ 1,
    D30 == "Degree or degree equivalent" ~ 2,
    D30 == "Higher degree" ~ 3,
    D30 == "O level/ GCSE Grades 4-9/A*-C, vocational level 2 and equivalents" ~ 4, 
    D30 == "Other Higher Education below degree level" ~ 5,
    D30 == "Other qualifications including overseas" ~ 6,    
    D30 == "Qualifications at level 1 and below" ~ 7,
    D30 == "Trade Apprenticeships" ~ 8,
    D30 == "Not disclosed" ~ 9
  ))

summary(dataEDA1e)
head(dataEDA1e)

count(dataEDA1e, D30)

# Changing 'L/term illness' to numeric

dataEDA1f <- dataEDA1e %>%
  mutate(D33=case_when(
    D33 == "Don't know" ~ 1,
    D33 == "No" ~ 2,
    D33 == "Prefer not to say"~ 3,
    D33 == "Yes" ~ 4
  ))

summary(dataEDA1f)
head(dataEDA1f)

count(dataEDA1f, D33)

# Changing ethnicity to numeric

dataEDA1g <- dataEDA1f %>%
  mutate(D22=case_when(
    D22 == "African" ~ 1,
    D22 == "Any other Asian/ Asian British background" ~ 2,
    D22 == "Any other Black/ Black British background" ~ 3,
    D22 == "Any other ethnic group" ~ 4, 
    D22 == "Any other mixed/ multiple ethnic background" ~ 5,
    D22 == "Any other white background" ~ 6,    
    D22 == "Arab" ~ 7,
    D22 == "Bangladeshi" ~ 8,
    D22 == "Caribbean" ~ 9,
    D22 == "Chinese" ~ 10,
    D22 == "English/ Welsh/ Scottish/ Northern Irish/ British" ~ 11,
    D22 == "Gypsy or Irish Traveller" ~ 12,
    D22 == "Indian" ~ 13,
    D22 == "Irish" ~ 14,
    D22 == "Pakistani" ~ 15,
    D22 == "Prefer not to say" ~ 16,
    D22 == "White and Asian" ~ 17,
    D22 == "White and Black African" ~ 18,
    D22 == "White and Black Caribbean" ~ 19
  ))

summary(dataEDA1g)
head(dataEDA1g)

count(dataEDA1g, D22)

# Changing 'Sexuality' to numeric

dataEDA1h <- dataEDA1g %>%
  mutate(D42=case_when(
    D42 == "Bisexual" ~ 1,
    D42 == "Gay or lesbian" ~ 2,
    D42 == "Heterosexual or straight"~ 3,
    D42 == "Prefer not to say" ~ 4, 
    D42 == "Prefer to self-define (write in)" ~ 5
  ))

summary(dataEDA1h)
head(dataEDA1h)

count(dataEDA1h, D42)

# Changing annual personal income to numeric

dataEDA1_mean_calc <- na.omit(dataEDA1h %>%
                                mutate(D39=case_when(
                                  D39 == "10,000 to 14,999 pa" ~ 15000,
                                  D39 == "100,000 to 149,999 pa" ~ 125000,
                                  D39 == "15,000 to 19,999 pa" ~ 17500,
                                  D39 == "150,000 to 249,999 pa" ~ 200000,
                                  D39 == "20,000 to 29,999 pa" ~ 25000,
                                  D39 == "250,000 or more pa" ~ 250000,    
                                  D39 == "30,000 to 39,999 pa" ~ 35000,
                                  D39 == "40,000 to 49,999 pa" ~ 45000,
                                  D39 == "5,000 to 9,999 pa" ~ 7500,
                                  D39 == "50,000 to 59,999 pa" ~ 55000,
                                  D39 == "60,000 to 69,999 pa" ~ 65000,
                                  D39 == "70,000 to 99,999 pa" ~ 85000,
                                  
                                  D39 == "Less than 5,000 a year or per annum (pa)" ~ 2500,
                                  
                                )))

a <- mean(dataEDA1_mean_calc$D39)

## Substituting mean D39 (valued) for DK and PNTS values

dataEDA1i <- (dataEDA1h %>%
                mutate(D39=case_when(
                  D39 == "10,000 to 14,999 pa" ~ 15000,
                  D39 == "100,000 to 149,999 pa" ~ 125000,
                  D39 == "15,000 to 19,999 pa" ~ 17500,
                  D39 == "150,000 to 249,999 pa" ~ 200000,
                  D39 == "20,000 to 29,999 pa" ~ 25000,
                  D39 == "250,000 or more pa" ~ 250000,    
                  D39 == "30,000 to 39,999 pa" ~ 35000,
                  D39 == "40,000 to 49,999 pa" ~ 45000,
                  D39 == "5,000 to 9,999 pa" ~ 7500,
                  D39 == "50,000 to 59,999 pa" ~ 55000,
                  D39 == "60,000 to 69,999 pa" ~ 65000,
                  D39 == "70,000 to 99,999 pa" ~ 85000,
                  D39 == "Don't know"  ~ a,                             
                  D39 == "Less than 5,000 a year or per annum (pa)" ~ 2500,
                  D39 == "Prefer not to say" ~ a                              
                )))

summary(dataEDA1i)
head(dataEDA1i)
dim(dataEDA1i)



count(dataEDA1i, D39)


# Changing annual household income to numeric

dataEDA1j <- dataEDA1i %>%
  mutate(D38=case_when(
    D38 == "10,000 to 14,999 pa" ~ 1,
    D38 == "100,000 to 149,999 pa" ~ 2,
    D38 == "15,000 to 19,999 pa" ~ 3,
    D38 == "150,000 to 249,999 pa" ~ 4, 
    D38 == "20,000 to 29,999 pa" ~ 5,
    D38 == "250,000 or more pa" ~ 6,    
    D38 == "30,000 to 39,999 pa" ~ 7,
    D38 == "40,000 to 49,999 pa" ~ 8,
    D38 == "5,000 to 9,999 pa" ~ 9,
    D38 == "50,000 to 59,999 pa" ~ 10,
    D38 == "60,000 to 69,999 pa" ~ 11,
    D38 == "70,000 to 99,999 pa" ~ 12,
    D38 == "Don't know" ~ 13,
    D38 == "Less than 5,000 a year or per annum (pa)" ~ 14,
    D38 == "Prefer not to say" ~ 15
  ))

summary(dataEDA1j)
head(dataEDA1j)

count(dataEDA1j, D38)

# Changing annual household income (brackets) to numeric


## Mean substitution for DK & PNTS

dataEDA1_mean_calc_dv <- na.omit(dataEDA1j %>%
                                   mutate(D38dv=case_when(
                                     D38dv == "10,000 to 14,999" ~ 15000,
                                     D38dv == "100,000 to 149,999" ~ 125000,
                                     D38dv == "15,000 to 19,999" ~ 17500,
                                     D38dv == "150,000 to 249,999" ~ 200000,
                                     D38dv == "20,000 to 29,999" ~ 25000,
                                     D38dv == "250,000 or more" ~ 250000,    
                                     D38dv == "30,000 to 39,999" ~ 35000,
                                     D38dv == "40,000 to 49,999" ~ 45000,
                                     D38dv == "5,000 to 9,999" ~ 7500,
                                     D38dv == "50,000 to 59,999" ~ 55000,
                                     D38dv == "60,000 to 69,999" ~ 65000,
                                     D38dv == "70,000 to 99,999" ~ 85000,
                                     D38dv == "Less than 5,000" ~ 2500,
                                   )))

dataEDA1_mean_calc_dv

b <- mean(dataEDA1_mean_calc_dv$D38dv)
b

dataEDA1k <- (dataEDA1j %>%
                mutate(D38dv=case_when(
                  D38dv == "10,000 to 14,999" ~ 15000,
                  D38dv == "100,000 to 149,999" ~ 125000,
                  D38dv == "15,000 to 19,999" ~ 17500,
                  D38dv == "150,000 to 249,999" ~ 200000,
                  D38dv == "20,000 to 29,999" ~ 25000,
                  D38dv == "250,000 or more" ~ 250000,    
                  D38dv == "30,000 to 39,999" ~ 35000,
                  D38dv == "40,000 to 49,999" ~ 45000,
                  D38dv == "5,000 to 9,999" ~ 7500,
                  D38dv == "50,000 to 59,999" ~ 55000,
                  D38dv == "60,000 to 69,999" ~ 65000,
                  D38dv == "70,000 to 99,999" ~ 85000,
                  D38dv == "Don't know"  ~ b,                             
                  D38dv == "Less than 5,000" ~ 2500,
                  D38dv == "Prefer not to say" ~ b                              
                )))


summary(dataEDA1k)
head(dataEDA1k)
count(dataEDA1k, D38dv)

count(dataEDA, D2M) # Age count table


################################################################################
################################################################################

###  Mapping of initial England data

# This mapping is for data exploration and does not appear in the final paper


SerLSOA <- read_csv()
head(SerLSOA)

dataEDA1 <- left_join(dataEDA2, SerLSOA, by=c("serial" = "SERIAL")) 

head(dataEDA1)

LSOA <- read_csv()
# terms & conditions - including referencing - for using LSOA data
# NB - this data set is for England only - hence the later use of an inner join.
# NB: this data has been converted from Grid to lat/long reference

head(LSOA)
LSOA1 <- rename(LSOA, lat = 'ETRS89GD-Lat', long = 'ETRS89GD-Long' )
head(LSOA1)

dataEDA2 <- inner_join(dataEDA1, LSOA1, by=c("LSOA"="code"))
head(dataEDA2)

summary(dataEDA2)


mapview(dataEDA2, xcol="long", ycol= "lat", crs=4326, grid=FALSE,
        zcol="D39", at = c(0, 1) )

head(dataEDA2)

#--------------------------------------------------------------------------------

##############################################################
###  Adjusting missing data indicators to selected series  ###
##############################################################

### Annual Personal Income D39
dataEDA2$DK39=ifelse(dataEDA2$D39 == 13, 1, 0 )  # Don't know answers
dataEDA2$MDI_D39=ifelse(dataEDA2$D39 == 15, 1, 0 ) # Prefer not to say answers

head(dataEDA2,10)

mapview(dataEDA2, xcol="long", ycol="lat", crs=4326, grid=FALSE,
        zcol="MDI_D39", at = c(0, 1) )

mapview(dataEDA2, xcol="long", ycol= "lat", crs=4326, grid=FALSE,
        zcol="DK39", at = c(0, 1) )


### Physical or mental health condition D33
dataEDA2$DK33=ifelse(dataEDA2$D33 == 1, 1, 0 )  # Don't know answers
dataEDA2$MDI_D33=ifelse(dataEDA2$D33 == 3, 1, 0 ) # Prefer not to say answers

mapview(dataEDA2, xcol="long", ycol= "lat", crs=4326, grid=FALSE,
        zcol="MDI_D33", at = c(0, 1) )

mapview(dataEDA2, xcol="long", ycol= "lat", crs=4326, grid=FALSE,
        zcol="DK33", at = c(0, 1) )

head(dataEDA2,10)

### Annual Household income brackets D38dv
dataEDA2$DK38dv=ifelse(dataEDA2$D38dv == 13, 1, 0 )  # Don't know answers
dataEDA2$MDI_D38dv=ifelse(dataEDA2$D38dv == 15, 1, 0 ) # Prefer not to say answers

head(dataEDA2,10)

mapview(dataEDA2, xcol="long", ycol= "lat", crs=4326, grid=FALSE,
        zcol="MDI_D38dv", at = c(0, 1) )

mapview(dataEDA2, xcol="long", ycol= "lat", crs=4326, grid=FALSE,
        zcol="DK38dv", at = c(0, 1) )

### Highest qualifiation (not needing adjustment) D30

mapview(dataEDA2, xcol="long", ycol= "lat", crs=4326, grid=FALSE,
        zcol="MDI_D30", at = c(0, 1) )

# now adjusted to separate "DK" and "PNTS" for series of interest.



############################################################################
############################################################################

#################################
## MAPPING AHI
#################################

##  LSOA Mapping - THIS IS THE MAPPING USED IN PAPER
#
#####################################################

# Importing data for mapping (already formatted)

dataEDA2 <- read_csv()
head(dataEDA2)

# LSOA, THIS TIME WITH GEOMETRY

LSOA <- read_sf()
head(LSOA)

# par(mar = c(0, 0, 0, 0))
plot(st_geometry(LSOA), col = "#f2f2f2", bg = "skyblue", lwd = 0.5, border = 0)


### Annual Household income brackets D38dv
dataEDA2$DK38dv=ifelse(dataEDA2$D38dv == 13, 1, 0 )  # Don't know answers
dataEDA2$MDI_D38dv=ifelse(dataEDA2$D38dv == 15, 1, 0 ) # Prefer not to say answers

dataEDA2S <- select(dataEDA2, D38dv, MDI_D38dv, DK38dv, LSOA)

testMap <- right_join(LSOA, dataEDA2S, by=c("LSOA11CD"="LSOA"))


#####  AVERAGING ACROSS LSOA ####

head(testMap)

England <- testMap %>%         # Specify data frame
  group_by(LSOA11CD) %>%                  # Specify group indicator
  summarise_at(vars(MDI_D38dv),           # Specify column
               list('% Missing Data' = mean)) 


head(England)

mapview(England, xcol="LONG", ycol="LAT", crs=4326, grid=FALSE,
        zcol="% Missing Data", 
        at = c(0, 0.2,  0.4, 0.6, 0.8, 1))



#############################################################
### map of AHI across England in the same way as the MDI  ###
#############################################################

testMap2 <- na.omit(testMap %>%
                      mutate(D38dv=case_when(
                        D38dv == 1 ~ 15000,
                        D38dv == 2 ~ 125000,
                        D38dv == 3 ~ 17500,
                        D38dv == 4 ~ 200000, 
                        D38dv == 5 ~ 25000,
                        D38dv == 6 ~ 250000,    
                        D38dv == 7 ~ 35000,
                        D38dv == 8 ~ 45000,
                        D38dv == 9 ~ 7500,
                        D38dv == 10 ~ 55000,
                        D38dv == 11 ~ 65000,
                        D38dv == 12 ~ 85000,
                        D38dv == 14 ~ 2500,
                      )))

head(testMap2)
# NB - different "England" set to MDI chart!!!!!!!!!!!!!!!!!
England1 <- testMap2 %>%         # Specify data frame
  group_by(LSOA11CD) %>%                  # Specify group indicator
  summarise_at(vars(D38dv),           # Specify column
               list('AHI' = mean)) 

head(England1)

mapview(England1, xcol="LONG", ycol="LAT", crs=4326, grid=FALSE,
        zcol="AHI", 
        at = c(0, 10000, 30000, 50000, 100000, 3000000))





##########################################################
##   SPDEV MORAN'S I TESTS
##########################################################


##### Moran's I tests - USING SPDEP - #########

# interpretation: https://rdrr.io/cran/ape/man/MoranI.html#:~:text=If%20the%20observed%20value%20of,this%20will%20indicate%20negative%20autocorrelation.
# https://stats.oarc.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/

# selecting co-ordinates

test <- select(dataEDA2, serial, D39, MDI_D39, DK39, D30, MDI_D30, D33, MDI_D33, DK33, D38dv, MDI_D38dv, DK38dv, x, y )
head(test)
dim(test)

coords <- select(test, x, y)
head(coords)

tabsf <- st_as_sf(coords, coords=c('x', 'y'))

######################################################################
###  Identifying ideal KNN number of neighbours
######################################################################


# KNN different values

# k=5

b <- knn2nb(knearneigh(coords, k=5, longlat=TRUE, use_kd_tree = FALSE))
b

c <- nb2listwdist(b, tabsf, longlat=FALSE)

moran.test(test$DK38dv, c, randomisation = TRUE)

#############################################

# k=10

b1 <- knn2nb(knearneigh(coords, k=10, longlat=TRUE, use_kd_tree = FALSE))

c1 <- nb2listwdist(b1, tabsf, longlat=FALSE)

moran.test(test$DK38dv, c1, randomisation = TRUE)

############################################

# k=20

b2 <- knn2nb(knearneigh(coords, k=20, longlat=TRUE, use_kd_tree = FALSE))

c2 <- nb2listwdist(b2, tabsf, longlat=FALSE)

moran.test(test$DK38dv, c1, randomisation = TRUE)


###  KNN = 10 appears to be a suitable choice for this analysis  ###
####################################################################

# Generating final KNN list

# k=10

b1 <- knn2nb(knearneigh(coords, k=10, longlat=FALSE, use_kd_tree = FALSE))

c1 <- nb2listwdist(b1, tabsf, longlat=FALSE)


### Physical or mental health condition D33

#  "Don't know"


moran.test(test$DK33, c1, randomisation = TRUE)


#  "Prefer not to say"


moran.test(test$MDI_D33, c1, randomisation = TRUE)


### Annual Household income brackets D38dv

#  "Don't know"

moran.test(test$DK38dv, c1, randomisation = TRUE)


#  "Prefer not to say"

moran.test(test$MDI_D38dv, c1, randomisation = TRUE)


### Annual Personal Income D39


# Don't know


moran.test(test$DK39, c1, randomisation = TRUE)


#  "Prefer not to say"


moran.test(test$MDI_D39, c1, randomisation = TRUE)


###########################################################################
###########################################################################

######  Generalised GWR USING 3 VARIABLES (Age, Marital Status, Annual Personal Income)

z <- data.frame(select(dataEDA2,  x, y) )
z1 <- data.matrix(z)
z1

#  D38dv - Annual Household Income

# bandwidth calculator

w3 <- gwr.sel(MDI_D38dv ~ D2M + D5 + D39, data = dataEDA2, coords = z1, adapt=FALSE, gweight=gwr.Gauss,
              method = "cv", verbose = TRUE, longlat=NULL, RMSE=FALSE, tol=.Machine$double.eps^0.25)
w3

# GGWR analysis
Dg <- ggwr(data = dataEDA2, MDI_D38dv ~ D2M + D5 + D39, coords = z1, bandwidth=w3, family="poisson", )

summary(Dg)

Dg


######################################################################################################
## GWR ANALYSIS USING 3 VARIABLES  (Age, Marital Status, Annual Personal Income)


dataEDA1k <- read_csv()
head(dataEDA1k)

dataEDA2 <- read_csv()
head(dataEDA2)

Spatial <- select(dataEDA2, serial, LSOA, x, y, lat, long, MDI_D38dv)
head(Spatial)

dataEDAM <- read_csv()
head(dataEDAM)

dataEDAMz <- select(dataEDAM, LSOA, MSOA)
dataEDAMz

dataEDA1kM <- inner_join(Spatial, dataEDAMz, by=c("LSOA"="LSOA"))
dataEDA1kM

# data file with level indicator

dataEDA2X <- inner_join(dataEDA2, dataEDAMz, by=c("LSOA"="LSOA"))
head(dataEDA2X, 10)

# data file with mean D38dv indicator

dataEDA2XM <- inner_join(dataEDA1k, dataEDA1kM, by=c("serial"="serial"))
head(dataEDA2XM, 10)

dataEDA2XM$D38dv=ifelse((dataEDA2XM$D38dv) == 55453.543435663, 0.0, dataEDA2XM$D38dv)
dataEDA2XM

test <- dataEDA2XM[,-13]
test

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

SerLSOA <- read_csv()
head(SerLSOA)

dataEDA1 <- inner_join(dataEDA, SerLSOA, by=c("serial" = "SERIAL"))
head(dataEDA1)
dim(dataEDA1)

MSOA <- read_csv()
head(MSOA)


dataEDAM <- inner_join(dataEDA1, MSOA, by=c("LSOA"="LSOA"))
head(dataEDAM, 20)
dim(dataEDAM)

dataEDAM1 <- select(dataEDAM, serial, LSOA, MSOA, D38dv)
head(dataEDAM1, 20)


dataEDAM1a <- na.omit(dataEDAM1 %>%
                        mutate(D38dv=case_when(
                          D38dv == "10,000 to 14,999" ~ 15000,
                          D38dv == "100,000 to 149,999" ~ 125000,
                          D38dv == "15,000 to 19,999" ~ 17500,
                          D38dv == "150,000 to 249,999" ~ 200000, 
                          D38dv == "20,000 to 29,999" ~ 25000,
                          D38dv == "250,000 or more" ~ 250000,    
                          D38dv == "30,000 to 39,999" ~ 35000,
                          D38dv == "40,000 to 49,999" ~ 45000,
                          D38dv == "5,000 to 9,999" ~ 7500,
                          D38dv == "50,000 to 59,999" ~ 55000,
                          D38dv == "60,000 to 69,999" ~ 65000,
                          D38dv == "70,000 to 99,999" ~ 85000,
                          D38dv == "Less than 5,000" ~ 2500,
                        )))

head(dataEDAM1a, 20)
dim(dataEDAM1a)

dataEDAM1a 

add <- select(dataEDAM1a, serial, D38dv)

dataEDA2XM1 <- inner_join(test, add, by=c("serial"="serial"))
dataEDA2XM1

dataEDA2XM1$D38dv

## NB: these have different dimensions to those for the indicator values

z <- data.frame(select(dataEDA2XM1,  x, y) )
z1 <- data.matrix(z)
z1

# bandwidth calculator

wM <- gwr.sel(D38dv ~ D2M + D5 + D39, data = dataEDA2XM1, coords = z1, adapt=FALSE, gweight=gwr.Gauss,
              method = "cv", verbose = TRUE, longlat=NULL, RMSE=FALSE, tol=.Machine$double.eps^0.25)

wM

# GWR calculation
DM <- gwr(data = dataEDA2XM1, D38dv ~ D2M + D5 + D39, coords = z1, bandwidth=wM, )

summary(DM)

DM

head(DM)

# lm fitted values

fv <- DM$lm$fitted.values
head(fv)
dim(fv)

names(DM$SDF)

pred <- DM$SDF$pred
head(pred)

DM_out <- as_tibble(cbind(pred, fv))

head(DM_out)


# extracting gwr coefficients 

summary(DM$SDF)

colnames(DM$SDF)
DM$SDF$X.Intercept.

gwM1 <- DM$SDF[,2]
gwM1
gwM2 <- cbind(gwM1, DM$SDF$D2M, DM$SDF$D5, DM$SDF$D39)

head(gwM2)

# extracting lm coefficients

summary(DM$lm)


dim(dataEDA2XM1)
dim(DM_out)

dataEDA2XM1_out <- cbind(dataEDA2XM1, DM_out)

head(dataEDA2XM1_out)


################################################################################

## AVERAGE INCOME CALCS



ONS_AHI <- read_csv()
head(ONS_AHI)

## Loading ONS AHI and MSOA average calculated from LSOA and level means

dataEDAM_ONS <- read_csv()
head(dataEDAM_ONS)
dim(dataEDAM_ONS)

###  MSE calculation - ignoring missing data

mse(dataEDAM_ONS$mean_D38dv, dataEDAM_ONS$Total_annual_income)

###################################################################

### Using values from GWR analysis (missing data omitted)

DM_out <- read_csv()
head(DM_out)
dim(DM_out)

# get DM_out
dataEDA2XM1_out <- read_csv()


### Changing D38dv data to average for range

#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#A#

dataEDAMz <- na.omit(dataEDAM1 %>%
                       mutate(D38dv=case_when(
                         D38dv == "10,000 to 14,999" ~ 15000,
                         D38dv == "100,000 to 149,999" ~ 125000,
                         D38dv == "15,000 to 19,999" ~ 17500,
                         D38dv == "150,000 to 249,999" ~ 200000, 
                         D38dv == "20,000 to 29,999" ~ 25000,
                         D38dv == "250,000 or more" ~ 250000,    
                         D38dv == "30,000 to 39,999" ~ 35000,
                         D38dv == "40,000 to 49,999" ~ 45000,
                         D38dv == "5,000 to 9,999" ~ 7500,
                         D38dv == "50,000 to 59,999" ~ 55000,
                         D38dv == "60,000 to 69,999" ~ 65000,
                         D38dv == "70,000 to 99,999" ~ 85000,
                         D38dv == "Less than 5,000" ~ 2500,
                         D38dv == "Prefer not to say" ~ 0
                       )))

head(dataEDAMz, 20)
dim(dataEDAMz)

dataEDA_MD <- filter(dataEDAMz, D38dv == 0)
dataEDA_MD1 <- rename(dataEDA_MD, MDD38dv = D38dv) 

dataEDA_MD1


# getting variables for the MD

# 1 - just MD

fulldata_MD <- inner_join(dataEDA2XM, dataEDA_MD1, by=c("serial"="serial"))
fulldata_MD

# all data

dataEDAMz1 <- rename(dataEDAMz, MDD38dv = D38dv)

fulldata_MDX <- inner_join(dataEDA2XM, dataEDAMz1, by=c("serial"="serial"))
fulldata_MDX


# Summary of GWR coefficient estimates at data points:
#                     Min.     1st Qu.      Median     3rd Qu.        Max.     Global
# X.Intercept. 26613.43588 32093.52053 40034.49138 47302.73999 48960.51402 38010.4990
# D2M           -331.62443  -301.92075  -173.00777   -81.29500    19.36573  -169.8621
# D5           -1856.05760  -982.40100  -917.02002  -711.10072  -306.28646  -846.2165
# D39              0.68775     0.80070     0.81887     0.84773     0.86655     0.8307



# calculating missing data based on global coefficients

fill_data <- mutate(fulldata_MDX, MXD38dv = (38010.4990 + 
                                               (D2M * -169.8621) + 
                                               (D5 * -846.2165) + 
                                               (D39 * 0.8307)
) )

fill_data

# filling in missing data based on the calculated values

fill_data$MDD38dv=ifelse(fill_data$MDD38dv == 0, fill_data$MXD38dv, fill_data$MDD38dv)

fill_data

## converting LSOA to MSOA data

fill_data_av <- fill_data %>%         # Specify data frame
  group_by(MSOA.y) %>%                  # Specify group indicator
  summarise_at(vars(MDD38dv),           # Specify column
               list(mean_MDD38dv = mean)) 
fill_data_av



###################################################

###  Extracting gwr coefficients from DM$SDF and adding them to data file


head(DM)
head(DM$SDF)

length(DM$SDF$'(Intercept)')

coefD1 <- DM$SDF$'(Intercept)'
coefD1

coefDM <- cbind(coefD1, DM$SDF$D2M,
                DM$SDF$D5,
                DM$SDF$D10,
                DM$SDF$D39)

coefDM <- as_tibble(coefDM)
coefDM

dataEDA_gwr_coef <- cbind(dataEDA2XM1, coefDM)
dataEDA_gwr_coef

### calculating average coefficient per LSOA

LSOA_cut <- select(dataEDA_gwr_coef, LSOA, coefD1, V2, V3, V4)
head(LSOA_cut)

LSOA_cut_av <- LSOA_cut %>% 
  group_by(LSOA) %>%
  summarise(across(everything(), mean))
LSOA_cut_av

#############################################################
###  Plotting GWR coefficients #####
##########################################################

testMapX <- right_join(LSOA, LSOA_cut_av, by=c("LSOA11CD"="LSOA"))

## Age GWR Coefficients Map  ##

head(testMapX)

EnglandX1 <- testMapX %>%         # Specify data frame
  group_by(LSOA11CD) %>%          # Specify group indicator
  summarise_at(vars(V2),          # Specify column
               list('Age GWR Coef.' = mean)) 

head(EnglandX1)

summary(EnglandX1$'Age GWR Coef.')

mapview(EnglandX1, xcol="LONG", ycol="LAT", crs=4326, grid=FALSE,
        zcol="Age GWR Coef.", 
        at = c(-332,-300, -174, -189, -82, 20)) 


## Marital Status GWR Coefficients Map ##

EnglandX2 <- testMapX %>%         # Specify data frame
  group_by(LSOA11CD) %>%                  # Specify group indicator
  summarise_at(vars(V3),           # Specify column
               list('Marital Status GWR Coef.' = mean)) 

head(EnglandX2)

summary(EnglandX2$'Marital Status GWR Coef.')

mapview(EnglandX2, xcol="LONG", ycol="LAT", crs=4326, grid=FALSE,
        zcol="Marital Status GWR Coef.", 
        at = c(-2000,-980, -920, -850, -700, -300)) 


## Annual Personal Income GWR Coefficients Map ##

EnglandX3 <- testMapX %>%         # Specify data frame
  group_by(LSOA11CD) %>%          # Specify group indicator
  summarise_at(vars(V4),          # Specify column
               list("API GWR Coef." = mean)) 

head(EnglandX3)

summary(EnglandX3$"API GWR Coef.")

mapview(EnglandX3, xcol="LONG", ycol="LAT", crs=4326, grid=FALSE,
        zcol="API GWR Coef.", 
        at = c(0.6,0.81, 0.82, 0.824, 0.849, 0.87)) 


##########################################################

### Calculating average coefficeint by MSOA

MSOA_cut <- select(dataEDA_gwr_coef, MSOA, coefD1, V2, V3, V4)
head(MSOA_cut)

MSOA_cut_av <- MSOA_cut %>% 
  group_by(MSOA) %>%
  summarise(across(everything(), mean))
MSOA_cut_av

# Adding MSOA averages to missing data values

fulldata_MDY <- inner_join(dataEDA2XM, MSOA_cut_av, by=c("MSOA"="MSOA"))
fulldata_MDY
head(fulldata_MDY, 20)

sum(is.na(fulldata_MDY$V2))

# Adding LSOA averages to missing data values

fulldata_MDY1 <- full_join(dataEDA2XM, LSOA_cut_av, by=c("LSOA"="LSOA"))
fulldata_MDY1
head(fulldata_MDY1, 20)

sum(is.na(fulldata_MDY1$V2))

## Calculating MSOA_based MD proxy values (D38dvM)

fulldata_MDYA <- mutate(fulldata_MDY, D38dvM = coefD1 + 
                          (D2M * V2) + 
                          (D5 * V3) + 
                          (D39 * V4) 
) 

head(fulldata_MDYA, 20)

colnames(fulldata_MDYA)

# Substituting D38dvM for MD in D38dv

fulldata_MDYA$D38dv=ifelse(fulldata_MDYA$MDI_D38dv == 1, 
                           fulldata_MDYA$D38dvM, fulldata_MDYA$D38dv)

head(fulldata_MDYA, 20)

# combining to MSOA

MSOA_MDYA <- fulldata_MDYA %>% 
  group_by(MSOA) %>%
  summarise(across(everything(), mean))
MSOA_MDYA


fulldata_MDX


###################################################################
####  MSE CALCULATIONS FOR IGNORING MD AND GLOBAL INFILL OF MD ####
###################################################################


#### calculating mse for MD based on global infill:

# combining data

test_data <- inner_join(dataEDAM_ONS, fill_data_av, by=c("MSOA" = "MSOA.y"))

test_data

mse(test_data$mean_MDD38dv, test_data$Total_annual_income)

#### calculating mse for MD based on GWR infill:


# combining data


test_dataG <- inner_join(dataEDAM_ONS, MSOA_MDYA, by=c("MSOA" = "MSOA"))

test_dataG

mse(test_dataG$D38dv, test_data$Total_annual_income)

### Consolidated results

m1 <- mse(test_data$mean_D38dv, test_data$Total_annual_income)

m2 <- mse(test_data$mean_MDD38dv, test_data$Total_annual_income)

m3 <- mse(test_dataG$D38dv, test_data$Total_annual_income)

m1; m2; m3

(m1 - m2) / m1 *100

(m1 - m3) / m1 *100

#################################################

##  MEAN VALUE CALCULATIONS

mean(test_data$Total_annual_income)  # mean ONS data
mean(test_data$mean_D38dv)           # mean CCA
mean(test_data$mean_MDD38dv)         # mean lm calculation 
mean(test_dataG$D38dv)               # mean GWR calculation

## SD VALUE CALCULATIONS

sd(test_data$Total_annual_income)  # mean ONS data
sd(test_data$mean_D38dv)           # mean CCA
sd(test_data$mean_MDD38dv)         # mean lm calculation 
sd(test_dataG$D38dv)               # mean GWR calculation

## SUMMARY

summary(test_data$Total_annual_income)  # mean ONS data
summary(test_data$mean_D38dv)           # mean CCA
summary(test_data$mean_MDD38dv)         # mean lm calculation 
summary(test_dataG$D38dv)               # mean GWR calculation

count(test_data, Total_annual_income)   # mean ONS data
count(test_data, mean_D38dv)            # mean CCA calculation
count(test_data, mean_MDD38dv)          # mean lm calculation
count(test_dataG, D38dv)                # mean GWR calculation


###  ENDS  ###
