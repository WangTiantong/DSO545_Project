# The setting directary varies with different file locations
options(useFancyQuotes = FALSE) # this is to solve gibberish issue

# Import libraries

library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(maps)
library(ggmap)
library(mapdata)
library(rgdal)
library(maptools)
library(grid)
library(broom)
library(leaflet)
library(sp)
library(viridis)
library(readr)
library(tidyr)
library(stringr)
library(scales)
library(plotly)

#--------------------#
# Declare variables



shinyServer(function(input, output, session) {
  
  #----------------------------------------------------------------------------------------------------------------------------#
  
  
  ### {PART 0: Read and clean data, RUN THIS PART FIRST!!!}
  
  
  # Read source files
  
  HC2015 = read.csv("data/HC2015.csv")
  HC2016 = read.csv("data/HC2016.csv")
  HC2017 = read.csv("data/HC2017.csv")
  data_311 = read.csv("data/311.csv")
  housing = read.csv("data/Housing_Inventory_2017.csv")
  
  #Change Create date to date format
  data_311$CreatedDate = mdy_hms(data_311$CreatedDate)
  
  
  
  #!------------------! Dataset: HC_streetcount !------------------! 
  
  # Summarize living units tabulated during street count for 2017
  
  HC2017_streetcount = HC2017 %>%
    summarize(totalCars = sum (totCars),
              totalVans = sum (totVans),
              totalCampers = sum(totCampers),
              totalTents = sum (totTents),
              totalEncamps = sum(totEncamp)) %>%
    gather(totalCars:totalEncamps, key = type, value = count)%>%
    mutate(year = "2017")
  
  
  
  # Summarize living units tabulated during street count for 2016
  
  
  HC2016_streetcount = HC2016 %>%
    summarize(totalCars = sum (totCars),
              totalVans = sum (totVans),
              totalCampers = sum(totCampers),
              totalTents = sum (totTents),
              totalEncamps = sum(totEncamp)) %>%
    gather(totalCars:totalEncamps, key = type, value = count) %>%
    mutate(year = "2016")
  
  
  # Summarize living units tabulated during street count for 2015
  
  
  HC2015_streetcount = HC2015 %>%
    summarize(totalCars = sum (Cars),
              totalVans = sum (Vans),
              totalCampers = sum(Campers),
              totalTents = sum (Tents),
              totalEncamps = sum(Encampments)) %>%
    gather(totalCars:totalEncamps, key = type, value = count)%>%
    mutate(year = "2015")
  
  
  
  # Merge tables HC2017_streetcount, HC2016_streetcount, HC2015_streetcount
  
  HC_streetcount = full_join(HC2017_streetcount, HC2016_streetcount)
  HC_streetcount = full_join(HC_streetcount, HC2015_streetcount)
  
  
  
  #!------------------! Dataset: HC_livingtype !------------------! 
  
  # Summarize homeless population in 2017 by living areas
  
  HC2017_livingtype = HC2017 %>%
    summarize(totalCarPeople = sum (totCarPeople),
              totalVanPeople = sum (totVanPeople),
              totalCamperPeople = sum(totCamperPeople),
              totalTentPeople = sum (totTentPeople),
              totalEncampPeople = sum(totEncampPeople)) %>%
    gather(totalCarPeople:totalEncampPeople, key = type, value = count)
  
  
  # Format variables of HC2016 (common between HC2016 and HC2017) in same pattern as HC2017
  
  
  HC2016 = HC2016 %>%
    setnames(names(HC2016) %>%
               stringr::str_replace("Memb", "Mem") %>%
               stringr::str_replace("totEsA", "totESA") %>%
               stringr::str_replace("totEsY", "totESY") %>%
               stringr::str_replace("totThA", "totTHA") %>%
               stringr::str_replace("totThY", "totTHY") %>%
               stringr::str_replace("totShA", "totSHA")%>%
               stringr::str_replace("totShY", "totSHY"))
  
  # Summarize homeless population in 2016 by living areas
  
  HC2016_livingtype = HC2016 %>%
    summarize(totalCarPeople = sum (totCarPeople),
              totalVanPeople = sum (totVanPeople),
              totalCamperPeople = sum(totCamperPeople),
              totalTentPeople = sum (totTentPeople),
              totalEncampPeople = sum(totEncampPeople)) %>%
    gather(totalCarPeople:totalEncampPeople, key = type, value = count)
  
  
  
  # Format variables of living areas of HC2015 in same pattern as HC2017 and HC2016
  
  setnames(HC2015, old = c("Car.People", "VanPeople", "Camper.People",
                           "Tent.People", "Encamp.People"),
           new = c("totCarPeople", "totVanPeople", "totCamperPeople",
                   "totTentPeople", "totEncampPeople"))
  
  # Summarize homeless population in 2015 by living areas
  
  
  HC2015_livingtype = HC2015 %>%
    summarize(totalCarPeople = sum (totCarPeople),
              totalVanPeople = sum (totVanPeople),
              totalCamperPeople = sum(totCamperPeople),
              totalTentPeople = sum (totTentPeople),
              totalEncampPeople = sum(totEncampPeople)) %>%
    gather(totalCarPeople:totalEncampPeople, key = type, value = count)
  
  # Add a column "year"
  
  HC2017_livingtype = HC2017_livingtype %>%
    mutate(year = "2017")
  HC2016_livingtype = HC2016_livingtype %>%
    mutate(year = "2016")
  HC2015_livingtype = HC2015_livingtype %>%
    mutate(year = "2015")
  
  
  # Merge tables HC2017_livingtype, HC2016_livingtype, HC2015_livingtype
  
  HC_livingtype = full_join(HC2017_livingtype, HC2016_livingtype)
  
  HC_livingtype = full_join(HC_livingtype, HC2015_livingtype)
  
  
  
  
  #!------------------! -Dataset: HC_total_unshelt_shelt !------------------! 
  
  
  
  # Summarize homeless in 2017 by unsheltered and sheltered
  
  HC2017_unshelt_shelt = HC2017 %>%
    summarize(totalUnsheltPeople = sum (totUnsheltPeople),
              totalESPeople = sum (totESPeople),
              totalTHPeople = sum(totTHPeople),
              totalSHPeople = sum (totSHPeople),
              totalSheltPeople = sum(totSheltPeople),
              totalPeople = sum (totPeople)) %>%
    gather(totalUnsheltPeople:totalPeople, key = type, value = count) %>%
    mutate(year ="2017")
  
  # Summarize homeless in 2016 by unsheltered and sheltered
  
  HC2016_unshelt_shelt = HC2016 %>%
    summarize(totalUnsheltPeople = sum (totUnsheltPeople),
              totalESPeople = sum (totESPeople),
              totalTHPeople = sum(totTHPeople),
              totalSHPeople = sum (totSHPeople),
              totalSheltPeople = sum(totSheltPeople),
              totalPeople = sum (totPeople)) %>%
    gather(totalUnsheltPeople:totalPeople, key = type, value = count) %>%
    mutate(year ="2016")
  
  
  # Change column names in HC2015 to match column names in HC2017 and HC2016
  
  setnames(HC2015, old = c("Unsheltered", "ES_People", "TH_People",
                           "Sheltered", "X2015_Total"),
           new = c("totUnsheltPeople", "totESPeople", "totTHPeople",
                   "totSheltPeople", "totPeople"))
  
  
  # Summarize homeless in 2015 by unsheltered and sheltered
  
  HC2015_unshelt_shelt = HC2015 %>%
    summarize(totalUnsheltPeople = sum (totUnsheltPeople),
              totalESPeople = sum (totESPeople),
              totalTHPeople = sum(totTHPeople),
              totalSheltPeople = sum(totSheltPeople),
              totalPeople = sum (totPeople)) %>%
    gather(totalUnsheltPeople:totalPeople, key = type, value = count) %>%
    mutate(year ="2015")
  
  # Join homeless unsheltered and sheltered data for 2015, 2016, 2017
  
  HC_total_unshelt_shelt = full_join(HC2017_unshelt_shelt, HC2016_unshelt_shelt)
  
  HC_total_unshelt_shelt = full_join(HC_total_unshelt_shelt, HC2015_unshelt_shelt)
  
  
  
  
  #!------------------! Dataset: HC_CD (summary stats for various categories) !------------------! 
  
  
  #Summarize number of homeless in various council districts
  #Summary stats obtained for below variables:
  #totStreetSingAdult, totStreetFamHH, totStreetFamMem, totCars, 
  #totVans, totCampers, totTents, totEncamp, totCarPeople, totVanPeople, 
  #totCamperPeople, totTentPeople, totEncampPeople, totYouthFamHH, 
  #totESAdultSingAdult, totESAdultFamHH, totESAdultFamMem, 
  #totESYouthSingYouth, totESYouthFamHH, totESYouthFamMem, 
  #totESYouthUnaccYouth, totTHAdultSingAdult, totTHAdultFamHH, 
  #totTHAdultFamMem, totTHYouthSingYouth, totTHYouthFamHH, 
  #totTHYouthFamMem, totTHYouthUnaccYouth, totSHAdultSingAdult, 
  #totSHAdultFamHH, totSHAdultFamMem, totSHYouthSingYouth, 
  #totSHYouthFamHH, totSHYouthFamMem, totSHYouthUnaccYouth, 
  #totUnsheltPeople, totESPeople, totTHPeople, totSHPeople, 
  #totSheltPeople, totPeople, 
  
  #Summary for above variables across council districts for 2017
  
  HC2017_CD = HC2017[ , c(9, 18: 30, 46:73 )]%>%
    group_by(CD) %>%
    summarize_all(funs(sum)) %>%
    mutate(Year = "2017") %>%
    filter(CD!=0)
  
  #Summary for above variables across council districts for 2017
  
  HC2016_CD = HC2016[ , c(8, 13: 25, 28, 30:56 )]%>%
    group_by(CD) %>%
    summarize_all(funs(sum)) %>%
    mutate(Year = "2016") %>%
    filter(CD!=0)
  
  
  #Join summary stats by council districts for year 2017 and 2016
  HC_CD = full_join(HC2017_CD, HC2016_CD)
  
  
  
  #!------------------! Dataset: Homeless Crime 2016-2017 Aug !------------------!
  
  crimeHLVict <- read_csv("data/Crime__Homeless_Victim_8_16_-_8_17.csv")
  
  ## Victim Age: fill blanks with 0
  crimeHLVict$`Victim Age`[is.na(crimeHLVict$`Victim Age`)] <- 0
  
  ## Premise Description: fill blanks with "UNKNOWN"
  crimeHLVict$`Premise Description`[is.na(crimeHLVict$`Premise Description`)] <- "UNKNOWN"
  
  ## Weapon Used Code: fill blanks with 0
  ## Weapon Description: fill blanks with "UNKNOWN"
  crimeHLVict$`Weapon Used Code`[is.na(crimeHLVict$`Weapon Used Code`)] <- 0
  crimeHLVict$`Weapon Description`[is.na(crimeHLVict$`Weapon Description`)] <- "UNKNOWN"
  
  # Location: convert to latitude (y, 1st coordinate) & longitude (x, 2nd coorinate)
  crimeHLVict <- crimeHLVict %>%
    separate(col = Location,into = c("Latitude", "Longitude"), sep = ",") %>%
    mutate(Latitude = str_replace(string = Latitude, pattern = "[(]",replacement = ""),
           Longitude = str_replace(string = Longitude, pattern = "[)]",replacement = ""))
  
  # Rename "West LA" to "West Los Angeles", "N Hollywood" to "North Hollywood"
  # to eliminate NULL during merge
  crimeHLVict <- crimeHLVict %>%
    mutate(`Area Name` = str_replace(string = `Area Name`, pattern = "West LA",replacement = "West Los Angeles"),
           `Area Name` = str_replace(string = `Area Name`, pattern = "N Hollywood",replacement = "North Hollywood"))
 
  #!--------------------------------------------!
  # Read extra data
  
  
  # Area Code & Name Match Data
  areaMatch <- distinct(crimeHLVict[5:6])
  areaNames <- areaMatch$`Area Name`
  
  # LAPD Divisions (Areas)
  LAPD_areas <- readOGR(dsn="data/lapd division.shp", layer="lapd division", encoding="UTF-8")
  
  # Homeless Shelters and Services
  homeless_SS <- readOGR(dsn="data/Homeless_Shelters_and_Services.shp", layer="Homeless_Shelters_and_Services", encoding="UTF-8")
  # make sure the projection crs are identical
  homeless_SS <- spTransform(homeless_SS, CRS(proj4string(LAPD_areas)))
  # only keep data points that are within LAPD_areas polygon
  homeless_SS <- homeless_SS[LAPD_areas, ]
  
  # Number of Victims per area
  countVict <- crimeHLVict %>%
    group_by(`Area ID`, `Area Name`) %>%
    count()
  
  # merge countVict with shapefile to get the polygon
  countVict <- merge(LAPD_areas, countVict, by.x="name", by.y="Area Name", all.x = TRUE)
  countVict <- spTransform(countVict, CRS("+proj=longlat +datum=WGS84 +no_defs")) # to transform the projection
  countVict$name_1 <- NULL
  
  la_cd = readOGR(dsn="data/l.a. city council district (2012).shp") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
  crimeHLVict_geo <- crimeHLVict
  crimeHLVict_geo$Latitude <- as.numeric(crimeHLVict_geo$Latitude)
  crimeHLVict_geo$Longitude <- as.numeric(crimeHLVict_geo$Longitude)
  crimeHLVict_geo <- filter(crimeHLVict_geo, crimeHLVict_geo$Longitude != 0 & crimeHLVict_geo$Latitude != 0)
  
  coordinates(crimeHLVict_geo)=~Longitude+Latitude
  
  proj4string(crimeHLVict_geo) <- CRS(proj4string(la_cd))
  
  crimeHLVict_cd <- over(crimeHLVict_geo, la_cd)
  crimeHLVict_cd_count <- crimeHLVict_cd %>%
    group_by(DISTRICT) %>%
    count() %>%
    filter(!is.na(DISTRICT))
  
  #----Cleaning LAHSA Homeless Population data sets----#
  
  ##############Full data set HC 2016 and 2017 ########################
  
  # Format variables of HC2016 (common between HC2016 and HC2017) in same pattern as HC2017
  
  setnames(HC2016, old = c("censusTract", "CommunityName", "SSD","AD"),
           new = c("tract", "Community_Name", "ca_ssd", "ca_sad"))
  
  
  HC2016 = HC2016 %>%
    setnames(names(HC2016) %>%
               stringr::str_replace("Memb", "Mem") %>%
               stringr::str_replace("totEsA", "totESA") %>%
               stringr::str_replace("totEsY", "totESY") %>%
               stringr::str_replace("totThA", "totTHA") %>%
               stringr::str_replace("totThY", "totTHY") %>%
               stringr::str_replace("totShA", "totSHA")%>%
               stringr::str_replace("totShY", "totSHY"))
  
  
  #Summarize number of homeless in various council districts
  #Summary stats obtained for below variables:
  #totStreetSingAdult, totStreetFamHH, totStreetFamMem, totCars, 
  #totVans, totCampers, totTents, totEncamp, totCarPeople, totVanPeople, 
  #totCamperPeople, totTentPeople, totEncampPeople, totYouthFamHH, 
  #totESAdultSingAdult, totESAdultFamHH, totESAdultFamMem, 
  #totESYouthSingYouth, totESYouthFamHH, totESYouthFamMem, 
  #totESYouthUnaccYouth, totTHAdultSingAdult, totTHAdultFamHH, 
  #totTHAdultFamMem, totTHYouthSingYouth, totTHYouthFamHH, 
  #totTHYouthFamMem, totTHYouthUnaccYouth, totSHAdultSingAdult, 
  #totSHAdultFamHH, totSHAdultFamMem, totSHYouthSingYouth, 
  #totSHYouthFamHH, totSHYouthFamMem, totSHYouthUnaccYouth, 
  #totUnsheltPeople, totESPeople, totTHPeople, totSHPeople, 
  #totSheltPeople, totPeople, 
  
  #Summary for above variables across council districts for 2017
  
  HC2017_updated = HC2017[ , c(1:3, 5:11, 18:30, 46:73 )]%>%
    mutate(Year = "2017")
  #Summary for above variables across council districts for 2017
  
  HC2016_updated = HC2016[ , c(1:10, 13:25, 28, 30:56 )]%>%
    mutate(Year = "2016")
  
  
  #Join summary stats by council districts for year 2017 and 2016
  HC_2017_2016 = full_join(HC2017_updated, HC2016_updated)
  
  
  ###############Dataset : Homeless count in 2017 only in LA City (CD 1 to 15) #######################
  
  
  hc_2017_CD =  HC_2017_2016%>%  filter(Year == 2017 & CD %in% c(seq(1,15,1)))
  
  str(hc_2017_CD)
  
  hc_2017_CD = hc_2017_CD[, c(8, 11:51)] %>%
    group_by(CD) %>%
    summarize_all(funs(sum))
  
  
  #Datset for homeless in different shelters, unsheltered and total homeless
  
  hc_2017_unshelt_shelt_total = hc_2017_CD %>%
    group_by(CD) %>%
    summarize(totalUnsheltPeople = sum (totUnsheltPeople),
              totalESPeople = sum (totESPeople),
              totalTHPeople = sum(totTHPeople),
              totalSHPeople = sum (totSHPeople),
              totalSheltPeople = sum(totSheltPeople),
              totalPeople = sum (totPeople)) %>%
    gather(totalUnsheltPeople:totalPeople, key = type, value = count) %>%
    mutate(year ="2017")
  
  
  #Dataset for sheltered and unsheltered homeless only
  
  hc_2017_unshelt_shelt = hc_2017_unshelt_shelt_total %>%
    group_by(CD, type) %>%
    summarize(total_count = sum(count)) %>%
    filter(type %in% c("totalUnsheltPeople", "totalSheltPeople")) 
  
  
  #----Cleaning housing and shelters dataset----#
  
  #Change CD to integers
  housing$CD = gsub("CD", "", housing$CD)
  housing$CD = as.integer(housing$CD)
  
  #Shelters and Housing only in LA City
  lacity_housing_shelter = housing%>% filter(CD %in% c(seq(1,15,1)))
  
  
  #Add Variable Bed_Occupancy to show utilization
  lacity_housing_shelter$Bed.Utilization.Rate = gsub("%", "", lacity_housing_shelter$Bed.Utilization.Rate)
  lacity_housing_shelter$Bed.Utilization.Rate = as.integer(lacity_housing_shelter$Bed.Utilization.Rate)
  lacity_housing_shelter = lacity_housing_shelter %>%
    mutate(Bed_Occupancy = ifelse(Bed.Utilization.Rate<50, "Under-Utilization", 
                                  ifelse(Bed.Utilization.Rate<90, "Moderate-Utilization", 
                                         "Optimal-Utlization")))
  
  
  
  #Change na(Total.Seasonal.Beds) to 0
  lacity_housing_shelter = lacity_housing_shelter %>%
    mutate(Total.Seasonal.Beds = ifelse(is.na(Total.Seasonal.Beds), 0, Total.Seasonal.Beds))
  
  #Add vairable shelter period
  lacity_housing_shelter = lacity_housing_shelter %>%
    mutate(shelter_period = ifelse(Total.Seasonal.Beds>0, "Winter-Shelter", "Year-Round"))
  
  lacity_housing_shelter$Proj..Type
  
  #Add variable to show if its a shelter or a housing type
  lacity_housing_shelter = lacity_housing_shelter %>%
    mutate(shelter_type = ifelse(Proj..Type %in% c ("OPH", "PSH", "RRH"), 
                                 "Permanent Housing", 
                                 "Homeless Shelter"))
  #Shelters across 15 council ditricts
  
  lacity_shelters = lacity_housing_shelter %>%
    filter(CD %in% c(seq(1,15,1))) %>%
    filter(shelter_type == "Homeless Shelter") %>%
    select(CD, Proj..Type, Bed_Occupancy, shelter_period)
  
  
  #Housing across 15 council ditricts
  lacity_housing = lacity_housing_shelter %>%
    filter(CD %in% c(seq(1,15,1))) %>%
    filter(shelter_type == "Permanent Housing") %>%
    select(CD, Proj..Type, Bed_Occupancy, shelter_period)
  
  
  #Create a new variable avergae utilization
  #Avg utilization shows the avergae bed utilization rate across different council districts
  
  lacity_avg_utilization_year = lacity_housing_shelter %>%
    filter(!is.na(Bed.Utilization.Rate)) %>%
    group_by(CD, shelter_type) %>%
    summarize(avg_utilization = mean(Bed.Utilization.Rate))
  
  lacity_avg_utilization_winter = lacity_housing_shelter %>%
    filter(!is.na(Bed.Utilization.Rate)) %>%
    filter(shelter_period == "Winter-Shelter") %>%
    group_by(CD, shelter_type) %>%
    summarize(avg_utilization = mean(Bed.Utilization.Rate))
  
  
  lacity_shelters_avg_utilization_year = lacity_avg_utilization_year %>% filter(shelter_type == "Homeless Shelter")
  lacity_housing_avg_utilization_year = lacity_avg_utilization_year %>% filter(shelter_type == "Permanent Housing")
  lacity_shelters_avg_utilization_winter = lacity_avg_utilization_winter %>% filter(shelter_type == "Homeless Shelter")
  lacity_housing_avg_utilization_winter = lacity_avg_utilization_winter %>% filter(shelter_type == "Permanent Housing")
  
  
  
  #----Combining Homeless population and shelters and housing datasets----#
  
  
  lacity_unshelt_shelt_total_2017  = hc_2017_unshelt_shelt_total %>%
    spread(key = type, value = count)
  
  lacity_shelters_housing_avg_utilization_year = lacity_avg_utilization_year %>%
    spread(key = shelter_type, value = avg_utilization)
  
  lacity_shelters_housing_avg_utilization_winter = lacity_avg_utilization_winter %>%
    spread(key = shelter_type, value = avg_utilization)
  
  
  lacity_year = full_join(lacity_unshelt_shelt_total_2017, lacity_shelters_housing_avg_utilization_year, by = "CD")
  lacity_winter = full_join(lacity_unshelt_shelt_total_2017, lacity_shelters_housing_avg_utilization_winter, by = "CD")
  
  
  #Read shape file
  la_cd_year = readOGR(dsn="data/l.a. city council district (2012).shp") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
  la_cd_year@data$DISTRICT = as.integer(la_cd_year@data$DISTRICT )
  
  la_cd_winter = la_cd_year
  
  
  #Merge shape data with 2017 council district data
  la_cd_year@data =  full_join(la_cd_year@data, lacity_year, by =  c( "DISTRICT" = "CD" ))
  la_cd_winter@data =  full_join(la_cd_winter@data, lacity_winter, by =  c( "DISTRICT" = "CD" ))
  
  
  #!------- Health Risk -------!#
  
  #reading data
  femalehealth=read.csv("data/hc2017-female-persons-data-summary.csv")
  malehealth=read.csv("data/hc2017-male-persons-data-summary.csv")
  
  #selecting the data we need
  femalehealth=femalehealth[34:39,1:4]
  malehealth=malehealth[34:39,1:4]
  
  #renaming the columns
  names(femalehealth)=c("Health/Disability_Indication",
                        "Sheltered","Unsheltered",
                        "Total")
  
  names(malehealth)=c("Health/Disability_Indication",
                      "Sheltered","Unsheltered",
                      "Total")
  
  #changing data types: factor to character
  femalehealth$Sheltered=as.character(femalehealth$Sheltered)
  femalehealth$Unsheltered=as.character(femalehealth$Unsheltered)
  malehealth$Sheltered=as.character(malehealth$Sheltered)
  malehealth$Unsheltered=as.character(malehealth$Unsheltered)
  femalehealth$Total=as.character(femalehealth$Total)
  malehealth$Total=as.character(malehealth$Total)
  
  #removing commas
  femalehealth$Sheltered=gsub(",","",femalehealth$Sheltered)
  femalehealth$Unsheltered=gsub(",","",femalehealth$Unsheltered)
  femalehealth$Total=gsub(",","",femalehealth$Total)
  
  malehealth$Sheltered=gsub(",","",malehealth$Sheltered)
  malehealth$Unsheltered=gsub(",","",malehealth$Unsheltered)
  malehealth$Total=gsub(",","",malehealth$Total)
  
  #changing data types: character to numeric
  femalehealth$Sheltered=as.numeric(femalehealth$Sheltered)
  femalehealth$Unsheltered=as.numeric(femalehealth$Unsheltered)
  malehealth$Sheltered=as.numeric(malehealth$Sheltered)
  malehealth$Unsheltered=as.numeric(malehealth$Unsheltered)
  femalehealth$Total=as.numeric(femalehealth$Total)
  malehealth$Total=as.numeric(malehealth$Total)
  
  #summing the female data and male data
  health=data.frame(Indication=femalehealth$`Health/Disability_Indication`) %>%
    mutate(Sheltered=femalehealth$Sheltered+malehealth$Sheltered) %>%
    mutate(Unsheltered=femalehealth$Unsheltered+malehealth$Unsheltered)%>%
    mutate(Total=femalehealth$Total+malehealth$Total)
  
  
  #----------------------------------------------------------------------------------------------------------------------------#
  
  ### {PART 1: Tab Introduction}
  
  
  #!------------------! Plot 1 !------------------! 
  
  #Plot 1 used in Introduction tab
  
  output$plot1 = renderPlot({
    
    HC_total_unshelt_shelt %>%
      filter(type %in% c("totalUnsheltPeople", "totalESPeople", "totalTHPeople", "totalSHPeople")) %>%
      ggplot(aes(x = year, y = count, fill = type))+
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c ("darkgoldenrod1", "cornflowerblue" , 
                                    "darkolivegreen4" , "red"),
                        labels = c("Emergency Shelters", "Safe Havens", "Transitional Housing",
                                   "Unsheltered"),
                        name = "") +
      ggtitle("Homeless Population in LA") +
      ylab("Total People")+
      xlab("Year") +
      theme(plot.title = element_text(face="bold", size=20, hjust = 0.5),
            plot.subtitle = element_text(face="bold", size=18, hjust = 0.5),
            axis.title = element_text(face="bold", size = 18),
            axis.text.x = element_text(face="bold", size = 12),
            axis.text.y = element_text(face="bold", size=14),
            legend.title = element_text(face="bold", size = 14),
            legend.text = element_text(face="bold", size = 12))
  })
  
  
  
  #----------------------------------------------------------------------------------------------------------------------------#
  
  ### {PART 2: Tab Living Arrangments}
  
  #!------------------! Map for Tab 2 !------------------!
  
  output$living_map = renderLeaflet({
    
    HC2017_living_CD = HC2017 %>%
      group_by(CD) %>%
      summarize(totalCarPeople = sum (totCarPeople),
                totalVanPeople = sum (totVanPeople),
                totalCamperPeople = sum(totCamperPeople),
                totalTentPeople = sum (totTentPeople),
                totalEncampPeople = sum(totEncampPeople)) %>%
      filter(CD!= "0")
    
    # Change the names of variables 
    
    
    setnames(HC2017_living_CD, old = c("totalCarPeople", "totalVanPeople",
                                       "totalCamperPeople", "totalTentPeople",
                                       "totalEncampPeople"),
             new = c("Cars", "Vans","Camps", "Tents","Encampments") )
    
    
    
    la_cd = readOGR(dsn="data/l.a. city council district (2012).shp") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
    
    la_cd@data$DISTRICT = as.integer(la_cd@data$DISTRICT )
    la_cd@data =  full_join(la_cd@data, HC2017_living_CD, by =  c( "DISTRICT" = "CD" ))
    
    
    
    
    #Define one common palette for all living arrangements
    
    pal_dat = seq(0, 1000, 200)
    
    pal_all <- colorBin("YlOrBr", domain = pal_dat, bins = 5)
    
    
    
    #Define labes for all living arrangements 
    
    labels_cars <- sprintf( "<strong>%s</strong><br/>%g homeless people living in cars",
                            paste("Council District ", la_cd@data$DISTRICT), la_cd@data$Cars) %>% 
      lapply(htmltools::HTML)
    
    
    labels_vans <- sprintf( "<strong>%s</strong><br/>%g homeless people living in vans",
                            paste("Council District ", la_cd@data$DISTRICT), la_cd@data$Vans) %>% 
      lapply(htmltools::HTML)
    
    labels_camps <- sprintf( "<strong>%s</strong><br/>%g homeless people living in camps",
                             paste("Council District ", la_cd@data$DISTRICT), la_cd@data$Camps) %>% 
      lapply(htmltools::HTML)
    
    labels_tents <- sprintf( "<strong>%s</strong><br/>%g homeless people living in tents",
                             paste("Council District ", la_cd@data$DISTRICT), la_cd@data$Tents) %>% 
      lapply(htmltools::HTML)
    
    labels_encamp <- sprintf( "<strong>%s</strong><br/>%g homeless people living in encampments",
                              paste("Council District ", la_cd@data$DISTRICT), la_cd@data$Encampments) %>% 
      lapply(htmltools::HTML)
    
    
    #Create cholopleth map
    
    leaflet(data = la_cd) %>%
      addTiles()%>%
      setView(lng = -118.247011, lat = 34.040942, zoom = 9) %>%
      
      #addPolygons for Cars
      addPolygons(fillColor = ~pal_all(la_cd@data$Cars),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.8,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.8,
                    bringToFront = TRUE),
                  label = labels_cars,
                  group="People living in Cars") %>%
      
      #addPolygons for Vans
      addPolygons(fillColor = ~pal_all(la_cd@data$Vans),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.8,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.8,
                    bringToFront = TRUE),
                  label = labels_vans,
                  group="People living in Vans") %>%
      
      #addPolygons for Camps
      addPolygons(fillColor = ~pal_all(la_cd@data$Camps),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.8,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.8,
                    bringToFront = TRUE),
                  label = labels_camps,
                  group="People living in Camps") %>%
      
      #addPolygons for Tents
      addPolygons(fillColor = ~pal_all(la_cd@data$Tents),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.8,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.8,
                    bringToFront = TRUE),
                  label = labels_tents,
                  group="People living in Tents") %>%
      
      #addPolygons for Encampments
      addPolygons(fillColor = ~pal_all(la_cd@data$Encampments),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.8,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.8,
                    bringToFront = TRUE),
                  label = labels_encamp,
                  group="People living in Encampments") %>%
      
      #Add a common legend for all 
      addLegend(pal =  pal_all, values = ~(pal_dat), opacity = 0.7, title = NULL,
                position = "bottomright") %>%
      
      #Add layer control
      addLayersControl(
        overlayGroups = c("People living in Cars",
                          "People living in Vans",
                          "People living in Camps",
                          "People living in Tents",
                          "People living in Encampments"),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE )
      ) 
    
    
  })
  
  
  #!------------------! Plot 2 !------------------!
  
  # Plot 2 used in Living Arrangements tab
  
  output$plot2 = renderPlotly({
    
    # Change the names of categories 
    
    HC_livingtype [HC_livingtype == "totalCarPeople"] = "Cars"
    HC_livingtype [HC_livingtype == "totalVanPeople"] = "Vans"
    HC_livingtype [HC_livingtype == "totalCamperPeople"] = "Camps"
    HC_livingtype [HC_livingtype == "totalTentPeople"] = "Tents"
    HC_livingtype [HC_livingtype == "totalEncampPeople"] = "Encampments"
    
    # Plot a bar graph for number of people in different living arrangement in a particular year
    # The year is given as an input
    
    p <- HC_livingtype %>%
      filter(year == 2017) %>%
      # filter(year == input$year) %>%
      ggplot(aes(x = reorder(type, count), y = count, fill = count)) +
        geom_bar(stat = "identity", aes(text=sprintf("Unit Type: %s<br>Total # of People: %s", type, count))) +
        ggtitle("People Living in Units in 2017") +
        ylab("Total People")+
        xlab("")+
        scale_fill_gradient(low = "coral", high = "coral4",
                            name = "") +
        scale_y_continuous(breaks = seq(0, 10000, 1000), labels = seq(0, 10000, 1000))+
        theme(plot.title = element_text(size=10, hjust = 0.5),
              plot.subtitle = element_text(size=10, hjust = 0.5),
              axis.title = element_text(size = 10),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              legend.position="none")
    
    ggplotly(p, tooltip = "text")
  })
  
  #!------------------! Plot 2.1 !------------------!
  
  # Plot 2.1 used in Living Arrangements tab
  
  # Change the names of categories 
  
  
  output$plotStreetCount = renderPlotly({

      HC_streetcount [HC_streetcount == "totalCars"] = "Cars"
      HC_streetcount [HC_streetcount == "totalVans"] = "Vans"
      HC_streetcount [HC_streetcount == "totalCampers"] = "Camps"
      HC_streetcount [HC_streetcount == "totalTents"] = "Tents"
      HC_streetcount [HC_streetcount == "totalEncamps"] = "Encampments"


    # Plot a bar graph for number of people in different living arrangement in a particular year
    # The year is given as an input

    p <- HC_streetcount %>%
      filter(year == 2017) %>%
      #filter(year == input$yearStreetCount) %>%
      ggplot(aes(x = reorder(type, count), y = count, fill = count)) +
        geom_bar(stat = "identity", aes(text=sprintf("Unit Type: %s<br>Total # of Units: %s", type, count))) +
        ggtitle("Units Tabulated during Street Count in 2017") +
        ylab("Total Units")+
        xlab("")+
        scale_fill_gradient(low = "coral", high = "coral4",
                            name = "") +
        scale_y_continuous(breaks = seq(0, 5000, 500), labels = seq(0, 5000, 500))+
        theme(plot.title = element_text(size=10, hjust = 0.5),
              plot.subtitle = element_text(size=10, hjust = 0.5),
              axis.title = element_text(size = 10),
              axis.text.x = element_text(size = 6),
              axis.text.y = element_text(size = 6),
              legend.position="none")

    ggplotly(p, tooltip = "text")

  })
  
  #----------------------------------------------------------------------------------------------------------------------------#
  
  ### {PART 3: Tab 311 Reports}
  

  #!------------------! Plot 3 !------------------!
  
  # Plot 3 used in 311 Reports tab
  
  output$plot3 = renderPlotly({
    
    #Plot the number of 311 reports in this year
    
    p <- data_311 %>%
      group_by(month = lubridate::month(data_311$CreatedDate, label = T)) %>%
      summarize(reports = n()) %>%
      filter(month!="Nov") %>%
      ggplot(aes(month, reports, fill = reports)) +
        geom_bar(stat = "identity", aes(text=sprintf("Month: %s<br># of Reports: %s", month, reports))) +
        scale_y_continuous(breaks = seq(0, 2600, 200), labels = seq(0, 2600, 200)) +
        scale_fill_gradient(low = "salmon1", high = "salmon4") +
        xlab("")+
        ylab("Number of Reports") +
        ggtitle("311 Reports from Jan 2017 - Oct 2017") +
        theme(plot.title = element_text(size=10, hjust = 0.5),
              axis.title = element_text(size = 10),
              axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size=6),
              legend.position="none")
    
    ggplotly(p, tooltip = "text")
    
  })
  
  
  
  #!------------------! Plot 4 !------------------!
  
  #Plot 4 used in 311 Reports tab
  
  output$plot4 = renderPlotly({
    
    #Plot the number of 311 reports by different source of reporting
    
    p <- data_311 %>%
      group_by(RequestSource) %>%
      summarize(reports = n()) %>%
      arrange(desc(reports)) %>%
      ggplot(aes(x= reorder(RequestSource, reports), y= reports, fill = reports)) +
        geom_bar(stat = "identity", aes(text=sprintf("Report Source: %s<br># of Reports: %s", RequestSource, reports))) +
        scale_y_continuous(breaks = seq(0, 12000, 1000), labels = seq(0, 12000, 1000)) +
        scale_fill_gradient(low = "salmon1", high = "salmon4") +
        labs(title = "311 Reporting Sources", x = "Report Sources", y ="Number of Reports")+
        theme(plot.title = element_text(size=10, hjust = 0.5),
              axis.title = element_text(size = 10),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 6),
              legend.position="none")
    
    ggplotly(p, tooltip = "text")
      
  })
  
  
  
  #!------------------! Map 1 !------------------!
  
  #Map 1 used in 311 Reports tab
  
  output$map1 = renderLeaflet({ 
    
    # Summarize the number of 311 reports by council dristricts
    summary_311 = data_311 %>%
      filter(!is.na(CD)) %>%
      group_by(CD) %>%
      summarize(reports = n())
    
    
    #Read the shape file
    #Join it with the summary data for 311 reports
    
    
    la_cd_311 = readOGR(dsn="data/l.a. city council district (2012).shp") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
    la_cd_311@data$DISTRICT = as.integer(la_cd_311@data$DISTRICT )
    la_cd_311@data =  full_join(la_cd_311@data, summary_311, by =  c( "DISTRICT" = "CD" ))
    
    
    #Set palette for chloropleth
    
    pal_311 <- colorBin("YlOrBr", domain = la_cd_311@data$reports, bins = 5)
    
    
    #Set labels for interactive cursor hovering
    
    labels_311 <- sprintf("<strong>%s</strong><br/>%g reports",
                          paste("Council District ", la_cd_311@data$DISTRICT), la_cd_311@data$reports) %>% 
      lapply(htmltools::HTML)
    
    
    #Create leaflet for cholorpleth map
    #Map shows numebr of 311 reports for the year 2017 across various council districts
    
    leaflet(data = la_cd_311) %>%
      addTiles()%>%
      setView(lng = -118.247011, lat = 34.040942, zoom = 9) %>%
      #addPolygons(data = la_cd_311) %>%
      addMarkers(data = data_311,
                 lng = ~as.numeric(Longitude), lat = ~as.numeric(Latitude), popup = ~Address, group = "311 Incidents",
                 clusterOptions = markerClusterOptions()) %>%
      
      addPolygons(
        fillColor = ~pal_311(reports),
        weight = 2,
        opacity = 1,
        color = "black",
        dashArray = "3",
        fillOpacity = 0.8,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels_311,
        group = "311 Distribution") %>%
      addLegend(pal = pal_311, values = ~(reports), opacity = 0.7, title = NULL,
                position = "bottomright") %>%
      addLayersControl(
        overlayGroups = c("311 Incidents", "311 Distribution"),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      )
  })
  
  
  
  
  #----------------------------------------------------------------------------------------------------------------------------#
  
  ### {PART 4: Tab Unsheltered Homeless}
  
  
  #!------------------! Map 2 !------------------!
  
  #Map 2 used in Unsheltered Homeless tab
  output$map2 = renderLeaflet({
    
    #create leaflet for cholorpleth map
    #Chloropleth map shows distribution of unsheltered homeless ("totUnsheltPeople") in 2017
    
    
    #Read shape file
    
    la_cd = readOGR(dsn="data/l.a. city council district (2012).shp") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
    la_cd@data$DISTRICT = as.integer(la_cd@data$DISTRICT )
    
    #Merge shape data with 2017 council district data
    la_cd@data =  full_join(la_cd@data, HC_CD[HC_CD$Year=="2017", ], by =  c( "DISTRICT" = "CD" ))
    
    #Set palette for chloropleth map
    pal <- colorBin("YlOrRd", domain = la_cd@data$totUnsheltPeople, 
                    bins = 4)
    
    #Set labels for interactive map when cursor is hovered over council district
    labels <- sprintf(
      "<strong>%s</strong><br/>%g people",
      paste("Council District ", la_cd@data$DISTRICT), la_cd@data$totUnsheltPeople
    ) %>% lapply(htmltools::HTML)
    
    
    # Crate data to show #of unsheltered people / total # of homeless people in 2017
    
    la_cd@data$pct <- la_cd@data$totUnsheltPeople/la_cd@data$totPeople
    
    #Set labels for interactive map when cursor is hovered over council district
     labelsPct <- sprintf(
       "<strong>%s</strong><br/>%s",
       paste("Council District ", la_cd@data$DISTRICT), percent(la_cd@data$pct/1)
     ) %>% lapply(htmltools::HTML)
     
    
    # Determine color palette
    palPct <- colorBin("Blues", la_cd@data$pct, 5, pretty = FALSE)
    
    #Create leaflet map distribution of unsheltered homeless ("totUnsheltPeople") in 2017
    
    leaflet(data = la_cd ) %>%
      addTiles()%>%
      #addPolygons(data = la_cd) %>%
      addPolygons(fillColor = ~pal(totUnsheltPeople),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  group = "# of Unsheltered Homeless People",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels) %>%
      addPolygons(data = la_cd,
                  fillColor = ~palPct(la_cd@data$pct),  
                  fillOpacity = 0.9,         
                  weight = 1.5,    
                  label = labelsPct,
                  group="% of Unsheltered Homeless People") %>%
      addLayersControl(
        overlayGroups = c("# of Unsheltered Homeless People", "% of Unsheltered Homeless People"),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
        ) %>%
      addLegend(pal = pal, values = ~(totUnsheltPeople), opacity = 0.7, title = NULL,
                position = "bottomright") %>%
      addLegend(pal = palPct, values = ~(pct), opacity = 0.7, title = NULL,
                position = "bottomleft")
    
  })


  
  #----------------------------------------------------------------------------------------------------------------------------#
  
  ### {PART 5: Tab Homeless Crime Victims}
  
  
  #!------------------! Map 4 !------------------!
  
  
  # # Determine the tooltips for crime homeless victims heatmap
  # popup_hlVict <- paste0("Area Name: ",
  #                        countVict$name,         #column containing the area names
  #                        "<br>Number of Homeless Victims: ",
  #                        countVict$n)        #column that contains the relative amount data
  
  
  # Plot the choropleth map for Crime Homeless victims - crimeHLVict
  
  output$crime_map <- renderLeaflet({
    
    la_cd = readOGR(dsn="data/l.a. city council district (2012).shp") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
    crimeHLVict_cd_count$DISTRICT = as.integer(crimeHLVict_cd_count$DISTRICT)
    la_cd@data$DISTRICT = as.integer(la_cd@data$DISTRICT)
    # data = full_join(la_cd@data, crimeHLVict_cd_count, by =  c( "DISTRICT" = "DISTRICT" ))
    
    crime_data <- merge(la_cd, crimeHLVict_cd_count, by.x="DISTRICT", by.y="DISTRICT", all.x = TRUE)
    crime_data <- spTransform(crime_data, CRS("+proj=longlat +datum=WGS84 +no_defs")) # to transform the projection
    
    # Determine color palette
    pal <- colorBin("YlOrBr", crime_data$n, 5, pretty = FALSE)
    
    #Set labels for interactive map when cursor is hovered over council district
    labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      paste("Council District ", crime_data$DISTRICT), paste(crime_data$n, " homeless victims")
    ) %>% lapply(htmltools::HTML)
    
    leaflet()%>%
      setView(lng = -118.247011, lat = 34.040942, zoom = 9) %>% 
      addTiles()%>% 
      
      addMarkers(data = crimeHLVict, 
                 lng = ~as.numeric(Longitude), lat = ~as.numeric(Latitude), 
                 popup = ~sprintf("Address: %s<br>Date Occurred:  %s<br>Time Occurred: %s<br>Crime Description: %s", Address, `Date Occurred`, `Time Occurred`, tolower(`Crime Code Description`)), 
                 group = "Homeless Crime Victims",
                 clusterOptions = markerClusterOptions()) %>%
      
      addPolygons(data = crime_data, 
                  fillColor = ~pal(crime_data$n),  
                  fillOpacity = 0.8,         
                  weight = 2,   
                  opacity = 1,
                  dashArray = "3",
                  color = "black",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.8,
                    bringToFront = TRUE),
                  label = labels,
                  group="Homeless Crime Victims Distribution") %>%
      addLegend("bottomright", pal = pal, values = crime_data$n,
                title = "# of homeless victims",
                opacity = 1) %>%
      addLayersControl(
        overlayGroups = c("Homeless Crime Victims", "Homeless Crime Victims Distribution"),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      )
  })
  
  
  #!------------------! Plot 5 !------------------!
  
  #Plot 5 used in Homeless Crime Victims tab
  #Plot shows different crimes against homeless
  
  output$crime_plot = renderPlotly({

    p <- crimeHLVict %>%
      group_by(`Crime Code Description`) %>%
      summarize(crimes= n()) %>%
      arrange(desc(crimes)) %>%
      head(5) %>%
      #head(as.numeric(input$top_n)) %>%
      filter(!is.na(`Crime Code Description`)) %>%
      ggplot(aes(x = reorder(`Crime Code Description`, crimes), y = crimes, fill = crimes)) +
        # geom_text(aes(label=`Crime Code Description`, 
        #             hjust=0), 
        #             position = position_dodge(width=1)) +
        geom_bar(stat = "identity", aes(text=sprintf("Crime Type: %s<br># of Homeless Victims: %s", `Crime Code Description`, crimes))) +
        scale_y_continuous(breaks = seq(0,800, 100), labels = seq(0,800, 100)) +
        scale_fill_gradient(low = "salmon1", high = "salmon4") +
        xlab("Crime Type")+
        ylab("Number of Homeless Victims") +
        # coord_flip() +
        ggtitle("Crimes Against the Homeless") +
        theme(plot.title = element_text(size=10, hjust = 0.5),
              axis.title = element_text(size = 10),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 6),
              legend.position="none")
    
    ggplotly(p, tooltip = "text")
    
  })
  
  
  
  #!------------------! Plot 6 !------------------!
  
  #Plot 6 used in Homeless Crime Victims tab
  #Plot shows most unsafe premises for homeless
  
  output$crimePremisesPlot = renderPlotly({
    
    p <- crimeHLVict %>%
      group_by(`Premise Description`) %>%
      summarize(crimes= n()) %>%
      arrange(desc(crimes)) %>%
      head(5) %>%
      # head(as.numeric(input$topPremise)) %>%
      filter(!is.na(`Premise Description`)) %>%
      ggplot(aes(x = reorder(`Premise Description`, crimes), y = crimes, fill = crimes)) +
        geom_bar(stat = "identity", aes(text=sprintf("Premise: %s<br># of Homeless Victims: %s", `Premise Description`, crimes))) +
        scale_y_continuous(breaks = seq(0,800, 100), labels = seq(0,800, 100)) +
        # coord_flip()+      
        scale_fill_gradient(low = "salmon1", high = "salmon4") +
        xlab("Premises")+
        ylab("Number of Homeless Crime Victims") +
        # coord_flip() +
        ggtitle("Most Unsafe Premises for Homeless") +
        theme(plot.title = element_text(size=10, hjust = 0.5),
              axis.title = element_text(size = 10),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 6),
              legend.position="none")
    
    ggplotly(p, tooltip = "text")
    
    })
  
  
  
  
  #----------------------------------------------------------------------------------------------------------------------------#
  
  ### {PART 6: Tab Homeless Shelters & Services}
  
  
  #!------------------! Map 3 !------------------!
  
  #Map used in Homeless Shelters & Services tab
  
  
  #Council District areas
  la_cd = readOGR(dsn="data/l.a. city council district (2012).shp") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
  # Homeless Shelters and Services
  homeless_SS <- readOGR(dsn="data/Homeless_Shelters_and_Services.shp", layer="Homeless_Shelters_and_Services", encoding="UTF-8")
  # make sure the projection crs are identical
  homeless_SS <- spTransform(homeless_SS, CRS(proj4string(la_cd)))
  # only keep data points that are within LAPD_areas polygon
  homeless_SS <- homeless_SS[la_cd, ]
  
  
  output$shelter_map_old <- renderLeaflet({
    
    #Labels for interactive cursor hovering
    labels <- sprintf("<strong>%s</strong>%g",
                      "Council District ", la_cd@data$DISTRICT) %>% 
      lapply(htmltools::HTML)
    
    #Leaflet Map
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -118.247011, lat = 34.040942, zoom = 9) %>%
      
      # add council districts division map
      addPolygons(data = la_cd,
                  fillColor = "lightblue",
                  fillOpacity = 0.7,         ## how transparent do you want the polygon to be?
                  color = "black",       ## color of borders between areas
                  weight = 1.5,            ## width of borders
                  label= labels,
                  group = "Council Districts")%>% 
      
      # add Homeless Shelters and Services data points
      addCircleMarkers(data = homeless_SS, lng = ~as.numeric(homeless_SS$coords.x1), lat = ~as.numeric(homeless_SS$coords.x2), 
                       popup = paste("Shelter & Service ID", homeless_SS$FID, sep=": "), 
                       radius = 4, color = 'blue', group = "Homeless Shelters and Services") %>%
      
      # add layer control 
      addLayersControl(
        overlayGroups = c("Homeless Shelters and Services", "Council Districts"))
  })
  
  
  #!------------------! Shelters vs Housing !------------------!
  
  #Bar graph to show number of shelters and number of houses
  
  output$overview1 = renderPlot({
    lacity_housing_shelter %>%
      filter(CD %in% c(seq(1,15,1))) %>%
      group_by(shelter_type) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = shelter_type, y = count, fill = shelter_type)) +
      geom_bar(stat ="identity")+
      scale_y_continuous(breaks = seq(0, 220, 10), labels = seq(0, 220, 10)) +
      xlab("")+
      ylab("Number") +
      ggtitle("2017 Housing Inventory Count - LA City") +
      theme(plot.title = element_text(face = "bold", size=12, hjust = 0.5),
             axis.title = element_text(size=12),
             axis.text.x = element_text(size = 12),
             axis.text.y = element_text(size = 12),	
            legend.position = "none")
  })
  
  #!------------------! Housing and Shelters across council districts !------------------!
  
  #Bar graph to show number of shelters across council districts
  
  output$shelter1 = renderPlot({
    
    
    lacity_shelters = if(input$yORs_shelter != "Year-round"){
      lacity_shelters%>%
        filter(shelter_period == "Winter-Shelter")
    } else {
      lacity_shelters
    }
    
    lacity_shelters%>%
      group_by(CD, Proj..Type) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = CD, y = count, fill = Proj..Type)) +
        geom_bar(stat ="identity")+
        scale_y_continuous(breaks = seq(0, 100, 5), labels = seq(0, 100, 5)) +
        scale_x_continuous(breaks = seq(1, 15, 1), labels = seq(1, 15, 1)) +
        scale_fill_manual(name="", values = c("coral1", "dodgerblue"),
                          labels = c("Emergency Shelters", "Transitional Housing")) +
        xlab("Council District")+
        ylab("Number of Shelters") +
        coord_flip()+
        ggtitle("Homeless Shelters across Council Districts") +
        theme(plot.title = element_text(size=12, hjust = 0.5),
              axis.title = element_text(10),
              axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size = 8),				
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 10),
              legend.position = "bottom")
  })
  
  # For Modal Windows - larger graphs
  output$shelter1m = renderPlot({
    
    
    lacity_shelters = if(input$yORs_shelter != "Year-round"){
      lacity_shelters%>%
        filter(shelter_period == "Winter-Shelter")
    } else {
      lacity_shelters
    }
    
    lacity_shelters%>%
      group_by(CD, Proj..Type) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = CD, y = count, fill = Proj..Type)) +
      geom_bar(stat ="identity")+
      scale_y_continuous(breaks = seq(0, 100, 5), labels = seq(0, 100, 5)) +
      scale_x_continuous(breaks = seq(1, 15, 1), labels = seq(1, 15, 1)) +
      scale_fill_manual(name="", values = c("coral1", "dodgerblue"),
                        labels = c("Emergency Shelters", "Transitional Housing")) +
      xlab("Council District")+
      ylab("Number of Shelters") +
      coord_flip()+
      ggtitle("Homeless Shelters across Council Districts") +
      theme(plot.title = element_text(size=18, hjust = 0.5),
            axis.title = element_text(size=16),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),				
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.position = "bottom")
  })
  
  #Bar graph to show number of housing across council districts
  
  output$housing1 = renderPlot({
    lacity_housing%>%
      group_by(CD, Proj..Type) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = CD, y = count, fill = factor(Proj..Type, levels=levels(lacity_housing$Proj..Type)[c(3,4,2,1,5,6)]))) +
      geom_bar(stat ="identity")+
      scale_y_continuous(breaks = seq(0, 100, 5), labels = seq(0, 100, 5)) +
      scale_x_continuous(breaks = seq(1, 15, 1), labels = seq(1, 15, 1)) +
      scale_fill_manual(name="", values = c( "dodgerblue","orange2", "seagreen4"),
                        labels = c("Permanent Supportive Housing", "Rapid Re-Housing", "Others")) +
      xlab("Council District")+
      ylab("Number of Permanent Housing") +
      coord_flip()+
      ggtitle("Permanent Housing across Council Districts") +
      theme(plot.title = element_text(size=12, hjust = 0.5),
            axis.title = element_text(size = 10),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),				
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 10),
            legend.position = "bottom")
  })
  
   # For Modal Windows - larger graphs
  output$housing1m <- renderPlot({
    lacity_housing%>%
      group_by(CD, Proj..Type) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = CD, y = count, fill = factor(Proj..Type, levels=levels(lacity_housing$Proj..Type)[c(3,4,2,1,5,6)]))) +
      geom_bar(stat ="identity")+
      scale_y_continuous(breaks = seq(0, 100, 5), labels = seq(0, 100, 5)) +
      scale_x_continuous(breaks = seq(1, 15, 1), labels = seq(1, 15, 1)) +
      scale_fill_manual(name="", values = c( "dodgerblue","orange2", "seagreen4"),
                        labels = c("Permanent Supportive Housing", "Rapid Re-Housing", "Others")) +
      xlab("Council District")+
      ylab("Number of Permanent Housing") +
      coord_flip()+
      ggtitle("Permanent Housing across Council Districts") +
      theme(plot.title = element_text(size=18, hjust = 0.5),
            axis.title = element_text(size = 16),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),				
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.position = "bottom")
  })
  

  # Type of Shelters: "Emergency Shelters", "Transitional Housing" and 
  # "Winter Shelters", "Year Round Shelters"
  
  
  #Bar Graphs to show different types of shelters 
  
  output$overview2 = renderPlot({
    lacity_shelters %>%
      group_by(Proj..Type, shelter_period) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = Proj..Type, y = count, fill = shelter_period)) +
      geom_bar(stat ="identity")+
      scale_y_continuous(breaks = seq(0, 100, 5), labels = seq(0, 100, 5)) +
      scale_x_discrete(labels = c("Emergency Shelters", "Transitional Housing")) +
      scale_fill_discrete(name="") +
      xlab("")+
      ylab("Number of Shelters") +
      ggtitle("2017 Housing Inventory Count - Shelters in LA City") +
      theme(plot.title = element_text(face = "bold", size=12, hjust = 0.5),
            axis.title = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),				
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.position = "bottom")
  })
  #!------------------! Housing and Shelters related homeless population !------------------!
  
  #Bar graph for sheletered and unsheltered 
  output$overview3 = renderPlot({
    hc_2017_unshelt_shelt%>%
      ggplot(aes(x = CD, y = total_count, fill = type)) +
      geom_bar(stat ="identity")+
      scale_y_continuous(breaks = seq(0, 8000, 500), labels = seq(0, 8000, 500)) +
      scale_x_continuous(breaks = seq(1, 15, 1), labels = seq(1, 15, 1)) +
      scale_fill_manual(name="Homeless Population", values = c( "springgreen4", "orange"),
                        labels = c("Sheltered", "Unsheltered")) +
      xlab("Council District")+
      ylab("Number of Homeless People") +
      coord_flip()+
      ggtitle("Homeless Population across Council Districts") +
      theme(plot.title = element_text(face = "bold", size=12, hjust = 0.5),
            axis.title = element_text(size = 12),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),				
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.position = "bottom")
  })
  
  
  
  #Cholopleth maps for (unshelterd homeless and shelters), and (total homeless and housing) 
  
  #Cholopleth maps for (unshelterd homeless and shelters)

  #Create leaflet map distribution of unsheltered homeless and number of shelters
  
  output$shelter_map <- renderLeaflet({
    
    la_cd = if (input$yORs_shelter == "Year-round") la_cd_year else la_cd_winter
    
    #Set palette for chloropleth map
    pal_unsh <- colorBin("Reds", domain = la_cd@data$totalUnsheltPeople, bins = 5)
    pal_shelters  <- colorBin("Blues", domain = la_cd@data$`Homeless Shelter`, bins = 5)
    
    #Set labels for interactive map when cursor is hovered over council district
    label_unsh <- sprintf("<strong>%s</strong><br/>%g Unsheltered Homeless",
                          paste("Council District ", la_cd@data$DISTRICT), la_cd@data$totalUnsheltPeople) %>% 
      lapply(htmltools::HTML)
    
    label_shelters <- sprintf("<strong>%s</strong><br/>%g Shelter Utilization",
                              paste("Council District ", la_cd@data$DISTRICT), la_cd@data$`Homeless Shelter`) %>% 
      lapply(htmltools::HTML)
    
      leaflet(data = la_cd) %>%
        addTiles()%>%
        addPolygons(fillColor = ~pal_unsh(totalUnsheltPeople),
                    weight = 2,
                    opacity = 1,
                    color = "black",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    group = "# of Unsheltered Homeless People",
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = label_unsh) %>%
        addPolygons(fillColor = ~pal_shelters(la_cd@data$`Homeless Shelter`),
                    fillOpacity = 0.9,
                    weight = 1.5,
                    color = "white",
                    label = label_shelters,
                    group="% Avg. Shelter Utilization Rate") %>%
        addLayersControl(
          #baseGroups = c("Seasonal", "Year-round"),
          overlayGroups = c("# of Unsheltered Homeless People", "% Avg. Shelter Utilization Rate"),
          options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
        ) %>%
        addLegend(pal = pal_unsh, values = ~(totalUnsheltPeople), opacity = 0.7,
                  position = "bottomleft", title = "# of Unsheltered<br>Homeless People") %>%
        addLegend(pal = pal_shelters, values = ~(`Homeless Shelter`),
                  opacity = 0.7, position = "bottomright", title = "% Avg. Shelter<br>Utilization Rate")
    
    
  })
  
  
  
  #Cholopleth maps for total homeless and housing
  
  output$housing_map <- renderLeaflet({
    
    la_cd = la_cd_year
    
    #Set palette for chloropleth map
    pal_homeless <- colorBin("Reds", domain = la_cd@data$totalPeople, bins = 5)
    pal_housing  <- colorBin("Blues", domain = la_cd@data$`Permanent Housing`, bins = 5)
    
    #Set labels for interactive map when cursor is hovered over council district
    label_homeless <- sprintf("<strong>%s</strong><br/>%g Homeless People",
                              paste("Council District ", la_cd@data$DISTRICT), la_cd@data$totalPeople) %>% 
      lapply(htmltools::HTML)
    
    label_housing <- sprintf("<strong>%s</strong><br/>%g Shelter Utilization",
                             paste("Council District ", la_cd@data$DISTRICT), la_cd@data$`Permanent Housing`) %>% 
      lapply(htmltools::HTML)
    
    #Create leaflet map distribution of unsheltered homeless and number of shelters
    
    leaflet(data = la_cd) %>%
      addTiles()%>%
      addPolygons(fillColor = ~pal_homeless(totalPeople),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  group = "# of Homeless People",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = label_homeless) %>%
      addPolygons(data = la_cd_year,
                  fillColor = ~pal_housing(la_cd@data$`Permanent Housing`),  
                  fillOpacity = 0.9,         
                  weight = 1.5,  
                  color = "white",
                  label = label_housing,
                  group="% Avg. Housing Utilization Rate") %>%
      addLayersControl(
        overlayGroups = c("# of Homeless People", "% Avg. Housing Utilization Rate"),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      ) %>%
      addLegend(pal = pal_homeless, values = ~(totalPeople), opacity = 0.7,
                position = "bottomleft", title = "# of Homeless People") %>%
      addLegend(pal = pal_housing, values = ~(`Permanent Housing`), 
                opacity = 0.7, position = "bottomright", title = "% Avg. Housing<br>Utilization Rate")
    
  })
  
  output$housing_map_title <- renderText({
    "Homeless People and Housing Utilization (Year-round)"
  })
  
  
  #Bar graph for total homeless people
  output$overview4 <- renderPlot({
    #plotting the data
    ggplot(health,aes(x=reorder(Indication,Total),
                      y=Total,fill=-Total))+
      geom_bar(stat="identity")+
      coord_flip()+
      xlab("Health/Disability Indication")+
      ylab("Number of People")+
      ggtitle("Health Threats for Homeless People")+
      theme(plot.title = element_text(face = "bold", size=12, hjust = 0.5),
            axis.title = element_text(size=12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),	
            legend.position = "none")
    })
  
  
  
  #Shelters and Housing utilization across council districts
  lacity_shelters$Bed_Occupancy = as.factor(lacity_shelters$Bed_Occupancy)
  
  #Bar graph for shelter utilization
  output$shelter2 <- renderPlot({
    
    lacity_shelters = if(input$yORs_shelter != "Year-round"){
      lacity_shelters%>%
        filter(shelter_period == "Winter-Shelter")
    } else {
      lacity_shelters
    }
    
    lacity_shelters %>%
      group_by(CD, Proj..Type, Bed_Occupancy) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = CD, y = count, fill = factor(Bed_Occupancy, levels = levels(lacity_shelters$Bed_Occupancy)[c(2,1,3)]))) +
      geom_bar(stat ="identity")+
      scale_y_continuous(breaks = seq(0, 100, 5), labels = seq(0, 100, 5)) +
      scale_x_continuous(breaks = seq(1, 15, 1), labels = seq(1, 15, 1)) +
      scale_fill_manual(name="", values = c("red", "orange2", "springgreen4"),
                        labels = c("Under-Utilization", "Moderate-Utilization", "Optimal-Utilization")) +
      xlab("Council District")+
      ylab("Number of Shelters") +
      coord_flip()+
      ggtitle("Shelter Utilization across Council Districts") +
      theme(plot.title = element_text(size=12, hjust = 0.5),
            axis.title = element_text(size = 10),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),				
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 10),
            legend.position = "bottom")
  })
  
  # For Modal Windows - larger graphs
  output$shelter2m <- renderPlot({
    
    lacity_shelters = if(input$yORs_shelter != "Year-round"){
      lacity_shelters%>%
        filter(shelter_period == "Winter-Shelter")
    } else {
      lacity_shelters
    }
    
    lacity_shelters %>%
      group_by(CD, Proj..Type, Bed_Occupancy) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = CD, y = count, fill = factor(Bed_Occupancy, levels = levels(lacity_shelters$Bed_Occupancy)[c(2,1,3)]))) +
      geom_bar(stat ="identity")+
      scale_y_continuous(breaks = seq(0, 100, 5), labels = seq(0, 100, 5)) +
      scale_x_continuous(breaks = seq(1, 15, 1), labels = seq(1, 15, 1)) +
      scale_fill_manual(name="", values = c("red", "orange2", "springgreen4"),
                        labels = c("Under-Utilization", "Moderate-Utilization", "Optimal-Utilization")) +
      xlab("Council District")+
      ylab("Number of Shelters") +
      coord_flip()+
      ggtitle("Shelter Utilization across Council Districts") +
      theme(plot.title = element_text(size=18, hjust = 0.5),
            axis.title = element_text(size = 16),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),				
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.position = "bottom")
  })
  
  #Bar graph for housing utilization
  output$housing2 <- renderPlot({
    lacity_housing %>%
      group_by(CD, Proj..Type, Bed_Occupancy) %>%
      summarize(count = n()) %>%
      filter(!is.na(Bed_Occupancy)) %>%
      ggplot(aes(x = CD, y = count, fill = factor(Bed_Occupancy, levels = levels(lacity_shelters$Bed_Occupancy)[c(2,1,3)]))) +
      geom_bar(stat ="identity")+
      scale_y_continuous(breaks = seq(0, 100, 5), labels = seq(0, 100, 5)) +
      scale_x_continuous(breaks = seq(1, 15, 1), labels = seq(1, 15, 1)) +
      scale_fill_manual(name="", values = c("red", "orange2", "springgreen4"),
                        labels = c("Under-Utilization", "Moderate-Utilization", "Optimal-Utilization")) +
      xlab("Council District")+
      ylab("Number of Houses") +
      coord_flip()+
      ggtitle("Housing Utilization across Council Districts") +
      theme(plot.title = element_text(size=12, hjust = 0.5),
            axis.title = element_text(size = 10),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),				
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 10),
            legend.position = "bottom")
  })
  
  # For Modal Windows - larger graphs
  output$housing2m <- renderPlot({
    lacity_housing %>%
      group_by(CD, Proj..Type, Bed_Occupancy) %>%
      summarize(count = n()) %>%
      filter(!is.na(Bed_Occupancy)) %>%
      ggplot(aes(x = CD, y = count, fill = factor(Bed_Occupancy, levels = levels(lacity_shelters$Bed_Occupancy)[c(2,1,3)]))) +
      geom_bar(stat ="identity")+
      scale_y_continuous(breaks = seq(0, 100, 5), labels = seq(0, 100, 5)) +
      scale_x_continuous(breaks = seq(1, 15, 1), labels = seq(1, 15, 1)) +
      scale_fill_manual(name="", values = c("red", "orange2", "springgreen4"),
                        labels = c("Under-Utilization", "Moderate-Utilization", "Optimal-Utilization")) +
      xlab("Council District")+
      ylab("Number of Houses") +
      coord_flip()+
      ggtitle("Housing Utilization across Council Districts") +
      theme(plot.title = element_text(size=18, hjust = 0.5),
            axis.title = element_text(size = 16),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),				
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.position = "bottom")
  })
  
  # Estimated homelessness density
  # observe the button click to calculate the metric
  observeEvent(input$gen_metric, {
    
    # if all coefficients are 0, show error message
    if(input$pit_wgt == 0 & input$t311_wgt == 0 & input$crime_wgt == 0){
      showNotification("Error: At least one coefficient must be greater than 0", type = "error")  
    }else{
      pit_wgt <- input$pit_wgt
      t311_wgt <- input$t311_wgt
      crime_wgt <- input$crime_wgt
      
      # get the data for all three variables
      
      pit_num = HC2017 %>%
        group_by(CD) %>%
        summarize(totPeople = pit_wgt * sum(totPeople)) %>%
        filter(CD!= "0")
      
      t311_num = data_311 %>%
        filter(!is.na(CD)) %>%
        group_by(CD) %>%
        summarize(reports = t311_wgt * n()) %>%
        filter(CD!= "0")
      
      crime_num <- summarize(crimeHLVict_cd_count, n = crime_wgt * n)

      # create a new variable "estimate" to be the metric value
      
      estimate_num <- t311_num
      colnames(estimate_num)[2] <- "estimate"
      estimate_num$estimate <- pit_num$totPeople + t311_num$reports + crime_num$n
      
      la_cd = readOGR(dsn="data/l.a. city council district (2012).shp") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
      
      la_cd@data$DISTRICT = as.integer(la_cd@data$DISTRICT )
      la_cd@data =  full_join(la_cd@data, estimate_num, by =  c( "DISTRICT" = "CD" ))
      
      # plot the map
      
      output$estimate_map <- renderLeaflet({
        
        # Determine color palette
        estimate_pal <- colorBin("BuGn", estimate_num$estimate, 5, pretty = FALSE)
        
        #Set labels for interactive map when cursor is hovered over council district
        labels <- sprintf(
          "<strong>%s</strong><br/>%s",
          paste("Council District ", la_cd@data$DISTRICT), paste("Estimated Homelessness Density: ", la_cd@data$estimate)
        ) %>% lapply(htmltools::HTML)
        
        leaflet(data = la_cd)%>%
          setView(lng = -118.247011, lat = 34.040942, zoom = 9) %>% 
          addTiles()%>% 
          addPolygons(fillColor = ~estimate_pal(la_cd@data$estimate),  
                      fillOpacity = 0.8,         
                      weight = 2,   
                      opacity = 1,
                      dashArray = "3",
                      color = "black",
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.8,
                        bringToFront = TRUE),
                      label = labels) %>%
          addLegend("bottomright", pal = estimate_pal, values = la_cd@data$estimate,
                    title = "Estimated Homelessness Density",
                    opacity = 1)
        
      })
      
    }
  })
  
})

  
  
  
  
  
  
  
  
  
  
  
  
  
  