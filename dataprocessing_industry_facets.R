###################################################################
############### Data Processing for subplots ######################
###################################################################


library(xlsx)
library(ggmap)
library(plyr)
library(car)


#data files
result_techno1 <- "industry07.xls"
result_techno2 <- "industry12.xls"
result_techno3 <- "industry99.xls"

# Path to data files
path <- "/Users/metzler/Documents/_Projekte/_GCB/2013_Kongresskarten/gcbmaps/data/icaa/"

# set options to prevent automatic factors
options(stringsAsFactors = FALSE)

# Read in xls
techno <- read.xlsx2(paste0(path, result_techno1), 1)
techno <- rbind(techno, read.xlsx2(paste0(path, result_techno1), 2))
# append the others
for (i in c("99", "07", "12")){
  path2 <- paste0("industry", i)
  path2 <- paste0(path2, ".xls")
  techno2 <- read.xlsx2(paste0(path, path2), 1)  
  techno2 <- rbind(techno2, read.xlsx2(paste0(path, path2), 2))
  techno <- rbind(techno, techno2)
}

head(techno)
table(techno$year)

table(techno$city)

#which cities?
cities <- unique(techno$city)

#geocode (uncomment before use)
cities_geo <- geocode(cities)

#merge city name and geocodes
cities <- cbind(cities, cities_geo)
#rename columns
names(cities) <- c("city", "lon", "lat")


#count events per city
eventno <- ddply(techno, .(city), summarize, numbertotal=length(ename))

#join to cities 
cities.event <- join(cities, eventno, by="city")
cities.event$id <- id(cities.event)

#convert year into numeric
techno$year <- as.numeric(techno$year)
 
# make 3 intervals
techno$yearc <- recode(techno$year, "1996:2000=1; 2001:2005=2; 2006:2010=3; else=NA")

# count events per city and interval
eventno.yearc <- ddply(techno, .(city, yearc), summarize, number = length(ename))

#join to cities
citiesn <- join(cities.event, eventno.yearc, by="city")



