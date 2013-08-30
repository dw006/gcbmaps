library(xlsx)
library(ggmap)
library(plyr)


####ICCA Database
#Automotive in Germany
subject <- c("Technology/Engineering/Automobiles", "Transport & Communication/Road", "Transport & Communication/Road/Cars & Trucks", "Transport & Communication/Road/Vehicles")
country <- c("Austria/Germany", "Belgium/Germany", "Belgium/Germany/Netherlands", "Czech Republic/Germany/United Arab Emirates", "France/Germany", "France/Germany/Switzerland", "Germany", "Germany/Lithuania/Poland", "Germany/Luxembourg", "Germany/Netherlands", "Germany/Norway", "Germany/Poland", "Germany/Russia", "Germany/Sweden", "Germany/Switzerland", "Germany/United Kingdom")
result_auto <- "automotive.xls"

#Technology in Germany
#1
subject1 <- c("Technology")
country1 <- c("Austria/Germany", "Belgium/Germany", "Belgium/Germany/Netherlands", "Czech Republic/Germany/United Arab Emirates", "France/Germany", "France/Germany/Switzerland", "Germany", "Germany/Lithuania/Poland", "Germany/Luxembourg", "Germany/Netherlands", "Germany/Norway", "Germany/Poland", "Germany/Russia", "Germany/Sweden", "Germany/Switzerland", "Germany/United Kingdom")
result_techno1 <- "techno95.xls"
calender_before1 <-"01 Jan 1995"

#2
subject2 <- c("Technology")
country2 <- c("Austria/Germany", "Belgium/Germany", "Belgium/Germany/Netherlands", "Czech Republic/Germany/United Arab Emirates", "France/Germany", "France/Germany/Switzerland", "Germany", "Germany/Lithuania/Poland", "Germany/Luxembourg", "Germany/Netherlands", "Germany/Norway", "Germany/Poland", "Germany/Russia", "Germany/Sweden", "Germany/Switzerland", "Germany/United Kingdom")
result_techno2 <- "techno04.xls"
calender_before2 <-"02 Jan 1995"
calender_after2 <- "01 Jan 2004"

#3
subject3 <- c("Technology")
country3 <- c("Austria/Germany", "Belgium/Germany", "Belgium/Germany/Netherlands", "Czech Republic/Germany/United Arab Emirates", "France/Germany", "France/Germany/Switzerland", "Germany", "Germany/Lithuania/Poland", "Germany/Luxembourg", "Germany/Netherlands", "Germany/Norway", "Germany/Poland", "Germany/Russia", "Germany/Sweden", "Germany/Switzerland", "Germany/United Kingdom")
result_techno3 <- "techno09.xls"
calender_before3 <-"02 Jan 2004"
calender_after3 <- "01 Jan 2009"

#4
subject4 <- c("Technology")
country4 <- c("Austria/Germany", "Belgium/Germany", "Belgium/Germany/Netherlands", "Czech Republic/Germany/United Arab Emirates", "France/Germany", "France/Germany/Switzerland", "Germany", "Germany/Lithuania/Poland", "Germany/Luxembourg", "Germany/Netherlands", "Germany/Norway", "Germany/Poland", "Germany/Russia", "Germany/Sweden", "Germany/Switzerland", "Germany/United Kingdom")
result_techno4 <- "techno12.xls"
calender_before4 <-"02 Jan 2004"
calender_after4 <- "31 Dec 2012"

# Path to data files
path <- "/Users/dmetzler/Documents/_Projekte/_GCB/2013_Kongresskarten/gcbmaps/data/"

# set options to prevent automatic factors
options(stringsAsFactors = FALSE)

# Read in xls
techno <- read.xlsx2(paste0(path, result_techno1), 1)

# append the others
for (i in c("04", "09", "12")){
  path2 <- paste0("techno", i)
  path2 <- paste0(path2, ".xls")
  techno2 <- read.xlsx2(paste0(path, path2), 1)  
  techno <- rbind(techno, techno2)
}

head(techno)
table(techno$city)

#which cities?
cities <- unique(techno$city)

#geocode (uncomment before use)
#cities_geo <- geocode(cities)

#merge city name and geocodes
cities <- cbind(cities, cities_geo)
#rename columns
names(cities) <- c("city", "lon", "lat")


#count events per city
eventno <- ddply(techno, .(city), summarize, number=length(ename))
#join to cities
cities <- join(cities, eventno, by="city")

#check geocoding
qmap("Germany", zoom = 6)+
  geom_point(data = cities, aes(x = lon, y = lat, colour = "red", size = number))


#convert year into numeric
techno$year <- as.numeric(techno$year)
  
  # cut into 4 equal intervals
  techno$yearc <- cut(techno$year, 4)

# count events per city and interval
eventnoc <- ddply(techno, .(city, yearc), summarize, number=length(ename))

#join to cities
citiesn <- join(cities, eventnoc, by="city")
citiesn <- citiesn[, -4]
head(citiesn)

# wait and plot
p <- ggplot(test) + 
  aes(long, lat, group = group, fill = bipc) + 
  geom_polygon() +
  geom_path(color = "white", size = 0.25) +
  coord_equal() +
  scale_fill_brewer("BIP/Einwohner (2009)", palette = "PiYG")+
  #changed the projection and zoom
  coord_map(project = "lambert", lat0 = 0,lon0 = 10, ylim = c(46, 56),xlim = c(5, 15))+
  scale_x_continuous(name = "", breaks = NULL)+
  scale_y_continuous(name = "", breaks = NULL)+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

#add points 
p+geom_point(aes(x = lon, y = lat, group = NULL, fill = NULL, size = number), data = cities)



#add barchart 
library(ggsubplot)

#example from ggsublot manual - works (scatterplot on specific coordinates)
head(nasa)
ggplot(nasa)+
  geom_subplot(aes(long, lat, group = id,subplot = geom_point(aes(surftemp, temperature), size = 1/4)))+
  coord_map()


# setup bar chart - works
ggplot(citiesn)+
  geom_bar(aes(x = factor(yearc), y = number), stat = "identity")

# now bar chart for each city, located on specific coordinates
# see also: http://stackoverflow.com/questions/16028659/plots-on-a-map-using-ggplot2
ggplot(citiesn)+
  geom_subplot2d(aes(lon, lat, group = city, subplot = geom_bar(aes(x = factor(yearc), y = number, stat = "identity"), size = 100, ref = NULL)))+
  coord_map(project = "lambert", lat0 = 0,lon0 = 10, ylim = c(46, 56),xlim = c(5, 15))



