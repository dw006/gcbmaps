###################################################################
############### Data Processing for subplots ######################
###################################################################

library(xlsx)
library(ggmap)
library(plyr)

#data files
result_techno1 <- "techno95.xls"
result_techno2 <- "techno04.xls"
result_techno3 <- "techno09.xls"
result_techno4 <- "techno12.xls"
# Path to data files
path <- "/Users/metzler/Documents/_Projekte/_GCB/2013_Kongresskarten/gcbmaps/data/icaa/"

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
  geom_point(data = cities, aes(x = lon, y = lat, size = number), colour = "red")


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
summary(citiesn)

mm <- ddply(citiesn, "cyl", summarise, mmpg = mean(mpg))

ggplot(citiesn, aes(x = factor(yearc), y = number))+
  geom_bar(stat = "identity")

    theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
    ,axis.title = element_blank()
    ,axis.text.y = element_blank()
    ,axis.ticks.y = element_blank()
  ) 
  
ggsave(file = "barchart.svg", dpi = 600)

?ggsave
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))
ggplot(citiesn)+
  geom_point(aes(x = factor(yearc), y = number))

# now bar chart for each city, located on specific coordinates
# see also: http://stackoverflow.com/questions/16028659/plots-on-a-map-using-ggplot2
# see ?geom_subplot
# geom_subplot2d is only for regular grids subplot takes lon/lat and group and plots

##play around with width and size, maybe something is wrong in cut (see above) as warnings() gives an error with "non-overlapping x intervals"
ggplot(citiesn)+
  geom_subplot(aes(lon, lat, group = city, subplot = geom_bar(aes(x = factor(yearc), y = number), width = 10, height = 10, reference = NULL, stat = "identity")))+
  coord_map(project = "lambert", lat0 = 0,lon0 = 10, ylim = c(46, 56),xlim = c(5, 15))


## with a small number of cities it works perfectly
test <- head(citiesn, 30)
ggplot(test)+
  geom_subplot(aes(lon, lat, group = city, subplot = geom_bar(aes(x = factor(yearc), y = number), reference = NULL, stat = "identity")))+
  coord_map(project = "lambert", lat0 = 0,lon0 = 10, ylim = c(46, 56),xlim = c(5, 15))


