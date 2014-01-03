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
techno <- rbind(techno, read.xlsx2(paste0(path, result_techno1), 2))
# append the others
for (i in c("04", "09", "12")){
  path2 <- paste0("techno", i)
  path2 <- paste0(path2, ".xls")
  techno2 <- read.xlsx2(paste0(path, path2), 1)  
  techno2 <- rbind(techno2, read.xlsx2(paste0(path, path2), 2))
  techno <- rbind(techno, techno2)
}

head(techno)
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

#only for the n-biggest event locations
n <- 15
eventno.n <- eventno[order(eventno$numbertotal, decreasing = TRUE)[1:n],]



#join to cities 
cities.event <- join(cities, eventno, by="city")
cities.event$id <- id(cities.event)

#subset to the n-biggest event locations
cities.eventn <- cities.event[cities.event$city %in% eventno.n$city,]
  
#check geocoding
# qmap("Germany", zoom = 6)+
#   geom_point(data = cities, aes(x = lon, y = lat, size = number), colour = "red")


#convert year into numeric
techno$year <- as.numeric(techno$year)
 
  # cut into 4 equal intervals
  techno$yearc <- cut(techno$year, 4, labels = FALSE)
# count events per city and interval
eventno.yearc <- ddply(techno, .(city, yearc), summarize, number = length(ename))

#join to cities
citiesn <- join(cities.eventn, eventno.yearc, by="city")




#citiesn <- citiesn[, c(-4,-5, -6, -7, -8, -9)]
citiesn.m <- melt(citiesn, measure.vars = "number")

# setup bar chart
tail(citiesn.m, 10)

#remove empty cells
citiesn.m <- citiesn.m[!citiesn.m$city == "",]
citiesn.m <- citiesn.m[!is.na(citiesn.m$lon), ]

 


for (i in citiesn.m$city) {
ggplot(citiesn.m[citiesn.m$city == i,], aes(x = factor(yearc), y = value))+
  geom_bar(stat = "identity")+
  scale_y_continuous(limits=c(0,225))+
  scale_x_discrete(limits = c("1", "2", "3", "4"))+
    theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
    ,axis.title = element_blank()
    ,axis.text.y = element_blank()
    ,axis.ticks.y = element_blank()
    ,axis.text.x = element_blank()
    ,axis.ticks.x = element_blank()
  ) 
file <- paste0("pixtecno/", citiesn.m[citiesn.m$city == i, "id"][1],".svg")
ggsave(file = file)
}



