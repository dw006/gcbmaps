###################################################################
############### Data Processing for Mapsetup ######################
###################################################################


##### Install necessary libraries
## we need the following libraries
needlib <- c("ggplot2", "ggmap", "maptools", "rgdal", "gpclib", "plyr", "scales", "gtools", "sp", "RColorBrewer")

## which of those are installed?
has   <- needlib %in% rownames(installed.packages())

##install the missing ones
if(any(!has)) install.packages(needlib[!has])

## load the libraries
lapply(needlib, function(x) {library(x, character.only=TRUE)}) 

#### Mapping

#set gpclibPermit to true
gpclibPermit()

#choose shp file and extract path
path <- dirname(file.choose())

#read in nuts2
nuts2 <- readOGR(dsn = path, layer = "n2")

    #Check for holes in polygons
    # use checkPolygonsHoles() to make sure that the holes are correctly
    # defined (holes have to be within something else)
    slot(nuts2, "polygons") <- lapply(slot(nuts2, "polygons"), checkPolygonsHoles) 

    # next run unionSpatialPolygons() to merge the Polygons objects that
    # belong to the same name (no probs)
    nuts2_n <- unionSpatialPolygons(nuts2, as.character(nuts2$nuts2))
    IDs <- sapply(nuts2_n@polygons, function(x) x@ID)
    test.df <- data.frame(bib = 1:length(nuts2), NUTS_ID = IDs )
    row.names(test.df) <- test.df$NUTS_ID
    nuts2.df <- SpatialPolygonsDataFrame(nuts2_n, test.df)
    

#choose shp file and extract path
path2 <- dirname(file.choose())

#read in nuts2
nuts0 <- readOGR(dsn = path2, layer = "n0")

#Check for holes in polygons
# use checkPolygonsHoles() to make sure that the holes are correctly
# defined (holes have to be within something else)
slot(nuts0, "polygons") <- lapply(slot(nuts0, "polygons"), checkPolygonsHoles) 
# next run unionSpatialPolygons() to merge the Polygons objects that
# belong to the same name (no probs)
nuts0_n <- unionSpatialPolygons(nuts0, as.character(nuts0$NUTS_ID))
IDs <- sapply(nuts0_n@polygons, function(x) x@ID)
nuts0.points <- fortify(nuts0, region = "NUTS_ID")



#convert to dataframe for plotting 
nuts2@data$id <- rownames(nuts2@data)
nuts2.points <- fortify(nuts2, region = "id")
nuts2.df <- join(nuts2.points, nuts2@data, by = "id")


#####Add Data

# join the bibtable to shapetable
geodata<-join(nuts2.df, bib.cast, by = "NUTS_ID")

#remove Nas, in geodata this leads to empty Brandenburg and Sachsen...
geodata <- na.omit(geodata)
citiesn <- na.omit(citiesn)

citiesn$yearcf <-  factor(citiesn$yearc, labels = c("1996-2000", "2001-2005", "2006-2010"))
geodata$yearcf <-  factor(geodata$yearc, labels = c("1996-2000", "2001-2005", "2006-2010"))

geodata <- geodata[substr(geodata$nuts2,1,2)=="DE",]


# wait and plot
p <- ggplot(data = citiesn) + 
    aes(x = long, y = lat,  group = group) + 
    geom_polygon(data = geodata, alpha = 10/10, aes(fill = reorder(bipc, desc(value))))+
    geom_path(data = geodata, color = "grey", size = 0.25) +
    geom_path(data = nuts0, color = "darkgrey", size = 0.25)+
    ##Add point layer, displaying circles not dots via shape = 1
    geom_point(data = citiesn, aes(x = lon, y = lat, size = number, group = lon), shape = 1, color = "black")+
    coord_equal() +
    scale_size_continuous("Anzahl der Veranstaltungen", range = c(1, 25)) +
    scale_fill_manual("BIP/Einwohner (Dynamik)", values = rev(brewer.pal(3,"Spectral")))+
    coord_map(project = "lambert", lat0 = 0, lon0 = 16.5, ylim = c(46, 55.5), xlim = c(4, 17))+
    scale_x_continuous(name = "", breaks = NULL)+
    scale_y_continuous(name = "", breaks = NULL)+
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
    #geom_text(data = cities.eventn, aes(x = lon, y = lat, label = id, group = city, size = 6))+
    facet_wrap(~yearcf, nrow = 1)
p

head(geodata)
table(geodata$nuts2)

display.brewer.pal(7,"BrBG")
#save
ggsave(file = "bibkarte_industry.svg", dpi = 600)

