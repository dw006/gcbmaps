###################################################################
############### Data Processing for Mapsetup ######################
###################################################################



##### Install necessary libraries
## we need the following libraries
needlib <- c("ggplot2", "ggmap", "maptools", "rgdal", "gpclib", "plyr", "scales", "gtools", "sp")

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
    

#convert to dataframe for plotting 
nuts2@data$id <- rownames(nuts2@data)
nuts2.points <- fortify(nuts2, region = "id")
nuts2.df <- join(nuts2.points, nuts2@data, by = "id")


#####Add Data

# join the bibtable to shapetable
geodata<-join(nuts2.df, bib.cast, by = "NUTS_ID")


# wait and plot
p <- ggplot(data = geodata) + 
    aes(x = long, y = lat,  group = group) + 
    geom_polygon(aes(fill = bipc)) +
    geom_path(color = "white", size = 0.25) +
    ##Add point layer, displaying circles not dots via shape = 1
    geom_point(data = cities.event, aes(x = lon, y = lat, size = numbertotal, group = lon), shape = 1)+
    coord_equal() +
    scale_fill_brewer("BIP/Einwohner (2010)", palette = "PiYG")+
    coord_map(project = "lambert", lat0 = 0, lon0 = 16.5, ylim = c(44, 58), xlim = c(2, 20))+
    scale_x_continuous(name = "", breaks = NULL)+
    scale_y_continuous(name = "", breaks = NULL)+
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+
    geom_text(data = cities.eventn, aes(x = lon, y = lat, label = id, group = city, size = 6))

p
  
#save
ggsave(file = "bibkarte.svg", dpi = 600)

