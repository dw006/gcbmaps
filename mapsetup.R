##### Install necessary libraries
## we need the following libraries
needlib <- c("ggplot2", "ggmap", "maptools", "rgdal", "gpclib", "plyr", "scales", "gtools")

## which of those are installed?
has   <- needlib %in% rownames(installed.packages())

##install the missing ones
if(any(!has)) install.packages(needlib[!has])

## load the libraries
lapply(needlib,function(x){library(x,character.only=TRUE)}) 

#### Mapping

#set gpclibPermit to true
gpclibPermit()

#choose shp file and extract path
path<-dirname(file.choose())

#read in nuts2
nuts2 = readOGR(dsn=path, layer="n2")
nuts2@data$id = rownames(nuts2@data)
nuts2.points = fortify(nuts2, region="id")
nuts2.df = join(nuts2.points, nuts2@data, by="id")


##test with bip data
#read in csv
bib<-read.csv(file.choose(),dec=".",na.strings="NA",sep=";",header=T, stringsAsFactors=FALSE)
#change columnnames
names(bib)<-c("NUTS_ID","bip")

#
bib$bipc<-factor(quantcut(bib$bip,q=seq(0,1,by=0.2),
                          dig.lab=6,
                          labels=c("<12.640","12.640-<20.600","20.600-<25.120","25.120-<29.600","29.600-<75.900")
                          )
                 )

test<-join(nuts2.df,bib,by="NUTS_ID")




ggplot(test) + 
  aes(long,lat,group=group,fill=bipc) + 
  geom_polygon() +
  geom_path(color="white",size=0.25) +
  coord_equal() +
  scale_fill_brewer("BIP/Einwohner (2009)",palette="PiYG")+
  coord_map(project="lambert",lat0=0,lon0=16.5,ylim=c(34.5,71),xlim=c(-9,44.5))+
  scale_x_continuous(name="",breaks=NULL)+
  scale_y_continuous(name="",breaks=NULL)+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


ggsave(file="bibkarte.png",dpi=600)


