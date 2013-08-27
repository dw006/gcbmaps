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

#Make quantiles of bib an put it in a factor
bib$bipc<-factor(quantcut(bib$bip,q=seq(0,1,by=0.2),
                          dig.lab=6,
                          labels=c("<12.640","12.640-<20.600","20.600-<25.120","25.120-<29.600","29.600-<75.900")
                          )
                 )

# join the bibtable to shapetable
test<-join(nuts2.df,bib,by="NUTS_ID")

# wait and plot
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

#save
ggsave(file="bibkarte.png",dpi=600)



####Attention some polygons are displayed wrongly

#KOnvergenz (receiving funds from the EU)
kon<-read.csv(file.choose(),dec=".",na.strings="NA",sep=";",header=T, stringsAsFactors=FALSE)
names(kon)<-c("Country","Region","NUTS_ID","Instr")
kon<-subset(kon,select=c("NUTS_ID","Instr"))

#join
konvergenz<-join(nuts2.df,kon,by="NUTS_ID",type="left")

#add field nuts0 containing the first two letters of NUTS_ID
konvergenz$nuts0<-substr(as.character(konvergenz$NUTS_ID),0,2)

#change all other countries manually
konvergenz$Konvergenz[konvergenz$nuts0=="PL"]<-1
konvergenz$Konvergenz[konvergenz$nuts0=="SI"]<-1
konvergenz$Konvergenz[konvergenz$nuts0=="EE"]<-1
konvergenz$Konvergenz[konvergenz$nuts0=="LV"]<-1
konvergenz$Konvergenz[konvergenz$nuts0=="LT"]<-1
konvergenz$Konvergenz[konvergenz$nuts0=="LT"]<-1

konvergenz$Konvergenz[konvergenz$nuts0=="SE"]<-2
konvergenz$Konvergenz[konvergenz$nuts0=="FI"]<-2
konvergenz$Konvergenz[konvergenz$nuts0=="DK"]<-2

konvergenz$Konvergenz[konvergenz$nuts0=="FR"]<-2
konvergenz$Konvergenz[konvergenz$nuts0=="AT"]<-2
konvergenz$Konvergenz[konvergenz$nuts0=="IT"]<-2
konvergenz$Konvergenz[konvergenz$nuts0=="ES"]<-2
konvergenz$Konvergenz[konvergenz$nuts0=="NL"]<-2
konvergenz$Konvergenz[konvergenz$nuts0=="BE"]<-2
konvergenz$Konvergenz[konvergenz$nuts0=="IE"]<-2
konvergenz$Konvergenz[konvergenz$nuts0=="UK"]<-2
konvergenz$Konvergenz[konvergenz$nuts0=="LU"]<-2

konvergenz$nuts1<-substr(konvergenz$NUTS_ID,0,3)
konvergenz$Konvergenz[konvergenz$nuts0=="DE"]<-1

konvergenz$Konvergenz[konvergenz$nuts1=="DE1"]<-2
konvergenz$Konvergenz[konvergenz$nuts1=="DE2"]<-2

konvergenz$Konvergenz[konvergenz$nuts1=="DE5"]<-2
konvergenz$Konvergenz[konvergenz$nuts1=="DE6"]<-2
konvergenz$Konvergenz[konvergenz$nuts1=="DE7"]<-2
konvergenz$Konvergenz[konvergenz$nuts1=="DE9"]<-2
konvergenz$Konvergenz[konvergenz$nuts1=="DEB"]<-2
konvergenz$Konvergenz[konvergenz$nuts1=="DEA"]<-2



konvergenz[ which(konvergenz$Instr=="1"),"Konvergenz"]<-1


konvergenz[ which(konvergenz$NUTS_ID=="DE30"),"Konvergenz"]<-2
konvergenz[ which(konvergenz$NUTS_ID=="DE3"),"Konvergenz"]<-2


konvergenz[ which(konvergenz$nuts1=="DEC"),"Konvergenz"]<-2
konvergenz[ which(konvergenz$nuts1=="DEF"),"Konvergenz"]<-2


#make factor out of Konvergenz for proper display
konvergenz$Konvergenz<-factor(konvergenz$Konvergenz,labels=c("Konvergenz","Wettbewerbsfähigkeit & \nBeschäftigung"))

#wait and plot
ggplot(konvergenz) +
  aes(long,lat,group=group,fill=Konvergenz)+
  geom_polygon(data=subset(konvergenz,NUTS_ID!="DE3" | NUTS_ID!="DEC" | NUTS_ID!="DEF")) +
  geom_polygon(data=subset(konvergenz,NUTS_ID=="DE3" | NUTS_ID=="DEC" | NUTS_ID=="DEF")) +
  geom_path(color="white",size=0.25) +
  coord_equal() +
  scale_fill_discrete(name="Ziele")+
  coord_map(project="lambert",lat0=0,lon0=16.5,ylim=c(34.5,71),xlim=c(-9,44.5))+
  scale_x_continuous(name="",breaks=NULL)+
  scale_y_continuous(name="",breaks=NULL)+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


