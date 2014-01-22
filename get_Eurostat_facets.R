###################################################################
############### Data Processing for Choropleths####################
###################################################################


library(SmarterPoland)
library(classInt)
library(gtools)
# toc <- grepEurostatTOC("econ")
# head(toc, 20)

#get Data, here: BIP 
options(stringsAsFactors = FALSE)
bib <- getEurostatRCV(kod = "nama_r_e2gdp")

# remove Nuts0, 1, and 3
bib <- bib[nchar(bib$geo)==4,]

#remove non-german nuts
bib <- bib[grep("DE", bib$geo),]

bib.cast <- cast(bib, geo ~ time , mean, subset= unit == "EUR_HAB")

# Percent increase in BIP between 1995 and 2010
bib.cast$bipdyn1 <- bib.cast$"2000"/bib.cast$"1996"
bib.cast$bipdyn2 <- bib.cast$"2005"/bib.cast$"2001"
bib.cast$bipdyn3 <- bib.cast$"2010"/bib.cast$"2006"

bib.cast <- bib.cast[, c("geo", "bipdyn1", "bipdyn2", "bipdyn3")]
bib.cast <- as.data.frame(bib.cast)
bib.cast <- melt(bib.cast, id = "geo")

names(bib.cast)[1] <- "NUTS_ID"

library(car)
bib.cast$yearc = as.numeric(recode(bib.cast$variable, "'bipdyn1'=1; 'bipdyn2'=2; 'bipdyn3'=3;", as.factor.result=FALSE))

#all other categories available via
#(bib.cast$bibc <- classIntervals(bib.cast$bip, n = 5, style = "jenks"))


bib.cast <- ddply(bib.cast, .(yearc), transform, bipc = factor(quantcut(value, q = seq(0, 1, by = 1/3),
                                                                            dig.lab = 6,
                                                                            labels=c("Bottom 33%", " ", "Top 33%") 
                                                                            )))




