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

bib.cast <- cast(bib, geo ~ time , mean, subset= unit == "EUR_HAB")
bib.cast <- bib.cast[, 1:2]


#change columnnames
names(bib.cast)<-c("NUTS_ID", "bip")

#Make quantiles of bib an put it in a factor
bib.quant <- quantile(bib.cast$bip, probs = seq(0, 1, by = 0.2), na.rm = TRUE)

#all other categories available via
(bib.cast$bibc <- classIntervals(bib.cast$bip, n = 5, style = "jenks"))


str(bib.quant)

bib.cast$bipc<-factor(quantcut(bib.cast$bip, q = seq(0, 1, by = 0.2),
                               dig.lab = 6,
                               labels=c("<14600", "14600-<21500", "21500-<26100", "26100-<31300", "31300-<81100")
)
)

