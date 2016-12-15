#########################
#----------------
# Read, correct and match all the data
#---------------
#########################

#----------------------------------
# Read GDP ("Gross domestic product at market prices") 2006-2015 annual values 
#---------------------------------
gdp.raw <- read.eurostat("original_data/nama_10_gdp_1_Data.csv", 
                         name = "GDP")
gdp.aha <- split(gdp.raw, gdp.raw$UNIT)
# Choice!!: "Current prices, million euro" or "Current prices, million Purchasing Power Standards"
gdp <- subset(gdp.raw, UNIT == "Current prices, million euro", 
              select = c("TIME", "GEO", "GDP"))
# Read, convert and append the Liechtenstein GDP data
# 1st Note: There is not data for the Liechtenstein GDP in 2014 & 2015 
# These values will be replace by the GDP from 2013
# 2nd Note: Liechtenstein GDP is in CHF
# --> Average exchange rates from the respective years are used to transform the GDP
liech.gdp <- cbind("Liechtenstein",
                   read.table("original_data/liechtenstein_gdp.csv", 
                                              header = TRUE, skip = 2, quote="\"", 
                                              sep=";", col.names=c("TIME", "GDP")))
# Insert missing GDP values in 2014 & 2015
liech.gdp <- rbind(liech.gdp, liech.gdp[rep(8,2),])
liech.gdp[9:10,2] <- c(2014,2015)
colnames(liech.gdp)[1] <- "GEO"
# Use exchange rates to transform the CHF GDP
liech.gdp$GDP <- liech.gdp$GDP/read.eurostat("original_data/ert_bil_eur_a_1_Data.csv", 
                                             name = "Value")$Value
gdp <- rbind(gdp, liech.gdp)

#----------------------------------------
# Read Population 1991-2015 annual values
#----------------------------------------
pop.raw <- read.eurostat("original_data/demo_pjan_1_Data.csv", name = "POP")
pop <- pop.raw[which(pop.raw$TIME>=1991), c(1,2,5)]

#---------------------------------
# Unemployment 2006-2015 annual
#-------------------------------
unemp.raw <- read.eurostat("original_data/une_rt_a_1_Data.csv", 
                           name = "UNEMPLOYMENT")
unemp <- unemp.raw[, c(1, 2, 6)]
# Append the Swiss data
unemp <- rbind(unemp, read.table("original_data/swiss_unemp.csv", 
                                 sep=",", dec=",", header = TRUE)[,c(2,1,3)])
# Get the Liechtenstein data and append it
liech.unemp <- cbind("Liechtenstein",read.table("original_data/liechtenstein_unemp.csv", 
           header = TRUE, skip = 2, quote="\"", 
           sep=";", col.names=c("TIME", "sex", "origin", "UNEMPLOYMENT"))[,c(1,4)])
colnames(liech.unemp)[1] <- "GEO"
unemp <- rbind(unemp, liech.unemp)
unemp$UNEMPLOYMENT <- as.numeric(as.character(unemp$UNEMPLOYMENT))

#------------------------------------
# Read age index data 2006-2015 annual (not used atm)
#------------------------------------
age.raw <- read.eurostat("original_data/demo_pjanind_1_Data.csv", name = "OLDAGE")
age.raw <- lapply(split(age.raw, age.raw$INDIC_DE, drop = TRUE), function(x)x[,c(1,2,4)])
# Choice
age <- age.raw$`Old dependency ratio 1st variant (population 65 and over to population 15 to64 years)`

#------------------------------------
# Read data on annual asylum applications (Eurostat) 2006-2015
#------------------------------------
if(FALSE){ #Abweichung von ca 400 vom monatlichen. Monatlicher geht bis 2016
  asyl.eu.raw <- read.eurostat("original_data/migr_asyappctza_1_Data.csv", name = "applic")
  asyl.eu.raw <- lapply(split(asyl.eu.raw, asyl.eu.raw$ASYL_APP, drop=TRUE), 
                        function(x) x[,c(1,2,8)])
  asyl.eu <- cbind(asyl.eu.raw[[2]], asyl.eu.raw[[1]]$applic)
  colnames(asyl.eu)[3:4] <- c("First", "All")
}
# Get the monthly data
asyl.eu.raw <- read.eurostat("original_data/migr_asyappctzm_1_Data.csv", name = "applic")
asyl.eu.raw <- split(asyl.eu.raw, asyl.eu.raw$ASYL_APP, drop=FALSE)
asyl.eu.raw <- lapply(asyl.eu.raw,function(x) {
  x <- x[,c(1,2,8)]
  x$TIME <- as.numeric(substr(x$TIME, 1,4))
  return(aggregate(applic ~ GEO+TIME, x, sum, drop = FALSE))
  })
asyl.eu <- merge(asyl.eu.raw[[2]], asyl.eu.raw[[1]], all = TRUE, by = c("GEO", "TIME"))
colnames(asyl.eu)[3:4] <- c("First", "All")

# Get old data on asylum applications (Eurostat)
# Note: Data on First Asylum Applications is monthly. The other is annual
asyl.eu.old.raw <- read.eurostat("original_data/migr_asyctzm_1_Data.csv", name = "First")
asyl.eu.old.raw$TIME <- as.numeric(substr(asyl.eu.old.raw$TIME, 1,4))
asyl.eu.old.raw <- aggregate(First~ TIME + GEO, data = asyl.eu.old.raw, FUN=sum, drop = FALSE)
asyl.eu.old.raw <- merge(asyl.eu.old.raw, 
                         read.eurostat("original_data/migr_asyctz_1_Data.csv", 
                                       name = "All")[,c(1,2,5)], by = c("GEO", "TIME"))

#------------------------------------
# Read data on monthly asyl Seekers (UNHCR) 1999-2016
#------------------------------------
asyl.unhcr.raw <- rbind.data.frame(read.table("original_data/unhcr_monthly.csv", 
                             sep = ",", header = TRUE, 
                             skip = 3, na.strings = "*", stringsAsFactors = FALSE),
                             read.table("original_data/unhcr_monthly_2016_08.csv", 
                                        sep = ",", header = TRUE, 
                                        skip = 3, na.strings = "*", stringsAsFactors = FALSE))
colnames(asyl.unhcr.raw)[c(1,3,5)] <- c("GEO", "TIME", "unhcr")
asyl.unhcr.raw$GEO <- gsub("^Czech.*$", "Czech Republic", asyl.unhcr.raw$GEO)
asyl.unhcr.raw$GEO <- gsub("^United Ki.*$", "United Kingdom", asyl.unhcr.raw$GEO)

# Turn/collapse/aggregate monthly into annual data
asyl.unhcr <- aggregate(unhcr ~ GEO + TIME, asyl.unhcr.raw, sum, drop = FALSE)
attr(asyl.unhcr, "out.attrs") <- NULL

# Check if it matches
#aha <- cbind(sort(unique(asyl.unhcr$GEO)), sort(unique(as.character(asyl.eu$GEO))))
#sum(!is.na(charmatch(sort(unique(asyl.unhcr$GEO)), sort(unique(as.character(asyl.eu$GEO)))))) # Should be 32

asyl.eu.old <- droplevels(merge(asyl.eu.old.raw, 
                     subset(asyl.unhcr, (TIME %in% unique(asyl.eu.old.raw$TIME)) & 
                              (GEO %in% unique(asyl.eu$GEO))), 
                     by = c("GEO", "TIME"), all.y = TRUE)[,1:4])
asyl.eu <- rbind(asyl.eu, asyl.eu.old)
asyl.eu <- asyl.eu[order(asyl.eu$GEO, asyl.eu$TIME),]

#----------------------------------
# Read ACCEPTED asylum seekers Eurostat monthly 2008-2016Q1
#-------------------------------
accept.raw <- read.eurostat("original_data/migr_asydcfstq_1_Data.csv", name = "Accepted")
accept.raw$TIME <- as.numeric(substr(accept.raw$TIME, 1,4))
accept.raw <- aggregate(Accepted~TIME+GEO+DECISION, data = accept.raw, FUN=sum, drop = FALSE)
accept.raw <- lapply(split(accept.raw, accept.raw$DECISION, drop=TRUE),
                     function(x) x[,c(1,2,4)])
# Choice!
accept <- accept.raw$`Total positive decisions`
colnames(accept) <- c("year", "country", "accept")

# Non member states
non.eu <- c("Iceland", "Norway", "Switzerland", "Liechtenstein")
non.dublin <- c("Denmark", "Ireland", "United Kingdom")

###########################
##########################
## Create asylum applications per 1 mil Pop.
# Average over the last 5 years
## For all 3 Variables (EUFIRSt, EUALL, UNHCR)
###########################
#############################
first.eff <- calc.asyleffect(asyl.eu, pop, name.app="First")
all.eff   <- calc.asyleffect(asyl.eu, pop, name.app = "All")
unhcr.eff <- calc.asyleffect(asyl.unhcr, pop, name.app="unhcr")



#-----------------------------------
# Match the data sets
#------------------------------------


# Merge the data for the Indices!
# GDP und Unemployment
total <- merge(gdp, unemp, by = c("TIME", "GEO"))

# + Population
total <- merge(total, pop, by = c("TIME", "GEO"))
total$gdp.per.capita <- (total$GDP*1000000)/total$POP

# + previously calculated asyleffects
total <- merge(total, first.eff, by = c("GEO", "TIME"))
total <- merge(total, all.eff, by = c("GEO", "TIME"))
total <- merge(total, unhcr.eff, by = c("GEO", "TIME"))

# Make list of different asyl application data-sets
asyl.ls <- lapply(list(first = asyl.eu[,1:3], all = asyl.eu[,c(1,2,4)], unhcr = asyl.unhcr),
                  function(x) {
                    colnames(x) <- c("country", "year", "applic")
                    return(x)
                    })

# Check it!!!!!!!!
colnames(total) <- tolower(colnames(total))
colnames(total)[1:4] <- c("country", "year", "gdp", "unemp")

# Remove stuff
rm(age, asyl.eu, asyl.eu.old, asyl.unhcr, asyl.unhcr.raw, asyl.eu.raw, gdp, gdp.raw, accept.raw, age.raw, gdp.aha,
   asyl.eu.old.raw, first.eff, all.eff, unhcr.eff, liech.gdp, liech.unemp, unemp, unemp.raw, pop, pop.raw,
   read.eurostat, checkit)
