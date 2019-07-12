options(stringsAsFactors = FALSE)
options(scipen=999)

library(tidyverse)
library(eurostat)
library(zoo)

# Read GDP
gdp <- get_eurostat("nama_10_gdp", type = "label", time_format = "num",
                        cache_dir = "eurostat_cache", stringsAsFactors = FALSE) %>%
  filter(unit == "Current prices, million euro", na_item == "Gross domestic product at market prices") %>%
  select(time, geo, values) %>% dplyr::rename(gdp = values) %>% mutate(geo = gsub("^Germany.*", "Germany", geo ))
# Read Liechtenstein GDP 
liech.gdp <- read.table("original_data/liechtenstein_gdp.csv", 
    header = TRUE, skip = 2, quote = "\"", sep = ";", col.names = c("time", 
        "gdp")) %>% mutate(geo = "Liechtenstein") %>% filter(time < 2013)
# Use exchange rates to transform the CHF GDP
liech.gdp <- get_eurostat("ert_bil_eur_a", type = "label", time_format = "num",
                            cache_dir = "eurostat_cache", stringsAsFactors = FALSE)  %>% 
  filter(currency == "Swiss franc", statinfo == "Average") %>% arrange(time) %>% 
  select(time,values) %>% right_join(liech.gdp, by = "time") %>% mutate(gdp = gdp/values) %>% select(-values)
gdp <- rbind(gdp, liech.gdp) %>% arrange(geo,time)

# Read Pop
pop <- get_eurostat("demo_pjan", type = "label", time_format = "num",
                        cache_dir = "eurostat_cache", stringsAsFactors = FALSE) %>%
  filter(age == "Total", sex == "Total") %>%
  select(time, geo, values) %>% dplyr::rename(pop = values) %>% arrange(geo,time) %>%
  mutate(geo = gsub("^Germany.*", "Germany", geo ))


# Read Unemp
unemp <- get_eurostat("une_rt_a", type = "label", time_format = "num",
                      cache_dir = "eurostat_cache", stringsAsFactors = FALSE) %>%
  filter(age == "Total", sex == "Total", unit == "Percentage of active population") %>%
  select(time, geo, values) %>% dplyr::rename(unemp = values)%>% arrange(geo,time) %>% mutate(geo = gsub("^Germany.*", "Germany", geo ))
# Append the Swiss and Liech Data
unemp <- rbind(unemp, read.table("original_data/swiss_unemp.csv", 
    sep = ",", dec = ".", header = TRUE)[, c(2, 1, 3)])
liech.unemp <- read.table("original_data/liechtenstein_unemp.csv", header = TRUE,
                          dec = ".", sep = ";", skip = 2) %>% mutate(geo = "Liechtenstein") %>%
  dplyr::rename(time = Jahr, unemp = Durchschnitt) %>% select(-c(Geschlecht, Heimat)) 
unemp <- rbind(unemp, liech.unemp) %>% arrange(geo, time)


# Read Age Index (not used atm)
age <- get_eurostat("demo_pjanind", type = "label", time_format = "num",
                    cache_dir = "eurostat_cache", stringsAsFactors = FALSE) %>%
  filter(indic_de == "Old dependency ratio 1st variant (population 65 and over to population 15 to64 years)") %>%
  select(time, geo, values) %>% dplyr::rename(oldage = values) %>% mutate(geo = gsub("^Germany.*", "Germany", geo ))

#------------------------------------
# Read data on annual asylum applications (Eurostat)
#------------------------------------
if (FALSE) {
    # Abweichung von ca 400 vom monatlichen. Monatlicher geht bis
    # 2016
    asyl.eu.raw <- read.eurostat("original_data/migr_asyappctza_1_Data.csv", # update path if used
        name = "applic")
    asyl.eu.raw <- lapply(split(asyl.eu.raw, asyl.eu.raw$ASYL_APP, 
        drop = TRUE), function(x) x[, c(1, 2, 8)])
    asyl.eu <- cbind(asyl.eu.raw[[2]], asyl.eu.raw[[1]]$applic)
    colnames(asyl.eu)[3:4] <- c("First", "All")
}
# Get recent data on first and all asylum applications
asyl.eu <- get_eurostat("migr_asyappctzm", type = "label", time_format = "num",
                        cache_dir = "eurostat_cache", stringsAsFactors = FALSE) %>%
  filter(sex == "Total", age == "Total", citizen == "Total") %>% mutate(time = floor(time)) %>% group_by(geo, time, asyl_app) %>% 
  summarise_at(vars(values), sum, na.rm = TRUE) %>% ungroup() %>% complete(geo,time,asyl_app) %>% spread(asyl_app, values) %>%
  dplyr::rename(all = "Asylum applicant", first = "First time applicant") 

# Get monthly old data on first asylum application
asyl.eu.old <- get_eurostat("migr_asyctzm", type = "label", time_format = "num",
                        cache_dir = "eurostat_cache", stringsAsFactors = FALSE) %>%
  filter(citizen == "Total") %>% mutate(time = floor(time)) %>% group_by(geo, time) %>%
  summarise_at(vars(values), sum, na.rm = TRUE) %>% ungroup() %>% complete(geo, time) %>%
  dplyr::rename(first = values)
# Get old yearly data on generall asylum applications
asyl.eu.old <- get_eurostat("migr_asyctz", type = "label", time_format = "num",
                            cache_dir = "eurostat_cache", stringsAsFactors = FALSE) %>%
  filter(citizen == "Total") %>% group_by(geo, time) %>%
  summarise_at(vars(values), sum, na.rm = TRUE) %>% ungroup() %>% complete(geo, time) %>% 
  dplyr::rename(all = values) %>% full_join(asyl.eu.old, by = c("geo", "time"))

asyl.eu <- rbind(asyl.eu, asyl.eu.old) %>% filter(geo != "Total") %>% 
  filter(!grepl("^European", geo)) %>% mutate(geo = gsub("^Germany.*", "Germany", geo )) %>% arrange(geo,time)


#------------------------------------
# Read data on monthly asyl Seekers (UNHCR) 1999-2016
#------------------------------------
asyl.unhcr <- read.table("original_data/unhcr_monthly.csv", 
    sep = ",", header = TRUE, skip = 3, na.strings = "*", stringsAsFactors = FALSE) 
colnames(asyl.unhcr)[c(1, 3, 5)] <- c("geo", "time", "unhcr")
asyl.unhcr <- asyl.unhcr %>% select(c(geo,time,unhcr))%>% mutate(geo = gsub("^Czech.*$", "Czechia", geo)) %>% 
  mutate(geo = gsub("^United Ki.*$", "United Kingdom", 
                    geo)) %>% 
  filter(!grepl("Korea|Macedonia|Serbia|Turkey|USA", geo)) %>% 
  group_by(geo, time) %>% summarise_at(vars(unhcr), sum, na.rm = TRUE) %>% ungroup() %>% arrange(geo, time)

asyl <- left_join(asyl.eu, asyl.unhcr, by = c("geo", "time"))

sum(abs(asyl$first-asyl$all), na.rm = TRUE)/1000000
sum(abs(asyl$first-asyl$unhcr), na.rm = TRUE)/1000000
sum(abs(asyl$all-asyl$unhcr), na.rm = TRUE)/1000000


# Read ACCEPTED asylum seekers Eurostat monthly 2008-2019Q1
accept <- get_eurostat("migr_asydcfstq", type = "label", time_format = "num",
                            cache_dir = "eurostat_cache", stringsAsFactors = FALSE) %>%
  filter(citizen == "Total", sex == "Total", age == "Total", decision == "Total positive decisions") %>% 
  mutate(time = floor(time)) %>% group_by(geo, time) %>%
  summarise_at(vars(values), sum, na.rm = TRUE) %>% ungroup() %>% complete(geo, time) %>%
  dplyr::rename(accept = values) %>% mutate(geo = gsub("^Germany.*", "Germany", geo )) %>% arrange(geo,time) %>%
  dplyr::rename(country = geo, year = time)

#### Merge all ####
alldat <- asyl %>% left_join(gdp, by = c("time", "geo")) %>%
  left_join(unemp, by = c("time", "geo")) %>%
  left_join(pop, by = c("time", "geo")) %>% mutate(gdp.per.capita = (gdp * 1e+06) / pop,
                                                   popr = round(pop/1e+06,2)) %>%
  mutate(firstr = first/popr, allr = all/popr, unhcrr = unhcr/popr) %>%
  group_by(geo) %>% mutate(first.asyleffect = lag(rollapply(firstr, 5, mean, fill = NA, align = "right", partial = TRUE, na.rm = TRUE),1),
                           all.asyleffect = lag(rollapply(allr, 5, mean, fill = NA, align = "right", partial = TRUE, na.rm = TRUE),1),
                           unhcr.asyleffect = lag(rollapply(unhcrr, 5, mean, fill = NA, align = "right", partial = TRUE, na.rm = TRUE),1)) %>% 
  ungroup() %>%
  dplyr::rename(country = geo, year = time)


# Check it!!!!!a!!!#
total <- alldat %>% select(country, year, gdp, unemp, pop, gdp.per.capita,
                           first.asyleffect, all.asyleffect, unhcr.asyleffect)
colnames(total) <- tolower(colnames(total))
# replace NaN values with NA
total[is.na(total)] <- NA


# Make list of different asyl application data-sets
asyl.ls <- lapply(list(first = asyl[, 1:3], all = asyl[, 
   c(1, 2, 4)], unhcr = asyl[,c(1,2,5)]), function(x) {
     colnames(x) <- c("country", "year", "applic")
     return(x)
   })


# Non member states
non.eu <- c("Iceland", "Norway", "Switzerland", "Liechtenstein")
non.dublin <- c("Denmark", "Ireland", "United Kingdom")

rm(age, alldat, asyl, asyl.eu, asyl.eu.old, asyl.unhcr, gdp, liech.gdp, liech.unemp, pop, unemp)
