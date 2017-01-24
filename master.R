# Autor: njh 
# Szenarien für eine europäische Lösung der Flüchtlingsproblematik

# To-DO:

# GDP Switzerland 2015

# sometimes germany and NL become switched??


rm(list = ls())
options(scipen=999)
# Load the required packages
library(reshape)
library(ggplot2)
library(Cairo)
library(gridExtra)
library(shiny)
library(scales)
library(grid)




# Read and match the data (GDP, POP, Unemployment, Asylum applications, etc...)
source("code/extraFunctions.R")
source("code/readandmatchData.R")
input.list <- list(total = total, asyl = asyl.ls, accept = accept)

ref.plot <- get.ref.plot(input.list, year.range = c(2014,2016),
             which.source = "unhcr",  which.show = "ratio",
             which.idx = "grech")   
#svg(filename = "ratio.svg", width = 12, height = 6, antialias = "default", pointsize = 10)
grid_arrange_shared_legend_out(ref.plot$q1, ref.plot$q2, nrow = 1, ncol = 2)
#dev.off()



all <- get.ref.table(input.list)
all2 <- get.index.data(input.list)

aha <- merge(total, asyl.ls$unhcr, by = c("country", "year"))
aha <- merge(aha, accept, by = c("country", "year"))
x <- log(aha$applic)
y <- aha$accept/aha$applic
col <- log(aha$gdp.per.capita)

qplot(x,y,colour = col, size = aha$unemp) + scale_colour_gradient(limits=c(8, 12), low="red", space="Lab", na.value = "black")
qplot(mpg, wt, data = mtcars, colour = miss) +
  scale_colour_gradient(na.value = "black")

#-------------
# Shiny Input
#------------
year.range <- c(2010, 2016)
which.source <- "first"
show.accept <- TRUE
countries <- c("0")
which.idx <- "grech"
w.pop <- 0.4
w.gdp <- 0.4
w.asyl <- 0.1
w.unemp <- 1 - (w.pop + w.gdp + w.asyl)

#--------------
# Make Plot
#--------------

aha <- get.ref.table(input.list)
aha$good <- aha$anteil.angenommen >= aha$anteil.schluessel
df <- merge(aha, get.index.data(input.list, year.range = 2013), by = c("Land"))

df$gdpper <- df$gdp.per.capita
df$jitper <- jitter(df$gdpper)
df$jitasyl <- jitter(df$unemp)
#df$good <- df$quota.accept>=df$quota.key
ggplot(df, aes(x = gdpper, y = population)) +
  geom_point(data = df, aes(x = jitper, y = population, size = share.accepted, colour = good), alpha = .4)+
  geom_text(data=df, aes(x=jitper, y=population, label = country)) + 
  scale_size(range = c(1,30)) + xlim(15000, 50000)  + guides(colour=FALSE)+ guides(size=FALSE) + 
  scale_y_log10(breaks = c(1,10,100)*1000000, limits = c(400000, 100000000))
  #scale_y_discrete(breaks =1:3 , labels=c("Low","High"," "), limits = c(1, 2))+
  #scale_x_discrete(breaks =1:4 , labels=c("Location A","Location B","Location C","Location D"), limits = c(1,2,3,4))+ 
  #+ theme_bw() #


# Example
require(ggplot2)
zz <- textConnection("Row PowerSource ProductSegment Price Model ManufacturingLocation Quantity
                     1 High SegmentA Low ModA LocationA 5000
                     2 Low SegmentB Low ModB LocationB 25000
                     3 High SegmentC Low ModC LocationC 15000
                     4 Low SegmentD High ModD LocationD 30000
                     5 High SegmentE High ModE LocationA 2500
                     6 Low SegmentA Low ModF LocationB 110000
                     7 High SegmentB Low ModG LocationC 20000
                     8 Low SegmentC Low ModH LocationD 3500
                     9 High SegmentD Low ModI LocationA 65500
                     10 Low SegmentE Low ModJ LocationB 145000
                     11 High SegmentA Low ModK LocationC 15000
                     12 Low SegmentB Low ModL LocationD 5000
                     13 High SegmentC Low ModM LocationA 26000
                     14 Low SegmentD Low ModN LocationB 14000
                     15 High SegmentE Mid ModO LocationC 75000
                     16 Low SegmentA High ModP LocationD 33000
                     17 High SegmentB Low ModQ LocationA 14000
                     18 Low SegmentC Mid ModR LocationB 33000
                     19 High SegmentD High ModS LocationC 95000
                     20 Low SegmentE Low ModT LocationD 4000
                     ")
df2 <- read.table(zz, header= TRUE)
close(zz)
df2



df2$JitCoOr <- jitter(as.numeric(factor(df2$ManufacturingLocation)))
df2$JitCoOrPow <- jitter(as.numeric(factor(df2$PowerSource)))

ggplot(df2, aes(x = ManufacturingLocation, y = PowerSource)) +
  geom_point(data=df2,aes(x=JitCoOr, y=JitCoOrPow,size = Quantity, colour = Price), alpha=.5)+
  geom_text(data=df2,aes(x=JitCoOr, y=JitCoOrPow,label=Model)) + 
  scale_size(range = c(1,50)) +
  scale_y_discrete(breaks =1:3 , labels=c("Low","High"," "), limits = c(1, 2))+
  scale_x_discrete(breaks =1:4 , labels=c("Location A","Location B","Location C","Location D"), limits = c(1,2,3,4))+ 
  theme_bw()


aha <- merge(input.list$total, input.list$accept, by = c("year", "country"))
aha2 <- cbind(aha$accept, aha$gdp, aha$pop, aha$unemp, aha$unhcr.asyleffect)
plot(aha2)
lm.aha <- lm(accept ~ gdp.per.capita + unemp + unhcr.asyleffect, data = aha)