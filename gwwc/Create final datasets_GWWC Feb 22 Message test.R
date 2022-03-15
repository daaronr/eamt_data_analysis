### GWWC Feb 22 Facebook Message Test: GENERATE FINAL DATA SETS ### 

rm(list = ls())
setwd("/Users/erinmorrissey/Desktop/EAMT/03.2022 EAMT GWWC messaging study/pivot tables from FB")  
getwd()


# Load library
library(car)
library(effects)
library(dplyr)

#### DATASET 1: RESULTS W/O DEMOGRAPHICS ####
### import data
fbdata <- read.csv("GWWC feb 22 test without age gender.csv") 
head(fbdata)
names(fbdata)

### Final datasets N = 48227
sum(fbdata$Impressions)

### data cleaning 
fbdata <- mutate_at(fbdata, c("Results","Impressions","Frequency","Link.clicks","Cost.per.result","Amount.spent..USD.","CPM..cost.per.1.000.impressions.","CPC..cost.per.link.click.","Cost.per.1.000.people.reached","X3.second.video.plays","ThruPlays"), ~replace(., is.na(.), 0)) #replace NA with 0

fbdata$ave.cost.impr <- fbdata[,"Amount.spent..USD."]/fbdata[,"Impressions"]
fbdata$ave.results.impr <- fbdata[,"Results"]/fbdata[,"Impressions"]
fbdata$ave.clicks.impr <- fbdata[,"Link.clicks" ]/ fbdata[,"Impressions"] 
fbdata$ave.vidview.impr <- fbdata[,"X3.second.video.plays" ]/ fbdata[,"Impressions"] 
fbdata$DV <- 0 #add empty column for DV 
fbdata <- fbdata[fbdata$Impressions !=0,] # remove all rows where impressions = zero 

### creating new dataframe with relevant columns 
newdata <- fbdata[,c("Campaign.name","Ad.Set.Name","Ad.name","DV","Frequency","ave.cost.impr","ave.results.impr","ave.clicks.impr","ave.vidview.impr","Results","Impressions")]

f1 <- function(data) {
  nr <- data$Impressions
  nd <- data$Results
  data <- data[rep_len(1L, nr), ] #in rep_len nr is the desired length of the output vector ## note: using 1L instead of "1" makes code run faster
  data$DV <- rep(0:1, c(nr - nd, nd))
  rownames(data) <- NULL
  data
}
f1(newdata[1, ])
res <- lapply(split(newdata, rownames(newdata)), f1)
fulldata1 <- do.call('rbind', res)

write.csv(fulldata1,"gwwc_feb22messsage_fulldataset_noagegender.csv", row.names = FALSE)


#### DATASET 2: DV LINK CLICKS INCLUDING AGE GENDER ####
### import data
fbdata2 <- read.csv("GWWC feb 22 test with age and gender.csv") 
head(fbdata2)
names(fbdata2)

### Final datasets N = 48227
sum(fbdata2$Impressions)

### data cleaning 
fbdata2 <- mutate_at(fbdata2, c("Results","Impressions","Frequency","Link.clicks","Cost.per.result","Amount.spent..USD.","CPM..cost.per.1.000.impressions.","CPC..cost.per.link.click.","Cost.per.1.000.people.reached","X3.second.video.plays","ThruPlays"), ~replace(., is.na(.), 0)) #replace NA with 0

fbdata2$ave.cost.impr <- fbdata2[,"Amount.spent..USD."]/fbdata2[,"Impressions"]
fbdata2$ave.results.impr <- fbdata2[,"Results"]/fbdata2[,"Impressions"]
fbdata2$ave.clicks.impr <- fbdata2[,"Link.clicks" ]/ fbdata2[,"Impressions"] 
fbdata2$ave.vidview.impr <- fbdata2[,"X3.second.video.plays" ]/ fbdata2[,"Impressions"] 
fbdata2$DV <- 0 #add empty column for DV 
fbdata2 <- fbdata2[fbdata2$Impressions !=0,] # remove all rows where impressions = zero 

### creating new dataframe with relevant columns 
newdata2 <- fbdata2[,c("Campaign.name","Ad.Set.Name","Ad.name","Age","Gender","DV","Frequency","ave.cost.impr","ave.results.impr","ave.clicks.impr","ave.vidview.impr","Link.clicks","Impressions")]

f2 <- function(data) {
  nr <- data$Impressions
  nd <- data$Link.clicks
  data <- data[rep_len(1L, nr), ] #in rep_len nr is the desired length of the output vector ## note: using 1L instead of "1" makes code run faster
  data$DV <- rep(0:1, c(nr - nd, nd))
  rownames(data) <- NULL
  data
}
f2(newdata2[1, ])
res <- lapply(split(newdata2, rownames(newdata2)), f2)
fulldata2 <- do.call('rbind', res)

write.csv(fulldata2,"gwwc_feb22messsage_dvlinkclicks.csv", row.names = FALSE)


#### DATASET 3: 3 SEC VIDEO PLAYS INCLUDING AGE GENDER ####
### import data
fbdata3 <- read.csv("GWWC feb 22 test with age and gender.csv") 
head(fbdata3)
names(fbdata3)

### Final datasets N = 48227
sum(fbdata3$Impressions)

### data cleaning 
fbdata3 <- mutate_at(fbdata3, c("Results","Impressions","Frequency","Link.clicks","Cost.per.result","Amount.spent..USD.","CPM..cost.per.1.000.impressions.","CPC..cost.per.link.click.","Cost.per.1.000.people.reached","X3.second.video.plays","ThruPlays"), ~replace(., is.na(.), 0)) #replace NA with 0

fbdata3$ave.cost.impr <- fbdata3[,"Amount.spent..USD."]/fbdata3[,"Impressions"]
fbdata3$ave.results.impr <- fbdata3[,"Results"]/fbdata3[,"Impressions"]
fbdata3$ave.clicks.impr <- fbdata3[,"Link.clicks" ]/ fbdata3[,"Impressions"] 
fbdata3$ave.vidview.impr <- fbdata3[,"X3.second.video.plays" ]/ fbdata3[,"Impressions"] 
fbdata3$DV <- 0 #add empty column for DV 
fbdata3 <- fbdata3[fbdata3$Impressions !=0,] # remove all rows where impressions = zero 

### creating new dataframe with relevant columns 
newdata3 <- fbdata3[,c("Campaign.name","Ad.Set.Name","Ad.name","Age","Gender","DV","Frequency","ave.cost.impr","ave.results.impr","ave.clicks.impr","ave.vidview.impr","X3.second.video.plays","Impressions")]

f3 <- function(data) {
  nr <- data$Impressions
  nd <- data$X3.second.video.plays
  data <- data[rep_len(1L, nr), ] #in rep_len nr is the desired length of the output vector ## note: using 1L instead of "1" makes code run faster
  data$DV <- rep(0:1, c(nr - nd, nd))
  rownames(data) <- NULL
  data
}
f3(newdata3[1, ])
res <- lapply(split(newdata3, rownames(newdata3)), f3)
fulldata3 <- do.call('rbind', res)

write.csv(fulldata3,"gwwc_feb22messsage_dvvideoplays.csv", row.names = FALSE)

