##### 
rm(list = ls()) # clear global environment

library(data.table)
library(tidyr)
library(digest)
library(tidyverse)
library(stringr)
library(lmtest)
library(plm)
library(sandwich)
library(miceadds)
library(quantreg)
library(readxl)
library(psych)
library(dplyr)
library(plotrix)
library(qualtRics)
library(psych)
library(pastecs)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(broom)
library(MASS)
library(corrr)
library(nFactors)
library(GPArotation)
library(reshape2)
library(skimr)
library(sjmisc)
library(modeest)
library(DescTools)
library(plm)
library(lmtest)
library(effectsize)
library(ggeffects)
library(mediation)
library(miceadds)
library(lavaan)
library(jtools)
library(scales)
library(RColorBrewer)

setwd("/Users/erinmorrissey/Desktop/Research_Josh/givingwhatwecan")  
getwd()

#### UPLOAD DATA ####
raw_data <- read.csv("textdata_dv_results.csv")

#### DATA CLEANING ####

data<-raw_data
data$Age <- factor( data$Age , ordered = FALSE )
data$Age <- relevel( data$Age, ref="25-34")
data <- data %>% 
  mutate(audience=
           case_when(grepl("Phil",Ad.Set.Name)~"philanthropy",
                     grepl("Anim",Ad.Set.Name)~"animal",
                     grepl("Clim",Ad.Set.Name)~"climate",
                     grepl("Glob",Ad.Set.Name)~"poverty",
                     grepl("Look",Ad.Set.Name)~"lookalike",
                     grepl("Ret",Ad.Set.Name)~"retargeting",
                     grepl("General",Ad.Set.Name)~"general"))
data <- data %>% 
  mutate(message=
           case_when(grepl("Emotional",Campaign.name)~"Emotional",
                     grepl("Factual",Campaign.name)~"Factual",
                     grepl("Hypercube",Campaign.name)~"Hypercube",
                     grepl("PPCo",Campaign.name)~"PPCo"))

data$audience <- factor( data$audience , ordered = FALSE )
data$audience <- relevel( data$audience, ref="philanthropy")

data$message <- factor( data$message , ordered = FALSE )
data$message <- relevel( data$message, ref="Factual")
#age restriction
data <- data %>% 
  mutate(restriction18_39=
           case_when(grepl("18-39",Ad.Set.Name)|grepl("V3",Ad.Set.Name)~1,
                     TRUE~0))
#age trinary
data <- data %>% 
  mutate(agetrin =
           case_when(Age=="18-24"|Age=="13-17"|Age=="25-34"~1,
                     Age=="35-44"~0,TRUE~-1))

#texts
print(data %>% group_by(Text) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100),n=50)
data <- data %>% 
  mutate(media=
           case_when(grepl("100x greater impact",Text)~"100x impact",
                     grepl("We Can has helped 6,000",Text)~"6000 people",
                     grepl("overwhelming with so many problems in the world",Text)~"overwhelming",
                     grepl("Only 3% of donors give based on charity effectiveness ",Text)~"only 3% research",
                     grepl("Use our free guide to learn",Text)~"learn",
                     grepl("Want to make a bigger difference next year?",Text)~"bigger difference next year/learn",
                     grepl("moved by animal welfare, the climate crisis",Text)~"cause list"))

data$media <- factor( data$media , ordered = FALSE )
data$media <- relevel( data$media, ref="bigger difference next year/learn")

##cost adjusted DV
data <- data %>% mutate(DV_costadj= DV/ave.cost.impr)
data$DV_costadj
summary(data$DV_costadj) 
summary(data$DV)
summary(data$ave.cost.impr)

print(data %>% group_by(Text,media) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100),n=50)


#### DATA SUMMMARY ####
data %>% group_by(Age) %>% summarise(n=n())
data %>% group_by(Gender) %>% summarise(n=n())
print(data %>% group_by(Gender,Age) %>% summarise(n=n(),cost=mean(ave.cost.impr)),n=40)
print(data %>% group_by(Ad.Set.Name) %>% summarise(n=n(),cost=mean(ave.cost.impr)),n=41)
print(data %>% group_by(Campaign.name,Ad.Set.Name) %>% summarise(n=n(),cost=mean(ave.cost.impr)),n=100)
data %>% group_by(audience) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100)
data %>% group_by(message) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100)

#### CHART DATA ####
print(data %>% group_by(media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results),n=50)

#### DEMOGRPAHICS AND COST ONLY ####
#regressions with interactions
summary(lm(data=data,DV~Gender*Age+ave.cost.impr))
summary(lm(data=data,DV~Gender*Age))

#regressions with no interactions
#just demographic, not control
summary(lm(data=data,DV~Gender+Age))
#just demographic, controlling for cost
summary(lm(data=data,DV~Gender+Age+ave.cost.impr))

#means and standard errors for age groups/gender
print(data %>% group_by(Gender,Age) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results),n=50)


#### DEMOGRAPHICS WITH CONTROLS FOR VIDEO AND COST ####

summary(lm(data=data,DV~Gender+Age+ave.cost.impr+Text))

#### AUDIENCES ####

#main effects
summary(lm(data=data,DV~Gender+Age+ave.cost.impr+audience))



  #interactions
  summary(lm(data=data,DV~Gender*audience+ave.cost.impr+Age))
  summary(lm(data=data,DV~Age*audience+ave.cost.impr+Gender))
  
  #means for audience
  print(data %>% group_by(audience) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results),n=50)

  

#### MESSAGES ####
  
#no controls
  summary(lm(data=data,DV~message))
  #control for cost only
  summary(lm(data=data,DV~message+ave.cost.impr))
  #check results with campaign
  summary(lm(data=data,DV~Campaign.name))
  #check results with campaign and cost control
  summary(lm(data=data,DV~Campaign.name+ave.cost.impr))
#with controls  
  summary(lm(data=data,DV~Gender+Age+ave.cost.impr+audience+message))
  
  #interactions
  #with audience
  summary(lm(data=data,DV~message*audience+ave.cost.impr+Age+Gender))
  #with Gender
  summary(lm(data=data,DV~message*Gender+ave.cost.impr+Age+audience))
  #with Age (emotional much worse with ages 65+)
  summary(lm(data=data,DV~message*Age+ave.cost.impr+Age+audience))  
  
#interaction with age and campaign restriction
  summary(lm(data=data,DV~message*agetrin+message*restriction18_39+ave.cost.impr+Age+Gender))
  summary(lm(data=data,DV~message*agetrin+message*restriction18_39+ave.cost.impr))
  #in just early campaigns
  summary(lm(data=subset(data,restriction18_39==0),DV~message*agetrin+ave.cost.impr))
  
  #### MEDIA ####
  #no controls
  summary(lm(data=data,DV~media))
  
    #means and SEs
    data %>% group_by(media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results)
    
  #control for cost only
  summary(lm(data=data,DV~media+ave.cost.impr))
  #with controls  
  summary(lm(data=data,DV~Gender+Age+ave.cost.impr+audience+media+Ad.Set.Name))
  
  #interactions
  summary(lm(data=data,DV~media*Age+media*Gender+media*audience+ave.cost.impr)) 
  
  #with audience
  summary(lm(data=data,DV~media*audience+ave.cost.impr+Age+Gender))
  #means and SEs
  print(data %>% group_by(audience,media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results),n=50)

  #with Gender
  summary(lm(data=data,DV~media*Gender+ave.cost.impr+Age+audience))
  #means and SEs
  data %>% group_by(Gender,media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results)
 

  #with Age (emotional much worse with ages 65+)
  summary(lm(data=data,DV~media*Age+ave.cost.impr+Age+audience))

  #with Ad set name
  summary(lm(data=data,DV~media*Ad.Set.Name+ave.cost.impr+Age+Gender+audience))
  
  #means and SEs
  print(data %>% group_by(Age,media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results),n = 50)
  print(data %>% group_by(audience,Age,media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results),n = 500)
  
  
  #interaction with age and campaign restriction - old people really hated factual long
  summary(lm(data=data,DV~media*agetrin+media*restriction18_39+ave.cost.impr+Age+Gender))
  summary(lm(data=data,DV~media*agetrin+media*restriction18_39+ave.cost.impr))
  #in just early campaigns
  summary(lm(data=subset(data,restriction18_39==0),DV~media*agetrin+ave.cost.impr))
  #means and SEs
  data %>% filter(restriction18_39==0) %>% group_by(Age,media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results)
  

####### PLOTS #############################
  
   ###### PLOT 1: Cost adjusted DV (Results) by text ####
  
  #check data
  print(data %>% filter(ave.cost.impr>0) %>% group_by(media) %>% summarise(results=mean(DV_costadj),SE=std.error(DV_costadj),n=n()),n=50)
  
  limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
  dodge <- position_dodge(width=0.9)
  
  data %>% 
    group_by(media) %>% 
    summarise(mean_dv = mean(DV_costadj, na.rm=TRUE),
              se_dv = sd(DV_costadj, na.rm=TRUE)/sqrt(n())) %>%
    ggplot(aes(x=media, y=mean_dv)) +
    geom_bar(stat='identity', fill='gray48',position=dodge) +
    geom_errorbar(limits, position=dodge, width=0.05)+
    theme_apa()+
    ylab('Results/$ spent')+
    xlab('Text')+
    ggtitle('Results/$ spent by Text')+
    theme(legend.position="none")+
    geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
    theme(text=element_text(size=11)) +
    scale_y_continuous(limits = c(0,.15), oob = rescale_none, breaks=seq(0,.15, by=.05)) +
    scale_x_discrete(labels=c("bigger difference \nnext year","100x impact","6000 people","cause list","learn","only 3% \nresearch","overwhelming" ))
  
  
  ##### PLOT 2: DV (Results) by text ####
 
  #check data
  print(data %>% filter(ave.cost.impr>0) %>% group_by(media) %>% summarise(results=100*mean(DV),SE=100*std.error(DV),n=n()),n=50)
  
  limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
  dodge <- position_dodge(width=0.9)
  
  data %>%
    group_by(media) %>% 
    summarise(mean_dv = 100*mean(DV, na.rm=TRUE),
              se_dv = 100*sd(DV, na.rm=TRUE)/sqrt(n())) %>%
    ggplot(aes(x=media, y=mean_dv)) +
    geom_bar(stat='identity', fill="gray48", position=dodge) +
    geom_errorbar(limits, position=dodge, width=0.05)+
    theme_apa()+
    ylab('Results (%)')+
    xlab('Text')+
    ggtitle('Results by Text')+
    theme(legend.position="none")+
    geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
    theme(text=element_text(size=11)) +
    scale_y_continuous(limits = c(0,.3), oob = rescale_none, breaks=seq(0,.3, by=.05)) +
    scale_x_discrete(labels=c("bigger difference \nnext year","100x impact","6000 people","cause list","learn","only 3% \nresearch","overwhelming" ))
  
  
  
  
  ##### PLOT 3: Cost adjusted DV (Results) by text and audience #####
 
  levels(data$media) <- c("bigger difference \nnext year","100x impact","6,000 people","cause list","learn","only 3% \nresearch","overwhelming")
  
  print(data %>% filter(ave.cost.impr>0 & audience !="retargeting") %>% group_by(media,audience) %>% summarise(results=mean(DV_costadj),SE=std.error(DV_costadj),n=n()),n=50)
  
  limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
  dodge <- position_dodge(width=0.9)
  
  data %>% filter(ave.cost.impr>0 & audience !="retargeting") %>%
    group_by(media, audience) %>% 
    summarise(mean_dv = mean(DV_costadj, na.rm=TRUE),
              se_dv = sd(DV_costadj, na.rm=TRUE)/sqrt(n())) %>%
    ggplot(aes(x=audience, y=mean_dv, group=media, fill=media)) +
    labs(fill="Text")+
    scale_fill_brewer(palette="RdYlBu")+
    geom_bar(stat='identity', position=dodge) +
    geom_errorbar(limits, position=dodge, width=0.05)+
    theme_apa()+
    ylab('Results/$ spent')+
    xlab('Audience')+
    ggtitle('Results/$ spent by Text and Audience')+
    theme_apa(legend.font.size = 8,legend.use.title = TRUE)+
    geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
    theme(text=element_text(size=10)) +
    scale_y_continuous(limits = c(0,.75), oob = rescale_none, breaks=seq(0,.75, by=.25)) +
    scale_x_discrete(labels=c("philanthropy","animal"   ,"climate","general","lookalike","poverty"))
  
  
  
  
  ##### PLOT 4: DV (Results) by text and audience #####
  ##### Questions: Same issue as plot 3 with size of overwhelming climate
  
  print(data %>% filter(ave.cost.impr>0 & audience !="retargeting") %>% group_by(media,audience) %>% summarise(results=100*mean(DV),SE=100*std.error(DV),n=n()),n=50)
  
  limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
  dodge <- position_dodge(width=0.9)
  
  data %>% filter(ave.cost.impr>0 & audience !="retargeting") %>%
    group_by(media, audience) %>% 
    summarise(mean_dv = 100*mean(DV, na.rm=TRUE),
              se_dv = 100*sd(DV, na.rm=TRUE)/sqrt(n())) %>%
    ggplot(aes(x=audience, y=mean_dv, group=media, fill=media)) +
    labs(fill="Text")+
    scale_fill_brewer(palette="RdYlBu")+
    geom_bar(stat='identity', position=dodge) +
    geom_errorbar(limits, position=dodge, width=0.05)+
    theme_apa()+
    ylab('Results (%)')+
    xlab('Audience')+
    ggtitle('Results by Text and Audience')+
    theme_apa(legend.font.size = 8,legend.use.title = TRUE)+
    geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
    theme(text=element_text(size=10)) +
    scale_y_continuous(limits = c(0,.92), oob = rescale_none, breaks=seq(0,.92, by=.1)) +
    scale_x_discrete(labels=c("philanthropy","animal"   ,"climate","general","lookalike","poverty"))


  