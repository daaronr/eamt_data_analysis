### GWWC Feb 22 Facebook Message Test: DV Results Analysis ### 
## Note: No demographics data ## 

rm(list = ls())
setwd("/Users/erinmorrissey/Desktop/EAMT/03.2022 EAMT GWWC messaging study/pivot tables from FB")  
getwd()

packages<-list("dplyr","data.table","tidyr","digest","tidyverse","stringr","lmtest","plm","sandwich", "miceadds","plotrix","quantreg","readxl","psych","pastecs","ggplot2","broom","MASS","corrr","nFactors","GPArotation","reshape2","skimr","sjmisc","modeest","DescTools","plm","lmtest","effectsize","ggeffects","mediation","lavaan","jtools","scales","RColorBrewer")
for (i in packages){
  if(!require(i,character.only = TRUE)) {install.packages(i); require(i,character.only = TRUE)}
}

#### UPLOAD DATA ####

raw_data <- read.csv("gwwc_feb22message_fulldataset_noagegender.csv")

#### DATA CLEANING ####

data<-raw_data

data <- data %>% 
  mutate(theme=
           case_when(grepl("Effe",Ad.Set.Name)~"effectiveness",
                     grepl("Giving",Ad.Set.Name)~"giving more",
                     grepl("Serv",Ad.Set.Name)~"services",
                     grepl("Soci",Ad.Set.Name)~"social proof",
                     grepl("Valu",Ad.Set.Name)~"values"))
data$theme <- factor( data$theme , ordered = FALSE )


data <- data %>% 
  mutate(text=
           case_when(grepl("Give more effectively",Ad.name)~"Give more effectively",
                     grepl("Same donation more impact",Ad.name)~"Same donation more impact",
                     grepl("Effectiveness matters",Ad.name)~"Effectiveness matters",
                     grepl("Do More Good",Ad.name)~"Do More Good",
                     grepl("Save a life each year",Ad.name)~"Save a life each year",
                     grepl("Give more, feel fulfilled",Ad.name)~"Give more, feel fulfilled",
                     grepl("Donate to great charities",Ad.name)~"Donate to great charities",
                     grepl("Find the best charities",Ad.name)~"Find the best charities",
                     grepl("Learn to do good better",Ad.name)~"Learn to do good better",
                     grepl("Give like Nobel laureates",Ad.name)~"Give like Nobel laureates",
                     grepl("Join 8",Ad.name)~"Join 8,000+ givers",
                     grepl("Thousands have max",Ad.name)~"Thousands have maximised their impact",
                     grepl("Are you in a",Ad.name)~"Are you in alignment with your values?",
                     grepl("Live up to your values",Ad.name)~"Live up to your values",
                     grepl("I want a better world",Ad.name)~"I want a better world"))
data$text <- factor( data$text , ordered = FALSE )
data$text <- relevel( data$text, ref="Effectiveness matters")

#Another way to create the text variable
data <- data %>% 
  mutate(theme_text=
           case_when(grepl("Give more effectively",Ad.name)~"effectiveness 1",
                     grepl("Same donation more impact",Ad.name)~"effectiveness 2",
                     grepl("Effectiveness matters",Ad.name)~"effectiveness 3",
                     grepl("Do More Good",Ad.name)~"giving more 1",
                     grepl("Save a life each year",Ad.name)~"giving more 2",
                     grepl("Give more, feel fulfilled",Ad.name)~"giving more 3",
                     grepl("Donate to great charities",Ad.name)~"services 1",
                     grepl("Find the best charities",Ad.name)~"services 2",
                     grepl("Learn to do good better",Ad.name)~"services 3",
                     grepl("Give like Nobel laureates",Ad.name)~"social proof 1",
                     grepl("Join 8",Ad.name)~"social proof 2",
                     grepl("Thousands have max",Ad.name)~"social proof 3",
                     grepl("Are you in a",Ad.name)~"values 1",
                     grepl("Live up to your values",Ad.name)~"values 2",
                     grepl("I want a better world",Ad.name)~"values 3"))
data$theme_text <- factor( data$theme_text , ordered = FALSE )

# create cost adjusted DV
data <- data %>% mutate(costadj_DV= DV/ave.cost.impr)
summary(data$costadj_DV) 

#### DATA SUMMMARY #### 

print(data %>% group_by(theme) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100),n=50)
print(data %>% group_by(theme_text) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100),n=50)

print(data %>% group_by(theme) %>% summarise(n=n(),cost=mean(ave.cost.impr)),n=41)
print(data %>% group_by(theme,theme_text) %>% summarise(n=n(),cost=mean(ave.cost.impr)),n=100)

print(data %>% group_by(theme) %>% summarise(n=n(),cost=mean(costadj_DV)))
print(data %>% group_by(theme_text) %>% summarise(n=n(),cost=mean(costadj_DV)))

#### MODELS ####

## THEME
#no controls
summary(lm(data=data,DV~theme))
#control for cost only
summary(lm(data=data,DV~theme+ave.cost.impr))
#cost adjusted DV
summary(lm(data=data,costadj_DV~theme))
#anova
summary(aov(data=data,costadj_DV~theme))
TukeyHSD(aov(data=data,costadj_DV~theme), conf.level=.95)

## MESSAGES
#no controls
summary(lm(data=data,DV~theme_text))
#control for cost only
summary(lm(data=data,DV~theme_text+ave.cost.impr))
#cost adjusted DV
summary(lm(data=data,costadj_DV~theme_text))
#anova
summary(aov(data=data,costadj_DV~theme_text))
TukeyHSD(aov(data=data,costadj_DV~theme_text), conf.level=.95)


#### CHARTS ####

# CHART 1: Results/$ spent by Theme

print(data %>% filter(ave.cost.impr>0) %>% group_by(theme) %>% summarise(results=mean(costadj_DV),SE=std.error(costadj_DV),n=n()),n=50)
levels(data$theme)

limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
dodge <- position_dodge(width=0.9)

data %>% 
  group_by(theme) %>% 
  summarise(mean_dv = mean(costadj_DV, na.rm=TRUE),
            se_dv = sd(costadj_DV, na.rm=TRUE)/sqrt(n())) %>%
  ggplot(aes(x=theme, y=mean_dv)) +
  geom_bar(stat='identity',fill="#0072B2", position=dodge) +
  geom_errorbar(limits, position=dodge, width=0.05)+
  theme_apa()+
  ylab('Results/$ spent')+
  xlab('Message theme')+
  ggtitle('Results/$ spent by Theme')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none")+
  theme(text=element_text(size=11)) +
  scale_y_continuous(limits = c(0,.6), oob = rescale_none, breaks=seq(0,.6, by=.1)) +
  scale_x_discrete(labels=c("effectiveness","giving more","services","social proof","values"))

# CHART 2: Results/$ spent by Message

print(data %>% filter(ave.cost.impr>0) %>% group_by(theme,theme_text) %>% summarise(results=mean(costadj_DV),SE=std.error(costadj_DV),n=n()),n=50)

limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
dodge <- position_dodge(width=.9)

data %>% 
  group_by(theme,theme_text) %>% 
  summarise(mean_dv = mean(costadj_DV, na.rm=TRUE),
            se_dv = sd(costadj_DV, na.rm=TRUE)/sqrt(n())) %>%
  ggplot(aes(x=theme_text, y=mean_dv, fill=theme)) +
  scale_fill_brewer(palette = "Paired")+
  geom_bar(stat='identity', position=dodge) +
  geom_errorbar(limits, position=dodge, width=0.05)+
  theme_apa()+
  ylab('Results/$ Spent')+
  xlab('Message')+
  ggtitle('Results/$ Spent by Message')+
  theme_apa(legend.font.size = 10,legend.use.title = TRUE)+
  theme(text=element_text(size=12)) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title.x = element_text(margin=margin(10,0,0,0)), 
        axis.title.y = element_text(margin=margin(0,10,0,0)))+
  scale_y_continuous(limits = c(0,1.25), oob = rescale_none, breaks=seq(0,1.25, by=.25)) +
  scale_x_discrete(labels=c("Give more effectively","Same donation more impact","Effectiveness matters","Do More Good","Save a life each year","Give more, feel fulfilled","Donate to great charities","Find the best charities","Learn to do good better","Give like Nobel laureates","Join 8,000+ givers","Thousands have \nmaximised their impact","Are you in alignment \nwith your values?","Live up to your values","I want a better world" ))


