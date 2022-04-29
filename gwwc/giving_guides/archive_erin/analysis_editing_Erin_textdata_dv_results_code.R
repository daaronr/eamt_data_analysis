
print(data %>% group_by(Video,media) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100),n=50)

##cost adjusted DV
data <- data %>% mutate(DV_costadj= DV/ave.cost.impr)
data$DV_costadj
summary(data$DV_costadj)
summary(data$DV)
summary(data$ave.cost.impr)


#### DATA SUMMMARY ####
data %>% group_by(Age) %>% summarise(n=n())
data %>% group_by(Gender) %>% summarise(n=n())
print(data %>% group_by(Gender,Age) %>% summarise(n=n(),cost=mean(ave.cost.impr)),n=40)
print(data %>% group_by(Ad.Set.Name) %>% summarise(n=n(),cost=mean(ave.cost.impr)),n=41)
print(data %>% group_by(Campaign.name,Ad.Set.Name) %>% summarise(n=n(),cost=mean(ave.cost.impr)),n=100)
data %>% group_by(audience) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100)
data %>% group_by(message) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100)

#### CHART DATA ####
print(data %>% group_by(audience,media) %>% summarise(results=mean(DV)*100,SE=plotrix::std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results),n=50)

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

hist(data$DV_costadj[data$DV_costadj>0])
hist(data$DV_costadj[data$DV_costadj>=0])

#### DEMOGRAPHICS WITH CONTROLS FOR VIDEO AND COST ####

summary(lm(data=data,DV~Gender+Age+ave.cost.impr))
summary(lm(data = data, DV_costadj~Gender+Age))

#### AUDIENCES ####

#main effects
summary(lm(data=data,DV~Gender+Age+ave.cost.impr+audience))
###NEW DV
summary(lm(data=data,DV_costadj~Gender+Age+audience))

  #interactions
  summary(lm(data=data,DV~Gender*audience+ave.cost.impr+Age))
    ## NEW DV
    summary(lm(data=data,DV_costadj~Gender*audience+Age))
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
    ## NEW DV
    summary(lm(data=data,DV_costadj~media))####THIS IS GOOD?

  #with controls
  summary(lm(data=data,DV~Gender+Age+ave.cost.impr+audience+media))
    ### NEW DV
    summary(lm(data=data,DV_costadj~Gender+Age+audience+media))

  #interactions
  summary(lm(data=data,DV~media*Age+media*Gender+media*audience+ave.cost.impr))
  summary(lm(data=data,DV_costadj~media*Age+media*Gender+media*audience))
  summary(lm(data=data,DV~media*Age+media*Gender+media*audience))
  summary(lm(data=data,DV_costadj~Age+Gender+media*audience))

  #with audience
  summary(lm(data=data,DV~media*audience+ave.cost.impr+Age+Gender))
  #means and SEs
  print(data %>% group_by(audience,media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results),n=50)

  print(data %>% filter(data$ave.cost.impr>0) %>% group_by(audience,media) %>% summarise(results=mean(DV_costadj),SE=std.error(DV_costadj),n=n()),n=50)

      #NEW DV with audience
      summary(lm(data=data,DV_costadj~media*audience+Age+Gender))
      #means and SEs
      print(data %>% group_by(audience,media) %>% summarise(results=mean(DV_costadj)*100,SE=std.error(DV_costadj)*100,n=n()),n=50)

  #with Gender
  summary(lm(data=data,DV~media*Gender+ave.cost.impr+Age+audience))
  #means and SEs
  data %>% group_by(Gender,media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results)


  #with Age (emotional much worse with ages 65+)
  summary(lm(data=data,DV~media*Age+ave.cost.impr+Age+audience))


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




##############



#### DEMOGRPAHICS AND COST ONLY ####
#regressions with interactions
summary(lm(data=data,DV_costadj~Gender*Age)) #no significant effects

#regressions with no interactions
#just demographic, not control
summary(lm(data=data,DV_costadj~Gender+Age))

#means and standard errors for age groups/gender
print(data %>% group_by(Gender,Age) %>% summarise(results=mean(DV_costadj)*100,SE=std.error(DV_costadj)*100,n=n()),n=50) ##idk if the changes i made to this line make sense


#### DEMOGRAPHICS WITH CONTROLS FOR VIDEO ####

summary(lm(data=data,DV_costadj~Gender+Age+Video))

#### AUDIENCES ####

#main effects
summary(lm(data=data,DV_costadj~Gender+Age+audience))

#interactions
summary(lm(data=data,DV_costadj~Gender*audience+Age))
summary(lm(data=data,DV_costadj~Age*audience+Gender))

#means for audience
print(data %>% group_by(audience) %>% summarise(results=mean(DV_costadj)*100,SE=std.error(DV_costadj)*100,n=n()),n=50) #Why all NaN?



#### MESSAGES ####

#no controls
summary(lm(data=data,DV_costadj~message))
summary(lm(data=data,DV~message+ave.cost.impr))
#check results with campaign
summary(lm(data=data,DV_costadj~Campaign.name))
#with controls
summary(lm(data=data,DV_costadj~Gender+Age+audience+message))

#interactions
#with audience
summary(lm(data=data,DV_costadj~message*audience+Age+Gender))
#with Gender
summary(lm(data=data,DV_costadj~message*Gender+Age+audience))
#with Age (emotional much worse with ages 65+)
summary(lm(data=data,DV_costadj~message*Age+Age+audience))

#interaction with age and campaign restriction
summary(lm(data=data,DV_costadj~message*agetrin+message*restriction18_39+Age+Gender))
summary(lm(data=data,DV_costadj~message*agetrin+message*restriction18_39))
#in just early campaigns
summary(lm(data=subset(data,restriction18_39==0),DV_costadj~message*agetrin))

#### MEDIA ####
#no controls
summary(lm(data=data,DV_costadj~media))

#means and SEs
data %>% filter(ave.cost.impr > 0) %>% group_by(media) %>% summarise(mean(DV_costadj),results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results, RPC = results/cost) ##idk how to do this line?

#with controls
summary(lm(data=data,DV_costadj~Gender+Age+audience+media))

#interactions
summary(lm(data=data,DV_costadj~media*Age+media*Gender+media*audience))

#with audience
summary(lm(data=data,DV_costadj~media*audience+Age+Gender))

#means and SEs
print(data %>% group_by(audience,media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results),n=50) ###skipped updating

#with Gender
summary(lm(data=data,DV_costadj~media*Gender+Age+audience))
#means and SEs
data %>% group_by(Gender,media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results) #####skipped updating


#with Age (emotional much worse with ages 65+) ### don't see the effect of emotional being much worse with ages 65+
summary(lm(data=data,DV_costadj~media*Age+Age+audience))


#means and SEs
print(data %>% group_by(Age,media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results),n = 50) ###idk how to update this
print(data %>% group_by(audience,Age,media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results),n = 500) ###idk how to update this


#interaction with age and campaign restriction - old people really hated factual long
summary(lm(data=data,DV_costadj~media*agetrin+media*restriction18_39+Age+Gender))
summary(lm(data=data,DV_costadj~media*agetrin+media*restriction18_39))
#in just early campaigns
summary(lm(data=subset(data,restriction18_39==0),DV_costadj~media*agetrin))
#means and SEs
data %>% filter(restriction18_39==0) %>% group_by(Age,media) %>% summarise(results=mean(DV)*100,SE=std.error(DV)*100,n=n(),cost=mean(ave.cost.impr),CPR=cost/results)



####### PLOTS #############################

###### PLOT 1: Cost adjusted DV (results) by video ####

#check data
print(data %>% filter(ave.cost.impr>0) %>% group_by(media) %>% summarise(results=mean(DV_costadj),SE=std.error(DV_costadj),n=n()),n=50)

limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
dodge <- position_dodge(width=0.9)

data %>%
  group_by(media) %>%
  summarise(mean_dv = mean(DV_costadj, na.rm=TRUE),
            se_dv = sd(DV_costadj, na.rm=TRUE)/sqrt(n())) %>%
  ggplot(aes(x=media, y=mean_dv)) +
  geom_bar(stat='identity',fill="#0072B2", position=dodge) +
  geom_errorbar(limits, position=dodge, width=0.05)+
  theme_apa()+
  ylab('Results/$ spent')+
  xlab('Video')+
  ggtitle('Results/$ spent by Video')+
  theme(legend.position="none")+
  geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
  theme(text=element_text(size=11)) +
  scale_y_continuous(limits = c(0,.2), oob = rescale_none, breaks=seq(0,.2, by=.05)) +
  scale_x_discrete(labels=c("factual short","animal","climate","factual long","hypercube","poverty"))



##### PLOT 2: DV (Results) by video ####

#check data
print(data %>% filter(ave.cost.impr>0) %>% group_by(media) %>% summarise(results=100*mean(DV),SE=100*std.error(DV),n=n()),n=50)

limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
dodge <- position_dodge(width=0.9)

data %>%
  group_by(media) %>%
  summarise(mean_dv = 100*mean(DV, na.rm=TRUE),
            se_dv = 100*sd(DV, na.rm=TRUE)/sqrt(n())) %>%
  ggplot(aes(x=media, y=mean_dv)) +
  geom_bar(stat='identity', fill="#0072B2",position=dodge) +
  geom_errorbar(limits, position=dodge, width=0.05)+
  theme_apa()+
  ylab('Results (%)')+
  xlab('Video')+
  ggtitle('Results by Video')+
  theme(legend.position="none")+
  geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
  theme(text=element_text(size=11)) +
  scale_y_continuous(limits = c(0,.3), oob = rescale_none, breaks=seq(0,.3, by=.05)) +
  scale_x_discrete(labels=c("factual short","animal","climate","factual long","hypercube","poverty"))



##### PLOT 3: Cost adjusted DV (results) by video and audience #####
##### Questions/Notes: Removed the retargeting audience

print(data %>% filter(ave.cost.impr>0 & audience !="retargeting") %>% group_by(media,audience) %>% summarise(results=mean(DV_costadj),SE=std.error(DV_costadj),n=n()),n=50)

limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
dodge <- position_dodge(width=0.9)

data %>% filter(ave.cost.impr>0 & audience !="retargeting") %>%
  group_by(media, audience) %>%
  summarise(mean_dv = mean(DV_costadj, na.rm=TRUE),
            se_dv = sd(DV_costadj, na.rm=TRUE)/sqrt(n())) %>%
  ggplot(aes(x=audience, y=mean_dv, group=media, fill=media)) +
  labs(fill="Video")+
  scale_fill_brewer(palette="RdBu")+
  geom_bar(stat='identity', position=dodge) +
  geom_errorbar(limits, position=dodge, width=0.05)+
  theme_apa()+
  ylab('Results/$ spent')+
  xlab('Audience')+
  ggtitle('Results/$ spent by Video and Audience')+
  theme_apa(legend.font.size = 8,legend.use.title = TRUE)+
  geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
  theme(text=element_text(size=10)) +
  scale_y_continuous(limits = c(0,.25), oob = rescale_none, breaks=seq(0,.25, by=.05)) +
  scale_x_discrete(labels=c("philanthropy","animal","climate","general","lookalike","poverty","retargeting"))

levels(data$audience)


##### PLOT 4: DV (results) by video and audience #####
##### Questions/Notes: Removed the retargeting audience

print(data %>% filter(ave.cost.impr>0 & audience !="retargeting") %>% group_by(media,audience) %>% summarise(results=100*mean(DV),SE=100*std.error(DV),n=n()),n=50)

limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
dodge <- position_dodge(width=0.9)

data %>% filter(ave.cost.impr>0 & audience !="retargeting") %>%
  group_by(media, audience) %>%
  summarise(mean_dv = 100*mean(DV, na.rm=TRUE),
            se_dv = 100*sd(DV, na.rm=TRUE)/sqrt(n())) %>%
  ggplot(aes(x=audience, y=mean_dv, group=media, fill=media)) +
  labs(fill="Video")+
  scale_fill_brewer(palette="RdBu")+
  geom_bar(stat='identity', position=dodge) +
  geom_errorbar(limits, position=dodge, width=0.05)+
  theme_apa()+
  ylab('Results (%)')+
  xlab('Audience')+
  ggtitle('Results by Video and Audience')+
  theme_apa(legend.font.size = 8,legend.use.title = TRUE)+
  geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
  theme(text=element_text(size=10)) +
  scale_y_continuous(limits = c(0,1.1), oob = rescale_none, breaks=seq(0,1.1, by=.1)) +
  scale_x_discrete(labels=c("philanthropy","animal","climate","general","lookalike","poverty","retargeting"))




#### PLOT 5: Cost adjusted DV (results) by audience ####

#check data
print(data %>% filter(ave.cost.impr>0 & audience !="retargeting") %>% group_by(audience) %>% summarise(results=mean(DV_costadj),SE=std.error(DV_costadj),n=n()),n=50)

limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
dodge <- position_dodge(width=0.9)

data %>% filter(ave.cost.impr>0 & audience !="retargeting") %>%
  group_by(audience) %>%
  summarise(mean_dv = mean(DV_costadj, na.rm=TRUE),
            se_dv = sd(DV_costadj, na.rm=TRUE)/sqrt(n())) %>%
  ggplot(aes(x=audience, y=mean_dv)) +
  geom_bar(stat='identity', position=dodge) +
  geom_errorbar(limits, position=dodge, width=0.05)+
  theme_apa()+
  ylab('Results/$ spent')+
  xlab('Audience')+
  ggtitle('Results/$ spent by Audience')+
  theme(legend.position="none")+
  geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
  theme(text=element_text(size=11)) +
  scale_y_continuous(limits = c(0,.2), oob = rescale_none, breaks=seq(0,.2, by=.05)) +
  scale_x_discrete(labels=c("philanthropy","animal"   ,"climate","general","lookalike","poverty"))


#### PLOT 6: DV (Results) by audience ####

#check data
print(data %>% filter(ave.cost.impr>0 & audience !="retargeting") %>% group_by(audience) %>% summarise(results=100*mean(DV),SE=100*std.error(DV),n=n()),n=50)

limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
dodge <- position_dodge(width=0.9)

data %>% filter(ave.cost.impr>0 & audience !="retargeting") %>%
  group_by(audience) %>%
  summarise(mean_dv = 100*mean(DV, na.rm=TRUE),
            se_dv = 100*sd(DV, na.rm=TRUE)/sqrt(n())) %>%
  ggplot(aes(x=audience, y=mean_dv)) +
  geom_bar(stat='identity', position=dodge) +
  geom_errorbar(limits, position=dodge, width=0.05)+
  theme_apa()+
  ylab('Results (%)')+
  xlab('Audience')+
  ggtitle('Results by Audience')+
  theme(legend.position="none")+
  geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
  theme(text=element_text(size=11)) +
  scale_y_continuous(limits = c(0,.4), oob = rescale_none, breaks=seq(0,.4, by=.05)) +
  scale_x_discrete(labels=c("philanthropy","animal"   ,"climate","general","lookalike","poverty"))

#### PLOT 7: Cost adjusted DV (results) by age and gender #####
#### Did not filter out the retargeting audience like i did for the other charts

data$Gender <- as.factor(data$Gender)
levels(data$Gender)

class(data$Age)
data$Age <- relevel(data$Age, ref="18-24")
data$Age <- relevel(data$Age, ref="13-17")
levels(data$Age)

print(data %>% filter(ave.cost.impr>0) %>% group_by(Age,Gender) %>% summarise(results=mean(DV_costadj),SE=std.error(DV_costadj),n=n()),n=50)

limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
dodge <- position_dodge(width=0.9)

data %>% filter(ave.cost.impr>0 & Age!="13-17") %>%
  group_by(Age, Gender) %>%
  summarise(mean_dv = mean(DV_costadj, na.rm=TRUE),
            se_dv = sd(DV_costadj, na.rm=TRUE)/sqrt(n())) %>%
  ggplot(aes(x=Age, y=mean_dv, group=Gender, fill=Gender)) +
  labs(fill="Gender")+
  scale_fill_brewer(palette="Paired")+
  geom_bar(stat='identity', position=dodge) +
  geom_errorbar(limits, position=dodge, width=0.05)+
  theme_apa()+
  ylab('Results/$ spent')+
  xlab('Age')+
  ggtitle('Results/$ spent by Age and Gender')+
  theme_apa(legend.font.size = 8,legend.use.title = TRUE)+
  geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
  theme(text=element_text(size=10)) +
  scale_y_continuous(limits = c(0,.35), oob = rescale_none, breaks=seq(0,.35, by=.1)) +
  scale_x_discrete(labels=c("18-24","25-34","35-44","45-54","55-64","65+" ))

#### PLOT 8: DV (results) by age and gender #####

data$Gender <- as.factor(data$Gender)
levels(data$Gender)

class(data$Age)
data$Age <- relevel(data$Age, ref="18-24")
data$Age <- relevel(data$Age, ref="13-17")
levels(data$Age)

print(data %>% filter(ave.cost.impr>0) %>% group_by(Age,Gender) %>% summarise(results=100*mean(DV),SE=std.error(100*DV),n=n()),n=50)

limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
dodge <- position_dodge(width=0.9)

data %>% filter(ave.cost.impr>0 & Age!="13-17") %>%
  group_by(Age, Gender) %>%
  summarise(mean_dv = 100*mean(DV, na.rm=TRUE),
            se_dv = 100*sd(DV, na.rm=TRUE)/sqrt(n())) %>%
  ggplot(aes(x=Age, y=mean_dv, group=Gender, fill=Gender)) +
  labs(fill="Gender")+
  scale_fill_brewer(palette="Paired")+
  geom_bar(stat='identity', position=dodge) +
  geom_errorbar(limits, position=dodge, width=0.05)+
  theme_apa()+
  ylab('Results (%)')+
  xlab('Age')+
  ggtitle('Results by Age and Gender')+
  theme_apa(legend.font.size = 8,legend.use.title = TRUE)+
  geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
  theme(text=element_text(size=10)) +
  scale_y_continuous(limits = c(0,.75), oob = rescale_none, breaks=seq(0,.75, by=.25)) +
  scale_x_discrete(labels=c("18-24","25-34","35-44","45-54","55-64","65+" ))

##### PLOT 9: Cost adjusted DV (results) by Video and Age #####

class(data$Age)
data$Age <- relevel(data$Age, ref="18-24")
data$Age <- relevel(data$Age, ref="13-17")
levels(data$Age)

print(data %>% filter(ave.cost.impr>0) %>% group_by(Age,media) %>% summarise(results=mean(DV_costadj),SE=std.error(DV_costadj),n=n()),n=50)

limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
dodge <- position_dodge(width=0.9)

data %>% filter(ave.cost.impr>0 & Age!="13-17") %>%
  group_by(media, Age) %>%
  summarise(mean_dv = mean(DV_costadj, na.rm=TRUE),
            se_dv = sd(DV_costadj, na.rm=TRUE)/sqrt(n())) %>%
  ggplot(aes(x=Age, y=mean_dv, group=media, fill=media)) +
  labs(fill="Video")+
  scale_fill_brewer(palette="RdBu")+
  geom_bar(stat='identity', position=dodge) +
  geom_errorbar(limits, position=dodge, width=0.05)+
  theme_apa()+
  ylab('Results/$ spent')+
  xlab('Age')+
  ggtitle('Results/$ spent by Video and Age')+
  theme_apa(legend.font.size = 8,legend.use.title = TRUE)+
  geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
  theme(text=element_text(size=10)) +
  scale_y_continuous(limits = c(0,.2), oob = rescale_none, breaks=seq(0,.2, by=.05)) +
  scale_x_discrete(labels=c("18-24","25-34","35-44","45-54","55-64","65+"))




##### PLOT 10: DV (results) by video and age #####

class(data$Age)
data$Age <- relevel(data$Age, ref="18-24")
data$Age <- relevel(data$Age, ref="13-17")
levels(data$Age)

print(data %>% filter(ave.cost.impr>0) %>% group_by(Age,media) %>% summarise(results=100*mean(DV),SE=100*std.error(DV),n=n()),n=50)

limits <- aes(ymax = mean_dv + (se_dv), ymin=mean_dv - (se_dv))
dodge <- position_dodge(width=0.9)

data %>% filter(ave.cost.impr>0 & Age !="13-17") %>%
  group_by(media, Age) %>%
  summarise(mean_dv = 100*mean(DV, na.rm=TRUE),
            se_dv = 100*sd(DV, na.rm=TRUE)/sqrt(n())) %>%
  ggplot(aes(x=Age, y=mean_dv, group=media, fill=media)) +
  labs(fill="Video")+
  scale_fill_brewer(palette="RdBu")+
  geom_bar(stat='identity', position=dodge) +
  geom_errorbar(limits, position=dodge, width=0.05)+
  theme_apa()+
  ylab('Results (%)')+
  xlab('Age')+
  ggtitle('Results by Video and Age')+
  theme_apa(legend.font.size = 8,legend.use.title = TRUE)+
  geom_text(aes(label = paste("$",mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white") +
  theme(text=element_text(size=10)) +
  scale_y_continuous(limits = c(0,.85), oob = rescale_none, breaks=seq(0,.85, by=.25)) +
  scale_x_discrete(labels=c("18-24","25-34","35-44","45-54","55-64","65+"))
