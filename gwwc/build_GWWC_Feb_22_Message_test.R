### GWWC Feb 22 Facebook Message Test: GENERATE FINAL DATA SETS ####

# Load data  ####

#### DATASET 1: RESULTS W/O DEMOGRAPHICS (aggregated) ####

### import data

fbdata <- read.csv(here::here("gwwc", "raw_data", "GWWC feb 22 test without age gender.csv")) %>%
                   as_tibble()

### Final datasets N = 48227
#sum(fbdata$Impressions)

### data cleaning

clean_fb_data <- function(df) {
  df <- mutate_at(df, c("Results","Impressions","Frequency","Link.clicks","Cost.per.result","Amount.spent..USD.","CPM..cost.per.1.000.impressions.","CPC..cost.per.link.click.","Cost.per.1.000.people.reached","X3.second.video.plays","ThruPlays"), ~replace(., is.na(.), 0)) #replace NA with 0
  
  df$ave.cost.impr <- df[,"Amount.spent..USD."]/df[,"Impressions"]
  df$ave.results.impr <- df[,"Results"]/df[,"Impressions"]
  df$ave.clicks.impr <- df[,"Link.clicks" ]/ df[,"Impressions"]
  df$ave.vidview.impr <- df[,"X3.second.video.plays" ]/ df[,"Impressions"]
  df$DV <- 0 #add empty column for DV
  df <- df[df$Impressions !=0,] # remove all rows where impressions = zero

  return(df)
}

fbdata <- fbdata %>% clean_fb_data

### ... dataframe with relevant columns only ####

newdata <- fbdata[,c("Campaign.name","Ad.Set.Name","Ad.name","DV","Frequency","ave.cost.impr","ave.results.impr","ave.clicks.impr","ave.vidview.impr","Results","Impressions")]

f1 <- function(data) {
  nr <- data$Impressions
  nd <- data$Results
  data <- data[rep_len(1L, nr), ] #in rep_len nr is the desired length of the output vector ## note: using 1L instead of "1" makes code run faster
  data$DV <- rep(0:1, c(nr - nd, nd))
  rownames(data) <- NULL
  data
}
#f1(newdata[1, ])
res <- lapply(split(newdata, rownames(newdata)), f1)
fulldata1 <- do.call('rbind', res)

#write.csv(fulldata1, "gwwc_feb22message_fulldataset_noagegender.csv", row.names = FALSE)
write_rds(fulldata1, here("gwwc", "gwwc_feb22_message_by_ad_set.rds"))


#### DATASET 2: DV LINK CLICKS INCLUDING AGE GENDER ####
### import data
fbdata2 <- read.csv(here("gwwc",  "raw_data", "GWWC feb 22 test with age and gender.csv"))
#head(fbdata2)
#names(fbdata2)

### Final datasets N = 48227
#sum(fbdata2$Impressions)

### data cleaning

fbdata2 <- fbdata2 %>% clean_fb_data

### ...  dataframe with relevant columns only ####
newdata2 <- fbdata2[,c("Campaign.name","Ad.Set.Name","Ad.name","Age","Gender","DV","Frequency","ave.cost.impr","ave.results.impr","ave.clicks.impr","ave.vidview.impr","Link.clicks","Impressions")]


f2 <- function(data) {
  nr <- data$Impressions
  nd <- data$Link.clicks
  data <- data[rep_len(1L, nr), ] #in rep_len nr is the desired length of the output vector ## note: using 1L instead of "1" makes code run faster
  data$DV <- rep(0:1, c(nr - nd, nd))
  rownames(data) <- NULL
  data
}
#f2(newdata2[1, ])
res <- lapply(split(newdata2, rownames(newdata2)), f2)
fulldata2 <- do.call('rbind', res)

#write.csv(fulldata2, "gwwc_feb22message_dvlinkclicks.csv", row.names = FALSE)
write_rds(fulldata2, here("gwwc", "gwwc_feb22_message_link_clicks.rds"))

#### DATASET 3: 3 SEC VIDEO PLAYS INCLUDING AGE GENDER ####
### import data
fbdata3 <- read.csv(here("gwwc", "raw_data", "GWWC feb 22 test with age and gender.csv"))
#head(fbdata3)
#names(fbdata3)

### Final datasets N = 48227
#sum(fbdata3$Impressions)

fbdata3 <- fbdata3 %>% clean_fb_data

### ... dataframe with relevant columns only ####s

newdata3 <- fbdata3[,c("Campaign.name","Ad.Set.Name","Ad.name","Age","Gender","DV","Frequency","ave.cost.impr","ave.results.impr","ave.clicks.impr","ave.vidview.impr","X3.second.video.plays","Impressions")]

f3 <- function(data) {
  nr <- data$Impressions
  nd <- data$X3.second.video.plays
  data <- data[rep_len(1L, nr), ] #in rep_len nr is the desired length of the output vector ## note: using 1L instead of "1" makes code run faster
  data$DV <- rep(0:1, c(nr - nd, nd))
  rownames(data) <- NULL
  data
}

#f3(newdata3[1, ])
res <- lapply(split(newdata3, rownames(newdata3)), f3)
fulldata3 <- do.call('rbind', res)

#write.csv(fulldata3,"gwwc_feb22message_dvvideoplays.csv", row.names = FALSE)
write_rds(fulldata3, here("gwwc", "gwwc_feb22_message_link_clicks.rds"))


# Further work on link clicks data ####


#### DATA CLEANING ... not sure where this ended up####

data <- fulldata2

data <- data %>%
  mutate(
    theme =
      case_when(
        grepl("Effe", Ad.Set.Name) ~ "effectiveness",
        grepl("Giving", Ad.Set.Name) ~ "giving more",
        grepl("Serv", Ad.Set.Name) ~ "services",
        grepl("Soci", Ad.Set.Name) ~ "social proof",
        grepl("Valu", Ad.Set.Name) ~ "values"
      )
  )
data$theme <- factor(data$theme , ordered = FALSE)

data <- data %>%
  mutate(
    text =
      case_when(
        grepl("Give more effectively", Ad.name) ~ "Give more effectively",
        grepl("Same donation more impact", Ad.name) ~ "Same donation more impact",
        grepl("Effectiveness matters", Ad.name) ~ "Effectiveness matters",
        grepl("Do More Good", Ad.name) ~ "Do More Good",
        grepl("Save a life each year", Ad.name) ~ "Save a life each year",
        grepl("Give more, feel fulfilled", Ad.name) ~ "Give more, feel fulfilled",
        grepl("Donate to great charities", Ad.name) ~ "Donate to great charities",
        grepl("Find the best charities", Ad.name) ~ "Find the best charities",
        grepl("Learn to do good better", Ad.name) ~ "Learn to do good better",
        grepl("Give like Nobel laureates", Ad.name) ~ "Give like Nobel laureates",
        grepl("Join 8", Ad.name) ~ "Join 8,000+ givers",
        grepl("Thousands have max", Ad.name) ~ "Thousands have maximised their impact",
        grepl("Are you in a", Ad.name) ~ "Are you in alignment with your values?",
        grepl("Live up to your values", Ad.name) ~ "Live up to your values",
        grepl("I want a better world", Ad.name) ~ "I want a better world"
      )
  )
data$text <- factor(data$text , ordered = FALSE)
data$text <- relevel(data$text, ref = "Effectiveness matters")

#Another way to create the text variable
data <- data %>%
  mutate(
    theme_text =
      case_when(
        grepl("Give more effectively", Ad.name) ~ "effectiveness 1",
        grepl("Same donation more impact", Ad.name) ~ "effectiveness 2",
        grepl("Effectiveness matters", Ad.name) ~ "effectiveness 3",
        grepl("Do More Good", Ad.name) ~ "giving more 1",
        grepl("Save a life each year", Ad.name) ~ "giving more 2",
        grepl("Give more, feel fulfilled", Ad.name) ~ "giving more 3",
        grepl("Donate to great charities", Ad.name) ~ "services 1",
        grepl("Find the best charities", Ad.name) ~ "services 2",
        grepl("Learn to do good better", Ad.name) ~ "services 3",
        grepl("Give like Nobel laureates", Ad.name) ~ "social proof 1",
        grepl("Join 8", Ad.name) ~ "social proof 2",
        grepl("Thousands have max", Ad.name) ~ "social proof 3",
        grepl("Are you in a", Ad.name) ~ "values 1",
        grepl("Live up to your values", Ad.name) ~ "values 2",
        grepl("I want a better world", Ad.name) ~ "values 3"
      )
  )
data$theme_text <- factor(data$theme_text , ordered = FALSE)

# create cost adjusted DV
data <- data %>% mutate(costadj_DV = DV / ave.cost.impr)
summary(data$costadj_DV)

# demographics
data$Age <- factor(data$Age , ordered = FALSE)
data$Gender <- factor(data$Gender , ordered = FALSE)

