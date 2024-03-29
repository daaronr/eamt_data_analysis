#### INPUT DATA ####


source(here("gwwc", "giving_guides", "clean_gg_data.R")) #a function for some specific cleaning s

# 1. textdata_dv_linkclicks.csv ####


#gwwc_text_clicks <- read.csv(here(raw_data_path, "textdata_dv_linkclicks.csv"))


gwwc_text_clicks <- clean_gwwc_gg(gwwc_text_clicks)

# texts ####

#print(gwwc_text_clicks %>% group_by(Text) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100),n=50)

gwwc_text_clicks <- gwwc_text_clicks  %>% text_clean

gwwc_text_clicks$media <- factor( gwwc_text_clicks$media , ordered = FALSE )
gwwc_text_clicks$media <- relevel( gwwc_text_clicks$media, ref="bigger difference next year/learn")

#print(gwwc_text_clicks %>% group_by(Text,media) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100),n=50)

##cost adjusted DV

gwwc_text_clicks <- gwwc_text_clicks %>% mutate(DV_costadj= DV/ave.cost.impr)

# 2. videodata_dv_linkclicks.csv ####

gwwc_vid_clicks <- read.csv(here(raw_data_path, "videodata_dv_linkclicks.csv"))

# videos ####

#print(gwwc_vid_clicks %>% group_by(Video) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100),n=50)

gwwc_vid_clicks <-  gwwc_vid_clicks %>% vid_clean

gwwc_vid_clicks$media <- factor( gwwc_vid_clicks$media , ordered = FALSE )
gwwc_vid_clicks$media <- relevel( gwwc_vid_clicks$media, ref="factual short")

#cost adjusted DV
gwwc_vid_clicks <- gwwc_vid_clicks %>% mutate(DV_costadj= DV/ave.cost.impr)
