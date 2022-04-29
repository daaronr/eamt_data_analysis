#### INPUT DATA ####



source(here("gwwc", "giving_guides", "clean_gg_data.R")) #a function for some specific cleaning s

# 3. textdata_dv_results.csv ####

gwwc_text_results <-  read.csv(here(raw_data_path, "textdata_dv_results.csv"))
gwwc_text_results <- clean_gwwc_gg(gwwc_text_results)
gwwc_text_results <- gwwc_text_results  %>% text_clean

gwwc_text_results$media <- factor( gwwc_text_results$media , ordered = FALSE )
gwwc_text_results$media <- relevel( gwwc_text_results$media, ref="bigger difference next year/learn")

##cost adjusted DV
gwwc_text_results <- gwwc_text_results %>% mutate(DV_costadj= DV/ave.cost.impr)


# 4. videodata_dv_results.csv ####
gwwc_vid_results <-  read.csv(here(raw_data_path, "videodata_dv_results.csv"))

gwwc_vid_results <- clean_gwwc_gg(gwwc_vid_results)

#videos
#print(data %>% group_by(Video) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100),n=50)

gwwc_vid_results <-  gwwc_vid_results %>% vid_clean

gwwc_vid_results$media <- factor( gwwc_vid_results$media , ordered = FALSE )
gwwc_vid_results$media <- relevel( gwwc_vid_results$media, ref="factual short")

#print(data %>% group_by(Video,media) %>% summarise(n=n(),cost=mean(ave.cost.impr)*100),n=50)

##cost adjusted DV
gwwc_vid_results <- gwwc_vid_results %>% mutate(DV_costadj= DV/ave.cost.impr)

