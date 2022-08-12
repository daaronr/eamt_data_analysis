# Clean GWWC giving guides data

rename_gg <- function(df) {
  data <- df
  
  if("campaign_name_1" %in% names(data)){
    data <- data %>%
    mutate(campaign_name = str_replace(campaign_name_1, "Giving Guide 2021 . ", "")) 
  }
  
  data <- data %>%
mutate(
  campaign_name = str_replace(campaign_name, "Emotional", "Cause-led"),
  ad_name = str_replace(ad_name, "Emotional", "Cause-led"),
) 
  
  if("ad_set_name" %in% names(data)){
  data <- data %>%
mutate(
    ad_set_name = str_replace(ad_set_name, "Emotional", "Cause-led"))
  }    
   
  
return(data)

}

gg_make_cols <- function(df) {
    data <- df
    
  if("ad_set_name" %in% names(data)){
    data <- data %>% 
  mutate(
    audience = case_when(
      str_detect(ad_set_name, "Animal") ~ "Animal",
      str_detect(ad_set_name, "Climate") ~ "Climate",
      str_detect(ad_set_name, "Poverty") ~ "Global Poverty",
      str_detect(ad_set_name, "Philanthropy") ~ "Philanthropy",
      str_detect(ad_set_name, "Retargeting") ~ "Retargeting",
      str_detect(ad_set_name, "Lookalikes") ~ "Lookalikes",
      str_detect(ad_set_name, "General") ~ "General audience"
      ),
restriction18_39 =
        case_when(grepl("18-39", ad_set_name)|grepl("V3", ad_set_name)~1,
          TRUE~0)
  )
  }
  

  if("campaign_name" %in% names(data)){
    data <- data %>% 
  mutate(
    campaign_name = case_when(
      grepl("Hypercube", campaign_name) ~ "Branded",
      TRUE ~ campaign_name
      ),
    campaign_theme =
           case_when(grepl("Cause-led",campaign_name)~"Cause-led",
                     grepl("Factual",campaign_name)~"Factual",
                     grepl("Branded",campaign_name)~"Branded",
                     grepl("PPCo",campaign_name)~"'Optimized' PPCo", # "'Optimized' Factual, Emotional- PPCo creatives",
                     TRUE ~ ""),
          version = case_when(
                              str_detect(campaign_name, "V2") &  str_detect(campaign_name, "Factual") ~ "V2 - factual shortened",
      #Emotional ads remained the same for V1 and V2, and a second set of filmed ads were used for V3
      #Factual ad was shortened for V2, and a second filmed ad was used for V3
                              str_detect(campaign_name, "V3") ~ "V3 - sometimes Luke",
                              str_detect(campaign_name, "Branded") ~ "Video/creatives",
                              str_detect(campaign_name, "PPCo ") ~ "Video/creatives",
                              TRUE ~ "V1"),
  )
  }
  
    if("ad_name" %in% names(data)){
      data <- data %>% 
        mutate(
          video_theme = case_when( # Cause category aggregation
            str_detect(ad_name, "Animal") & str_detect(ad_name, "Cause-led V3|Cause-led")  ~ "Animal" ,
            str_detect(ad_name, "Climate") & str_detect(ad_name, "Cause-led V3|Cause-led") ~ "Climate",
            str_detect(ad_name, "Poverty") & str_detect(ad_name, "Cause-led V3|Cause-led")  ~ "Poverty",
            #str_detect(ad_name, "Animated") ~ "Animated",
            str_detect(ad_name, "Cause-led") ~ "Cause-led (any)",
            grepl("Branded|Hypercube", ad_name)~"Branded (factual)",
            grepl("Factual", campaign_theme) & grepl("V1", version) ~"Factual long",
            grepl("Factual", campaign_theme)  ~ "Factual short",
            grepl("factual short", version) ~"Factual short",
            grepl("Factual V3", campaign_name) ~"Factual long"
            )
        ) 
    }
    
  if("age" %in% names(data)){
    data <- data %>% 
      mutate(
        agetrin =
           case_when(age=="18-24"|age=="13-17"|age=="25-34"~1,
                     age=="35-44"~0, TRUE~-1)
        )
  }
    
return(data)
}



# Text rename and stuff
text_clean <- function(df) { 
    df <- df %>%
  dplyr::mutate(
                text_treat = case_when(
                str_detect(text, "^Want to make a bigger difference next year?") ~ "Bigger difference",
                str_detect(text, "^Did you know that the best charities can have a 100x greater impact?") ~ "100x impact",
                str_detect(text, "^Giving What We Can has helped") ~ "6000+ people",
                str_detect(text, "^Whether we’re moved by animal welfare, the climate crisis") ~ "Cause list",
                str_detect(text, "^Use our free guide to learn") ~ "Learn",
                str_detect(text, "^Only 3% of donors give based on charity effectiveness yet") ~ "Only 3% research",
                str_detect(text, "^It can be overwhelming with so many problems") ~ "Overwhelming",
                TRUE ~ ""
                )
  )
}



