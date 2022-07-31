# Clean GWWC giving guides data

clean_gwwc_gg_raw <- function(df) {
data <- df
data$age <- factor( data$age , ordered = FALSE )
data$age <- relevel( data$age, ref="25-34") ### Question: Why make 25-34 the ref
data <- data %>%
  mutate(audience=
           case_when(grepl("Phil",audience)~"philanthropy",
                     grepl("Anim",audience)~"animal",
                     grepl("Clim",audience)~"climate",
                     grepl("Glob",audience)~"poverty",
                     grepl("Look",audience)~"lookalike",
                     grepl("Ret",audience)~"retargeting",
                     grepl("General",audience)~"general"))

data <- data %>%
  mutate(message=
           case_when(grepl("Emotional",campaign_name)~"Emotional",
                     grepl("Factual",campaign_name)~"Factual",
                     grepl("Hypercube",campaign_name)~"Hypercube",
                     grepl("PPCo",campaign_name)~"PPCo"))
data$audience <- factor( data$audience , ordered = FALSE )
data$audience <- relevel( data$audience, ref="philanthropy")

data$message <- factor( data$message , ordered = FALSE )
data$message <- relevel( data$message, ref="Factual")

#age restriction
data <- data %>%
  mutate(restriction18_39=
           case_when(grepl("18-39", ad_set_name)|grepl("V3", ad_set_name)~1,
                     TRUE~0))
#age trinary
data <- data %>%
  mutate(agetrin =
           case_when(Age=="18-24"|Age=="13-17"|Age=="25-34"~1,
                     Age=="35-44"~0,TRUE~-1))

data <- data %>% as_tibble()

return(data)
}


# Video rename and stuff

vid_clean <- function(df) {
    df <- df %>%
  dplyr::mutate(media=
           case_when(grepl("2384",Video)~"hypercube",
                     grepl("factual_short",Video)|grepl("Factual Short",Video)~"factual short",
                     grepl("factual_final",Video)~"factual long",
                     grepl("Glob",Video)|grepl("set_3",Video)|grepl("Free Effective Giving Guide",Video)~"poverty",
                     grepl("Climate",Video)|grepl("set_2",Video)~"climate",
                     grepl("set_1",Video)| grepl("Animal",Video)~"animal"))

return(df)
}

# Text rename and stuff
text_clean <- function(df) {
    df <- df %>%
  dplyr::mutate(media=
           case_when(grepl("100x greater impact",Text)~"100x impact",
                     grepl("We Can has helped 6,000",Text)~"6000 people",
                     grepl("overwhelming with so many problems in the world",Text)~"overwhelming",
                     grepl("Only 3% of donors give based on charity effectiveness ",Text)~"only 3% research",
                     grepl("Use our free guide to learn",Text)~"learn",
                     grepl("Want to make a bigger difference next year?",Text)~"bigger difference next year/learn",
                     grepl("moved by animal welfare, the climate crisis",Text)~"cause list"))
}


