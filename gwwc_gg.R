## ---- include=FALSE--------------------------
library(here)
source(here("code", "shared_packages_code.R"))
library(dplyr, janitor)
library(pacman)

install.packages('BiocManager')
#remotes::install_github("stan-dev/cmdstanr@fix-install-win11")
p_load(cmdstanr)

#install.packages("tidybayes")
library(tidybayes)


library(GDAtools, include.only = 'wtable', 'dichotom')



## ----import_raw_0----------------------------
#importing for dynamic input of descriptions below

#Todo: get direct import working: See https://cran.r-project.org/web/packages/rfacebookstat/rfacebookstat.pdf

raw_data_path <- list("gwwc", "gg_raw_data_shareable")

mini_clean <- function(df){
  df %>%
    as_tibble() %>%
  janitor::clean_names() %>%
  janitor::remove_empty() %>% # removes empty rows and columns, here `unique_link_clicks`
    janitor::remove_constant()
    }

gg_campaign_by_ad_by_text <- read_csv(here(raw_data_path, "gg_campaign_by_ad_by_text.csv"), show_col_types=FALSE) %>%
  dplyr::select(-"Campaign name...4") %>% #duplicate columns?
 mini_clean


## ---- include=FALSE--------------------------
library(here)
source(here("code", "shared_packages_code.R"))


## ----wppage, out.width="85%"-----------------

knitr::include_url("https://effective-giving-marketing.gitbook.io/untitled/partner-organizations-and-trials/gwwc/giving-guides-+")


## ----import_real_raw_data--------------------

raw_data_path <- list("gwwc", "gg_raw_data_shareable")

#already input above: gg_campaign_by_ad_by_text

#Version allowing demographic breakdown:
gg_campaign_by_ad_by_text_age_gender <- read_csv(here(raw_data_path, "gg-campaign-by-ad-set-text-age-gender.csv"), show_col_types=FALSE) %>%
  #dplyr::select(-"Campaign name...4") %>% #duplicate columns?
 mini_clean()

#Version with information on cause videos shown (even to those in 'general' groups):

gg_video_breakdowns <- read_csv(here(raw_data_path, "gg-image-video-breakdowns.csv"), show_col_types=FALSE)

#capture and remove columns that are the same everywhere
attribution_setting_c <- gg_campaign_by_ad_by_text_age_gender$attribution_setting %>% .[1]
reporting_starts_c <- gg_campaign_by_ad_by_text_age_gender$reporting_starts %>% .[1]
reporting_ends_c <- gg_campaign_by_ad_by_text_age_gender$reporting_ends %>% .[1]

gg_campaign_by_ad_by_text_age_gender  %<>% mini_clean()
gg_video_breakdowns  %<>% mini_clean()

#functions to clean these specific data sets 'gg_campaign_by_ad_by_text_age_gender' and 'gg_campaign_by_ad_by_text':
source(here("gwwc", "giving_guides", "clean_gg_raw_data.R"))

#Many cleaning steps: audience, video_theme, campaign_theme, agetrin; releveling
gg_campaign_by_ad_by_text_age_gender %<>%
  rename_gg() %>%
gg_make_cols() %>%
  text_clean() %>%  # Shorter 'text treatment' column
  dplyr::select(campaign_name, everything(), -campaign_name_1, -campaign_name_7) #campaign_name_7 was the same as campaign_name_1

gg_video_breakdowns %<>%
  rename_gg() %>%
gg_make_cols()

#gg_campaign_by_ad_by_text_age_gender %>% collapse::descr()



## ----video_general_encoding------------------

gg_video_breakdowns %<>%
  mutate(
    video_theme = case_when(
      str_det(image_video_and_slideshow, "set_1|Animals") ~ "Animal",
      str_det(image_video_and_slideshow, "set_2|Climate") ~ "Climate",
      str_det(image_video_and_slideshow, "set_3|Poverty") ~ "Poverty",
      str_det(image_video_and_slideshow, "Free Effective") & video_theme=="Cause-led (any)" ~ "Poverty",
      str_det(image_video_and_slideshow, "factual_short|Factual Short") ~ "Factual short",
    TRUE ~   video_theme
    ),
    video_theme =  factor(video_theme),
    video_theme = fct_relevel(video_theme, c("Animal", "Climate", "Poverty", "Factual short", "Factual long", "Branded (factual)"))
  )



## ----export----------------------------------

cleaned_data_path <- list("gwwc", "gg_processed_data")

#export 'cleaned' data for others to play with immediately
write.csv(gg_video_breakdowns, here(cleaned_data_path, "gg_video_breakdowns.csv"), row.names = FALSE)
write.csv(gg_campaign_by_ad_by_text_age_gender, here(cleaned_data_path, "gg_campaign_by_ad_by_text_age_gender.csv"), row.names = FALSE)

write_rds(gg_video_breakdowns, here(cleaned_data_path, "gg_video_breakdowns.Rdata"))
write_rds(gg_campaign_by_ad_by_text_age_gender, here(cleaned_data_path, "gg_campaign_by_ad_by_text_age_gender.Rdata"))





## ----reach_count-----------------------------

(
  reach_campaign_theme <- gg_video_breakdowns %>%
  dplyr::select(campaign_name, video_theme, reach) %>%
   uncount(weights = .$reach) %>%
    dplyr::select(-reach) %>%
   tabyl(campaign_name, video_theme) %>%
    dplyr::select(campaign_name, Animal, Climate, Poverty, everything()) %>%
  .kable(caption = "Campaign names and video themes: unique impressions") %>%
  .kable_styling()
)

reach_campaign_audience <- gg_campaign_by_ad_by_text_age_gender %>% #created but not shown for now
  dplyr::select(campaign_name, audience, reach) %>%
   uncount(weights = .$reach) %>%
    dplyr::select(-reach) %>%
   tabyl(campaign_name, audience) %>%
  .kable(caption = "Campaign names and audiences: unique impressions") %>%
  .kable_styling()





## --------------------------------------------
(
  versions_of_videos <- gg_campaign_by_ad_by_text_age_gender %>%
  dplyr::select(video_theme, version, reach) %>%
   uncount(weights = .$reach) %>%
    dplyr::select(-reach) %>%
   table %>%
  .kable(caption = "Versions of videos: unique impressions") %>%
  .kable_styling()
)


## ----reach_video_audience--------------------

video_levels <- c("Animal", "Climate", "Poverty", "Factual short",  "Factual long", "Branded (Factual)", "Total")

audience_levels <- c("Animal", "Climate", "Global Poverty", "Philanthropy", "General audience", "Lookalikes", "Retargeting")


adorn_opts <- function(df) {
  df %>% 
    adorn_percentages("all") %>%
    adorn_totals(where = c("row", "col")) %>%
    adorn_pct_formatting(digits = 2)  
}

(
reach_video_audience <- gg_video_breakdowns %>%
    dplyr::select(video_theme, audience, reach) %>%
    uncount(weights = .$reach) %>%
    dplyr::select(-reach) %>%
    tabyl(audience, video_theme) %>%
    adorn_opts() %>% 
    dplyr::select(audience, Animal, Climate,Poverty, everything()) %>%
   mutate(audience =  factor(audience, levels = audience_levels)) %>%
  arrange(audience)  %>%
    .kable(caption = "Video themes (columns) by audience (rows): share of unique impressions", digits=3) %>%
  .kable_styling() 


)



## ----reach_video_text------------------------

video_levels_gg <- c("Animal", "Climate", "Poverty", "Cause-led (any)", "Factual short",  "Factual long", "Branded (Factual)", "Total")

(
  reach_video_text <- gg_campaign_by_ad_by_text_age_gender %>%
  dplyr::select(video_theme, text_treat, reach) %>%
   uncount(weights = .$reach) %>%
    dplyr::select(-reach) %>%
        tabyl(video_theme, text_treat) %>%
       adorn_opts() %>% 
   mutate(video_theme =  factor(video_theme, levels = video_levels_gg)) %>%
  arrange(video_theme)  %>%
  .kable(caption = "Video themes by text treatment: unique impressions", digits=3) %>%
  .kable_styling()
)



## --------------------------------------------
(
  reach_text_campaign <- gg_campaign_by_ad_by_text_age_gender %>%
  dplyr::select(campaign_name, text_treat, reach) %>%
   uncount(weights = .$reach) %>%
    dplyr::select(-reach) %>%
 tabyl(campaign_name, text_treat) %>%
    adorn_percentages("row") %>%
     adorn_totals(where = c("col")) %>%
    adorn_pct_formatting(digits = 2) %>%
  .kable(caption = "Text treatments as shares of unique impressions by campaign", digits=3) %>%
  .kable_styling()
)





## --------------------------------------------

(
  reach_age_gender <- gg_campaign_by_ad_by_text_age_gender %>%
  dplyr::select(age, gender, reach) %>%
   uncount(weights = .$reach) %>%
    dplyr::select(-reach) %>%
  tabyl(age, gender) %>%
       adorn_opts() %>% 
  .kable(caption = "Unique impressions: shares by Age and Gender", digits=2) %>%
  .kable_styling()
)



## ----statista--------------------------------

knitr::include_url("https://www.statista.com/statistics/187549/facebook-distribution-of-users-age-group-usa/#:~:text=As%20of%20June%202022%2C%2023.5,13%20to%2017%20years%20old")



## ----campaign_date_outcomes------------------

base_results_sum <- function(df) {
    df %>%
     dplyr::summarize(
  Cost = sum(round(amount_spent_usd,0)),
      `reach`=sum(reach),
      `Link clicks`=sum(link_clicks, na.rm = TRUE),
      Results=sum(results, na.rm = TRUE),
      `$/ impr.` = round(Cost/reach,3),
      `$/ click` = round(Cost/ `Link clicks`,1),
      `$/ result` = round(Cost/Results,1),
      `Results/ 1k impr.` = round(Results*1000/reach,1)
)
     }

(
  campaign_date_outcomes <-  gg_campaign_by_ad_by_text_age_gender %>%
    group_by(campaign_name, starts, ends) %>%
    rename('Campaign' = campaign_name) %>%
    filter(reach>200) %>%
    base_results_sum %>%
    arrange(starts) %>%
    .kable(caption = "Results by Campaign and start date") %>%
    .kable_styling() %>%
    add_footnote("'False start' campaign dates with less than 200 reach are excluded")
)



## ----campaign_date_outcomes_comp_1_2---------
(
  campaign_date_outcomes_comp_1_2 <-  gg_campaign_by_ad_by_text_age_gender %>%
    filter(starts<= "2021-12-08") %>% 
    filter(!str_det(text_treat, "100x impact|Overwhelming|Only 3% research|Cause list")) %>%
    filter(audience == "Philanthropy" | audience =="Lookalikes") %>%
    group_by(video_theme, audience) %>%
    filter(reach>100) %>%
    base_results_sum %>%
     dplyr::select(-Cost, -`Results`, -`Link clicks`) %>%
    arrange(audience, -`Results/ 1k impr.`) %>% 
    .kable(caption = "'Comparable' Parts of Tests 1 & 2: reach, impressions, clicks, results") %>%
    .kable_styling() 
)





## ----campaign_date_outcomes_comp_3-----------
(
  campaign_date_outcomes_comp_3 <-  gg_campaign_by_ad_by_text_age_gender %>%
    filter(starts== "2021-12-23") %>% 
    filter(!str_det(text_treat, "100x impact|Overwhelming|Only 3% research|Cause list")) %>%
    filter(audience == "Philanthropy" | audience =="Lookalikes") %>%
    group_by(video_theme, audience) %>%
    filter(reach>100) %>%
    base_results_sum %>%
     dplyr::select(-Cost, -`Results`, -`Link clicks`) %>%
    arrange(audience, -`Results/ 1k impr.`) %>% 
    .kable(caption = "'Comparable' Parts of Tests 3") %>%
    .kable_styling() 
)



## ----campaign_date_outcomes_comp_4_vid-------


campaign_date_outcomes_comp_4_vid <-  gg_video_breakdowns %>%
    filter(starts== "2022-01-07") %>% 
    group_by(video_theme, audience) %>%
    filter(audience!="Retargeting") %>%
    base_results_sum %>%
     dplyr::select(-Cost, -`Results`, -`Link clicks`) %>%
    arrange(audience, `$/ result`) %>% 
    dplyr::select(audience, everything())

campaign_date_outcomes_comp_4_vid %>% 
        DT::datatable(caption = "Test 4 (roughly comparable); blank cells indicate NA/no results", filter="top",  rownames= FALSE) 




## --------------------------------------------

campaign_date_outcomes_comp_4_vid %>% filter(`$/ result`<=9.1) %>% dplyr::select(audience, video_theme, `$/ result`) %>% 
  dplyr::arrange(`$/ result`) %>% 
  .kable() %>% .kable_styling()



## --------------------------------------------

campaign_date_outcomes_comp_4_vid %>% filter(`$/ result`>20 |  is.na(`$/ result`) ) %>% dplyr::select(audience, video_theme, `$/ result`) %>% 
    dplyr::arrange(-`$/ result`) %>% 
.kable() %>% .kable_styling()



## --------------------------------------------





## ----campaign_date_outcomes_comp_4_aud_vt----


campaign_date_outcomes_comp_4_aud <-  gg_video_breakdowns %>%
    filter(starts== "2022-01-07") %>% 
      group_by(audience) %>%
    filter(audience!="Retargeting") %>%
    base_results_sum %>%
    mutate(`Video type` = "All") %>% 
     dplyr::select(-Cost, -`Results`, -`Link clicks`) %>%
     dplyr::select(audience, `Video type`, everything()) %>% 
      arrange(`Video type`, `$/ result`)



campaign_date_outcomes_comp_4_aud_vt <-  gg_video_breakdowns %>%
    mutate(`Video type` = if_else(str_det(video_theme, "Factual|factual"), "Non-cause", "Cause")) %>% 
    filter(starts== "2022-01-07") %>% 
      group_by(`Video type`, audience) %>%
    filter(audience!="Retargeting") %>%
    base_results_sum %>%
     dplyr::select(-Cost, -`Results`, -`Link clicks`) %>%
    arrange(`Video type`, `$/ result`) %>% 
    dplyr::select(audience, everything())

campaign_date_outcomes_comp_4_aud_vt_all <- bind_rows(campaign_date_outcomes_comp_4_aud, campaign_date_outcomes_comp_4_aud_vt)

campaign_date_outcomes_comp_4_aud_vt_all %>% 
        DT::datatable(caption = "Test 4 by audience and video type", filter="top",  rownames= FALSE) 



## ----campaign_date_outcomes_comp_4_text_pool----

campaign_date_outcomes_comp_4_text_pool <-  gg_campaign_by_ad_by_text_age_gender %>%
    filter(starts== "2022-01-07") %>% 
    group_by(text_treat) %>%
    filter(audience!="Retargeting") %>%
    base_results_sum %>%
     dplyr::select(-Cost, -`Results`, -`Link clicks`) %>%
    arrange(`$/ result`) 

campaign_date_outcomes_comp_4_text_pool %>% 
        DT::datatable(caption = "Tests 4: Performance by text; blank cells indicate NA/no results") 

worst_to_best <- campaign_date_outcomes_comp_4_text_pool[[5,5]]/campaign_date_outcomes_comp_4_text_pool[[1,5]]




## ----campaign_date_outcomes_comp_4_text------

campaign_date_outcomes_comp_4_text <-  gg_campaign_by_ad_by_text_age_gender %>%
    filter(starts== "2022-01-07") %>% 
    group_by(text_treat, audience) %>%
    filter(audience!="Retargeting") %>%
    base_results_sum %>%
     dplyr::select(-Cost, -`Results`, -`Link clicks`) %>%
    arrange(audience, `$/ result`) %>% 
    dplyr::select(audience, everything())

campaign_date_outcomes_comp_4_text %>% 
        DT::datatable(caption = "Tests 4: Performance by audience and text; blank cells indicate NA/no results", filter="top",  rownames= FALSE) 



## ----age_gender_outcomes---------------------
(
  age_outcomes_pre_12_09 <- gg_campaign_by_ad_by_text_age_gender %>%
    group_by(age) %>%
        filter(reach>500, starts<= "2021-12-08") %>%
    base_results_sum() %>%
    .kable(caption = "Results by Age: Campaigns starting on or before Dec 8 2021") %>%
    .kable_styling()
)



## ----age_outcomes_post_12_09-----------------

(
  age_outcomes_post_12_09 <- gg_campaign_by_ad_by_text_age_gender %>%
    group_by(age) %>%
        filter(reach>500, starts > "2021-12-08") %>%
    base_results_sum() %>%
    .kable(caption = "Results by Age: Campaigns starting on or after Dec 23 2021") %>%
    .kable_styling()
)




## --------------------------------------------


(
  gender_outcomes <- gg_campaign_by_ad_by_text_age_gender %>%
    group_by(gender) %>%
    base_results_sum() %>%
 dplyr::select(-Cost, -`Results`, -`Link clicks`) %>%
    .kable(caption = "Results by Gender") %>%
    .kable_styling()
)



## ----audience_outcomes-----------------------

video_outcomes_phil_0 <- gg_video_breakdowns %>%
    filter(audience=="Philanthropy") %>%
    group_by(video_theme) %>%
    base_results_sum() 

(
video_outcomes_phil <- video_outcomes_phil_0 %>% 
     dplyr::select(-Cost, -`Results`, -`Link clicks`) %>%
    arrange(video_theme) %>%
    .kable(caption = "Results by Video theme for 'Philanthropy' audience") %>%
    .kable_styling()
)


bot_table <- function(df,  outvar, outcol) {
  df %>% 
    dplyr::arrange({{outvar}}) %>%
    dplyr::select({{outcol}}) %>% 
    mutate_if(is.factor, as.character) %>%
    .[[1,1]] 
  } 

top_table <- function(df, outcol, outvar) {
  df %>%
    dplyr::arrange(-{{outvar}}) %>%
    dplyr::select({{outcol}}) %>%
    mutate_if(is.factor, as.character) %>%
    .[[1,1]]
}


# { ov <- {{outvar}}
# ifelse(reverse,
#          dplyr::arrange(ov),
#    dplyr::arrange(-ov)
#   )} %>% 

#   
#   dplyr::arrange({{outvar}}),
 #         dplyr::arrange(-{{outvar}}))} %>% 

top_vid_phil <- video_outcomes_phil_0 %>% bot_table( outvar=`$/ result`, outcol=video_theme)
bot_vid_phil <- video_outcomes_phil_0 %>% top_table( outvar=`$/ result`, outcol=video_theme)



## --------------------------------------------

outcomes_by_text <- gg_campaign_by_ad_by_text_age_gender %>%
    filter(str_det(campaign_name, "factual|branded")) %>%
    group_by(text_treat) %>%
    base_results_sum() 

outcomes_by_text %>%   
   dplyr::select(-Cost, -`Results`, -`Link clicks`) %>%
    .kable(caption = "Results by text; later campaigns") %>%
    .kable_styling()

top_text_later <- outcomes_by_text %>% bot_table( outvar=`$/ result`, outcol=text_treat)
bot_text_later <- outcomes_by_text %>% top_table( outvar=`$/ result`, outcol=text_treat)




## --------------------------------------------

audience_outcomes_all <- gg_video_breakdowns %>%
         filter(audience!="Retargeting" & audience!="General audience")  %>% #latter filter because they were only in the final trial
  mutate(`Video type` = "All") %>% 
    group_by(`Video type`, audience) %>%
    base_results_sum %>%
   dplyr::select(-Cost, -`Results`, -`Link clicks`) %>%
    arrange(`Video type`, `$/ result`)

audience_outcomes_vt <- gg_video_breakdowns %>%
         filter(audience!="Retargeting" & audience!="General audience")  %>% #latter filter because they were only in the final trial
  mutate(`Video type` = if_else(str_det(video_theme, "Factual|factual"), "Non-cause", "Cause")) %>% 
    group_by(`Video type`, audience) %>%
    base_results_sum %>%
   dplyr::select(-Cost, -`Results`, -`Link clicks`) %>%
    arrange(`Video type`, `$/ result`)


audience_outcomes_vt_all <- bind_rows(audience_outcomes_all, audience_outcomes_vt)

audience_outcomes_vt_all %>%
    DT::datatable(caption = "Results by audience; cause vs non-cause (and overall)",  filter="top",  rownames= FALSE)




## ----packages--------------------------------
library(pacman)
p_load(brms, install=FALSE)
library(brms)
p_load(tidybayes, install=FALSE)
library(tidybayes)



## ----sum_results_collapse--------------------

sum_results <- function(df) {
    df %>%
     summarise(
    results = sum(results, na.rm = TRUE),
    spend = sum(amount_spent_usd, na.rm = TRUE),
    clicks = sum(link_clicks, na.rm = TRUE),
    impressions = sum(impressions, na.rm = TRUE),
    reach = sum(reach, na.rm = TRUE)
  )
}

# collapse into the categories of interest
gg_video_breakdowns_col <- gg_video_breakdowns %>%
  group_by(video_theme, audience) %>%
  #group_by(video_theme, audience, starts) %>%
sum_results
#%>%   mutate(starts = as.factor(starts))
#rem: we are just summing outcomes, so no weights are needed here

gg_video_breakdowns_col_mix <- gg_video_breakdowns %>%
  group_by(video_theme, audience, starts) %>%
  #group_by(video_theme, audience, starts) %>%
sum_results
#%>%   mutate(starts = as.factor(starts))
#rem: we are just summing outcomes, so no weights are needed here


# collapse into the categories of interest
gg_campaign_by_text_col <-  gg_campaign_by_ad_by_text_age_gender %>%
   uncount(weights = .$reach) %>%
  group_by(text_treat, audience) %>% 
sum_results
#%>%   mutate(starts = as.factor(starts))
#rem: we are just summing outcomes, so no weights are needed here

gg_campaign_by_text_only <-  gg_campaign_by_ad_by_text_age_gender %>%
   uncount(weights = .$reach) %>%
  group_by(text_treat) %>% 
sum_results

gg_video_breakdowns_col_mix <- gg_video_breakdowns %>%
  group_by(video_theme, audience, starts) %>%
  #group_by(video_theme, audience, starts) %>%
sum_results
#%>%   mutate(starts = as.factor(starts))
#rem: we are just summing outcomes, so no weights are needed here





## ----priors_clicks---------------------------

#helper function to 'make priors'
make_prior_normal <- function(mean, sd, ...) {
  prior_string(paste0("normal(", mean, ",", sd, ")"), ...)
}

prior_click_per_reach <- 0.028

prior_intercept_click <-  logit(prior_click_per_reach)

prior_intercept_click_ub <- 0.15
se_prior_click <- (logit(prior_intercept_click_ub) - prior_intercept_click)/1.96

prior_slope_click_ub <- 0.25
se_prior_slope_click <- (logit(prior_slope_click_ub) - prior_intercept_click)/1.96

prior_click <- c(
  make_prior_normal(prior_intercept_click, se_prior_click, class = "Intercept"),
make_prior_normal(0, se_prior_slope_click, class = "b")
)

#DR @Jamie: This is a log ratio of probabilities, iirc. Why are we assuming it is normally distributed? 



## ----priors_results--------------------------

prior_results_per_click <- 0.20

prior_intercept_results <-  logit(prior_results_per_click)

prior_intercept_results_ub <- 0.75
se_prior_results <- (logit(prior_intercept_results_ub) - prior_intercept_results)/1.96

prior_slope_results_ub <- 0.85
se_prior_slope_results <- (logit(prior_slope_results_ub) - prior_intercept_results)/1.96

prior_results <- c(
  make_prior_normal(prior_intercept_results, se_prior_results, class = "Intercept"),
make_prior_normal(0, se_prior_slope_results, class = "b")
)



## ----bayes_model_formulas--------------------
clicks_per_reach_video_aud <- as.formula("clicks | trials(reach) ~ video_theme + audience + video_theme:audience")

# +  starts
results_per_click_video_aud <- as.formula("results | trials(clicks) ~ video_theme + audience + video_theme:audience")

#  For comparison: a one-step model 
results_per_reach_video_aud <- as.formula("results | trials(reach) ~ video_theme + audience + (video_theme|audience)")

#including messages
clicks_per_reach_text_aud <- as.formula("clicks | trials(reach) ~ text_treat + audience + text_treat:audience")

# +  starts
results_per_click_text_aud <- as.formula("results | trials(clicks) ~ text_treat + audience + text_treat:audience")


#  Fuller, mixed model 
# results_per_reach_video_aud_mix <- as.formula("results | trials(reach) ~ (1|starts) + video_theme + audience + (video_theme | audience) + (video_theme:audience)")



## --------------------------------------------
# label: bayes_args_estimates
#| output: false
#| warning: false

## passing a list
#arg.list <- list(init = 0, chains = 4, cores = 4, iter = 2500, warmup = 500, backend = "cmdstanr", seed = 1010, silent=2,  refresh = 0) 

clicks_logit_vid <-  do.call("brm", c(list(
  data = gg_video_breakdowns_col, 
  formula = clicks_per_reach_video_aud,
                  family = binomial("logit"),
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                   prior = prior_click),
                  init = 0, chains = 4, cores = 4, iter = 2500, warmup = 500, seed = 1010, silent=2,  refresh = 0,                  
  )
)

results_logit_vid <-  do.call("brm", c(list(
  data = gg_video_breakdowns_col, 
  formula = results_per_click_video_aud,
                  family = binomial("logit"),
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  prior = prior_results),
                  init = 0, chains = 4, cores = 4, iter = 2500, warmup = 500, backend = "cmdstanr", seed = 1010, silent=2,  refresh = 0,                   
  list(threads = threading(8)))
)

results_per_reach_logit_vid <-  do.call("brm", c(list(
  data = gg_video_breakdowns_col, 
  formula = results_per_reach_video_aud,
                  family = binomial("logit"),
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  prior = prior_results),
                  init = 0, chains = 4, cores = 4, iter = 2500, warmup = 500, backend = "cmdstanr", seed = 1010, silent=2,  refresh = 0,                   
  list(threads = threading(8)))
)


## ----bayes_args_estimates_text---------------
# label: bayes_args_estimates_text
#| output: false
#| warning: false

#for texts:
clicks_logit_text <-  do.call("brm", c(list(
  data = gg_campaign_by_text_col, 
  formula = clicks_per_reach_text_aud,
                  family = binomial("logit"),
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                   prior = prior_click),
                  init = 0, chains = 4, cores = 4, iter = 2500, warmup = 500, backend = "cmdstanr", seed = 1010, silent=2,  refresh = 0,                   
  list(threads = threading(8)))
)

results_logit_text <-  do.call("brm", c(list(
  data = gg_campaign_by_text_col, 
  formula = results_per_click_text_aud,
                  family = binomial("logit"),
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  prior = prior_results),
                  init = 0, chains = 4, cores = 4, iter = 2500, warmup = 500, backend = "cmdstanr", seed = 1010, silent=2,  refresh = 0,                   
  list(threads = threading(8)))
)


#for texts only:
clicks_logit_text_only <-  do.call("brm", c(list(
  data = gg_campaign_by_text_only, 
  formula = clicks | trials(reach) ~ text_treat,
                  family = binomial("logit"),
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                   prior = prior_click),
                  init = 0, chains = 4, cores = 4, iter = 2500, warmup = 500, backend = "cmdstanr", seed = 1010, silent=2,  refresh = 0,                   
  list(threads = threading(8)))
)

results_logit_text_only <-  do.call("brm", c(list(
  data = gg_campaign_by_text_only, 
  formula = results | trials(clicks) ~ text_treat,
                  family = binomial("logit"),
                  control = list(adapt_delta = 0.99, max_treedepth = 15),
                  prior = prior_results),
                  init = 0, chains = 4, cores = 4, iter = 2500, warmup = 500, backend = "cmdstanr", seed = 1010, silent=2,  refresh = 0,                   
  list(threads = threading(8)))
)



## ----full_crossing_vid-----------------------

# posterior expectations:
full_crossing <- expand_grid(
  "video_theme" = unique(gg_video_breakdowns_col$video_theme),
  "audience" = unique(gg_video_breakdowns_col$audience)) %>% 
  mutate(reach = 1, #it is estimating the number of clicks based on the reach
         clicks = 1) %>% 
filter(!(audience=="Animal" & (video_theme == "Climate" | video_theme == "Poverty")))  %>%
    filter(!(audience=="Climate" & (video_theme == "Animal" | video_theme == "Poverty")))  %>%
    filter(!(audience=="Global Poverty" & (video_theme == "Animal" | video_theme == "Climate"))) #remove combinations where we don't have data ... at least for now

# setting reach and clicks = 1 will give us proportion of conversions (because it makes a prediction for total outcomes per unit )

click_post <- posterior_epred(clicks_logit_vid, newdata = full_crossing) #ndraws = 1000, 
results_post <- posterior_epred(results_logit_vid, full_crossing)

combined_post <- click_post * results_post #DR: multiplying the predicted probabilities of click and conversion here?

results_per_reach_post <- posterior_epred(results_per_reach_logit_vid, full_crossing)



## ----tib_post_vid----------------------------

#make tibbles of the stuff above, put it together s
post_tib_clean <- function(df, name) {
  data <- df %>% 
    as.tibble() %>% 
    pivot_longer(cols = everything(),
    names_to = "identifier",
    values_to = "probability") 
  data <- data %>% 
     mutate(level = name,
       theme = rep(full_crossing$video_theme, dim(data)[1]/dim(full_crossing)[1]),
       audience = rep(full_crossing$audience,  dim(data)[1]/dim(full_crossing)[1])
       #     starts = rep(full_crossing$starts, 1000) #DR: I think I want starts (start date as a factor) in the model, because the audience may be systematically different on those days, and other things change we leave out here. However, I don't want to see it in the graphs. How to do that? 
     )
  return(data)
}

# make tibbles
tib_click_post <- click_post %>% 
  post_tib_clean( "1. Reach to clicks")
tib_result_post <- results_post  %>%
    post_tib_clean("2. Clicks to signups")
tib_combined_post <- combined_post %>%
      post_tib_clean("3. Total") 
full_post <- bind_rows(tib_click_post,
                       tib_result_post,
                       tib_combined_post)


## ----full_crossing_text----------------------

post_tib_clean_text <- function(df, name) {
  data <- df %>% 
    as.tibble() %>% 
    pivot_longer(cols = everything(),
    names_to = "identifier",
    values_to = "probability") 
  data <- data %>% 
     mutate(level = name)
       #     starts = rep(full_crossing$starts, 1000) #DR: I think I want starts (start date as a factor) in the model, because the audience may be systematically different on those days, and other things change we leave out here. However, I don't want to see it in the graphs. How to do that? 
  return(data)
}


# posterior expectations:

full_crossing_text <- expand_grid(
  "text_treat" = unique(gg_campaign_by_text_col$text_treat),
  "audience" = unique(gg_campaign_by_text_col$audience)) %>% 
  mutate(reach = 1, #it is estimating the number of clicks based on the reach
         clicks = 1) 

full_crossing_text_only <- expand_grid(
  "text_treat" = unique(gg_campaign_by_text_col$text_treat)) %>% 
  mutate(reach = 1, #it is estimating the number of clicks based on the reach
         clicks = 1)

click_post_text <- posterior_epred(clicks_logit_text, newdata = full_crossing_text) #ndraws = 1000, 
results_post_text <- posterior_epred(results_logit_text, full_crossing_text)
combined_post_text <- click_post_text * results_post_text 

text_audience_reps <- function(df) {
  df %>% 
 mutate(
         text_treat = rep(full_crossing_text$text_treat, dim(tib_click_post_text)[1]/dim(full_crossing_text)[1]),
       audience = rep(full_crossing_text$audience,  dim(tib_click_post_text)[1]/dim(full_crossing_text)[1])
)}
  

# make tibbles
tib_click_post_text <- click_post_text %>% 
  post_tib_clean_text( "1. Reach to clicks") 
tib_click_post_text %<>%  text_audience_reps

tib_result_post_text <- results_post_text  %>%
    post_tib_clean_text("2. Clicks to signups")
tib_result_post_text %<>% text_audience_reps

tib_combined_post_text <- combined_post_text %>%
      post_tib_clean_text("3. Total") 
tib_combined_post_text %<>% text_audience_reps

full_post_text <- bind_rows(tib_click_post_text,
                       tib_result_post_text,
                       tib_combined_post_text)

#for text only, 1 variable model

click_post_text_only <- posterior_epred(clicks_logit_text_only, newdata = full_crossing_text_only) #ndraws = 1000, 

results_post_text_only <- posterior_epred(results_logit_text_only, full_crossing_text_only)

combined_post_text_only <- click_post_text_only * results_post_text_only


# make tibbles

text_only_reps <- function(df) {
  df %>% 
   mutate(text_treat = rep(full_crossing_text_only$text_treat, dim(tib_click_post_text_only)[1]/dim(full_crossing_text_only)[1]))
}          

tib_click_post_text_only <- click_post_text_only %>% 
  post_tib_clean_text( "1. Reach to clicks")
tib_click_post_text_only %<>%   text_only_reps

tib_results_post_text_only <- results_post_text_only  %>%
    post_tib_clean_text("2. Clicks to signups")
tib_results_post_text_only %<>%  text_only_reps

tib_combined_post_text_only <- combined_post_text_only %>%
      post_tib_clean_text("3. Total") 

tib_combined_post_text_only %<>%  text_only_reps

full_post_text_only <- bind_rows(tib_click_post_text_only,
                       tib_results_post_text_only,
                       tib_combined_post_text_only)



## ----post_summary----------------------------

hdi <- HDInterval::hdi

CI_choice_narrow <- 0.6
CI_choice_wide <- 0.9

sum_mean_hdi <- function(
    df, 
  var = probability, scaleme=100, CI_choice_n=CI_choice_narrow, CI_choice_w = CI_choice_wide) {
  df %>% 
    summarise(
      mean = mean({{var}}) * scaleme,
            lower_n = hdi({{var}}, credMass = CI_choice_n)[1] * scaleme,
            upper_n = hdi({{var}}, credMass = CI_choice_n)[2] * scaleme,
              lower_w = hdi({{var}}, credMass = CI_choice_w)[1] * scaleme,
            upper_w = hdi({{var}}, credMass = CI_choice_w)[2] * scaleme,
          lower_eti = quantile({{var}}, (1-CI_choice_w)/2) * scaleme,
    upper_eti = quantile({{var}}, 1-(1-CI_choice_w)/2) * scaleme,
      check = length(hdi({{var}}))
    )
} 


mutate_mean_hdi <- function(
    df, 
  var = probability, scaleme=100, CI_choice_n=CI_choice_narrow, CI_choice_w = CI_choice_wide) {
  df %>% 
    dplyr::mutate(
      mean = mean({{var}}) * scaleme,
            lower_n = hdi({{var}}, credMass = CI_choice_n)[1] * scaleme,
            upper_n = hdi({{var}}, credMass = CI_choice_n)[2] * scaleme,
              lower_w = hdi({{var}}, credMass = CI_choice_wide)[1] * scaleme,
            upper_w = hdi({{var}}, credMass = CI_choice_wide)[2] * scaleme,
          lower_eti = quantile({{var}}, (1-CI_choice_wide)/2) * scaleme,
    upper_eti = quantile({{var}}, 1-(1-CI_choice_wide)/2) * scaleme,
      check = length(hdi({{var}}))
    )
} 




## ----aud_plots-------------------------------

aud_plots <- function(df) {
  df %>% 
       filter(audience!="Retargeting")  %>%
    group_by(level, audience) %>% #, starts
    sum_mean_hdi %>% 
    mutate(audience = reorder(as.factor(audience), `mean`)) %>% 
    ggplot() + 
  geom_errorbarh(aes(xmin = lower_n, xmax = upper_n, y = audience), height = .7, color = "red") +
  geom_errorbarh(aes(xmin = lower_w, xmax = upper_w, y = audience),  height=.25, color = "blue") +
  geom_point(
    aes(x=`mean`, 
    y = audience))
    } 

(
reach_to_clicks_aud_plot <-  full_post  %>%
    aud_plots() +
  labs(title = "By audience (reach, clicks, signups)",
       x = "Estimated % 'converting' with 60% and 90% HDI")  +
      facet_grid(~level, scales = "free")
)

(
reach_to_clicks_aud_plot_no_cause <-  full_post  %>%
    filter(!str_det(theme, "Animal|Climate|Poverty"))  %>%
        aud_plots() +
 labs(title = "By audience, no cause videos",
       x = "Estimated % 'converting' with 60% and 90% HDI")  +
      facet_grid(~level, scales = "free")
)

(
reach_to_clicks_aud_plot_cause <-  full_post  %>%
    filter(str_det(theme, "Animal|Climate|Poverty"))  %>%
        aud_plots() +
 labs(title = "By audience, Cause videos only",
       x = "Estimated % 'converting' with 60% and 90% HDI")  +
      facet_grid(~level, scales = "free")
)



## ----reach_to_clicks_aud_plot_climate--------
(
reach_to_clicks_aud_plot_climate <-  full_post  %>%
    filter(str_det(theme, "Climate"))  %>%
    filter(audience!="Lookalikes") %>% 
        aud_plots() +
 labs(title = "By audience, Climate videos only",
       x = "Estimated % 'converting' with 60% and 90% HDI")  +
      facet_grid(~level, scales = "free")
)



## --------------------------------------------

vid_plots <- function(df) {
  df %>% 
    group_by(level, theme) %>% #, starts
    sum_mean_hdi %>% 
    mutate(theme = reorder(as.factor(theme), `mean`)) %>% 
    ggplot() + 
  geom_errorbarh(aes(xmin = lower_n, xmax = upper_n, y = theme), height = .7, color = "red") +
  geom_errorbarh(aes(xmin = lower_w, xmax = upper_w, y = theme),  height=.25, color = "blue") +
  geom_point(
    aes(x=`mean`, 
    y = theme))
    } 

(
reach_to_clicks_vid_plot <-  full_post  %>%
    vid_plots() +
  labs(title = "By video theme (reach, clicks, signups)",
       x = "Estimated % 'converting' with 60% and 90% HDI")  +
      facet_grid(~level, scales = "free")
)

(
reach_to_clicks_vid_plot_phil <-  full_post  %>%
            filter(audience == "Philanthropy")  %>%
    vid_plots() +
  labs(title = "By theme, 'philanthropy' audience only",
       x = "Estimated % 'converting' with 60% and 90% HDI")  +
      facet_grid(~level, scales = "free")
)




## ----full_plots------------------------------

(
reach_to_clicks_plot <-  full_post  %>%
      filter(grepl("1", level)) %>% 
    group_by(theme, audience) %>% #, starts
    sum_mean_hdi %>% 
      mutate(theme = reorder(as.factor(theme), `mean`)) %>% 
      filter(audience!="Retargeting")  %>%
    ggplot() + 
  geom_errorbarh(aes(xmin = lower_n, xmax = upper_n, y = theme), height = .7, color = "red") +
  geom_errorbarh(aes(xmin = lower_w, xmax = upper_w, y = theme),  height=.25, color = "blue") +
  geom_point(aes(x = mean, y = theme)) +
  coord_cartesian(xlim=c(0, 1.75)) +
  facet_wrap(~audience, scales = "fixed") +
  labs(title = "Clicks by video theme and audience",
       x = "Estimated % clicking with 60% and 90% HDIs") +
  facet_wrap(~audience, scales = "fixed")
)

(
reach_to_results_plot <-  full_post %>% 
      filter(grepl("3", level)) %>% 
      group_by(theme, audience) %>% #, startslevel, 
      sum_mean_hdi %>% 
        mutate(theme = reorder(as.factor(theme), `mean`)) %>% 
      filter(audience!="Retargeting")  %>%
    ggplot() + 
  geom_errorbarh(aes(xmin = lower_n, xmax = upper_n, y = theme), height = .7, color = "red") +
  geom_errorbarh(aes(xmin = lower_w, xmax = upper_w, y = theme),  height=.25, color = "blue") +  geom_point(aes(x = mean, y = theme)) +
  facet_wrap(~audience, scales = "fixed") +
    coord_cartesian(xlim=c(0, 0.8)) +
  labs(title = "Reach to results (total), by audience by theme",
       x = "Estimated % results with 60% and 90% HDIs") +
  facet_wrap(~factor(audience, levels=audience_levels), scales = "fixed")
)

(
reach_to_results_plot_flip <-  full_post %>% 
      filter(grepl("3", level)) %>% 
      group_by(audience, theme) %>% #, startslevel, 
      sum_mean_hdi %>% 
        mutate(audience = reorder(as.factor(audience), `mean`)) %>% 
      filter(audience!="Retargeting")  %>%
    ggplot() + 
  geom_errorbarh(aes(xmin = lower_n, xmax = upper_n, y = audience), height = .7, color = "red") +
  geom_errorbarh(aes(xmin = lower_w, xmax = upper_w, y = audience),  height=.25, color = "blue") +  geom_point(aes(x = mean, y = audience)) +
  facet_wrap(~audience, scales = "fixed") +
    coord_cartesian(xlim=c(0, 0.8)) +
  labs(title = "Reach to results (total), by theme by audience",
       x = "Estimated % results with 60% and 90% HDIs") +
  facet_wrap(~theme, scales = "fixed")
)



## ----reach_to_results_sum_text_only----------



reach_to_results_sum_text_only <-  full_post_text_only %>% 
      filter(grepl("3", level)) %>% 
      group_by(text_treat) %>% #, startslevel, 
      sum_mean_hdi( CI_choice_n=0.90, CI_choice_w = 0.99) %>%         
  mutate(text_treat = reorder(as.factor(text_treat), `mean`)) 

reach_to_results_sum_text_only  %>%
  DT::datatable(caption = "Texts: mean performance by text, 0% and 99% HDI bounds", filter="top",  rownames= FALSE) %>%
  formatRound(1:length(reach_to_results_sum_text_only), digits=3)

(
reach_to_results_sum_text_only_plot <- 
reach_to_results_sum_text_only %>% 
    ggplot() + 
  geom_errorbarh(aes(xmin = lower_n, xmax = upper_n, y = text_treat), height = .7, color = "red") +
  geom_errorbarh(aes(xmin = lower_w, xmax = upper_w, y = text_treat),  height=.25, color = "blue") +  geom_point(aes(x = mean, y = text_treat)) +
    coord_cartesian(xlim=c(0.13, .45)) +
  labs(title = "Reach to results (total), by text",
       x = "Estimated % results with 90% and 99% HDIs") 
)


## ----reach_results_text----------------------

reach_to_results_sum_text <-  full_post_text %>% 
      filter(grepl("3", level)) %>% 
      group_by(text_treat, audience) %>% #, startslevel, 
      sum_mean_hdi %>%         
  mutate(text_treat = reorder(as.factor(text_treat), `mean`)) %>% 
      filter(audience!="Retargeting")  

reach_to_results_sum_text  %>%
  DT::datatable(caption = "Texts:  mean performance by audience, 60 and 90% HDI bounds", filter="top",  rownames= FALSE) %>%  
    formatRound(1:length(reach_to_results_sum_text_only), digits=3)


(
reach_to_results_sum_text_plot <- 
reach_to_results_sum_text %>% 
    ggplot() + 
  geom_errorbarh(aes(xmin = lower_n, xmax = upper_n, y = text_treat), height = .7, color = "red") +
  geom_errorbarh(aes(xmin = lower_w, xmax = upper_w, y = text_treat),  height=.25, color = "blue") +  geom_point(aes(x = mean, y = text_treat)) +
  facet_wrap(~audience, scales = "fixed") +
    coord_cartesian(xlim=c(0, .6)) +
  labs(title = "Reach to results (total), by audience by text",
       x = "Estimated % results with 60% and 90% HDIs") +
  facet_wrap(~factor(audience, levels=audience_levels), scales = "fixed") 
)

(
reach_to_results_plot_flip <-  full_post_text %>% 
      filter(grepl("3", level)) %>% 
      group_by(audience, text_treat) %>% #, startslevel, 
      sum_mean_hdi %>% 
        mutate(audience = reorder(as.factor(audience), `mean`)) %>% 
      filter(audience!="Retargeting")  %>%
    ggplot() + 
  geom_errorbarh(aes(xmin = lower_n, xmax = upper_n, y = audience), height = .7, color = "red") +
  geom_errorbarh(aes(xmin = lower_w, xmax = upper_w, y = audience),  height=.25, color = "blue") +  geom_point(aes(x = mean, y = audience)) +
  facet_wrap(~audience, scales = "fixed") +
    coord_cartesian(xlim=c(0, 1)) +
  labs(title = "Reach to results (total), by text by audience",
       x = "Estimated % results with 60% and 90% HDIs") +
  facet_wrap(~text_treat, scales = "fixed")
)



## --------------------------------------------

# Joining spending and conversion ####

cost_tibble <- 
  left_join(tib_combined_post,
    rename(gg_video_breakdowns_col, theme=video_theme)) %>% 
  mutate(
    reach_per_dollar = reach / spend,
    sign_per_dollar = reach_per_dollar * probability,
    #NOTE this uses the simulated distribution of probabilities, not just averages! 
    sign_per_100d = 100*sign_per_dollar,
    cost_per_signup = 1/sign_per_dollar
    ) %>% 
  filter(is.na(spend) == FALSE)  

cost_summary <- cost_tibble %>% 
  group_by(theme, audience) %>% 
  sum_mean_hdi(var= sign_per_100d, scaleme=1)

#dim(filter(cost_summary, check > 2))[1] ... where HDI is not continuous, I guess... this doesn't happen here atm



## ----sign_per_100d_plot_vid_by_aud-----------

(
  sign_per_100d_plot_vid_by_aud <- cost_tibble %>%
      filter(audience!="Retargeting")  %>%
ggplot() +
  scale_x_continuous(limits = c(0, 20)) +
  ggridges::geom_density_ridges(
    aes(
      x = sign_per_100d, 
      y = theme
      )
    ) +
  geom_point(data = cost_summary %>% filter(audience!="Retargeting"), 
    aes(x = mean, y = theme)) +
  labs(title = "Signups per $100: Comparing videos by audience",
    x = "Density plots, means") +
  facet_wrap(~factor(audience, levels = audience_levels), scales = "free_x"  )
)

# 
# ggplot(filter(cost_tibble, upper_eti < 100)) +
#   geom_errorbarh(aes(xmin = lower_eti, xmax = upper_eti, y = audience)) +
#   geom_point(aes(x = mean, y = audience)) +
#   labs(title = "Cost per signup ($)",
#     x = "Estimated cost per signup (USD) with 95% ETI") +
#   facet_wrap(~theme, scales = "free_x")



## ----sign_per_100d_plot_aud_by_vid-----------
(
  sign_per_100d_plot_aud_by_vid <- cost_tibble %>%
      filter(audience!="Retargeting" & audience!="General audience")  %>%
ggplot() +
  scale_x_continuous(limits = c(0, 20)) +
  ggridges::geom_density_ridges(
    aes(
      x = sign_per_100d, 
      y = audience
      )
    ) +
  geom_point(data = cost_summary %>% filter(audience!="Retargeting" & audience!="General audience"), 
    aes(x = mean, y = audience)) +
  labs(title = "Signups per $100: Comparing audiences for each video",
    x = "Density plots, means") +
  facet_wrap(~theme, scales = "free_x")
)




## ----targets, eval=FALSE---------------------
## 
## #THE content belo
## #targets:
## bin_out <- c("d_don_1k", "d_don_10pct")
## 
## num_out <- c('donation_usd', 'don_av2_yr', 'l_don_usd', "l_don_av_2yr", "don_share_inc_imp_bc5k", "donation_plan_usd")
## targets <- c(bin_out, num_out)
## targets_short <- c("don_av2_yr", "don_share_inc_imp_bc5k", "d_don_1k")
## 
## #Note -- don_av2_yr is the right one for qpoisson as it already expresses things in exponents. l_don_av2_yr was the one to use in the loglinear model, which we are not emphasizing
## 
## targets_short_names <- c("Log (Avg don +1)", "Don/Income", "Donated 1k+")


## ----impute_norm_features, warning=FALSE-----
#imputing and normalization


## ----key_labels, eval=FALSE------------------
## 
## #labeling for model output
## 
## key_eas_all_labels <- c( #note these are converted to a list with as.list before assigning them
##     donation_usd = "Donation (USD)",
##     l_don_usd = "Log Don. (USD)",
##     l_don_av_2yr = "Log Don. 'avg.'",
##     ln_age = "Log age",
##     don_av2_yr = "Don. 'avg'",
##     donation_plan_usd = "Don. plan (USD)")


## ----features, eval = FALSE------------------
## 
## feat_list = list(
##   c(key_demog, feat_income_employ, controls),
##   c(key_demog, feat_income_employ, controls, robust_controls),
##   c(key_demog, feat_income_employ, feat_gwwc_etg, controls) )
## 
## feat_names = c("Baseline", "Robust controls",  "Base + EtG & GWWC")
## 


## ----vars_list, eval = FALSE-----------------
## 
## rhs_vars_list <- rep(feat_list, length(targets_short))
## 
## outcome_vars_list <- rep(as.list(targets_short), each=length(feat_list))
## 
## dfs <- rep(list(eas_all_s_rl_imp), length(outcome_vars_list))
## 


## ----model-prep-linear, warning=FALSE, eval = FALSE----
## 
## ## Create dataframe for modeling
## (linear_models <- make_model_df(rhs_vars_list, outcome_vars_list, dfs))
## 


## ----fit_linear_models, eval=FALSE-----------
## 
## linear_models <- linear_models %>%
##   mutate(
##     lm_fit = fit_models(
##       linear_models, "formulas", "dfs", fun = fit_lm)
##     )
## 
## # Extract coefficients, fitted and residuals
## model_feat_names <- rep(c(feat_names), times= length(targets_short))
## model_oc_names <- rep(c(targets_short_names), each= length(feat_names))
## model_names <- paste(model_oc_names, model_feat_names, sep = ": ")
## 


## ----set_base_levels-------------------------
#set base levels before doing modeling

gg_campaign_by_ad_by_text_age_gender$age <- factor( gg_campaign_by_ad_by_text_age_gender$age,
  ordered = FALSE )

gg_campaign_by_ad_by_text_age_gender$age <- relevel( gg_campaign_by_ad_by_text_age_gender$age,
  ref="18-24")

gg_campaign_by_ad_by_text_age_gender$audience <- factor( gg_campaign_by_ad_by_text_age_gender$audience,
  ordered = FALSE )

gg_campaign_by_ad_by_text_age_gender$audience <- relevel( gg_campaign_by_ad_by_text_age_gender$audience,
  ref="Philanthropy")



## --------------------------------------------

summarise_stuff <- function(df) {
  df %>%
     summarise(
      amount_spent_usd = sum(amount_spent_usd),
      reach=sum(reach),
      link_clicks = sum(link_clicks, na.rm = TRUE),
      results = sum(results, na.rm = TRUE),
      cost_per_impression = amount_spent_usd/reach,
      cost_per_click = amount_spent_usd/link_clicks,
      results = ifelse(is.na(results), 0, results ),
      results_per_100usd = results/(amount_spent_usd/100),
      results_per_1k_reach = results*1000/reach)
}

model_data_0 <- gg_campaign_by_ad_by_text_age_gender %>%
  filter(audience!="Retargeting") %>%
  ungroup() %>%
    group_by(video_theme, text_treat, audience) %>% #starts,
    #filter(reach>200) %>%
    summarise_stuff() %>%
  ungroup() %>%
  as.tibble() %>%
  mutate(
    across(c(video_theme, text_treat, audience), #starts,
      as.factor )
    )

model_data_start <- gg_campaign_by_ad_by_text_age_gender %>%
  filter(audience!="Retargeting") %>%
  ungroup() %>%
    group_by(video_theme, text_treat, audience, starts) %>% #starts,
    #filter(reach>200) %>%
        summarise_stuff() %>%
  ungroup() %>%
  as.tibble() %>%
  mutate(
    across(c(video_theme, text_treat, audience, starts), #starts,
      as.factor )
    )

model_data <- gg_campaign_by_ad_by_text_age_gender %>%
    filter(audience!="Retargeting") %>%
  ungroup() %>%
    group_by(video_theme, text_treat, audience, gender, agetrin) %>% #starts,
    #filter(reach>200) %>%
        summarise_stuff() %>%
  ungroup() %>%
  as.tibble() %>%
  mutate(
    across(c(video_theme, text_treat, audience, gender, agetrin), #starts,
      as.factor )
    )

Rp100usd_vid_text_audience0 <- model_data_0 %>%
  lm(
    results_per_100usd ~ 1 + video_theme + text_treat + audience,
    data = .,
    weights = reach)


Rp100usd_vid_text_audience_starts <- model_data_start %>%
  lm(
    results_per_100usd ~ 1 + starts + video_theme + text_treat + audience,
    data = .,
    weights = reach)


Rp100usd_vid_text_audience_demo <- model_data %>%
  lm(
    results_per_100usd ~ 1 + video_theme + text_treat + audience + gender + agetrin,
    data = .,
    weights = reach)

#Do NOT do this: Rp100usd_vid_text_audience0$df.residual <- sum(model_data_0$reach) - length(coef(Rp100usd_vid_text_audience0))

huxtable::huxreg(Rp100usd_vid_text_audience0, Rp100usd_vid_text_audience_starts, Rp100usd_vid_text_audience_demo)




## --------------------------------------------

limits <- aes(ymax = mean_dv + (se_dv), ymin = mean_dv - (se_dv))
dodge <- position_dodge(width = 0.9)


gg_gg_options <- list(geom_bar(stat = 'identity', position=dodge),
  geom_errorbar(limits, position=dodge,  width=0.25, color="red"),
  jtools::theme_apa(),
  theme(legend.position="none"),
  geom_text(aes(label = paste("$", mean_dv %>% round(.,2)), y=5), position = position_dodge(.9), size=4, color="white"),
  theme(text=element_text(size=10))
)
  

limits <- aes(ymax = mean_dv , ymin = mean_dv)
dodge <- position_dodge(width = 0.9)



(
  result_by_age <-  gg_campaign_by_ad_by_text_age_gender %>%
        mutate(results_per_100usd = results/(amount_spent_usd/100)) %>%
     uncount(weights = .$reach) %>%
   group_by(age) %>%
  summarise(mean_dv = mean(results_per_100usd, na.rm=TRUE)) %>% 
  ggplot(aes(x=age, mean_dv)) +
  geom_bar(stat='identity',fill="#0072B2", position=dodge) +
  ylab('Results/$ spent') +
  xlab('Video') +
  ggtitle('Results/$ spent by Age') 
#+  scale_x_discrete(labels=vid_types)
)




## ----common_plot_options---------------------

#Plot options in common

#limits <- aes(ymax = mean_dv + (ci_spread), ymin = mean_dv - (ci_spread))
dodge <- position_dodge(width = 0.9)

vid_types <-
  c("factual short",
    "animal",
    "climate",
    "factual long",
    "branded",
    "poverty")



grpsumgg <- function(df, gvar, var, ci_ends) {
  df %>%
  group_by({{gvar}}) %>%
  summarise(mean_dv = mean({{var}}, na.rm=TRUE),
            se_dv = sd({{var}}, na.rm=TRUE)/sqrt(n()),
            ci_spread = qt(1 - (ci_ends / 2), n() - 1) * se_dv)
}



## ----gwwc_vid_results_plot-------------------

gg_video_breakdowns %>%
        mutate(results_per_100usd = results/(amount_spent_usd/100)) %>%
     uncount(weights = .$reach) %>%
   group_by(video_theme) %>%
  summarise(mean_dv = mean(results_per_100usd, na.rm=TRUE)) %>% 
  #grpsumgg(video_theme, results_per_100usd, 0.1) %>%
  ggplot(aes(x=video_theme, mean_dv)) +
  geom_bar(stat='identity',fill="#0072B2", position=dodge) +
  #gg_gg_options +
  ylab('Results/$ spent') +
  xlab('Video') +
  #ggtitle('Results/$ spent by Video, 90% CIs') +
  ggtitle('Results/$ spent by Video') +
  scale_x_discrete(labels=vid_types)


# gwwc_vid_results %>%
#   grpsumgg(media, DV_costadj, 0.1) %>%
#   ggplot(aes(x=media, y=mean_dv)) +
#   geom_bar(stat='identity',fill="#0072B2", position=dodge) +
#   gg_gg_options +
#   ylab('Results/$ spent') +
#   xlab('Video') +
#   ggtitle('Results/$ spent by Video, 90% CIs') +
#   scale_y_continuous(limits = c(0,.2),  breaks=seq(0,.2, by=.05)) +
#   scale_x_discrete(labels=vid_types)
# 

