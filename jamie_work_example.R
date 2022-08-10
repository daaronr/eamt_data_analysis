# possible there are other packages I don't realise I've used cos I was just doing this
# in an r session that was already running
library(tidyverse)
library(brms)
library(tidybayes)

adset_1 <- readr::read_rds("gg_campaign_by_ad_by_text_age_gender.RData")
adset_2 <- readr::read_rds("gg_video_breakdowns.RData")

names(adset_1)
names(adset_2)

# collapse into the categories of interest
#group it by campaigh

adset_2_coll <- adset_2 %>% group_by(video_theme, audience) %>% summarise(results = sum(results, na.rm = TRUE),
  spend = sum(amount_spent_usd, na.rm = TRUE),
  clicks = sum(link_clicks, na.rm = TRUE),
  impressions = sum(impressions, na.rm = TRUE),
  reach = sum(reach, na.rm = TRUE))

#the function below is totally unnecesaary/counterproductive!
# but leaving in for now, it just turns the data into actual observations
# but this just massively slows down the model (as bernoulli) vs. binomial
# binarify <- function(input, type = "clicks") {
#   
#   if(type == "clicks") {
#     binary_tibble <- tibble(theme = input[[1, "video_theme"]],
#                             audience = input[[1, "audience"]],
#                             clicked = c(rep(1, input[[1, "clicks"]]), rep(0, (input[[1, "impressions"]] - input[[1, "clicks"]]))))
#   }
#   else if(type == "results") {
#     binary_tibble <- tibble(theme = input[[1, "video_theme"]],
#                             audience = input[[1, "audience"]],
#                             signedup = c(rep(1, input[[1, "results"]]), rep(0, (input[[1, "clicks"]] - input[[1, "results"]]))))
#   }
#   
#   return(binary_tibble)
#   
# }
# 
# clicks_data <- map_dfr(.x = adset_2_coll %>% group_by(video_theme, audience) %>% group_split(),
#                        .f = binarify,
#                        type = "clicks")
# 
# results_data <- map_dfr(.x = adset_2_coll %>% group_by(video_theme, audience) %>% group_split(),
#                         .f = binarify,
#                         type = "results")

# checking some reasonable priors by looking at %age conversion rate with different numbers
plogis(-3.5)


clicks_reg2 <- brm(formula = clicks | trials(reach) ~ video_theme + audience + video_theme:audience,
  family = binomial("logit"),
  data = adset_2_coll,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  prior = c(prior(normal(-3.5, 2), class = Intercept),
    prior(normal(0, 1), class = b)),
  init = 0,
  chains = 4,
  cores = 4,
  iter = 2500,
  warmup = 500,
  backend = "cmdstanr",
  threads = threading(8),   
  seed = 1010)

signedup_reg2 <- brm(formula = results | trials(clicks) ~ video_theme + audience + audience:video_theme,
  family = binomial("logit"),
  data = adset_2_coll,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  prior = c(prior(normal(-1, 2), class = Intercept),
    prior(normal(0, 1), class = b)),
  #init = 0,
  chains = 4,
  cores = 4,
  iter = 2500,
  warmup = 500,
  backend = "cmdstanr",
  threads = threading(4),
  seed = 1010)

# posterior expectations:
full_crossing <- expand_grid("video_theme" = unique(adset_2_coll$video_theme),
  "audience" = unique(adset_2_coll$audience)) %>% 
  mutate(reach = 1,
    clicks = 1)
# setting reach/clicks = 1 will give us proportion of conversions

click_post <- posterior_epred(clicks_reg2,
  ndraws = 1000,
  newdata = full_crossing)

signedup_post <- posterior_epred(signedup_reg2,
  ndraws = 1000,
  full_crossing)

combined_post <- click_post * signedup_post

tib_click_post <- as_tibble(click_post) %>% pivot_longer(cols = everything(),
  names_to = "identifier",
  values_to = "probability") %>% 
  mutate(level = "1. Reach to clicks")
tib_click_post$theme <- rep(full_crossing$video_theme, 1000)
tib_click_post$audience <- rep(full_crossing$audience, 1000)

tib_signedup_post <- as_tibble(signedup_post)  %>% pivot_longer(cols = everything(),
  names_to = "identifier",
  values_to = "probability") %>% 
  mutate(level = "2. Clicks to signups")
tib_signedup_post$theme <- rep(full_crossing$video_theme, 1000)
tib_signedup_post$audience <- rep(full_crossing$audience, 1000)

tib_combined_post <- as_tibble(combined_post) %>% pivot_longer(cols = everything(),
  names_to = "identifier",
  values_to = "probability") %>% 
  mutate(level = "3. Total")
tib_combined_post$theme <- rep(full_crossing$video_theme, 1000)
tib_combined_post$audience <- rep(full_crossing$audience, 1000)

full_post <- bind_rows(tib_click_post,
  tib_signedup_post,
  tib_combined_post)

post_summary <- full_post %>% group_by(level, theme, audience) %>% 
  summarise(mean = mean(probability) * 100,
    lower = hdi(probability)[1] * 100,
    upper = hdi(probability)[2] * 100)

ggplot(filter(post_summary, grepl("1", level))) +
  geom_errorbarh(aes(xmin = lower, xmax = upper, y = audience)) +
  geom_point(aes(x = mean, y = audience)) +
  facet_wrap(~theme, scales = "free_x") +
  labs(title = "Reach to clicks",
    x = "Estimated %age conversion with 95% HDI") +
  facet_wrap(~theme, scales = "free_x")

ggplot(filter(post_summary, grepl("2", level))) +
  geom_errorbarh(aes(xmin = lower, xmax = upper, y = audience)) +
  geom_point(aes(x = mean, y = audience)) +
  facet_wrap(~theme, scales = "free_x") +
  labs(title = "Clicks to signups",
    x = "Estimated %age conversion with 95% HDI") +
  facet_wrap(~theme, scales = "free_x")

ggplot(filter(post_summary, grepl("3", level))) +
  geom_errorbarh(aes(xmin = lower, xmax = upper, y = audience)) +
  geom_point(aes(x = mean, y = audience)) +
  labs(title = "Total conversion rate",
    x = "Estimated %age conversion with 95% HDI") +
  facet_wrap(~theme, scales = "free_x")

#### Joining spending and conversion ####
adset_2_coll <- adset_2_coll %>% mutate(theme = video_theme)

cost_tibble <- 
  left_join(tib_combined_post,
    adset_2_coll) %>% 
  mutate(reach_per_dollar = reach / spend,
    sign_per_dollar = reach_per_dollar * probability,
    cost_per_signup = 1 / sign_per_dollar) %>% 
  filter(is.na(spend) == FALSE)

# as a potential substantive point, I also wonder if the cost should also be done at two levels
# we could first make cost per click. Then from there we'd use the model to say the n clicks and
# cost per click, and use that to calculate the final cost per signup.
# At the moment it just takes the final conversion rate from the combined model and gets
# the cost by using the amount spent at the start.
# DR: Not sure I understand what you are proposing; maybe something we could talk through?


# the 'check' here finds those places where the HDI is split
# that means it can't be simply plotted with 1 whisker
# and I haven't yet worked out a quick function to plot multiple whiskers
cost_summary <- cost_tibble %>% group_by(theme, audience) %>% 
  summarise(mean = mean(cost_per_signup),
    lower = hdi(cost_per_signup)[1],
    upper = hdi(cost_per_signup)[2],
    check = length(hdi(cost_per_signup)),
    lower_eti = quantile(cost_per_signup, .025),
    upper_eti = quantile(cost_per_signup, .975))

filter(cost_summary, check > 2)

ggplot(cost_tibble) +
  scale_x_continuous(limits = c(0, 100)) +
  geom_density_ridges(aes(x = cost_per_signup, y = audience)) +
  #geom_point(aes(x = mean, y = audience)) +
  labs(title = "Cost per signup ($)",
    x = "Estimated cost per signup (USD) with 95% ETI") +
  facet_wrap(~theme, scales = "free_x")

ggplot(filter(cost_summary, upper_eti < 100)) +
  geom_errorbarh(aes(xmin = lower_eti, xmax = upper_eti, y = audience)) +
  geom_point(aes(x = mean, y = audience)) +
  labs(title = "Cost per signup ($)",
    x = "Estimated cost per signup (USD) with 95% ETI") +
  facet_wrap(~theme, scales = "free_x")


SOME SUGGESTED THINGS TO DO

LMER package 

clicks | trials(reach) ~ 1 + video_theme + audience + video_theme:audience + (video_theme | campaign) + (audience | campaign) + (video_theme:audience | campaign) ~ 1 + video*audience*campaign

clicks | trials(reach) ~ 1 + video_theme + audience + video_theme:audience + campaign + (video_theme | campaign) + (audience | campaign) + (video_theme:audience | campaign)

results_per_click_video_aud <- as.formula("results | trials(clicks) ~ video_theme + audience + video_theme:audience")

~ 1 + (1 | video_theme) + (1 | audience) + (1 | video_theme:audience)


I think random effects do not deal with OVB ... Jamie does not agree; we may have different terms for random effects -- see slack thread [here](https://rethinkpriorities.slack.com/archives/G01BDCD2QPR/p1660060733533869)