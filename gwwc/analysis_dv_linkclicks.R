### GWWC Feb 22 Facebook Message Test: DV Link Clicks Analysis ###

# DRL Removed setwd stuff because we need things to be portable
#  DR: Removed package install here because renv will do it for us

# DR: Move data build over to the other file because you gotta separate build and analysis

#### DATA SUMMMARY ####

print(data %>% group_by(theme) %>% summarise(n = n(), cost = mean(ave.cost.impr) *
    100), n = 50)
print(data %>% group_by(theme_text) %>% summarise(n = n(), cost = mean(ave.cost.impr) *
    100),
  n = 50)

print(data %>% group_by(theme) %>% summarise(n = n(), cost = mean(ave.cost.impr)), n =
    41)
print(data %>% group_by(theme, theme_text) %>% summarise(n = n(), cost =
    mean(ave.cost.impr)),
  n = 100)

print(data %>% filter(ave.cost.impr > 0) %>% group_by(theme) %>% summarise(n =
    n(), cost = mean(costadj_DV)))
print(
  data %>% filter(ave.cost.impr > 0) %>% group_by(theme_text) %>% summarise(n =
      n(), cost = mean(costadj_DV))
)

#### MODELS ####

## DEMOGRAPHICS
#just demographic, not control
summary(lm(data = data, DV ~ Gender + Age))
#just demographic, controlling for cost
summary(lm(data = data, DV ~ Gender + Age + ave.cost.impr))

# cost adjusted DV and demographics
summary(lm(data = data, costadj_DV ~ Gender + Age))

## THEME
#no controls
summary(lm(data = data, DV ~ theme))
#control for cost only
summary(lm(data = data, DV ~ theme + ave.cost.impr))

#cost adjusted DV
summary(lm(data = data, costadj_DV ~ theme))
#cost adjusted DV + control for demographics
summary(lm(data = data, costadj_DV ~ theme + Age + Gender))

## MESSAGES
#no controls
summary(lm(data = data, DV ~ theme_text))
#control for cost only
summary(lm(data = data, DV ~ theme_text + ave.cost.impr))

#cost adjusted DV
summary(lm(data = data, costadj_DV ~ theme_text))
#cost adjusted DV + control for demographics
summary(lm(data = data, costadj_DV ~ theme_text + Age + Gender))


#### CHARTS ####

# CHART 1: Link clicks/$ spent by Theme

print(
  data %>% filter(ave.cost.impr > 0) %>% group_by(theme) %>% summarise(
    results = mean(costadj_DV),
    SE = std.error(costadj_DV),
    n = n()
  ),
  n = 50
)
levels(data$theme)

limits <- aes(ymax = mean_dv + (se_dv), ymin = mean_dv - (se_dv))
dodge <- position_dodge(width = 0.9)

data %>%
  group_by(theme) %>%
  summarise(
    mean_dv = mean(costadj_DV, na.rm = TRUE),
    se_dv = sd(costadj_DV, na.rm = TRUE) / sqrt(n())
  ) %>%
  ggplot(aes(x = theme, y = mean_dv)) +
  geom_bar(stat = 'identity',
    fill = "#0072B2",
    position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.05) +
  theme_apa() +
  ylab('Link Clicks/$ Spent') +
  xlab('Message Theme') +
  ggtitle('Link Clicks/$ Spent by Theme') +
  theme(legend.position = "none") +
  theme(text = element_text(size = 11)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(
    limits = c(0, 1.25),
    oob = rescale_none,
    breaks = seq(0, 1.25, by = .25)
  ) +
  scale_x_discrete(labels = c(
    "effectiveness",
    "giving more",
    "services",
    "social proof",
    "values"
  ))

# CHART 2: Link clicks/$ spent by Message & Theme

print(
  data %>% filter(ave.cost.impr > 0) %>% group_by(theme, theme_text) %>% summarise(
    results = mean(costadj_DV),
    SE = std.error(costadj_DV),
    n = n()
  ),
  n = 50
)

limits <- aes(ymax = mean_dv + (se_dv), ymin = mean_dv - (se_dv))
dodge <- position_dodge(width = .9)

data %>%
  group_by(theme, theme_text) %>%
  summarise(
    mean_dv = mean(costadj_DV, na.rm = TRUE),
    se_dv = sd(costadj_DV, na.rm = TRUE) / sqrt(n())
  ) %>%
  ggplot(aes(x = theme_text, y = mean_dv, fill = theme)) +
  scale_fill_brewer(palette = "Paired") +
  geom_bar(stat = 'identity', position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.05) +
  theme_apa() +
  ylab('Link Clicks/$ Spent') +
  xlab('Message') +
  ggtitle('Link Clicks/$ Spent by Message') +
  theme_apa(legend.font.size = 10, legend.use.title = TRUE) +
  theme(text = element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0))
  ) +
  scale_y_continuous(limits = c(0, 2),
    oob = rescale_none,
    breaks = seq(0, 2, by = .25)) +
  scale_x_discrete(
    labels = c(
      "Give more effectively",
      "Same donation more impact",
      "Effectiveness matters",
      "Do More Good",
      "Save a life each year",
      "Give more, feel fulfilled",
      "Donate to great charities",
      "Find the best charities",
      "Learn to do good better",
      "Give like Nobel laureates",
      "Join 8,000+ givers",
      "Thousands have \nmaximised their impact",
      "Are you in alignment \nwith your values?",
      "Live up to your values",
      "I want a better world"
    )
  )

# CHART 3: Link clicks/$ spent by Age & Gender
# Note: Removed ages 13-17 bc N = 7
# Note: Removed Gender unknown bc error bars very large and small sample size

print(
  data %>% filter(ave.cost.impr > 0 &
      Age != "13-17" &
      Gender != "unknown") %>% group_by(Age, Gender) %>% summarise(
        results = mean(costadj_DV),
        SE = std.error(costadj_DV),
        n = n()
      ),
  n = 50
)

limits <- aes(ymax = mean_dv + (se_dv), ymin = mean_dv - (se_dv))
dodge <- position_dodge(width = .9)

data %>% filter(ave.cost.impr > 0 &
    Age != "13-17" & Gender != "unknown") %>%
  group_by(Age, Gender) %>%
  summarise(
    mean_dv = mean(costadj_DV, na.rm = TRUE),
    se_dv = sd(costadj_DV, na.rm = TRUE) / sqrt(n())
  ) %>%
  ggplot(aes(x = Age, y = mean_dv, fill = Gender)) +
  scale_fill_brewer(palette = "Paired") +
  geom_bar(stat = 'identity', position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.05) +
  theme_apa() +
  ylab('Link Clicks/$ Spent') +
  xlab('Age') +
  ggtitle('Link Clicks/$ Spent by Age and Gender') +
  theme_apa(legend.font.size = 10, legend.use.title = TRUE) +
  theme(text = element_text(size = 13)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0))
  ) +
  scale_y_continuous(
    limits = c(0, 1.25),
    oob = rescale_none,
    breaks = seq(0, 1.25, by = .25)
  ) +
  scale_x_discrete(labels = c("18-24", "25-34", "35-44"))


# CHART 4: Link clicks/$ spent by Theme & Age
# Note: Removed ages 13-17 bc N = 7

print(
  data %>% filter(ave.cost.impr > 0 &
      Age != "13-17") %>% group_by(theme, Age) %>% summarise(
        results = mean(costadj_DV),
        SE = std.error(costadj_DV),
        n = n()
      ),
  n = 50
)

limits <- aes(ymax = mean_dv + (se_dv), ymin = mean_dv - (se_dv))
dodge <- position_dodge(width = .9)

data %>% filter(ave.cost.impr > 0 & Age != "13-17") %>%
  group_by(theme, Age) %>%
  summarise(
    mean_dv = mean(costadj_DV, na.rm = TRUE),
    se_dv = sd(costadj_DV, na.rm = TRUE) / sqrt(n())
  ) %>%
  ggplot(aes(x = Age, y = mean_dv, fill = theme)) +
  scale_fill_brewer(palette = "Paired") +
  geom_bar(stat = 'identity', position = dodge) +
  geom_errorbar(limits, position = dodge, width = 0.05) +
  theme_apa() +
  ylab('Link Clicks/$ Spent') +
  xlab('Age') +
  ggtitle('Link Clicks/$ Spent by Theme and Age') +
  theme_apa(legend.font.size = 10, legend.use.title = TRUE) +
  theme(text = element_text(size = 12)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0))
  ) +
  scale_y_continuous(limits = c(0, 2),
    oob = rescale_none,
    breaks = seq(0, 2, by = .25)) +
  scale_x_discrete(labels = c("18-24", "25-34", "35-44"))
