library(readr)
library(dplyr)
library(broom)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(timesTheme)

# opposite of intersect()
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

#########################################################################
# load all data
provinces_regions <- read_csv("data/provinces_regions.csv")
results_provinces_2013 <- read_csv("data/2013_results_provinces.csv")
results_districts_2013 <- read_csv("data/2013_results_districts.csv")

# people at risk of poverty or social exclusion by NUTS-2
# source: Eurostat | data: 2016
poverty_title <- "People at risk of poverty or social exclusion, by party"
poverty <- read_csv("data/ilc_peps11_1_Data.csv") %>%
  select(-`Flag and Footnotes`, -TIME, -UNIT)
names(poverty) <- c('District', 'poverty_2016')

poverty_plot <- merge(results_districts_2013, poverty, by='District') %>%
  select(-District) %>%
  melt(id=c("District Long", "poverty_2016")) %>% 
  na.omit() %>%
  mutate(vote_share = as.numeric(value)) %>%
  select(-value) %>%
  ggplot(aes(x=poverty_2016, y=vote_share, color=variable)) + 
  geom_point(aes(alpha=0.5)) + 
  facet_grid(. ~ variable) + 
  geom_smooth(method="lm") +
  ggtitle(label=poverty_title) + times_theme()

# usual resident population by NUTS-3
# source: Eurostat | data: 2016
# plot and lm based on a ratio between 65+ compared to the younger population (15 to 64)
age_title <- "Over 65 compared to the general pop, by party"
age_groups <- read_csv("data/20152016_age_groups.csv")
names(age_groups) <- c('Province', '15_64', 'over_65')

age_plot <- merge(results_provinces_2013, age_groups) %>% 
  mutate(ratio = `over_65` / `15_64`) %>% 
  select(-`15_64`, -`over_65`) %>%
  melt(id=c("Province", "ratio")) %>%
  mutate(vote_share = as.numeric(value)) %>% 
  ggplot(aes(x=ratio, y=vote_share, color=variable)) + geom_point(aes(alpha=0.5)) +
  geom_smooth(method="lm") +
  facet_grid( . ~ variable) +
  ggtitle(label=age_title) + times_theme()

# unemployment rates 
# source: Eurostat | data: 2016
# plot and lm based on a ratio between 15-24 y.o. and general population (15 to 74)
unemployment_title <- "Youth unemployment compared to the general pop, by party"
unemployment <- read_csv("data/unemployment_2016.csv")
names(unemployment) <- c('District', '15_24', 'over_25', '15_74')

unemployment_plot <- merge(results_districts_2013, unemployment, by="District") %>%
  mutate(ratio = `15_24` / `15_74`) %>%
  select(-`15_24`, -`over_25`, -`15_74`, -`District`) %>%
  melt(id=c("District Long", "ratio")) %>%
  na.omit() %>%
  mutate(vote_share = as.numeric(value)) %>% 
  ggplot(aes(x=`ratio`, y=`vote_share`, color=variable)) + geom_point(aes(alpha=0.5)) +
  geom_smooth(method="lm") +
  facet_grid( . ~ variable) +
  ggtitle(label=unemployment_title) + times_theme()

################################################################################
# arrange plots together
grid.arrange(poverty_plot, age_plot, unemployment_plot, nrow=3)

################################################################################
# look at regressions
poverty_groups <- merge(results_districts_2013, poverty, by='District') %>%
  select(-District) %>%
  melt(id=c("District Long", "poverty_2016")) %>% 
  na.omit() %>%
  mutate(vote_share = as.numeric(value)) %>%
  select(-value) %>% 
  group_by(variable)
poverty_models <- poverty_groups %>% group_by(variable) %>%
  do(model = lm(vote_share ~ poverty_2016, data=.))
poverty_models %>% glance(model)

age_groups <- merge(results_provinces_2013, age_groups) %>% 
       mutate(ratio = `over_65` / `15_64`) %>% 
       select(-`15_64`, -`over_65`) %>%
       melt(id=c("Province", "ratio")) %>%
       mutate(vote_share = as.numeric(value)) %>% 
       group_by(variable)
age_models <- age_groups %>% group_by(variable) %>%
       do(model = lm(vote_share ~ ratio, data=.))
age_models %>% glance(model)

unemployment_groups <- merge(results_districts_2013, unemployment, by="District") %>%
  mutate(ratio = `15_24` / `15_74`) %>%
  select(-`15_24`, -`over_25`, -`15_74`, -`District`) %>%
  melt(id=c("District Long", "ratio")) %>%
  na.omit() %>%
  mutate(vote_share = as.numeric(value)) %>%
  group_by(variable)
unemployment_models <- unemployment_groups %>% group_by(variable) %>%
  do(model = lm(vote_share ~ `ratio`, data=.))
unemployment_models %>% glance(model)
