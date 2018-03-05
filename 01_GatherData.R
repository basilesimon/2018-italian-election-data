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

makePlot <- function(data, x, title) {
  ggplot(data = data,
         aes(x = x, y = vote_share, color = variable)) +
    geom_point(aes(alpha = 0.5)) + 
    facet_grid(. ~ variable) + 
    geom_smooth(method = "lm") +
    ggtitle(label = title) + times_theme()
}

partiesToKeep <- c("LEGA", "FORZA ITALIA", "FRATELLI D'ITALIA CON GIORGIA MELONI", "NOI CON L'ITALIA - UDC",
                   "MOVIMENTO 5 STELLE", "PARTITO DEMOCRATICO", "+EUROPA", "LIBERI E UGUALI")

#########################################################################
# load all data
provinces_regions <- read_csv("data/provinces_regions_lower.csv")
# results_provinces_2013 <- read_csv("data/2013_results_provinces.csv")
#results_districts_2013 <- read_csv("data/2013_results_districts.csv")
results_districts_2018 <- read_csv("data/2018/results2018.csv") %>%
  select(-variable, -value) %>%
  filter(descr_lista %in% partiesToKeep) %>%
  distinct() %>%
  spread(key = descr_lista, value = perc) %>%
  mutate(`District Long` = tolower(name)) %>% select(-name) %>%
  rowwise() %>%
  mutate(`District` = strsplit(`District Long`, " ")[[1]][1]) %>% distinct()

# people at risk of poverty or social exclusion by NUTS-2
# source: Eurostat | data: 2016
poverty_title <- "People at risk of poverty or social exclusion, by party"
poverty <- read_csv("data/ilc_peps11_1_Data.csv") %>%
  select(-`Flag and Footnotes`, -TIME, -UNIT) %>%
  mutate(District = tolower(GEO)) %>% select(-GEO)
names(poverty) <- c('poverty_2016', 'District')

poverty_data <- merge(results_districts_2018, poverty, by='District') %>%
  select(-District) %>%
  melt(id=c("District Long", "poverty_2016")) %>% 
  na.omit() %>%
  mutate(vote_share = as.numeric(sub(",", ".", value, fixed = TRUE))) %>%
  select(-value)
poverty_plot <- makePlot(poverty_data, poverty_data$poverty_2016, poverty_title)

# usual resident population by NUTS-3
# source: Eurostat | data: 2016
# plot and lm based on a ratio between 65+ compared to the younger population (15 to 64)
age_title <- "Over 65 compared to the general pop, by party"
age_groups <- read_csv("data/20152016_age_groups.csv")
names(age_groups) <- c('Province', '15_64', 'over_65')

age_data <- merge(results_provinces_2013, age_groups) %>% 
  mutate(ratio = `over_65` / `15_64`) %>% 
  select(-`15_64`, -`over_65`) %>%
  melt(id=c("Province", "ratio")) %>%
  mutate(vote_share = as.numeric(value)) 
age_plot <-  makePlot(age_data, age_data$ratio, age_title)

# unemployment rates 
# source: Eurostat | data: 2016
# plot and lm based on a ratio between 15-24 y.o. and general population (15 to 74)
unemployment_title <- "Youth unemployment compared to the general pop, by party"
unemployment <- read_csv("data/unemployment_2016.csv") %>%
  mutate(District = tolower(GEO)) %>% select(-GEO)
names(unemployment) <- c('15_24', 'over_25', '15_74', 'District')

unemployment_data <- merge(results_districts_2018, unemployment, by="District") %>%
  mutate(ratio = `15_24` / `15_74`) %>%
  select(-`15_24`, -`over_25`, -`15_74`, -`District`) %>%
  melt(id=c("District Long", "ratio")) %>%
  na.omit() %>%
  mutate(vote_share = as.numeric(sub(",", ".", value, fixed = TRUE))) %>% select(-value)
unemployment_plot <- makePlot(unemployment_data, unemployment_data$ratio, unemployment_title)

# percentage of people who have never used a computer
# source: Eurostat | data: 2015
itc_title <- "Difference to average IT literacy, by party"
itc <- read_csv("data/2015_neverusedcomputer.csv")  %>%
  mutate(District = tolower(GEO)) %>% select(-GEO)
names(itc) <- c("itc", "District")

itc_data <- merge(results_districts_2018, itc, by="District") %>%
  mutate(diff = 32 - itc) %>%
  select(-`District`, -`itc`) %>%
  melt(id=c("District Long", "diff")) %>%
  na.omit() %>%
  mutate(vote_share = as.numeric(sub(",", ".", value, fixed = TRUE))) %>% select(-value)
itc_plot <- makePlot(itc_data, itc_data$diff, itc_title)

# net migration crude rate
# source: Eurostat | data: 2016
nmigration_title <- "Crude net migration rate"
nmigration <- read_csv("data/demo_r_gind3_1_Data2.csv") %>% select(-TIME, -INDIC_DE)
names(nmigration) <- c("Province", "nmigration")

nmigration_data <- merge(results_provinces_2013, nmigration, by="Province") %>%
  melt(id=c("Province", "nmigration")) %>%
  na.omit() %>%
  mutate(vote_share = as.numeric(value))
nmigration_plot <- makePlot(nmigration_data, nmigration_data$nmigration, nmigration_title)

################################################################################
# arrange plots together
grid.arrange(poverty_plot,
             # age_plot, 
             unemployment_plot, itc_plot, #nmigration_plot, 
             nrow=3)

################################################################################
# look at regressions
poverty_models <- poverty_data %>% group_by(variable) %>%
  do(model = lm(vote_share ~ poverty_2016, data=.))
poverty_models %>% glance(model)

age_models <- age_data %>% group_by(variable) %>%
       do(model = lm(vote_share ~ ratio, data=.))
age_models %>% glance(model)

unemployment_models <- unemployment_data %>% group_by(variable) %>%
  do(model = lm(vote_share ~ `ratio`, data=.))
unemployment_models %>% glance(model)

itc_models <- itc_data %>% group_by(variable) %>%
  do(model = lm(vote_share ~ diff, data=.))
itc_models %>% glance(model)

nmigration_models <- nmigration_data %>% group_by(variable) %>%
  do(model=lm(vote_share ~ nmigration, data=.))
nmigration_models %>% glance(model)
