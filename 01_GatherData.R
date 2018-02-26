library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)

# opposite of intersect()
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

provinces_regions <- read_csv("data/provinces_regions.csv")
results_provinces_2013 <- read_csv("data/2013_results_provinces.csv")
results_districts_2013 <- read_csv("data/2013_results_districts.csv")

# people at risk of poverty or social exclusion by NUTS-2
# source: Eurostat | data: 2016
poverty_title <- "People at risk of poverty or social exclusion, by party"
poverty <- read_csv("data/ilc_peps11_1_Data.csv") %>%
  select(-`Flag and Footnotes`, -TIME, -UNIT)
names(poverty) <- c('District', 'poverty_2016')

merge(results_districts_2013, poverty, by='District') %>%
  select(-District) %>%
  melt(id=c("District Long", "poverty_2016")) %>% 
  na.omit() %>%
  mutate(vote_share = as.numeric(value)) %>%
  select(-value) %>%
  ggplot(aes(x=poverty_2016, y=vote_share, color=variable)) + 
  geom_point(aes(alpha=0.5)) + 
  facet_grid(. ~ variable) + 
  geom_smooth(method="lm") +
  ggtitle(label=poverty_title)
