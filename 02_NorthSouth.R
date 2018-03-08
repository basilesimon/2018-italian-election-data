
# plot shorthand function, colouring by north/south divide
makeNSPlot <- function(data, x, title) {
  ggplot(data = data,
         aes(x = x, y = vote_share, color = north_south)) +
    geom_point(aes(alpha = 0.5)) + 
    facet_grid(. ~ variable) + 
    geom_smooth(method = "lm") +
    ggtitle(label = title) + 
    times_theme() +
    theme(legend.position="right")
}

#########################################################################
# load all data
# and match north/south regions
results_and_regions_2018 <- left_join(results_districts_2018, provinces_regions %>% 
     select(District, `Macro- Region`) %>%
     distinct(District, `Macro- Region`), 
   by="District")

results_and_regions_2018 <- results_and_regions_2018 %>%
  mutate(north_south = ifelse(
    `Macro- Region` == "nord-ovest" | `Macro- Region` == "nord-est","north", "the rest")
  )
  
poverty_data <- merge(results_and_regions_2018, poverty, by='District') %>%
  select(-`Macro- Region`) %>%
  melt(id=c("District", "poverty_2016", "north_south")) %>% 
  na.omit() %>%
  mutate(vote_share = as.numeric(sub(",", ".", value, fixed = TRUE))) %>% 
  select(-value)
poverty_northsouth_ <- makeNSPlot(poverty_data, poverty_data$poverty_2016, poverty_title)

# age_data <- merge(results_and_regions_provinces_2013, age_groups) %>% 
#   mutate(ratio = `over_65` / `15_64`) %>% 
#   select(-`15_64`, -`over_65`, -`Macro- Region`) %>%
#   melt(id=c("Province", "ratio", "north_south")) %>%
#   mutate(vote_share = as.numeric(value)) 
# age_northsouth_ <-  makeNSPlot(age_data, age_data$ratio, age_title)

unemployment_data <- merge(results_and_regions_2018, unemployment, by="District") %>%
  mutate(ratio = `15_24` / `15_74`) %>%
  select(-`15_24`, -`over_25`, -`15_74`, -`District`, -`Macro- Region`) %>%
  melt(id=c("District Long", "ratio", "north_south")) %>%
  na.omit() %>%
  mutate(vote_share = as.numeric(sub(",", ".", value, fixed = TRUE))) %>% 
  select(-value)
unemployment_northsouth_ <- makeNSPlot(unemployment_data, unemployment_data$ratio, unemployment_title)

itc_data <- merge(results_and_regions_2018, itc, by="District") %>%
  mutate(diff = 32 - itc) %>%
  select(-`District`, -`itc`, -`Macro- Region`) %>%
  melt(id=c("District Long", "diff", "north_south")) %>%
  na.omit() %>%
  mutate(vote_share = as.numeric(sub(",", ".", value, fixed = TRUE))) %>% 
  select(-value)
itc_northsouth_ <- makeNSPlot(itc_data, itc_data$diff, itc_title)

# nmigration_data <- merge(results_and_regions_provinces_2013, nmigration, by="Province") %>%
#   select(-`Macro- Region`) %>%
#   melt(id=c("Province", "nmigration", "north_south")) %>%
#   na.omit() %>%
#   mutate(vote_share = as.numeric(value))
# nmigration_northsouth_ <- makeNSPlot(nmigration_data, nmigration_data$nmigration, nmigration_title)

################################################################################
# arrange plots together
grid.arrange(poverty_northsouth_, #age_northsouth_, 
             unemployment_northsouth_, itc_northsouth_, #nmigration_northsouth_, 
             nrow=3)

################################################################################
# the unemployment ratio was confusing, let's explain it
unemployment2 <- merge(results_and_regions_2018, unemployment, by="District") %>%
  mutate(ratio = `15_24` / `15_74`) %>%
  select(-`over_25`, -`District`, -`Macro- Region`) %>%
  select(`District Long`, north_south, "15_24", "15_74", ratio, `MOVIMENTO 5 STELLE`) %>%
  mutate(vote_share = as.numeric(sub(",", ".", `MOVIMENTO 5 STELLE`, fixed = TRUE))) %>%
  select(-`MOVIMENTO 5 STELLE`) %>%
  melt(id.vars=c("District Long", "north_south", "vote_share"))

one <- ggplot(unemployment2 %>% filter(variable == "15_24" | variable == "15_74"),
              aes(x = value, y = vote_share,
                  color = north_south)) +
  geom_point(aes(alpha = 0.5))  + 
  facet_grid(. ~ variable) + 
  geom_smooth(method="lm") +
  times_theme()
three <- ggplot(unemployment2 %>% filter(variable == "ratio"),
              aes(x = value, y = vote_share,
                  color = north_south)) +
  geom_point(aes(alpha = 0.5))  + 
  geom_smooth(method="lm") +
  times_theme() +
  theme(legend.position="right")

grid.arrange(one, three,
             ncol=2, top = "Five Start performs better as both youth and general
              unemployement rise, but lose shares of the vote is unemployment is evenly
             distributed in the population")

