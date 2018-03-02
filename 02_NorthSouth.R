library(timesTheme)

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
results_and_regions_2013 <- left_join(results_districts_2013, provinces_regions %>% 
                                        select(District, `Macro- Region`) %>%
                                        distinct(District, `Macro- Region`), 
                                      by="District")

results_and_regions_2013 <- results_and_regions_2013 %>%
  mutate(north_south = ifelse(
    `Macro- Region` == "Nord-Ovest" | `Macro- Region` == "Nord-Est","North", "the rest"))

poverty_data <- merge(results_and_regions_2013, poverty, by='District') %>%
  select(-District, -`Macro- Region`) %>%
  melt(id=c("District Long", "poverty_2016", "north_south")) %>% 
  na.omit() %>%
  mutate(vote_share = as.numeric(value)) %>%
  select(-value)
poverty_plot <- makeNSPlot(poverty_data, poverty_data$poverty_2016, poverty_title)

unemployment_data <- merge(results_and_regions_2013, unemployment, by="District") %>%
  mutate(ratio = `15_24` / `15_74`) %>%
  select(-`15_24`, -`over_25`, -`15_74`, -`District`, -`Macro- Region`) %>%
  melt(id=c("District Long", "ratio", "north_south")) %>%
  na.omit() %>%
  mutate(vote_share = as.numeric(value))
unemployment_plot <- makeNSPlot(unemployment_data, unemployment_data$ratio, unemployment_title)

itc_data <- merge(results_and_regions_2013, itc, by="District") %>%
  mutate(diff = 32 - itc) %>%
  select(-`District`, -`itc`, -`Macro- Region`) %>%
  melt(id=c("District Long", "diff", "north_south")) %>%
  na.omit() %>%
  mutate(vote_share = as.numeric(value))
itc_plot <- makeNSPlot(itc_data, itc_data$diff, itc_title)

nmigration_data <- merge(results_and_regions_2013, nmigration, by="District") %>%
  select(-District, -`Macro- Region`) %>%
  melt(id=c("District Long", "nmigration", "north_south")) %>%
  na.omit() %>%
  mutate(vote_share = as.numeric(value))
nmigration_plot <- makeNSPlot(nmigration_data, nmigration_data$nmigration, nmigration_title)

################################################################################
# arrange plots together
grid.arrange(poverty_plot, unemployment_plot, itc_plot, nmigration_plot, nrow=4)
