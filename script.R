# load packages
library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(gridExtra)

# load 2013 results data
# rename columns and remove unused data points
results <- read_csv('2013_results.csv')
results.melt <- melt(results, id="Party")
names(results.melt) <- c("geo", "party", "result_2013")
results.melt$result_2013 <- as.numeric(as.character(results.melt$result_2013))
results.clean <- results.melt

# load net migration data
# pick only 2013 and remove unused data points
df <- read_csv('raw\ data/demo_r_gind3_1_Data.csv')
df.clean <- df %>% 
  filter(TIME == "2013") %>%
  select(-TIME, -`Flag and Footnotes`, -INDIC_DE)
names(df.clean) <- c("geo", "net_migration")

# show how our dataset intersect, ie how well the merge will go
intersect(df.clean$geo, results.clean$geo)

# merge the two datasets
df.merge <- merge(results.clean, df.clean, by="geo")
df.merge <- df.merge %>%
  group_by(party)

plot_migration <- df.merge %>% 
  group_by(geo, party) %>%
  ggplot(aes(x=net_migration, y=result_2013, color=party)) + 
  geom_point() +
  scale_x_continuous(trans=log2_trans(), breaks=c(0,1000,5000,100000)) +
  geom_smooth(method="lm") + 
  facet_grid(. ~ party) + 
  ggtitle("Number of migrants per constituency") 

# load patent applications data
patents <- read_csv("raw\ data/pat_ep_rtec_1_Data.csv")
patents.clean <- patents %>%
  select(-TIME, -IPC, -UNIT)
names(patents.clean) <- c('geo', 'patents')
patents.clean$patents <- as.numeric(as.character(patents.clean$patents))

patents.merge <- merge(results.clean, patents.clean, by="geo")

plot_patents <- patents.merge %>% 
  group_by(geo, party) %>%
  ggplot(aes(x=patents, y=result_2013, color=party)) + 
  geom_point() +
  #scale_x_continuous(trans=log2_trans(), breaks=c(0,1000,5000,100000)) +
  geom_smooth(method="lm") + 
  facet_grid(. ~ party) + 
  ggtitle("Patents per 10,000 inhabitants")

# load purchasing power standard data
pps <- read_csv("raw\ data/nama_10r_3gdp_1_Data.csv")
pps.clean <- pps %>%
  select(-TIME, -UNIT)
names(pps.clean) <- c('geo', 'pps')
pps.clean$pps <- as.numeric(as.character(pps.clean$pps))

pps.merge <- merge(results.clean, pps.clean, by="geo")

plot_pps <- pps.merge %>% 
  group_by(geo, party) %>%
  ggplot(aes(x=pps, y=result_2013, color=party)) + 
  geom_point() +
  #scale_x_continuous(trans=log2_trans(), breaks=c(0,1000,5000,100000)) +
  geom_smooth(method="lm") + 
  facet_grid(. ~ party) + 
  ggtitle("Purchasing power per inhabitant")

grid.arrange(plot_migration, plot_patents, plot_pps, nrow=3)
