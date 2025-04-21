### Population loss and planning for shrinkage - final script

###libraries###
library(tidyverse)
library(ggplot2)
library(urbnmapr)
library(sf)
library(tigris)
library(ggrepel)
library(urbnthemes)
library(tigris)

set_urbn_defaults(style = "print")
options(scipen = 999)

#setting wd
setwd("C:/Users/tmaginn/Desktop/tmaginn/Projects/Blogs/pop loss and shrinkage/nhgis0018_csv")
fp <- file.path("C:/Users/tmaginn/Desktop/tmaginn/Projects/Blogs/pop loss and shrinkage/nhgis0018_csv")

############ loading in data #################
#2010 data
pop_2010 <- read.csv("nhgis0018_ds176_20105_county.csv")%>%
  mutate(GEOID = str_sub(GEOID, -5))%>%
  select(GEOID, YEAR, STATE, COUNTY, JL0E001, JMAE001)
colnames(pop_2010) <- c("GEOID", "Year", "State", "County", "median_age10", "population10")

#2019 data
pop_2019 <- read.csv("nhgis0018_ds244_20195_county.csv")%>%
  mutate(GEOID = str_sub(GEOID, -5))%>%
  select(GEOID, YEAR, STATE, COUNTY, ALUBE001)
colnames(pop_2019) <- c("GEOID", "Year", "State", "County", "population19")

#2020 data
pop2020 <- read.csv("pop2020.csv")%>%
  mutate(GEO_ID = str_sub(GEOID, -5))%>%
  select(GEO_ID, YEAR, STATE, COUNTY, AMPVE001)
colnames(pop2020) <- c("GEOID", "Year", "State", "County", "population20")


#2023 data
pop_2023 <- read.csv("nhgis0018_ds267_20235_county.csv")%>%
  mutate(GEO_ID = str_sub(GEO_ID, -5))%>%
  select(GEO_ID, YEAR, STATE, COUNTY, ASN1E001, ASNRE001)
colnames(pop_2023) <- c("GEOID", "Year", "State", "County", "population23", "median_age23")

#load in county and state shapefiles
counties <- get_urbn_map("counties", sf = TRUE) %>%
  select(county_fips, geometry)
colnames(counties) <- c("GEOID", "geometry")

states <- get_urbn_map("states", sf = TRUE) %>%
  select(state_name, geometry)
colnames(states) <- c("State.x", "geometry")

#join datasets - for now we can exclude
df <- pop_2010 %>%
  left_join(pop_2019, by = "GEOID") %>%
  left_join(pop_2023, by = "GEOID") %>%
  left_join(pop2020, by = "GEOID") %>%
  left_join(counties, by = "GEOID") %>%
  left_join(states, by= "State.x")%>%
  st_as_sf()



############ descriptive stats on population change over various time intervals ################

### computing population changes
#2010 to 2019
df <- df %>%
  mutate(pop_pct_change_1019 = ((population19 - population10)/population10))%>%
  mutate(raw_pop_change_1019 = population19-population10)

#2010 to 2020
df <- df %>%
  mutate(pop_pct_change_1020 = ((population20-population10)/population10))%>%
  mutate(raw_pop_change_1020 = population20-population10)

#2010 to 2023
df <- df %>%
  mutate(pop_pct_change_1023 = ((population23 - population10)/population10))%>%
  mutate(raw_pop_change_1023 = population23 - population10)

#### what share of counties lost population over each time period?

#2019
counties_lost_population_1019 <- sum(df$raw_pop_change_1019 < 0, na.rm = TRUE) #count number of counties with negative pop change
total_counties <- nrow(df) #county number of counties
percentage_lost_population_1019 <- (counties_lost_population_1019 / total_counties) * 100 #compute pct of counties that lost population
percentage_lost_population_1019 #51.4747% of counties lost population

#2020
counties_lost_population_1020 <- sum(df$raw_pop_change_1020 <0, na.rm = TRUE)
percentage_lost_population_1020 <- (counties_lost_population_1020/total_counties) * 100
percentage_lost_population_1020 #51.69202% of counties lost population

#2023
counties_lost_population_1023 <- sum(df$raw_pop_change_1023 < 0, na.rm = TRUE)
percentage_lost_population_1023 <- (counties_lost_population_1023/total_counties) * 100
percentage_lost_population_1023 #50.45017% of counties lost population






############## mapping county-level population change ################
map_data_1020 <- df %>%
  select(GEOID, County.y, State.y, geometry.x, geometry.y, pop_pct_change_1020)%>%
  mutate(pct_change_bin = cut(pop_pct_change_1020, breaks = c(-1, -0.1, -0.05, 0,2), 
                              labels = c("Lost 10% or more ", "-5 to -10%", "0 to -5%", "Gained population"),
                              include.lowest = TRUE))

map_data_1020$pct_change_bin <- factor(map_data_1020$pct_change_bin, 
                                       levels = c("Lost 10% or more ", "-5 to -10%", "0 to -5%", "Gained population"))
plot <- ggplot() +
  geom_sf(map_data_1020, mapping = aes(fill = pct_change_bin), show.legend = TRUE) +
  geom_sf(data = states, fill = NA, color = "grey", size = 0.5) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_diverging[c(1,2,3,5)], # you can adjust this palette to match your number of bins
    name = "% Change in Population, 2010-2020",
    breaks = c("Lost 10% or more ", "-5 to -10%", "0 to -5%", "Gained population")
  ) +
  theme_urbn_map()

print(plot)

ggsave(file.path(fp, "population_change_10_20.png"), width = 14, height = 6, dpi = 300)

#################### Analysis of population loss by census region ###################

# Assigning states to census region definitions
map_data_1020$region <- ifelse(map_data_1020$State.y %in% c("Alabama", "Arkansas", "District of Columbia", "Delaware", "Florida", "Georgia",
                                                            "Kentucky", "Louisiana", "Maryland", "Mississippi", "North Carolina", "Oklahoma",
                                                            "South Carolina", "Tennessee", "Texas", "Virginia", "West Virginia"), "South",
                               ifelse(map_data_1020$State.y %in% c("Alaska", "Arizona", "California", "Colorado", "Hawaii", 
                                                                   "Idaho", "Montana", "Nevada", "New Mexico", "Oregon", "Utah", 
                                                                   "Washington", "Wyoming"), "West",
                                      ifelse(map_data_1020$State.y %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", 
                                                                          "New Jersey", "New York", "Pennsylvania", "Rhode Island", 
                                                                          "Vermont"), "Northeast",
                                             ifelse(map_data_1020$State.y %in% c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", 
                                                                                 "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", 
                                                                                 "Wisconsin"), "Midwest", NA))))




### which region had the highest share of shrinking counties?
shrinking_counties <- map_data_1020[map_data_1020$pop_pct_change_1020 < 0, ] #filter out growing counties
shrinking_count_per_region <- table(shrinking_counties$region) #counts of shrinking counties, by region
total_count_per_region <- table(map_data_1020$region) #tabulate
share_losing_population <- shrinking_count_per_region / total_count_per_region #computing share

### let's make a bar chart to show which region has the highest share of shrinking counties - note this is 2010-2020
# Create a df with region and share of counties losing population
share_df <- data.frame(
  region = names(share_losing_population),
  share_losing_population = as.numeric(share_losing_population)
)

#plot bar chart
ggplot(share_df) +
  geom_col(aes(x = region, y = share_losing_population, fill = region), position = "dodge") +
  geom_text(aes(x = region, y = share_losing_population, label = scales::percent(share_losing_population, accuracy = 0.1)),
            vjust = -0.5, family = "Lato", size = 3) +
  geom_hline(yintercept = 0.5169202, linetype = "dotted", color = "black", size = 0.8) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, .7), 
                     breaks = c(.1, .2, .3, .4, .5, .6, .7),
                     labels = scales::percent) +
  labs(x = "Census Region", y = "Share of Counties Losing Population") +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 9, family = "Lato"),
    axis.text.y = element_text(size = 9, family = "Lato"),
    axis.title.x = element_text(size = 9, family = "Lato"),
    axis.title.y = element_text(size = 9, family = "Lato")
  )



###################### Analysis of fertility rates ########################
fertility <- read.csv("fertility rate.csv") #loading in data
colnames(fertility) <- c("Year", "Fertility")

fertility$year <- year(mdy(fertility$Year)) #reformat dates

#simple line chart of fertility rate
ggplot(fertility, aes(x = year, y = Fertility)) +
  geom_line(size = 1.2) +  # Increase the size value to make the line thicker
  labs(
    x = "Year",
    y = "Fertility Rate") +
  scale_x_continuous(breaks = seq(1960, 2022, by = 10)) +
  scale_y_continuous(limits = c(1, 4),
                     breaks = seq(1, 4, by = 0.5)) +  # Set the y-axis limits from 0 to 3
  geom_hline(yintercept = 2.1, linetype = "dotted", color = "black", size = 1) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11, family = "Lato"),
    axis.text.y = element_text(size = 11, family = "Lato"),
    axis.title.x = element_text(size = 11, family = "Lato"),
    axis.title.y = element_text(size = 11, family = "Lato")
  )

ggsave(file.path(fp, "fertility_rate.png"), width = 14, height = 6, dpi = 300)


##################### Analysis of migration rates and projections #####################
migration <-read.csv("projected migration.csv") #load world bank net migration data








