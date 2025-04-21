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
setwd("C:/Users/tmaginn/Box/PERC - Equity Analysis")


###reading in data###
#read in county-level funding
county_funding <- read.csv("Analysis\\Year 2\\IIJA and IRA FY23\\Outputs\\county_program_list.csv")%>%
  mutate(GEOID = str_pad(GEOID, 5, "left", 0))%>%
  select(-X)

#read in county indicator data
county_indicators <- read.csv("Analysis\\Year 2\\IIJA and IRA FY23\\Outputs\\table3_county-indicator.csv")%>%
  mutate(GEOID = str_pad(GEOID, 5, "left", 0))%>%
  select(-X)

#read in state indicator data
state_indicators <- read.csv("Analysis\\Year 2\\IIJA and IRA FY23\\Outputs\\table4_state-indicator.csv")%>%
  mutate(GEOID = str_pad(GEOID, 2, "left", 0))%>%
  select(-X)



###broadband analysis###
#selecting broadband indicators
broadband_indicators <- select(county_indicators, c('GEOID', 'state', 'percent_urban', 'percent_rural', 'urban', 'rural', 'total_pop', 'disadvantaged_county', 'persistent_poverty_county', 'percent_poc','poverty_rate','pop_density','med_hh_income','broadband_speed', 'hh_int_access'))

#selecting broadband programs
broadband_county_funding <- county_funding %>%
  select(GEOID, total_county_pop, acp_outreach, ag_reconnect, tribal_broadband, ntia_middle_mile)%>%
  rowwise()%>%
  mutate(total_broadband = sum(across(acp_outreach:ntia_middle_mile), na.rm = T))%>% #create a total funding column for broadband programs
  mutate(broadband_ever = ifelse(total_broadband > 0, 1, 0))%>% #create a flag that shows whether a county has ever gotten broadband funding
  left_join(broadband_indicators, by = "GEOID")%>% #join in indicators
  ungroup()


#create a df with per capita funding amounts
broadband_county_funding_pc <- broadband_county_funding%>%
  mutate(across(acp_outreach:total_broadband, ~.x/total_county_pop)) #divide all funding columns by population



### making program funding table for fact sheet ####
#table 1 in report: create basic funding totals by program
broadband_county_funding_table <- broadband_county_funding%>%
  group_by()%>%
  summarize(acp_outreach = sum(acp_outreach),
            ag_reconnect = sum(ag_reconnect),
            tribal_broadband = sum(tribal_broadband),
            ntia_middle_mile = sum(ntia_middle_mile))%>%
  mutate(total = sum(acp_outreach, ag_reconnect, tribal_broadband, ntia_middle_mile))






### making a county map of per capita funding (note that this is ugly and we will likely use a state map for the fact sheet) ###
counties <- get_urbn_map("counties", sf = TRUE)%>% #get the county shapefile from urbnmapr
  filter(state_fips != "09")%>% #remove connecticut because they're still in the old county boundaries
  select(county_fips, state_name, geometry)

counties_ct <- counties(state = 09, year = 2022)%>% #get the 2022 connecticut planning region shapefiles
  rename(county_fips = GEOID)%>%
  mutate(state_name = "Connecticut")%>%
  select(county_fips, state_name, geometry)%>%
  st_transform(crs = st_crs(counties)) #set the crs to match the crs of the counties shapefile

#add the new connecticut planning region rows back into the full county list
counties_full <- counties%>% 
  rbind(counties_ct)

#join the county shape file to the per capita county funding info
broadband_county_funding_pc_sf <- broadband_county_funding_pc %>%
  rename(county_fips = GEOID)%>%
  mutate(total_broadband = ifelse(total_broadband==0, NA, 1)) %>% #make 0s NAs so they don't show up on the map
  left_join(counties_full, by = "county_fips")%>%
  st_as_sf()

#map per capita county funding
ggplot() +
  geom_sf(broadband_county_funding_pc_sf, mapping = aes(fill = total_broadband),
          show.legend = TRUE)+
  labs(fill = NULL, title = "Counties receiving broadband funding")+
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5))+ 
  theme_urbn_map()

ggsave("Analysis\\Year 2\\2024 End of Year\\Outputs\\map_counties_broadband.png")

#it's definitely not as ugly as energy funding, could be salvageable



### same thing but with state funding (also going to have a gradient for funding amt)
#figure 1 on the fact sheet
states <- get_urbn_map("states", sf = TRUE) #get state shapefiles


#sum up the county-level funding info to the state level - NOTE: may have to adjust bins
broadband_state_funding_pc_sf <- broadband_county_funding %>%
  group_by(state)%>% #group by state
  summarize(total_broadband = sum(total_broadband, na.rm = T), #sum up total funding
            state_population = sum(total_county_pop, na.rm = T))%>% #sum up population
  mutate(state_funding_pc = total_broadband/state_population)%>% #creating state level per capita funding amount
  mutate(state_funding_pc_bin = case_when(state_funding_pc == 0 ~ "No funding",
                                          state_funding_pc < 5 & state_funding_pc >= 0 ~ "Up to $5", #making custom bins, because there are several high outliers that throw off the map scale otherwise
                                          state_funding_pc >=5 & state_funding_pc < 20 ~ "$5–20",
                                          state_funding_pc >=20 & state_funding_pc <100 ~ "$20–100",
                                          state_funding_pc >=100 ~ "$100 or more"))%>%
  mutate(total_broadband_bin = case_when(total_broadband == 0 ~ "No funding",
                                         total_broadband >= 200000000 ~ "$200 million or more",
                                         total_broadband >= 100000000 & total_broadband < 200000000 ~ "$100–200 million",
                                         total_broadband >= 50000000 & total_broadband < 100000000 ~ "$50–100 million",
                                         total_broadband < 50000000 & total_broadband >= 0 ~ "Up to $50 million"))%>%
  mutate(state_funding_pc_bin = factor(state_funding_pc_bin, levels = c("No funding", "Up to $5", "$5–20", "$20–100", "$100 or more"),
                                       labels = c("No funding", "Up to $5", "$5–20", "$20–100", "$100 or more")))%>%
  mutate(total_broadband_bin = factor(total_broadband_bin, levels = c("No funding", "Up to $50 million", "$50–100 million", "$100–200 million",
                                                                      "$200 million or more"),
                                      labels = c("No funding", "Up to $50 million", "$50–100 million", "$100–200 million",
                                                 "$200 million or more")))%>%
  left_join(states, join_by("state" == "state_name"))%>% #join to the state shapefile
  st_as_sf() #set as a spatial object


#mapping state per capita funding
ggplot() +
  geom_sf(broadband_state_funding_pc_sf, mapping = aes(fill = state_funding_pc_bin),
          show.legend = TRUE)+
  # labs(fill = NULL, title = "State per capita funding")+
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5))+ 
  scale_fill_manual(
    values = palette_urbn_cyan[c(9, 1, 3, 5, 7)],
    name = "Per capita broadband funding"
  )+
  theme_urbn_map()


ggsave("Analysis\\Year 2\\2024 End of Year\\Outputs\\map_states_broadband.png", width = 8, height = 3)

#also going to map total funding
ggplot() +
  geom_sf(broadband_state_funding_pc_sf, mapping = aes(fill = total_broadband_bin),
          show.legend = TRUE)+
  # labs(fill = NULL, title = "Total state funding")+
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5))+ 
  scale_fill_manual(
    values = palette_urbn_cyan[c(9, 1, 3, 5, 7)],
    name = "Total broadband funding"
  )+
  theme_urbn_map()

ggsave("Analysis\\Year 2\\2024 End of Year\\Outputs\\map_states_total_broadband.png", width = 8, height = 3)

### county demographic table ###
#table 2 in the report
urban_rural_county_demo <- broadband_county_funding %>%
  mutate(count = 1)%>%
  group_by(urban, broadband_ever)%>% #group by urban/rural, and whether they've ever won broadband funding
  summarize(count = sum(count), #count the number of counties
            med_hh_income = median(med_hh_income, na.rm = TRUE), #take the median of household income, share poc, and poverty rate in each category
            percent_poc = median(percent_poc, na.rm = TRUE),
            poverty_rate = median(poverty_rate, na.rm = TRUE),
            total_pop = sum(total_county_pop, na.rm = T))%>%
  group_by(urban)%>%
  mutate(total = sum(count)) #get a total overall count of urban vs rural counties



### exploratory tables - probably won't use but good to have for our knowledge ###

#how many urban/rural counties got funding - probably not necessary since most broadband goes to rural anyways
urban_rural_counts <- broadband_county_funding %>%
  mutate(total_broadband = ifelse(total_broadband >0, 1, 0)) %>%
  group_by(urban) %>% 
  summarize(count_funding = sum(total_broadband))

urban_rural_counts #out of 900 counties that received funding, 522 were rural
#write.csv(urban_rural_counts, "Analysis\\Year 2\\2024 End of Year\\Outputs\\broadband_counties_urban_counts.csv")


#how many disadvantaged counties got funding?
disadvantaged_funding <- broadband_county_funding %>%
  mutate(total_broadband = ifelse(total_broadband >0, 1, 0)) %>%
  group_by(disadvantaged_county)%>%
    summarize(disadvantaged_count = sum(total_broadband))

disadvantaged_funding #out of 900 counties that received funding, 413 were disadvantaged

# write.csv(disadvantaged_counts, "Analysis\\Year 2\\2024 End of Year\\Outputs\\broadband_counties_disadvantaged_counts.csv")

#how many persistent poverty counties got funding?
persistent_pov_funding <- broadband_county_funding %>%
  mutate(total_broadband = ifelse(total_broadband >0, 1, 0)) %>%
  group_by(persistent_poverty_county)%>%
  summarize(persistent_pov_count = sum(total_broadband))

persistent_pov_funding #out of 900 counties that received funding, 127 were persistent poverty

# write.csv(poverty_counts, "Analysis\\Year 2\\2024 End of Year\\Outputs\\broadband_counties_poverty_counts.csv")

#comparing per capita mean funding by urban/rural
urban_rural_means <- broadband_county_funding_pc %>%
  filter(!state %in% (c("New Hampshire", "Delaware")))%>%
  group_by(urban)%>%
  summarize(across(acp_outreach:total_broadband, ~mean(.x)))

#checking per capita mean funidng by disadvantaged
disadvantaged_means <- broadband_county_funding_pc %>%
  filter(!state %in% (c("New Hampshire", "Delaware")))%>%
  group_by(disadvantaged_county)%>%
  summarize(across(acp_outreach:total_broadband, ~mean(.x)))

#checking per capita mean funding by persistent poverty
poverty_means <- broadband_county_funding_pc %>%
  filter(!state %in% (c("New Hampshire", "Delaware")))%>%
  group_by(persistent_poverty_county)%>%
  summarize(across(acp_outreach:total_broadband, ~mean(.x)))




### demographic distribution ###

#checking total US population in data
sum(broadband_county_funding$total_county_pop) 
# 329824976

#commenting out below code because urban/rural is not a helpful distinction for broadband

#creating a df for funding going to high-poverty counties (counties with higher than 20% poverty)
share_funding_poverty <- broadband_county_funding %>%
  mutate(category = ifelse(poverty_rate > .2, "High poverty rate", "0"))%>% #create high poverty flag
  group_by(urban, category)%>% #group by urban/rural status and high poverty status
  summarize(total_broadband = sum(total_broadband), #sum energy funding
            total_county_pop = sum(total_county_pop))%>% #sum county population
  ungroup()%>%
  mutate(total = sum(total_broadband))%>% #create a total funding column
  mutate(share_funding = total_broadband/total)%>% #calculate share of funding in each category
  mutate(total_population = sum(total_county_pop))%>% #create a total population column
  group_by(category)%>% #group by high poverty status
  mutate(high_need_pop_share = sum(total_county_pop)/total_population)%>% #calculate the share of the total us population living in high poverty counties
  filter(category == "High poverty rate")%>% #only keep high poverty rows
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural"))) #set urban and rural as a factor for the graph



#creating a df for funding going to counties with high share people of color (greater than 50%)
share_funding_poc <- broadband_county_funding %>%
  mutate(category = ifelse(percent_poc > .5, "High share of people of color", "0"))%>%
  group_by(urban, category)%>%
  summarize(total_broadband = sum(total_broadband),
            total_county_pop = sum(total_county_pop))%>%
  ungroup()%>%
  mutate(total = sum(total_broadband))%>%
  mutate(share_funding = total_broadband/total)%>%
  mutate(total_population = sum(total_county_pop))%>%
  group_by(category)%>%
  mutate(high_need_pop_share = sum(total_county_pop)/total_population)%>%
  filter(category == "High share of people of color")%>%
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural"))) #set urban and rural as a factor for the graph



#creating a df for share of funding going to low income counties (med hh income below $60,000)
share_funding_income <- broadband_county_funding %>%
  mutate(category = ifelse(med_hh_income < 60000, "Low income", "0"))%>%
  group_by(urban, category)%>%
  summarize(total_broadband = sum(total_broadband),
            total_county_pop = sum(total_county_pop))%>%
  ungroup()%>%
  mutate(total = sum(total_broadband))%>%
  mutate(share_funding = total_broadband/total)%>%
  mutate(total_population = sum(total_county_pop))%>%
  group_by(category)%>%
  mutate(high_need_pop_share = sum(total_county_pop)/total_population)%>%
  filter(category == "Low income")%>%
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural"))) #set urban and rural as a factor for the graph



#bind all three demo df's together
share_funding_demo <- share_funding_poverty%>%
  rbind(share_funding_poc)%>%
  rbind(share_funding_income)


#plot bar chart by urban/rural
ggplot(share_funding_demo)+
  geom_col(mapping = aes(x = category, y = share_funding, fill = urban), position = "dodge")+
  # geom_line(mapping = aes(x = category, y = high_need_pop_share))+
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, .6), breaks = c(.1, .2, .3, .4, .5, .6),
                     labels = scales::percent) +
  labs(title = "Share of broadband programs going towards community types")+
  theme(legend.position="top",
        legend.text = element_text(size=9.5, family="Lato"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=8.5, family="Lato"),
        axis.text.y = element_text(size=8.5, family="Lato"))

ggsave("Analysis\\Year 2\\2024 End of Year\\Outputs\\broadband_county_urban_funding_share_demo.png", width = 8, height = 2.5)


# 
# #looking at distribution to each category by program 
# 
# #create a new df selecting out variables we care about
# share_funding_program <- broadband_county_funding %>%
#   select(GEOID, total_county_pop, acp_outreach, ag_reconnect, tribal_broadband,
#          ntia_middle_mile, total_broadband, percent_poc, poverty_rate, med_hh_income)%>%
#   mutate(
#     high_poverty = ifelse(poverty_rate > 0.2, "High Poverty", NA),
#     high_poc = ifelse(percent_poc > 0.5, "High POC", NA),
#     low_income = ifelse(med_hh_income < 60000, "Low Income", NA)
#   )
# 
# #keep only high poverty, high poc, low income
# share_funding_program <- share_funding_program %>%
#   filter(!is.na(high_poverty) | !is.na(high_poc) | !is.na(low_income))
# 
# #reshaping data to long (one row for each program per county)
# long_share_funding_program <- share_funding_program %>%
#   pivot_longer(cols =  c("acp_outreach", "ag_reconnect", "tribal_broadband", "ntia_middle_mile"),
#                names_to = "program",
#                values_to = "funding") %>%
#   # calculating total funding for each program
#   group_by(high_poverty, high_poc, low_income, program) %>%
#   summarise(total_program_funding = sum(funding), .groups = "drop") %>%
#   ungroup()
# 
# #calculate total funding per program across all counties
# program_totals <- long_share_funding_program %>%
#   group_by(program) %>%
#   summarize(total_funding = sum(total_program_funding))
# 
# #merge to calculate share of each program's total funding
# long_share_funding_program <- long_share_funding_program %>%
#   left_join(program_totals, by = "program") %>%
#   mutate(funding_share = total_program_funding / total_funding)
# 
# #reshapign data again and putting high poverty, high poc, low income as categories
# long_share_funding_program_tidy <- long_share_funding_program %>%
#   pivot_longer(cols = c(high_poverty, high_poc, low_income),
#                names_to = "category",
#                values_to = "group") %>%
#   filter(!is.na(group))  # Exclude rows where the category is NA
# 
# #making bar chart of share of each program's funding going to each category
# ggplot(long_share_funding_program_tidy, aes(x = group, y = funding_share, fill = program)) +
#   geom_col(position = "dodge") +
#   scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, .6), breaks = c(.1, .2, .3, .4, .5, .6),
#                      labels = scales::percent) +
#   # labs(title = "Share of broadband funding going towards county types, by program")+
#   geom_segment(x = 0.5, xend = 1.5, y = 0.08526897, yend = 0.08526897, linetype="dashed", lwd=0.5)+ #add a line across the poverty rate bars that represents share of population living in high poverty counties. Get y value from share_funding_demo_line
#   geom_segment(x = 1.5, xend = 2.5, y = 0.34434429, yend = 0.34434429, linetype="dashed", lwd=0.5)+#add a line across the poverty rate bars that represents share of population living in high share POC counties. Get y value from share_funding_demo_line
#   geom_segment(x = 2.5, xend = 3.5, y = 0.38102518, yend = 0.38102518, linetype="dashed", lwd=0.5)+#add a line across the poverty rate bars that represents share of population living in low income counties. Get y value from share_funding_demo_line
#   scale_fill_discrete(
#     labels = c("ACP Outreach", "AG ReConnect", "Tribal Broadband", "NTIA Middle Mile")) + #editing program names in the legend
#   scale_x_discrete(
#     limits = c("High Poverty", "High POC", "Low Income"),
#     labels = c(
#       "High Poverty" = "High poverty rate",
#       "High POC" = "High share of people of color",
#       "Low Income" = "Low income"
#     )) +
#   theme(legend.position="top",
#         legend.text = element_text(size=9.5, family="Lato"),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(size=8.5, family="Lato"),
#         axis.text.y = element_text(size=8.5, family="Lato"))



############## Trying alternative strategy####################
### how much of ACP Outreach program funding is going to each community type ###
share_acp_pov <- broadband_county_funding %>%
  mutate(category = ifelse(poverty_rate >0.2, "High poverty rate", "0"))%>% #setting high pov threshold
  group_by(urban, category)%>% #group by high poverty status
  summarize(total_acp = sum(acp_outreach))%>% #getting total ACP funding to high poverty counties
  ungroup()%>% #ungrouping
  mutate(total = sum(total_acp))%>% #total ACP funding (independent of pov status)
  mutate(share_funding = total_acp/total)%>% #calculating share going to high pov counties
  filter(category == "High poverty rate")%>% #filtering out non-high poverty counites
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural")))

#ACP high poc
share_acp_poc <- broadband_county_funding %>%
  mutate(category = ifelse(percent_poc > .5, "High share of people of color", "0"))%>%
  group_by(urban, category)%>%
  summarize(total_acp = sum(acp_outreach))%>%
  ungroup()%>%
  mutate(total = sum(total_acp))%>%
  mutate(share_funding = total_acp/total)%>%
  filter(category == "High share of people of color") %>%
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural")))

#ACP low income
share_acp_lowinc <- broadband_county_funding %>%
  mutate(category = ifelse(med_hh_income < 60000, "Low income", "0"))%>%
  group_by(urban, category)%>%
  summarize(total_acp = sum(acp_outreach))%>%
  ungroup()%>%
  mutate(total = sum(total_acp))%>%
  mutate(share_funding = total_acp/total)%>%
  filter(category == "Low income")%>%
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural")))

# rbind these together
share_funding_demo_acp <- share_acp_pov %>%
  rbind(share_acp_poc) %>%
  rbind(share_acp_lowinc)


### repeating above for AG ReConnect ###

share_ag_pov <- broadband_county_funding %>%
  mutate(category = ifelse(poverty_rate >0.2, "High poverty rate", "0"))%>%
  group_by(urban, category)%>%
  summarize(total_ag = sum(ag_reconnect))%>%
  ungroup()%>% 
  mutate(total = sum(total_ag))%>% 
  mutate(share_funding = total_ag/total)%>%
  filter(category == "High poverty rate") %>%
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural")))

share_ag_poc <- broadband_county_funding %>%
  mutate(category = ifelse(percent_poc > .5, "High share of people of color", "0"))%>%
  group_by(urban, category)%>%
  summarize(total_ag = sum(ag_reconnect))%>%
  ungroup()%>%
  mutate(total = sum(total_ag))%>%
  mutate(share_funding = total_ag/total)%>%
  filter(category == "High share of people of color")%>%
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural")))

share_ag_lowinc <- broadband_county_funding %>%
  mutate(category = ifelse(med_hh_income < 60000, "Low income", "0"))%>%
  group_by(urban, category)%>%
  summarize(total_ag = sum(ag_reconnect))%>%
  ungroup()%>%
  mutate(total = sum(total_ag))%>%
  mutate(share_funding = total_ag/total)%>%
  filter(category == "Low income")%>%
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural")))

# rbind these together
share_funding_demo_ag <- share_ag_pov %>%
  rbind(share_ag_poc) %>%
  rbind(share_ag_lowinc)

### repeating above for Tribal Broadband ###

share_tribal_pov <- broadband_county_funding %>%
  mutate(category = ifelse(poverty_rate >0.2, "High poverty rate", "0"))%>%
  group_by(urban, category)%>%
  summarize(total_tribal = sum(tribal_broadband))%>%
  ungroup()%>% 
  mutate(total = sum(total_tribal))%>% 
  mutate(share_funding = total_tribal/total)%>%
  filter(category == "High poverty rate") %>%
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural")))

share_tribal_poc <- broadband_county_funding %>%
  mutate(category = ifelse(percent_poc > .5, "High share of people of color", "0"))%>%
  group_by(urban, category)%>%
  summarize(total_tribal = sum(tribal_broadband))%>%
  ungroup()%>%
  mutate(total = sum(total_tribal))%>%
  mutate(share_funding = total_tribal/total)%>%
  filter(category == "High share of people of color")%>%
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural")))

share_tribal_lowinc <- broadband_county_funding %>%
  mutate(category = ifelse(med_hh_income < 60000, "Low income", "0"))%>%
  group_by(urban, category)%>%
  summarize(total_tribal = sum(tribal_broadband))%>%
  ungroup()%>%
  mutate(total = sum(total_tribal))%>%
  mutate(share_funding = total_tribal/total)%>%
  filter(category == "Low income")%>%
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural")))

# rbind these together
share_funding_demo_tribal <- share_tribal_pov %>%
  rbind(share_tribal_poc) %>%
  rbind(share_tribal_lowinc)

### repeating above for NTIA ###

share_ntia_pov <- broadband_county_funding %>%
  mutate(category = ifelse(poverty_rate >0.2, "High poverty rate", "0"))%>%
  group_by(urban, category)%>%
  summarize(total_ntia = sum(ntia_middle_mile))%>%
  ungroup()%>% 
  mutate(total = sum(total_ntia))%>% 
  mutate(share_funding = total_ntia/total)%>%
  filter(category == "High poverty rate") %>%
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural")))

share_ntia_poc <- broadband_county_funding %>%
  mutate(category = ifelse(percent_poc > .5, "High share of people of color", "0"))%>%
  group_by(urban, category)%>%
  summarize(total_ntia = sum(ntia_middle_mile))%>%
  ungroup()%>%
  mutate(total = sum(total_ntia))%>%
  mutate(share_funding = total_ntia/total)%>%
  filter(category == "High share of people of color")%>%
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural")))

share_ntia_lowinc <- broadband_county_funding %>%
  mutate(urban, category = ifelse(med_hh_income < 60000, "Low income", "0"))%>%
  group_by(urban, category)%>%
  summarize(total_ntia = sum(ntia_middle_mile))%>%
  ungroup()%>%
  mutate(total = sum(total_ntia))%>%
  mutate(share_funding = total_ntia/total)%>%
  filter(category == "Low income")%>%
  mutate(urban = as.factor(ifelse(urban == 1, "Urban", "Rural")))

# rbind these together
share_funding_demo_ntia <- share_ntia_pov %>%
  rbind(share_ntia_poc) %>%
  rbind(share_ntia_lowinc)

#combine the total funding and rural funding into one chart##


share_funding_demo_acp_1 <- share_funding_demo_acp%>%
  group_by(category, total)%>%
  summarize(total_acp = sum(total_acp))%>%#combine the urban and rural categories
  mutate(share_funding = total_acp/total)%>%#recalculate share of funding
  mutate(type = "ACP Outreach")

share_funding_demo_ag_1 <- share_funding_demo_ag%>%
  group_by(category, total)%>%
  summarize(total_ag = sum(total_ag))%>%#combine the urban and rural categories
  mutate(share_funding = total_ag/total)%>%#recalculate share of funding
  mutate(type = "ReConnect")

share_funding_demo_tribal_1 <- share_funding_demo_tribal%>%
  group_by(category, total)%>%
  summarize(total_tribal = sum(total_tribal))%>%#combine the urban and rural categories
  mutate(share_funding = total_tribal/total)%>%#recalculate share of funding
  mutate(type = "Tribal Broadband")

share_funding_demo_ntia_1 <- share_funding_demo_ntia%>%
  group_by(category, total)%>%
  summarize(total_ntia = sum(total_ntia))%>%#combine the urban and rural categories
  mutate(share_funding = total_ntia/total)%>%#recalculate share of funding
  mutate(type = "Middle Mile")
         
         

#combine all program and rural program info
share_funding_combo <- share_funding_demo_acp_1 %>%
  rbind(share_funding_demo_ag_1)%>%
  rbind(share_funding_demo_tribal_1)%>%
  rbind(share_funding_demo_ntia_1)



#plot share of funding from eaching program going to each community type
ggplot(share_funding_combo)+
  geom_col(mapping = aes(x = category, y = share_funding, fill = type), position = "dodge")+
  # geom_point(share_funding_demo_line, mapping = aes(x = category, y = high_need_pop_share), shape = 15, color = "black") + #add squares (shape = 15) to show the share of the us population living in each type of community
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, .85), breaks = c(.1, .2, .3, .4, .5, .6, .7, .8),
                     labels = scales::percent) +
  geom_segment(x = 0.5, xend = 1.5, y = 0.08526897, yend = 0.08526897, linetype="dashed", lwd=0.5)+ #add a line across the poverty rate bars that represents share of population living in high poverty counties. Get y value from share_funding_demo_line
  geom_segment(x = 1.5, xend = 2.5, y = 0.34434429, yend = 0.34434429, linetype="dashed", lwd=0.5)+#add a line across the poverty rate bars that represents share of population living in high share POC counties. Get y value from share_funding_demo_line
  geom_segment(x = 2.5, xend = 3.5, y = 0.38102518, yend = 0.38102518, linetype="dashed", lwd=0.5)+#add a line across the poverty rate bars that represents share of population living in low income counties. Get y value from share_funding_demo_line
  # labs(title = "Share of rural enviro programs going towards community types")+
  theme(legend.position="top",
        legend.text = element_text(size=9.5, family="Lato"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=8.5, family="Lato"),
        axis.text.y = element_text(size=8.5, family="Lato"))


ggsave("Analysis\\Year 2\\2024 End of Year\\Outputs\\broadband_program_funding_share_community_type.png", width = 8, height = 2)




### disadvantaged and persistent poverty ###
#figure 3 in the brief#

share_funding_disadvantaged <- broadband_county_funding %>%
  group_by(disadvantaged_county)%>% #group by disadvantaged county status
  summarize(across(total_county_pop:ntia_middle_mile, ~sum(.x, na.rm = T)))%>% #sum up each column
  mutate(across(total_county_pop:ntia_middle_mile, ~.x/sum(.x, na.rm = T)))%>% #divide each value by the column total to get a share
  pivot_longer(acp_outreach:ntia_middle_mile, #take dataframe from horizontal to vertical
               names_to = "program",
               values_to = "share")%>%
  mutate(program = factor(program, levels = c("acp_outreach", "ag_reconnect", "tribal_broadband", "ntia_middle_mile"), 
                          labels = c("ACP Outreach", "ReConnect", "Tribal Broadband", "Middle Mile")))%>% #relabeling program names to be what I want to show up in the chart
  filter(disadvantaged_county == 1)%>% #keep only the disadvantaged rows
  mutate(type = "Disadvantaged county")%>% #create a clean label for the graph
  select(!disadvantaged_county) #remove the disadvantaged county so it joins to the other dataset cleanly


share_funding_poverty <- broadband_county_funding %>%
  group_by(persistent_poverty_county)%>%
  summarize(across(total_county_pop:ntia_middle_mile, ~sum(.x, na.rm = T)))%>%
  mutate(across(total_county_pop:ntia_middle_mile, ~.x/sum(.x, na.rm = T)))%>%
  pivot_longer(acp_outreach:ntia_middle_mile,
               names_to = "program",
               values_to = "share")%>%
  mutate(program = factor(program, levels = c("acp_outreach", "ag_reconnect", "tribal_broadband", "ntia_middle_mile"), 
                          labels = c("ACP Outreach", "ReConnect", "Tribal Broadband", "Middle Mile")))%>% #relabeling program names to be what I want to show up in the chart
  filter(persistent_poverty_county == 1)%>%
  mutate(type = "Persistent poverty county")%>%
  select(!persistent_poverty_county)

pov_dis_combo <- share_funding_disadvantaged%>% #join the disadvantaged and persistent poverty data together
  rbind(share_funding_poverty)


#plotting
ggplot(pov_dis_combo, mapping = aes(x = program, y = share, fill = type))+
  geom_col(position = "dodge")+
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, .7), breaks = c(.1, .2, .3, .4, .5, .6, .7),
                     labels = scales::percent) +
  geom_hline(yintercept = 0.23500707, linetype="dashed", lwd=0.5)+ #adding a dashed line for share of population living in disadvantaged counties. Get value from total_county_pop in pov_dis_combo
  geom_hline(yintercept = 0.05454018, linetype="dashed", lwd=0.5)+ #adding a dashed line for share of population living in persistent poverty counties. Get value from total_county_pop in pov_dis_combo
  # annotate("text", x=4.5, y=0.25, label="Share of population in disadvantaged counties")+
  # labs(title = "Share of broadband programs going towards county types")+
  theme(legend.position="top",
        legend.text = element_text(size=9.5, family="Lato"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=8.5, family="Lato"),
        axis.text.y = element_text(size=8.5, family="Lato"))


ggsave("Analysis\\Year 2\\2024 End of Year\\Outputs\\broadband_funding_share_dis_pov.png", width = 8, height = 2.5)




### state indicator plot ###
#figure 5 in the report


broadband_state_funding_pc <- broadband_state_funding_pc_sf%>%
  st_drop_geometry()%>%
  left_join(state_indicators, join_by("state" == "NAME"))%>%
  select(state, state_population, state_funding_pc, med_hh_income, hh_int_access)%>%
  mutate(share_without_int = 1 - hh_int_access)%>%#compute share hh without int access
  mutate(income_bucket = case_when(med_hh_income < 60000 ~ "Low income", #creating income buckets for the colors in the chart
                                   med_hh_income >=60000 & med_hh_income < 75000 ~ "Middle income",
                                   med_hh_income >=75000 ~ "High income"))%>%
  mutate(income_bucket = factor(income_bucket, levels = c("Low income" , "Middle income", "High income")))%>% #converting to factor so the order shows up correctly
  filter(state_funding_pc != 0) #removing states with 0 funding

#NOTE: Alaska per capita funding is ~$1218. Removing so it doesn't throw off the scatterplot, but will include a note on it
broadband_state_funding_pc <- broadband_state_funding_pc %>% filter(state != "Alaska")



#creating state labels for states that are most visible in the chart
s1 <- subset(broadband_state_funding_pc, state == "Washington")
s2 <- subset(broadband_state_funding_pc, state == "Colorado")
s3 <- subset(broadband_state_funding_pc, state == "Utah")
s4 <- subset(broadband_state_funding_pc, state == "Montana")
s5 <- subset(broadband_state_funding_pc, state == "New Mexico")
s6 <- subset(broadband_state_funding_pc, state == "Oklahoma")
s7 <- subset(broadband_state_funding_pc, state == "Oregon")
s8 <- subset(broadband_state_funding_pc, state == "North Dakota")
s10 <- subset(broadband_state_funding_pc, state == "Missouri")
s11 <- subset(broadband_state_funding_pc, state == "Puerto Rico")
s12 <- subset(broadband_state_funding_pc, state == "Mississippi")
s13 <- subset(broadband_state_funding_pc, state == "Arkansas")
s14 <- subset(broadband_state_funding_pc, state == "California")
s15 <- subset(broadband_state_funding_pc, state == "West Virginia")
s16 <- subset(broadband_state_funding_pc, state == "Louisiana")
s18 <- subset(broadband_state_funding_pc, state == "Alabama")
s19<- subset(broadband_state_funding_pc, state == "Arizona")
s20<- subset(broadband_state_funding_pc, state == "Minnesota")
s21<- subset(broadband_state_funding_pc, state == "Kansas")
s23<- subset(broadband_state_funding_pc, state == "Tennessee")
s24<- subset(broadband_state_funding_pc, state == "Kentucky")
s25<- subset(broadband_state_funding_pc, state == "South Carolina")


#plotting the per capita broadband funding by share of hh access to internet
ggplot(broadband_state_funding_pc, mapping = aes(x = share_without_int , y = state_funding_pc)) +
  geom_point(mapping = aes(color = income_bucket), alpha = 0.5) +
  geom_text_repel(data = s1, label = "Washington", vjust=0.5, hjust = 1.3, color = "black", show.legend = FALSE, nudge_x = .003)+ ##adding labels for some of the states
  geom_text_repel(data = s2, label = "Colorado", vjust=0.5, hjust = 1.5, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s4, label = "Montana", vjust=0, hjust = 0, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s5, label = "New Mexico", vjust=0, hjust = 0, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s6, label = "Oklahoma", vjust=-1, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s7, label = "Oregon", vjust=-1, hjust = -0, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s8, label = "North Dakota", vjust=-0.5, hjust =1, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s10, label = "Missouri", vjust=-1, hjust = 1, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s11, label = "Puerto Rico", vjust=0.5, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s12, label = "Mississippi", vjust=-1, hjust = 0, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s13, label = "Arkansas", vjust=-1, hjust = 1.25, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s14, label = "California", vjust=3, hjust = 1, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s15, label = "West Virginia", vjust=1, hjust = 0.2, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s16, label = "Louisiana", vjust=2, hjust = -0.5, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s18, label = "Alabama", vjust=0, hjust = -0.5, color = "black", show.legend = FALSE, nudge_x = .003)+
  geom_text_repel(data = s19, label = "Arizona", vjust = 2.5, hjust = 1, color = "black", show.legend = FALSE, nudge_x = 0.003)+
  geom_text_repel(data = s20, label = "Minnesota", vjust = -1, hjust = 2, color = "black", show.legend = FALSE, nudge_x = 0.003)+
  geom_text_repel(data = s21, label = "Kansas", vjust = 1, hjust = -0.5, color = "black", show.legend = FALSE, nudge_x = 0.003)+
  geom_text_repel(data = s23, label = "Tennessee", vjust = 2, hjust = 0, color = "black", show.legend = FALSE, nudge_x = 0.003)+
  geom_text_repel(data = s24, label = "Kentucky", vjust = -1.5, hjust = 0.5, color = "black", show.legend = FALSE, nudge_x = 0.003)+
  geom_text_repel(data = s25, label = "South Carolina", vjust = 0, hjust = -0.2, color = "black", show.legend = FALSE, nudge_x = 0.003)+
  scatter_grid() +
  theme(plot.margin = margin(r = 20))+
  scale_x_continuous(limits = c(0, 0.4), labels = scales::percent)+
  scale_y_continuous(breaks = c(0, 75, 150, 225),
                     labels = scales::dollar)+
  # labs(title = "Per capita state broadband funding by share of households with internet access, per income level")+
  xlab("Share of households without internet access")+
  theme(legend.position="top",
        legend.text = element_text(size=9.5, family="Lato"),
        axis.title.x = element_text(size=8.5, family="Lato"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size=8.5, family="Lato"),
        axis.text.y = element_text(size=8.5, family="Lato"),
        plot.margin = margin(10, 10, 10, 10)) #adding a buffer so x axis label doesn't get cut off


ggsave("Analysis\\Year 2\\2024 End of Year\\Outputs\\state_broadband_cost_income.png", width = 8, height = 4.5)





### County Indicators Graphs ###

## Key indicators in terms of percentiles
##figure 4 in the report

percent_rank <- unlist(mapply(rep, broadband_county_funding$percent_poc, broadband_county_funding$total_pop, SIMPLIFY = FALSE))
broadband_county_funding$percent_rank_percent_poc <- ecdf(percent_rank)(broadband_county_funding$percent_poc)
percent_rank <- c() # clear the data
percent_rank <- unlist(mapply(rep, broadband_county_funding$poverty_rate, broadband_county_funding$total_pop, SIMPLIFY = FALSE))
broadband_county_funding$percent_rank_poverty_rate <- ecdf(percent_rank)(broadband_county_funding$poverty_rate)
percent_rank <- c() # clear the data
percent_rank <- unlist(mapply(rep, broadband_county_funding$pop_density, broadband_county_funding$total_pop, SIMPLIFY = FALSE))
broadband_county_funding$percent_rank_pop_density <- ecdf(percent_rank)(broadband_county_funding$pop_density)
percent_rank <- c() # clear the data
percent_rank <- unlist(mapply(rep, broadband_county_funding$med_hh_income, broadband_county_funding$total_pop, SIMPLIFY = FALSE))
broadband_county_funding$percent_rank_med_hh_income <- ecdf(percent_rank)(broadband_county_funding$med_hh_income)
percent_rank <- c() # clear the data
percent_rank <- unlist(mapply(rep, broadband_county_funding$hh_int_access, broadband_county_funding$total_pop, SIMPLIFY = FALSE))
broadband_county_funding$percent_rank_hh_int_access <- ecdf(percent_rank)(broadband_county_funding$hh_int_access)
percent_rank <- c() # clear the data
percent_rank <- unlist(mapply(rep, broadband_county_funding$broadband_speed, broadband_county_funding$total_pop, SIMPLIFY = FALSE))
broadband_county_funding$percent_rank_broadband_speed <- ecdf(percent_rank)(broadband_county_funding$broadband_speed)
percent_rank <- c() # clear the data

###
beginGroup <- c(0,.20,.40,.60,.80)
endGroup <- c(.20,.40,.60,.80,1.00)
nPrograms <- 4
nIndicators <- 6
countyMat <- data_frame(matrix(nrow=5*nPrograms*nIndicators,ncol=6))
colnames(countyMat) <- c("percentile","program","group","share","program_label","group_label","share_diff")
countyMat[,1] <- rep(c(1:5),nPrograms*nIndicators)
countyMat[,2] <- c(rep("acp_outreach",5*nIndicators),rep("ag_reconnect",5*nIndicators),rep("tribal_broadband",5*nIndicators),rep("ntia_middle_mile",5*nIndicators)) # program
countyMat[,3] <- rep(c(rep("percent_rank_percent_poc",5),rep("percent_rank_poverty_rate",5),rep("percent_rank_med_hh_income",5),rep("percent_rank_pop_density",5),
                       rep("percent_rank_hh_int_access",5),rep("percent_rank_broadband_speed",5)),nPrograms) # group
for(i in 1:nrow(countyMat)){
  pastePct <- as.numeric(countyMat[i,1])
  pastePg <- as.character(countyMat[i,2])
  pasteGrp <- as.character(countyMat[i,3])
  prgSum <- sum(subset(broadband_county_funding[,pastePg], broadband_county_funding[,pasteGrp] > beginGroup[pastePct] & broadband_county_funding[,pasteGrp] <= endGroup[pastePct])) # program total in percentile group
  popSum <- sum(subset(broadband_county_funding$total_pop, broadband_county_funding[,pasteGrp] > beginGroup[pastePct] & broadband_county_funding[,pasteGrp] <= endGroup[pastePct])) # program total in percentile group
  
  countyMat[i,4] <- (prgSum / popSum) / (sum(broadband_county_funding[,pastePg])/sum(broadband_county_funding$total_pop)) # per capita versus overall average
}
countyMat[,5] <- c(rep("ACP Outreach",5*nIndicators),rep("ReConnect",5*nIndicators),rep("Tribal Broadband",5*nIndicators),rep("Middle Mile",5*nIndicators))
countyMat[,6] <- rep(c(rep("% POC",5),rep("% Poverty",5),rep("Med. HH inc.",5),rep("Pop. density",5),rep("% HH int. access",5),rep("Broadband speed",5)),nPrograms)
countyMat[,7] <- countyMat[,4]-(countyMat[,1]/100)
colnames(countyMat) <- c("percentile","program","group","share","program_label","group_label","share_diff")
countyMat$program_label <- factor(countyMat$program_label, levels = c("ACP Outreach", "ReConnect", "Tribal Broadband", "Middle Mile"))
countyMat$share_max <- ifelse(countyMat$share>2,2,countyMat$share) # Note -- dealing with high levels in some places that go off chart

countyMat %>%
  ggplot(mapping = aes(x = percentile, y = share_max )) +
  geom_line(size=0.8) +
  facet_grid(program_label ~ group_label) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)),
                     limits = c(1,5),
                     breaks=c(1,3,5),
                     labels=c("Low","","High")) +  
  scale_y_continuous(expand = expansion(mult = c(0, 0)),
                     limits = c(0,2), 
                     breaks=c(0,0.5,1,1.5,2),
                     labels=c("0%","50%","100%","150%","≥ 200%")) +
  #labels = scales::percent) +
  labs(y = "Funding compared with average") +
  geom_hline(yintercept=1, linetype="solid", color="darkgray") +
  theme(panel.spacing = unit(20L, "pt"),
        axis.text.x = element_text(angle = 90, vjust = 1, hjust=1),
        axis.title.x = element_blank()) +
  remove_ticks() +
  scatter_grid()


ggsave("Analysis\\Year 2\\2024 End of Year\\Outputs\\broadband_program_indicator_matrix.png", width = 9, height = 6)






