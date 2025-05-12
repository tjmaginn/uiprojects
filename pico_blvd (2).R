####----Setup----####

#Install all the below packages if not already installed, using install.packages("package")

## WCG: IMO, it's good to load tidyverse first--it's your workhorse for everyday tasks
## then load other packages thereafter because some will intentionally mask tidyverse functions
## note that I've removed things like tidyr, ggplot, etc.--these are contained within the tidyverse
## I'd also vote against commenting packages unless they're lesser-used ones, just for readability

library(tidyverse)
library(tidycensus)
library(readxl)
library(openxlsx)
library(geofacet)
library(sf)
library(osmdata)
library(urbnthemes)
options(scipen=999)
set_urbn_defaults(style="print")

##WCG: try to never use paths with a hard-coded username--these are destined to fail
username = getwd() %>% str_split("\\/") %>% unlist %>% .[3]
file_path <- file.path("C:", "Users", username, "Box", "LA Transit project/Social Climate Analysis")

#load in data dictionary for acs
v23 <- load_variables(2023, "acs5", cache = TRUE)
# write.csv(v23, file.path(file_path, "Fileshare", "census_variables.csv"), row.names = FALSE)

####----Geographic Data----####

##census tracts and shapefiles##
##WCG: no capital letters in variable names, virtually literally
ca_tracts <- st_read(file.path(file_path, "Hoover/Mapping/CA_census_Tracts.shp"))
la_tracts <- ca_tracts %>% filter(COUNTYFP == "037")

# if geometry goes down, these are LA tract shapefiles
la_shp <- st_read(file.path(file_path, "Data/tl_2023_06_tract/tl_2023_06_tract.shp"))

#pico tracts
pico_buffer_tracts <- st_read(file.path(file_path, "Pico/Maps/pico_buffer_tracts.shp"))
##WCG: always have spaces on either side of pipe operators
pico_tracts <- la_shp %>%
  filter(GEOID %in% pico_buffer_tracts$GEOID)
pico_tracts %>% pull(GEOID)
#streets
#create specific set of streets we want to see
bbox <- st_bbox(pico_tracts)
#load in
streets <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway") %>%  # 'highway' includes roads, streets, etc.
  osmdata_sf()

#extract just streets
streets_lines <- streets$osm_lines

#filter to main streets
major_streets <- streets_lines %>%
  filter(highway %in% c("primary", "secondary", "tertiary")) %>%
  st_transform(st_crs(pico_tracts))

#create a version that clips the streets to the focus area
major_streets_clipped <- st_intersection(major_streets, st_union(st_geometry(pico_tracts)))

#load in census api key
#note - you will need to get a census api key to access this#
##WCG: tidycensus will pull this behind the scenes
Sys.getenv("CENSUS_API_KEY")

#set fips codes
state_fips <- "06"
county_fips <- "037"

####----Population----####
#pull population table from ACS
sex_by_age <- get_acs(
  geography = "tract",
  table = "B01001",
  state = state_fips,
  county = county_fips,
  year = 2023,
  survey = "acs5",
  geometry = TRUE
)

#save
write.csv(sex_by_age, file.path(file_path, "Fileshare", "sex_by_age.csv"), row.names = FALSE)

#restructure so that each row is a census tract
pop_wide_pico <- sex_by_age %>%
  select(GEOID, variable, estimate, geometry) %>%
  filter(variable == "B01001_001", GEOID %in% pico_tracts$GEOID) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename_with(~ "population", .cols = "B01001_001")

#add census tracts
pop_wide_pico <- left_join(pico_tracts, st_drop_geometry(pop_wide_pico), by = "GEOID")

#for data - get the total population
totalpop <- sum(pop_wide_pico$population)

##WCG: One consideration here: census tracts are standardized by population
## That's not to say there's not variation in total population by tract, but because
## total population is used to define a tract, you just want to be careful how you think about 
## the distribution of tract-level population counts. Sometimes population density can be a more
## meaningful metric here.
#create a histogram
population_histogram_pico <- ggplot(pop_wide_pico, aes(x = population)) +
  geom_histogram(binwidth = 500) +
  labs(
    title = NULL,
    x = "Population",
    y = "Census Tracts"
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/population_histogram_pico.png"), width = 8, height = 6, dpi = 300)

## WCG: probably want to add census tract boundaries on top of the the roads layer
## so that they're clearly visible
## Also, because your geography of interest is horizontally-oriented,
## you might consider flipping your legend so it's oriented horizontally
## and then placing it at the bottom of the figure, which will give you more space for your map
## also--the text sizes shouldn't (generally) need adjustment. these look a little large IMO. 
## regardless, you can use `str_wrap()` to automatically insert newlines so that, e.g., your legend
## title is only 10 characters wide (or whatver character count you want)
#create map of population
population_map_pico <- ggplot(pop_wide_pico) +
  geom_sf(aes(fill = population), show.legend = TRUE) +
  # Add the buffer overlay
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0.2, lwd = 1) +
  # Add the street overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.3) +
  scale_fill_gradientn(
    colors = palette_urbn_cyan[c(2, 4, 6, 7)],
    name = "Population",
    labels = scales::comma
  ) +
  theme_urbn_map() +  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(1.2, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(8, "cm"),
      barheight = unit(0.6, "cm")
    )
  )

ggsave(file.path(file_path, "Pico/Outputs/population_map_pico.png"), width = 14, height = 6, dpi = 300)


####----Age----####
#see sex_by_age

#create bins of different age groups, based on census data dictionary (v23)
age_bins <- list(
  age_0_17  = c("B01001_003", "B01001_004", "B01001_005", "B01001_006",
                "B01001_027", "B01001_028", "B01001_029", "B01001_030"),
  age_18_29 = c("B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011",  
                "B01001_031", "B01001_032", "B01001_033","B01001_034", "B01001_035"),
  age_30_44 = c("B01001_012","B01001_013", "B01001_014", 
                "B01001_036", "B01001_037", "B01001_038"),
  age_45_59 = c("B01001_015", "B01001_016", "B01001_017", 
                "B01001_039", "B01001_040", "B01001_041"),
  age_60_up = c("B01001_018", "B01001_019","B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
 "B01001_041", "B01001_042", "B01001_043", "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049")
)


#summarise total by age, for each tract
age_wide_pico <- sex_by_age %>%
  filter(variable %in% unlist(age_bins), GEOID %in% pico_tracts$GEOID)%>%
  group_by(GEOID)%>%
  mutate(age_group = case_when(
    variable %in% age_bins$age_0_17 ~ "age_0_17",
    variable %in% age_bins$age_18_29 ~ "age_18_29",
    variable %in% age_bins$age_30_44 ~ "age_30_44",
    variable %in% age_bins$age_45_59 ~ "age_45_59",
    variable %in% age_bins$age_60_up ~ "age_60_up",
  )) %>%
  group_by(GEOID, age_group)%>%
  summarise(population = sum(estimate, na.rm = TRUE), .groups = "drop")%>%
  pivot_wider(names_from = age_group, values_from = population)

#calculate the total age 
age_totals <- age_wide_pico %>%
  st_drop_geometry() %>%
  summarise(across(starts_with("age_"), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "age_group",
    values_to = "population"
  )

## WCG: probably don't want to use discrete fills for this variable
## perhaps just all blue?
#create a bar chart
pop_by_age_group <- ggplot(age_totals, aes(x = age_group, y = population, fill = age_group)) +
  geom_col() +
  labs(
    x = "Age Group",
    y = "Population"
  ) +
  scale_x_discrete(
    labels = c(
      "age_0_17" = "0-17", 
      "age_18_29" = "18-29", 
      "age_30_44" = "30-44",
      "age_45_59" = "45-59",
      "age_60_up" = "60+"
    ))+
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none")+
  theme(
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text.x = element_text(size = 16),
  axis.text.y = element_text(size = 16))
  
#save
ggsave(file.path(file_path, "Pico/Outputs/age_chart_pico.png"), width = 14, height = 6, dpi = 300)


####----Ethnicity----####

#decided to remove race for now, since ethnicity captures everything we are looking for

#pull ethnicity  from ACS
ethnicity <- 
  get_acs(
    geography = "tract",
    table = "B03002",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE
  )


write.csv(ethnicity, file.path(file_path, "Fileshare", "ethnicity.csv"), row.names = FALSE)

#reformat and calculate percentages and totals
ethnicity_wide_pico <- ethnicity %>%
  filter(GEOID %in% pico_tracts$GEOID,
         variable %in% c("B03002_001",  # Total pop
                         "B03002_003",  # White
                         "B03002_004",  # Black
                         "B03002_005",  # American Indian
                         "B03002_006",  # Asian
                         "B03002_007",  # Hawaiian and Pacific Islander
                         "B03002_010",  # Other
                         "B03002_012"   # Hispanic
         )) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate,
    names_prefix = "eth_"
  ) %>%
  rename(
    total = eth_B03002_001,
    white = eth_B03002_003,
    black = eth_B03002_004,
    native_am = eth_B03002_005,
    asian = eth_B03002_006,
    hawaiian_pac = eth_B03002_007,
    other = eth_B03002_010,
    hispanic = eth_B03002_012
  ) %>%
  mutate(
    native_hawaiian = native_am + hawaiian_pac,
    pct_white = white / total * 100,
    pct_black = black / total * 100,
    pct_native_hawaiian = native_hawaiian / total * 100,
    pct_asian = asian / total * 100,
    pct_other = other / total * 100,
    pct_hispanic = hispanic / total * 100
  ) %>%
  bind_rows(
    summarise(.,
              GEOID = "Total",
              total = sum(total, na.rm = TRUE),
              white = sum(white, na.rm = TRUE),
              black = sum(black, na.rm = TRUE),
              native_am = sum(native_am, na.rm = TRUE),
              hawaiian_pac = sum(hawaiian_pac, na.rm = TRUE),
              native_hawaiian = native_am + hawaiian_pac,
              asian = sum(asian, na.rm = TRUE),
              other = sum(other, na.rm = TRUE),
              hispanic = sum(hispanic, na.rm = TRUE),
              pct_white = white / total * 100,
              pct_black = black / total * 100,
              pct_native_hawaiian = native_hawaiian / total * 100,
              pct_asian = asian / total * 100,
              pct_other = other / total * 100,
              pct_hispanic = hispanic / total * 100)
  )

#reformat for ggplot
ethnicity_long <- ethnicity_wide_pico %>%
  filter(GEOID == "Total") %>%
  select(starts_with("pct_")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "group",
    values_to = "percent"
  ) %>%
  mutate(
    group = recode(group,
                   pct_white = "White",
                   pct_black = "Black",
                   pct_native_hawaiian = "Native American, Alaska Native, Native Hawaiiwan, and other Pacific Islander",
                   pct_asian = "Asian",
                   pct_other = "Other",
                   pct_hispanic = "Hispanic")
  )



## WCG: might consider shortening the label to something like "Native", "Indigenous", or "AIAN/NHPI"
#create a barchart
ethnicity_chart_pico <- ggplot(ethnicity_long, aes(x = reorder(group, -percent), y = percent, fill = group)) +
  geom_bar(stat = "identity") +
  labs(
    title = NULL,
    x = NULL,
    y = "Percent of Corridor"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) + 
  theme(legend.position = "none")+
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16))

#save
ggsave(file.path(file_path, "Pico/Outputs/ethnicity_chart_pico.png"), width = 14, height = 6, dpi = 300)

####----Country of Origin----####
##Country of origin##
#pull from acs
country_origin <- 
  get_acs(
    geography = "tract",
    table = "B05006",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE)

write.csv(country_origin, file.path(file_path, "Fileshare", "country_origin.csv"), row.names = FALSE)

#filter for pico tracts 
country_origin_filtered_pico <- country_origin %>%
  filter(GEOID %in% pico_tracts$GEOID)

country_of_origin_totals <- country_origin_filtered_pico %>%
  group_by(variable) %>%
  summarise(total = sum(estimate, na.rm = TRUE), .groups = "drop")

##calculate total # share of foreign born

#divide by the total population 
foreign_value <- country_origin_filtered_pico %>%
  filter(variable == "B05006_001") %>%
  summarise(total_estimate = sum(estimate, na.rm = TRUE)) %>%
  mutate(result = total_estimate / totalpop) %>%
  pull(result)

#after reviewing the table above, edit to manually pull out the highest (in this case I chose the top countries with more than 1,000). This is done manually because some of the variables include 'Total', 'Asia', 'Central America', etc.
country_origin_top_10_pico <- country_origin_filtered_pico %>%
  filter(
    GEOID %in% pico_tracts$GEOID
  ) %>%
  mutate(country = case_when(
    variable == "B05006_158" ~ "Guatemala",
    variable == "B05006_157" ~ "El Salvador",
    variable == "B05006_160" ~ "Mexico",
    variable == "B05006_054" ~ "Korea",
    variable == "B05006_049" ~ "China",
    variable == "B05006_074" ~ "Phillipnes",
    variable == "B05006_159" ~ "Honduras",
  )) %>%
  group_by(country) %>%
  summarise(total = sum(estimate, na.rm = TRUE), .groups = "drop")

write.csv(country_origin_filtered_pico, file.path(file_path, "Fileshare", "country_origin_filtered_pico.csv"), row.names = FALSE)

####----Disability----####
#pulling disability status data
disability <-
  get_acs(
    geography = "tract",
    variables = c(
      total_pop = "B18101_001", 
      v1="B18101_004", 
      v2="B18101_007", 
      v3="B18101_010", 
      v4="B18101_013", 
      v5="B18101_016", 
      v6="B18101_019",
      v7="B18101_023", 
      v8="B18101_026", 
      v9="B18101_029", 
      v10="B18101_032", 
      v11="B18101_035", 
      v12="B18101_038"
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE) %>%
  select(GEOID, variable, estimate) %>%
  ## WCG: Spaces! 
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting so each tract is one row and each variable is one column
  mutate(
    total_disability =v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12, #summing diability by sex by age to get total population with disability by tract
    share_disability = total_disability/total_pop)%>% #generating share of tract population with disability
  select(GEOID,total_pop,total_disability,share_disability) #reducing df to necessary variables

#binding with pico tracts to limit our data to the study area
pico_disability <- disability %>%
  filter(GEOID %in% pico_tracts$GEOID)

#add geometry from census tracts to create a map
## WCG: left_join will be a bit cleaner for this purpose I think
## left_join(pico_tracts, pico_disability, by = "GEOID")
st_geometry(pico_disability) <- st_geometry(pico_tracts[match(pico_disability$GEOID, pico_tracts$GEOID), ])

#creating custom bins for a map of %pop w/ disability
map_disability_pico <- pico_disability%>% 
  mutate(bin_disability = cut(total_disability, breaks=c(0,200, 300, 400, 500,800),
                              labels  = c("Less than 200", "200-300", "300-400", "400-500", "500 or more"),
                              include.lowest = TRUE))
## WCG: prefer to consolidate this into the mutate call above rather than use the $ operator
#next, make each of the bins a factor
map_disability_pico$bin_disability <- factor(map_disability_pico$bin_disability, 
                                       levels = c("Less than 200", "200-300", "300-400", "400-500", "500 or more"))

## WCG: just noting that this is a count variable. This could be appropriate depending on the
## intended use, but alternately, a standardized (percentage) variable might be better
#making the map
plot <-ggplot()+
  geom_sf(map_disability_pico, mapping = aes(fill = bin_disability), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,5,6,8)], #can adjust the palette or color scheme as necessary
    name = str_wrap("Number of individuals with a disability", width = 30),
    breaks = c("Less than 200", "200-300", "300-400", "400-500", "500 or more")
  )+
  theme_urbn_map() +  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(1.2, "cm")
  ) +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1))

print(plot) #view map
ggsave(file.path(file_path, "Pico/Outputs/disability_map_pico.png"), width = 14, height = 6, dpi = 300)

####----Median Income / Poverty----####
#pull income from ACS
median_income <- 
  get_acs(
    geography = "tract",
    table = "B19013",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE
  )

#save
write.csv(median_income, file.path(file_path, "Fileshare", "median_income.csv"), row.names = FALSE)

##Map of median incomes by Census Tract

#edit the table so that each row is a census tract
income_wide_pico <- median_income %>%
  filter(variable == "B19013_001", GEOID %in% pico_tracts$GEOID) %>%
  mutate(income = estimate) %>%
  select(GEOID, income, NAME, moe, geometry)

#create a map based on incomes
income_map_pico <- ggplot(income_wide_pico) +
  geom_sf(aes(fill = income), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0.2, lwd = 1) +
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.3) +
  scale_fill_gradientn(
    colors = palette_urbn_green[c(1,2,4,6,8)],
    name = "Median Income",
    labels = scales::label_dollar(scale_cut = scales::cut_short_scale())
  ) +
  theme_urbn_map() +  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(1.2, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(8, "cm"),
      barheight = unit(0.6, "cm")
    )
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/income_map_pico.png"), width = 14, height = 6, dpi = 300)


#pull poverty level from ACS
poverty_level <-
  get_acs(
    geography = "tract",
    table = "B17001",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = TRUE
    )

#save
write.csv(poverty_level, file.path(file_path, "Fileshare", "poverty_level.csv"), row.names = FALSE)

#edit table so that each row is a census tract and we can calculate the share
poverty_wide_pico <- poverty_level %>%
  filter(variable %in% c("B17001_001", "B17001_002"),
         GEOID %in% pico_tracts$GEOID) %>%
  select(GEOID, variable, estimate, geometry) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    pct_below_pov = 100 * B17001_002 / B17001_001,
    pov_bin = case_when(
      pct_below_pov < 10 ~ "Less than 10%",
      pct_below_pov >= 10 & pct_below_pov < 20 ~ "10–20%",
      pct_below_pov >= 20 & pct_below_pov < 30 ~ "20–30%",
      pct_below_pov >= 30 ~ "30% or more",
      TRUE ~ NA_character_
    ),
    pov_bin = factor(pov_bin, levels = c("Less than 10%", "10–20%", "20–30%", "30% or more"))
  )%>%
  rename(
    total_pop = B17001_001,
    below_pov = B17001_002
  ) %>%
  select(GEOID, total_pop, below_pov, pct_below_pov, pov_bin, geometry)

sum(poverty_wide_pico$below_pov)

total_share_below_pov <- sum(poverty_wide_pico$below_pov, na.rm = TRUE) / sum(poverty_wide_pico$total_pop, na.rm = TRUE)

#Map of tracts by poverty level status
poverty_map_pico <- ggplot(poverty_wide_pico) +
  geom_sf(aes(fill = pov_bin), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0.2, lwd = 1) +
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.3) +
  scale_fill_manual(
    values = c(
      "Less than 10%" = "#d0f0c0",
      "10–20%" = "#a1d99b",
      "20–30%" = "#74c476",
      "30% or more" = "#238b45"
    ),
    name = "Share Below Poverty",
    drop = FALSE
  ) +
  theme_urbn_map() +  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(1.2, "cm")
  ) +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  )

ggsave(file.path(file_path, "Pico/Outputs/poverty_map_pico.png"), width = 14, height = 6, dpi = 300)

####----Employment----####
#pull employment status from acs
employment_status <-
  get_acs(
    geography = "tract",
    table = "B23001",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )

write.csv(employment_status, file.path(file_path, "Fileshare", "labor_force.csv"), row.names = FALSE)

#create employment bins to capture employed vs. unemplyed

employment_bins <- v23 %>%
  filter(str_starts(name, "B23001")) %>%
  mutate(employment_status = case_when(
    str_detect(label, regex("\\bEmployed\\b", ignore_case = FALSE)) ~ "Employed",
    str_detect(label, "Unemployed") ~ "Unemployed",
    str_detect(label, "Armed") ~ "Armed Forces",
    str_detect(label, "Not in labor force") ~ "Not in the Labor Force",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(employment_status)) %>%
  select(variable = name, employment_status) 

#create a new table that lists the employment status (total and share) for each tract

employment_wide_pico <- employment_status %>%
  filter(variable %in% employment_bins$variable, GEOID %in% pico_tracts$GEOID) %>%
  left_join(employment_bins, by = "variable") %>%  # bring in employment_status
  filter(!is.na(employment_status)) %>%
  group_by(GEOID, employment_status) %>%
  summarise(population = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = employment_status, values_from = population, values_fill = list(population = 0)) %>%
  left_join(select(employment_status, GEOID, NAME) %>% distinct(), by = "GEOID")

#restructure for bar chart
employment_bar_data <- employment_wide_pico %>%
  select(`Employed`, `Unemployed`, `Not in the Labor Force`) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "employment_status", values_to = "total") %>%
  mutate(share = total / sum(total))

#create the bar chart
employment_bar_chart_pico <- ggplot(employment_bar_data, aes(x = employment_status, y = share, fill = employment_status)) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_col() + 
  geom_text(aes(label = scales::percent(share, accuracy = 1)), vjust = -0.5, size = 6) + 
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank()  
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/employment_bar_chart_pico.png"), width = 14, height = 6, dpi = 300)


####----Gender----####
#see sex_by_age

## WCG: when you summarize observations with geometries, it implicitly
## dissolves the geometry boundaries, which can be reallllllly slow
gender = sex_by_age %>%
  st_drop_geometry()

#pivot to take Total Male and Total Female population
gender_wide_pico <- gender %>%
  filter(
    variable %in% c("B01001_001", "B01001_002", "B01001_026"),
    GEOID %in% pico_tracts$GEOID
  ) %>%
  st_drop_geometry() %>%
  select(GEOID, variable, estimate) %>%
  group_by(GEOID) %>%
  summarise(
    Total = sum(estimate[variable == "B01001_001"], na.rm = TRUE),
    Male = sum(estimate[variable == "B01001_002"], na.rm = TRUE),
    Female = sum(estimate[variable == "B01001_026"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    gender %>% select(GEOID, NAME) %>% distinct(),
    by = "GEOID"
  ) %>%
  mutate(
    share_male = Male / Total,
    share_female = Female / Total
  ) %>%
  #Add total row
  bind_rows(
    summarise(
      .,
      GEOID = "Total",
      NAME = "Total",
      Total = sum(Total, na.rm = TRUE),
      Male = sum(Male, na.rm = TRUE),
      Female = sum(Female, na.rm = TRUE),
      share_male = sum(Male, na.rm = TRUE) / sum(Total, na.rm = TRUE),
      share_female = sum(Female, na.rm = TRUE) / sum(Total, na.rm = TRUE)
    )
  )

##WCG: probably just a written statistic, if any mention at all
#create figure
gender_bar_chart_pico <- ggplot(
  tibble(
    status = c("Male", "Female"),
    share = c(
      gender_wide_pico$share_male[gender_wide_pico$GEOID == "Total"],
      gender_wide_pico$share_female[gender_wide_pico$GEOID == "Total"]
    )
  ),
  aes(x = status, y = share, fill = status)
) +
  geom_col() + 
  geom_text(aes(label = scales::percent(share, accuracy = 1)), vjust = -0.5, size = 6) + 
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.1))) +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 16),  
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank()  
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/gender_bar_chart_pico.png"), width = 14, height = 6, dpi = 300)


####----Household Structure----####
#pull family structure
family_structure <-
  get_acs(
    geography = "tract",
    table = "B11003",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )


#save
write.csv(family_structure, file.path(file_path, "Fileshare", "family_structure.csv"), row.names = FALSE)

## WCG: I don't think "single" is correct; "not married" or "spouse not present" would be more precise
#pivot to get descriptive stats
family_bins <- list(
  married_children = "B11003_003",
  married_no_children = "B11003_007",
  single_children = c("B11003_010", "B11003_016"),
  single_no_children = c("B11003_014", "B11003_020")
)


#summarise total by family structure, for each tract
# Pivot family_structure data wider
family_wide_pico <- family_structure %>%
  filter(variable %in% unlist(family_bins), GEOID %in% pico_tracts$GEOID) %>%
  mutate(family_structure = case_when(
    variable %in% family_bins$married_children ~ "Married with children under 18",
    variable %in% family_bins$married_no_children ~ "Married without children under 18",
    variable %in% family_bins$single_children ~ "Single parent with children under 18",
    variable %in% family_bins$single_no_children ~ "Single parent without children under 18",
    TRUE ~ NA_character_ 
  )) %>%
    filter(!is.na(family_structure)) %>%
  group_by(GEOID, family_structure) %>%
  summarise(population = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  # Pivot the data wider to have family types as columns
  pivot_wider(names_from = family_structure, values_from = population, values_fill = list(population = 0)) %>%
    left_join(select(family_structure, GEOID, NAME) %>% distinct(), by = "GEOID")


#calculate the totals 
family_totals <- family_wide_pico %>%
  summarise(across(starts_with("Married"), ~sum(.x, na.rm = TRUE)), 
            across(starts_with("Single"), ~sum(.x, na.rm = TRUE))) %>%
  # Reshape the data to a long format for easier analysis
  pivot_longer(
    cols = everything(),
    names_to = "family_structure",
    values_to = "population"
  )


#create a bar chart
pop_by_family_type_pico <- ggplot(family_totals, aes(x = family_structure, y = population, fill = family_structure)) +
  geom_col() +
  geom_text(aes(label = scales::comma(population)), vjust = -0.5, size = 5) +
  labs(
    x = NULL,
    y = "Population"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) + 
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/pop_by_family_type_pico.png"), width = 14, height = 6, dpi = 300)

####----Language Access / Culture----####

###pull hearing difficulty status
hearing_diff <- 
  get_acs(
    geography = "tract",
    variables = c(
      total_pop = "B18102_001", 
      v1="B18102_004", 
      v2="B18102_007", 
      v3="B18102_010", 
      v4="B18102_013", 
      v5="B18102_016", 
      v6="B18102_019",
      v7="B18102_023", 
      v8="B18102_026", 
      v9="B18102_029", 
      v10="B18102_032", 
      v11="B18102_035", 
      v12="B18102_038"
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE
  ) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    total_hearing_diff =v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12, #summing hearing diff by sex by age to get total population with hearing diff
    share_hearing_diff = total_hearing_diff/total_pop)%>% #generating share of tract population with hearing diff
  select(GEOID,total_pop,total_hearing_diff,share_hearing_diff)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

#add geometry for map
st_geometry(hearing_diff) <- st_geometry(pico_tracts[match(hearing_diff$GEOID, pico_tracts$GEOID), ])
class(hearing_diff)

#creating custom bins for a map of %pop w/ hearing diff
map_hearing_diff <- hearing_diff%>% 
  mutate(bin_hearing_diff = cut(total_hearing_diff, breaks=c(0,50,100,150,99999),
                              labels  = c("Less than 50", "50-100", "100-150", "150 or more"),
                              include.lowest = TRUE))
#making each bin a factor
map_hearing_diff$bin_hearing_diff <- factor(map_hearing_diff$bin_hearing_diff, 
                                             levels = c("Less than 50", "50-100", "100-150", "150 or more"))

##WCG: if I were to guess, there are no statistically significant differences in counts
##of individuals with hearing difficulty across the tracts you're looking at because
##the margions of error are so high. I'd consider dropping all of the subsets of disability here--
##they're interesting, but probably not appropriate to map absent more context.
#plot map
plot <-ggplot()+
  geom_sf(map_hearing_diff, mapping = aes(fill = bin_hearing_diff), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,6,8)], #can adjust the palette or color scheme as necessary
    name = str_wrap("Number of individuals with hearing difficulty", width = 30),
    breaks = c("Less than 50", "50-100", "100-150", "150 or more")
  )+
  theme_urbn_map() +  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(1.2, "cm")
  ) +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1))
print(plot)
ggsave(file.path(file_path, "Pico/Outputs/hearing_diff_map_pico.png"), width = 14, height = 6, dpi = 300)


#pull vision difficulty status data
vision_diff <- 
  get_acs(
    geography = "tract",
    variables = c(
      total_pop = "B18103_001", 
      v1="B18103_004", 
      v2="B18103_007", 
      v3="B18103_010", 
      v4="B18103_013", 
      v5="B18103_016", 
      v6="B18103_019",
      v7="B18103_023", 
      v8="B18103_026", 
      v9="B18103_029", 
      v10="B18103_032", 
      v11="B18103_035", 
      v12="B18103_038"
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE
  ) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    total_vision_diff =v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12, #summing hearing diff by sex by age to get total population with vision diff
    share_vision_diff = total_vision_diff/total_pop)%>% #generating share of tract population with vision diff
  select(GEOID,total_pop,total_vision_diff,share_vision_diff)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

#add geometry for map
st_geometry(vision_diff) <- st_geometry(pico_tracts[match(vision_diff$GEOID, pico_tracts$GEOID), ])
class(vision_diff)

#creating custom bins for a map of %pop w/ vision diff
map_vision_diff <- vision_diff%>% 
  mutate(bin_vision_diff = cut(total_vision_diff, breaks=c(0,50,100,150,99999),
                                labels  = c("Less than 50", "50-100", "100-150", "150 or more"),
                                include.lowest = TRUE))
#making each bin a factor
map_vision_diff$bin_vision_diff <- factor(map_vision_diff$bin_vision_diff, 
                                            levels = c("Less than 50", "50-100", "100-150", "150 or more"))

#plot map
plot <-ggplot()+
  geom_sf(map_vision_diff, mapping = aes(fill = bin_vision_diff), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,6,8)], #can adjust the palette or color scheme as necessary
    name = str_wrap("Number of individuals with a vision difficulty", width = 30),
    breaks = c("Less than 50", "50-100", "100-150", "150 or more")
  )+
  theme_urbn_map() +  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(1.2, "cm")
  ) +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1))


print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/vision_diff_map_pico.png"), width = 14, height = 6, dpi = 300)

#pulling ambulatory difficulty data
ambulatory_diff <-
  get_acs(
    geography = "tract",
    table = "B18105",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )%>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    total =B18105_001,
    total_ambulatory_diff = (B18105_004 + B18105_007 + B18105_010 + B18105_013 + B18105_016 +
      B18105_020 + B18105_023 + B18105_026 + B18105_029 + B18105_032),
    share_ambulatory_diff = total_ambulatory_diff/total
  )%>%
  select(GEOID,total, total_ambulatory_diff, share_ambulatory_diff)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts


#add geometry for map
st_geometry(ambulatory_diff) <- st_geometry(pico_tracts[match(ambulatory_diff$GEOID, pico_tracts$GEOID), ]
                                            )
class(ambulatory_diff)

#creating custom bins for # of individuals w/ ambulatory diff by tract
map_amb_diff <- ambulatory_diff%>% 
  mutate(bin_amb_diff = cut(total_ambulatory_diff, breaks=c(0,50,100,200, 300,500),
                               labels  = c("Fewer than 50", "50-100", "100-200", "200-300", "300 or more"),
                               include.lowest = TRUE))
#making each bin a factor
map_amb_diff$bin_amb_diff <- factor(map_amb_diff$bin_amb_diff, 
                                          levels = c("Fewer than 50", "50-100", "100-200", "200-300", "300 or more"))

#plot map
plot <-ggplot()+
  geom_sf(map_amb_diff, mapping = aes(fill = bin_amb_diff), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,5,7,8)], #can adjust the palette or color scheme as necessary
    name = str_wrap("Number of individuals with ambulatory difficulty", width = 30),
    breaks = c("Fewer than 50", "50-100", "100-200", "200-300", "300 or more")
  )+
  theme_urbn_map() +  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(1.2, "cm")
  ) +
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1))


print(plot) #view map

# save
ggsave(file.path(file_path, "Pico/Outputs/ambulatory_diff_map_pico.png"), width = 14, height = 6, dpi = 300)



####----Work Hours----####
#pull hours worked data
hrs_worked <-
  get_acs(
    geography = "tract",
    table = "B23022",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )

####----Internet / Computer Access----####
#pull internet data
internet_subs <-
  get_acs(
    geography = "tract",
    variables = c(
      total_pop = "B28002_001",
      no_int_access = "B28002_013"),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE, #change back to TRUE to make a map
  )%>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    share_no_int = no_int_access/total_pop)%>% #generating share of tract population without internet access
  select(GEOID,total_pop,no_int_access,share_no_int)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

#adding geometry for map
st_geometry(internet_subs) <- st_geometry(pico_tracts[match(internet_subs$GEOID, pico_tracts$GEOID), ])
class(internet_subs)

#creating custom bins for a map of share w/out int access
map_no_int <- internet_subs%>% 
  mutate(bin_no_int = cut(share_no_int, breaks=c(0,0.05, 0.1, 0.15,0.2,1),
                               labels  = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20% or more"),
                               include.lowest = TRUE))
#making each bin a factor
map_no_int$bin_no_int <- factor(map_no_int$bin_no_int, 
                                          levels = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20% or more"))

#plot map
plot <-ggplot()+
  geom_sf(map_no_int, mapping = aes(fill = bin_no_int), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,2,4,6,8)], #can adjust the palette or color scheme as necessary
    name = "Share of households without internet access",
    breaks = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20% or more")
  )+
  theme_urbn_map() +
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))


print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/internet_access_map_pico.png"), width = 14, height = 6, dpi = 300)

####----Tech Access----####
tech_access <-
  get_acs(
    geography = "tract",
    variables = c(
      total_pop = "B28001_001",
      no_computer = "B28001_011"
      ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )%>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    share_no_computer = no_computer/total_pop)%>% #generating share of tract population with different types of computer access
  select(GEOID,total_pop,no_computer,share_no_computer)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

#add geometry for map
st_geometry(tech_access) <- st_geometry(pico_tracts[match(tech_access$GEOID, pico_tracts$GEOID), ])
class(tech_access)

#creating custom bins for a map of %pop w/ vision diff
map_tech_access <- tech_access%>% 
  mutate(bin_no_computer = cut(share_no_computer, breaks=c(0,0.01,0.05,0.1,1),
                               labels  = c("Less than 1%", "1-5%", "5-10%", "10% or higher"),
                               include.lowest = TRUE))
#making each bin a factor
map_tech_access$bin_no_computer <- factor(map_tech_access$bin_no_computer, 
                                          levels = c("Less than 1%", "1-5%", "5-10%", "10% or higher"))

#plot map
plot <-ggplot()+
  geom_sf(map_tech_access, mapping = aes(fill = bin_no_computer), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,6,8)],
    name = "Share of households without access to a device",
    breaks = c("Less than 1%", "1-5%", "5-10%", "10% or higher")
  )+
  theme_urbn_map() +
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))


print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/tech_access_map_pico.png"), width = 14, height = 6, dpi = 300)

###bar chart of share of total population by device access type
tech_by_type <-
  get_acs(
    geography = "tract",
    variables = c(
      total_pop = "B28001_001",
      has_computer = "B28001_003",
      computer_only = "B28001_004",
      has_smartphone = "B28001_005",
      smartphone_only = "B28001_006",
      has_tablet = "B28001_007",
      tablet_only = "B28001_008",
      no_computer = "B28001_011"
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE
  )%>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    share_computer = has_computer/total_pop,
    share_computer_only = computer_only/total_pop,
    share_smartphone = has_smartphone/total_pop,
    share_smartphone_only = smartphone_only/total_pop,
    share_tablet = has_tablet/total_pop,
    share_tablet_only = tablet_only/total_pop,
    share_no_computer = no_computer/total_pop)%>% #generating share of tract population with different types of computer access
  select(GEOID,total_pop,no_computer, has_computer, computer_only,
         has_smartphone, smartphone_only, has_tablet, tablet_only,)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

#making separate df for bar chart  
tech_sums <- as.data.frame(t(colSums(tech_by_type[, 2:9]))) %>%
  mutate(
    share_computer = has_computer/total_pop,
    share_computer_only = computer_only/total_pop,
    share_smartphone = has_smartphone/total_pop,
    share_smartphone_only = smartphone_only/total_pop,
    share_tablet = has_tablet/total_pop,
    share_tablet_only = tablet_only/total_pop,
    share_no_computer = no_computer/total_pop)
tech_sums_long <- tech_sums %>% #selecting columns we want
  select(share_computer, share_smartphone, share_tablet, share_no_computer) %>%
  pivot_longer(cols = everything(), names_to = "device_type", values_to = "share") 

# Reorder the device_type factor to the custom order with smartphone and computer swapped
tech_sums_long$device_type <- factor(tech_sums_long$device_type, 
                                     levels = c("share_computer", "share_smartphone", "share_tablet", "share_no_computer"))  # Custom order

# Now plot the data
plot <- ggplot(tech_sums_long) +
  geom_col(mapping = aes(x = device_type, y = share), position = "dodge") +
  geom_text(
    aes(x = device_type, y = share, label = scales::percent(share, accuracy = 1)), 
    vjust = -0.5, size = 3, family = "Lato"
  ) +
  labs(
    x = "Device Type",
    y = "Share of Households"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0)), 
    limits = c(0, 1), 
    breaks = c(.25, .5, .75)
  ) +
  scale_x_discrete(labels = c(
    "share_computer" = "Access to laptop or desktop computer", 
    "share_no_computer" = "No access to any device", 
    "share_smartphone" = "Access to smartphone",
    "share_tablet" = "Access to tablet or other mobile device"
  )) +
  theme(
    legend.position="top",
    legend.text = element_text(size=12, family="Lato"),
    axis.title.x = element_text(size = 12, family = "Lato"),
    axis.title.y = element_text(size = 12, family = "Lato"),
    axis.text.x = element_text(size=10, family="Lato"),
    axis.text.y = element_text(size=10, family="Lato")
  )

  
print(plot)

#saving
ggsave(file.path(file_path, "Pico/Outputs/tech_access_bar_pico.png"),  width = 9, height = 3)                       
  

####----Housing and Displacement----####

### Homeowners/Renters ###
#pull housing tenure data
tenure <-   
  get_acs(
  geography = "tract",
  variables = c(
    total_occupied = "B25003_001",
    owner_occupied = "B25003_002",
    renter_occupied = "B25003_003"
  ),
  state = state_fips,
  county = county_fips,
  year = 2023,
  survey = "acs5",
  geometry = FALSE,
)%>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    share_owner_occupied = owner_occupied/total_occupied,
    share_renter_occupied = renter_occupied/total_occupied)%>% #generating shares of HU that are owner/renter occupied
  select(GEOID,total_occupied,owner_occupied,share_owner_occupied, renter_occupied, share_renter_occupied)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts



#adding geometry for map
st_geometry(tenure) <- st_geometry(pico_tracts[match(tenure$GEOID, pico_tracts$GEOID), ])
class(tenure)

#creating custom bins for a map of share of occupied HU that are renter
map_tenure <- tenure%>% 
  mutate(bin_tenure = cut(share_renter_occupied, breaks=c(0.5, 0.6, 0.7,0.8,0.9,1),
                          labels  = c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
                          include.lowest = TRUE))
#making each bin a factor
map_tenure$bin_tenure <- factor(map_tenure$bin_tenure, 
                                levels =c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%"))

#plot map
plot <-ggplot()+
  geom_sf(map_tenure, mapping = aes(fill = bin_tenure), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_quintile[], #can adjust the palette or color scheme as necessary
    name = "Share of occupied housing units that are rented",
    breaks = c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
  )+
  theme_urbn_map()+
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))


print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/share_renter_occupied_map_pico.png"), width = 14, height = 6, dpi = 300)

#check overall homeownership for the report
total_share_owner_occupied <- sum(tenure$owner_occupied) / sum(tenure$total_occupied)


####----Language----####

#pull from acs
language <-
  get_acs(
    geography = "tract",
    table = "B16004",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )

#save
write.csv(language, file.path(file_path, "Fileshare", "language.csv"), row.names = FALSE)

#create bins based on the table
language_bins <- list(
  very_well = c("B16004_005", "B16004_010", "B16004_015", "B16004_020", "B16004_027", "B16004_032", "B16004_037", "B16004_042", "B16004_049", "B16004_054", "B16004_059", "B16004_064"),
  well = c("B16004_006", "B16004_011", "B16004_016", "B16004_021", "B16004_028", "B16004_033", "B16004_038", "B16004_043", "B16004_050", "B16004_055", "B16004_060", "B16004_065"),
  not_well = c("B16004_007", "B16004_012", "B16004_017", "B16004_022", "B16004_029", "B16004_034", "B16004_039", "B16004_044", "B16004_051", "B16004_056", "B16004_061", "B16004_066"),
  none =c("B16004_008", "B16004_013", "B16004_018", "B16004_023", "B16004_030", "B16004_035", "B16004_040", "B16004_045", "B16004_052", "B16004_057", "B16004_062", "B16004_067")
)

#summarise total by language, for each tract
# Pivot  data wider
language_wide_pico <- language %>%
  filter(variable %in% unlist(language_bins), GEOID %in% pico_tracts$GEOID) %>%
  mutate(language_skill = case_when(
    variable %in% language_bins$very_well ~ "Very well",
    variable %in% language_bins$well ~ "Well",
    variable %in% language_bins$not_well ~ "Not well",
    variable %in% language_bins$none ~ "Not at all",
    TRUE ~ NA_character_  
  )) %>%
  filter(!is.na(language_skill)) %>%
  group_by(GEOID, language_skill) %>%
  summarise(population = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  # Pivot the data wider to have language skills as columns
  pivot_wider(names_from = language_skill, values_from = population, values_fill = list(population = 0)) %>%
  left_join(select(language, GEOID, NAME) %>% distinct(), by = "GEOID")

#calculate totals for bar chart
language_totals <- language_wide_pico %>%
  select(-GEOID, -NAME) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "language_skill",
    values_to = "population"
  )

#reorder so that the bars show up in the order we want
language_totals$language_skill <- factor(
  language_totals$language_skill,
  levels = c("Very well", "Well", "Not well", "Not at all")
)

#create a bar chart
pop_by_language_skill_pico <- ggplot(language_totals, aes(x = language_skill, y = population, fill = language_skill)) +
  geom_col() +
  geom_text(aes(label = scales::comma(population)), vjust = -0.5, size = 5) +
  labs(
    x = NULL,
    y = "Population"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) + 
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank()
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/pop_by_language_skill_pico.png"), width = 14, height = 6, dpi = 300)

#repeat the above but calculating percentages
# Calculate percentages for each language skill category
language_totals_pct <- language_wide_pico %>%
  select(-GEOID, -NAME) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "language_skill",
    values_to = "population"
  ) %>%
  mutate(
    percent = population / sum(population)
  )

# Reorder the factor levels
language_totals_pct$language_skill <- factor(
  language_totals_pct$language_skill,
  levels = c("Very well", "Well", "Not well", "Not at all")
)

# Plot as a percentage bar chart
pop_by_language_skill_pct_pico <- ggplot(language_totals_pct, aes(x = language_skill, y = percent, fill = language_skill)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percent, accuracy = 1)), vjust = -0.5, size = 5) +  # Percent labels
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0, 0.1))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) + 
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/pop_by_language_skill_pct_pico.png"), width = 14, height = 6, dpi = 300)


##Other languages spoken in the area##
#see language

#create bins for each language spoken
language_bins_1 <- list(
  total = "B16004_001",
  only_english = c("B16004_003", "B16004_025", "B16004_047"),
  spanish = c("B16004_004", "B16004_026", "B16004_048"),
  other_indo_euro = c("B16004_009", "B16004_031", "B16004_053"),
  asian_pac_island =c("B16004_014", "B16004_036", "B16004_058"),
  other_language = c("B16004_19", "B16004_041", "B16004_063")
)

#summarise total by language, for each tract
# Pivot  data wider
spoken_language_wide_pico <- language %>%
  filter(variable %in% unlist(language_bins_1), GEOID %in% pico_tracts$GEOID) %>%
  mutate(language_spoken = case_when(
    variable %in% language_bins_1$total ~ "Total",
    variable %in% language_bins_1$only_english ~ "English Only",
    variable %in% language_bins_1$spanish ~ "Spanish",
    variable %in% language_bins_1$other_indo_euro ~ "Other Indo-European Languages",
    variable %in% language_bins_1$asian_pac_island ~ "Asian and Pacific Island Languages",
    variable %in% language_bins_1$other_language ~ "Other Languages",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(language_spoken)) %>%
  group_by(GEOID, language_spoken) %>%
  summarise(population = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
  # Pivot wider to get one column per language
  pivot_wider(names_from = language_spoken, values_from = population, values_fill = list(population = 0)) %>%
  mutate(
    share_english_only = `English Only` / Total,
    share_spanish = Spanish / Total,
    share_other_indo_euro = `Other Indo-European Languages` / Total,
    share_asian_pac_island = `Asian and Pacific Island Languages` / Total,
    share_other_language = `Other Languages` / Total
  ) %>%
  left_join(select(language, GEOID, NAME) %>% distinct(), by = "GEOID")

#calculate totals for bar chart
language_spoken_totals <- spoken_language_wide_pico %>%
  select(-GEOID, -NAME) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "language_spoken",
    values_to = "population"
  )

#reorder so that the bars show up in the order we want
language_spoken_totals$language_spoken <- factor(
  language_spoken_totals$language_spoken,
  levels = c("English Only", "Spanish", "Other Indo-European Languages", "Asian and Pacific Island Languages", "Other Languages")
)

#create a bar chart
pop_by_language_spoken_pico <- ggplot(language_spoken_totals, aes(x = language_spoken, y = population, fill = language_spoken)) +
  geom_col() +
  geom_text(aes(label = scales::comma(population)), vjust = -0.5, size = 5) +
  labs(
    x = NULL,
    y = "Population"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) + 
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/pop_by_language_spoken_pico.png"), width = 14, height = 6, dpi = 300)

#replicate in case we want percentages
# Calculate percentages for each language spoken category
language_spoken_totals_pct <- spoken_language_wide_pico %>%
  select(-GEOID, -NAME) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "language_spoken",
    values_to = "population"
  ) %>%
  mutate(
    percent = population / sum(population)
  )

# Reorder the factor levels
language_spoken_totals_pct$language_spoken <- factor(
  language_spoken_totals_pct$language_spoken,
  levels = c("English Only", "Spanish", "Other Indo-European Languages", "Asian and Pacific Island Languages", "Other Languages")
)

# Plot as a percentage bar chart
pop_by_language_spoken_pct_pico <- ggplot(language_spoken_totals_pct, aes(x = language_spoken, y = percent, fill = language_spoken)) +
  geom_col() +
  geom_text(aes(label = scales::percent(percent, accuracy = 1)), vjust = -0.5, size = 5) +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0, 0.1))) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 16)) + 
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/pop_by_language_spoken_pct_pico.png"), width = 14, height = 6, dpi = 300)

#create a map
spoken_language_wide_pico_sf <- spoken_language_wide_pico %>%
  left_join(select(pico_tracts, GEOID, geometry), by = "GEOID") %>%
  st_as_sf()

spanish_map_pico <- ggplot(spoken_language_wide_pico_sf) +
  geom_sf(aes(fill = share_spanish), show.legend = TRUE) +
  #add the buffer overlay
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0.2, lwd = 1) +
  #add the street overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.3) +
  scale_fill_gradientn(
    colors = palette_urbn_magenta[c(1, 3, 5, 7)], 
    name = "Share of Spanish Speakers",
    labels = scales::percent_format()
  ) +
  theme_urbn_map() +  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(1.2, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(8, "cm"),
      barheight = unit(0.6, "cm")
    )
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/spanish_map_pico.png"), width = 14, height = 6, dpi = 300)


api_map_pico <- ggplot(spoken_language_wide_pico_sf) +
  geom_sf(aes(fill = share_asian_pac_island), show.legend = TRUE) +
  #add the buffer overlay
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0.2, lwd = 1) +
  #add the street overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.3) +
  scale_fill_gradientn(
    colors = palette_urbn_magenta[c(2, 4, 6, 8)],
    name = str_wrap("Share of Asian or Pacific Island Language Speakers", width = 30),
    labels = scales::percent_format()
  ) +
  theme_urbn_map() +  
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.key.height = unit(0.6, "cm"),
    legend.key.width = unit(1.2, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(8, "cm"),
      barheight = unit(0.6, "cm")
    )
  )

#save
ggsave(file.path(file_path, "Pico/Outputs/api_map_pico.png"), width = 14, height = 6, dpi = 300)

####----Housing Cost Burden----####
#pull cost burden data

### for renters
renter_burden <-   
  get_acs(
    geography = "tract",
    variables = c(
      total = "B25070_001",
      spend_30_35 = "B25070_007",
      spend_35_40 = "B25070_008",
      spend_40_50="B25070_009",
      spend_50_more = "B25070_010",
      median_share_inc_housing = "B25071_001"
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )%>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting
  mutate(
    total_over_30 = (spend_30_35 + spend_35_40+spend_40_50 + spend_50_more),
    share_over_30 = (spend_30_35 + spend_35_40+spend_40_50 + spend_50_more)/total,
    share_over_50 = spend_50_more/total)%>% #generating shares of HH that are rent burdened and extremely rent burdened
  select(GEOID,total,spend_50_more,total_over_30,share_over_30,share_over_50,median_share_inc_housing)%>% #reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

#adding geometry for map
st_geometry(renter_burden) <- st_geometry(pico_tracts[match(renter_burden$GEOID, pico_tracts$GEOID), ])
class(renter_burden)

## first, map out the median income spent on housing by census tract
#making bins
map_med_inc_housing <- renter_burden%>%
  mutate(bin_median_share = cut(median_share_inc_housing, breaks=c(25, 30, 35, 40, 45, 50),
                                labels = c("less than 30%", "30-35%", "35-40%", "40-45%", "45-50%"),
                                include.lowest = TRUE))
#make each bin a factor
map_med_inc_housing$bin_median_share <- factor(map_med_inc_housing$bin_median_share,
                                               levels = c("less than 30%", "30-35%", "35-40%", "40-45%", "45-50%"))
#plotting
plot <-ggplot()+
  geom_sf(map_med_inc_housing, mapping = aes(fill = bin_median_share), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "black", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_diverging[c(5,4,3,2,1)], #can adjust the palette or color scheme as necessary
    name = "Median share of income spent on housing",
    breaks = c("less than 30%", "30-35%", "35-40%", "40-45%", "45-50%")
  )+
  theme_urbn_map()+
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))


print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/med_share_inc_housing_map_pico.png"), width = 14, height = 6, dpi = 300)


## next, map out the share of individuals spending more than 30% of income on rental housing
#making bins
map_30_pct <- renter_burden%>%
  mutate(bin_30_pct = cut(share_over_30, breaks=c(.30, .40, .50, .60, .70, .80),
                                labels = c("less than 40%", "40-50%", "50-60%", "60-70%", "70-80%"),
                                include.lowest = TRUE))
#make each bin a factor
map_30_pct$bin_30_pct <- factor(map_30_pct$bin_30_pct,
                                               levels = c("less than 40%", "40-50%", "50-60%", "60-70%", "70-80%"))
#plotting
plot <-ggplot()+
  geom_sf(map_30_pct, mapping = aes(fill = bin_30_pct), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_red[c(1,2,4,6,8)], #can adjust the palette or color scheme as necessary
    name = "Share of renters spending more than 30% of income on housing",
    breaks = c("less than 40%", "40-50%", "50-60%", "60-70%", "70-80%")
  )+
  theme_urbn_map()+
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))


print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/rent_burdened_map_pico.png"), width = 14, height = 6, dpi = 300)


## repeating for share severaly rent burdened (spending over 50% income on housing)
#making bins
map_50_pct <- renter_burden%>%
  mutate(bin_50_pct = cut(share_over_50, breaks=c(.20, .25, .30, .35, .40, .45),
                          labels = c("less than 25%", "25-30%", "30-35%", "35-40%", "40% or higher"),
                          include.lowest = TRUE))
#make each bin a factor
map_50_pct$bin_50_pct <- factor(map_50_pct$bin_50_pct,
                                levels = c("less than 25%", "25-30%", "30-35%", "35-40%", "40% or higher"))
#plotting
plot <-ggplot()+
  geom_sf(map_50_pct, mapping = aes(fill = bin_50_pct), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_red[c(1,2,4,6,7,8)], #can adjust the palette or color scheme as necessary
    name = "Share of renters spending more than 50% of income on housing",
    breaks = c("less than 25%", "25-30%", "30-35%", "35-40%", "40% or higher")
  )+
  theme_urbn_map()+
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))


print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/severely_rent_burdened_map_pico.png"), width = 14, height = 6, dpi = 300)



### for owners
owner_burden <-   
  get_acs(
    geography = "tract",
    variables = c(
      median_owner_costs_share_inc = "B25092_001"
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  ) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>% # pivoting
  select(GEOID, median_owner_costs_share_inc) %>% # reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) # limiting to pico tracts

# adding geometry for map
st_geometry(owner_burden) <- st_geometry(pico_tracts[match(owner_burden$GEOID, pico_tracts$GEOID), ])
class(owner_burden)

# categorize into bins and label NA values
map_owner_cost_burden <- owner_burden %>%
  mutate(bin_median_share = cut(median_owner_costs_share_inc, breaks = c(0, 20, 30, 100),
                                labels = c("less than 20%", "20-30%", "30% or higher"),
                                include.lowest = TRUE)) %>%
  mutate(bin_median_share = as.character(bin_median_share),
         bin_median_share = ifelse(is.na(bin_median_share), "No data", bin_median_share),
         bin_median_share = factor(bin_median_share, 
                                   levels = c("less than 20%", "20-30%", "30% or higher", "No data")))

# plotting
plot <- ggplot() +
  geom_sf(map_owner_cost_burden, mapping = aes(fill = bin_median_share), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "black", alpha = 0, lwd = 1) + # transparent buffer zone
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + # streets
  scale_fill_manual(
    values = c(
      "less than 20%" = palette_urbn_diverging[6],
      "20-30%" = palette_urbn_diverging[4],
      "30% or higher" = palette_urbn_diverging[2],
      "No data" = "gray80"  # na color
    ),
    name = "Median share of income spent on housing",
    breaks = c("less than 20%", "20-30%", "30% or higher", "No data"),
    na.value = "gray80",  # color for NA areas on the map
    guide = guide_legend(override.aes = list(alpha = 1))  # making sure NA values show in legend
  ) +
  theme_urbn_map()+
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))


print(plot)


ggsave(file.path(file_path, "Pico/Outputs/owner_med_share_inc_housing_map_pico.png"), width = 14, height = 6, dpi = 300)

#do this for share of HH rent burdened (not median)
owner_burden <-   
  get_acs(
    geography = "tract",
    variables = c(
      total = "B25091_001",
      total_mortgatge = "B25091_002",
      total_30_35_mort = "B25091_008",
      total_35_40_mort = "B25091_009",
      total_40_50_mort = "B25091_010",
      total_50_plus_mort = "B25091_011",
      total_no_mort = "B25091_013",
      total_30_35_no_mort = "B25091_019",
      total_35_40_no_mort = "B25091_020",
      total_40_50_no_mort = "B25091_021",
      total_50_plus_no_mort = "B25091_022"
      
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  ) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>% # pivoting
  mutate(
    total_30_plus = (total_30_35_mort + total_35_40_mort + total_40_50_mort + total_50_plus_mort + 
                       total_30_35_no_mort + total_35_40_no_mort + total_40_50_no_mort+ total_50_plus_no_mort),
    total_50_plus = (total_50_plus_mort + total_50_plus_no_mort),
    share_30_plus = (total_30_plus/total),
    share_50_plus = (total_50_plus/total)
  )%>%
  select(GEOID, total, total_30_plus, total_50_plus, share_30_plus, share_50_plus) %>% # reducing df to necessary variables
  filter(GEOID %in% pico_tracts$GEOID) # limiting to pico tracts

# adding geometry for map
st_geometry(owner_burden) <- st_geometry(pico_tracts[match(owner_burden$GEOID, pico_tracts$GEOID), ])
class(owner_burden)

# categorize into bins and assign "No data" to NA values
map_owner_cost_burden <- owner_burden %>%
  mutate(
    bin_share_30 = cut(share_30_plus, breaks = c(0, .20, .40, .60, .80, 1),
                       labels = c("less than 20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                       include.lowest = TRUE),
    bin_share_30 = as.character(bin_share_30),  # convert to character to add custom label
    bin_share_30 = ifelse(is.na(bin_share_30), "No data", bin_share_30),  # replace NA with label
    bin_share_30 = factor(bin_share_30, levels = c("less than 20%", "20-40%", "40-60%", "60-80%", "80-100%", "No data"))
  )

# plotting
plot <- ggplot() +
  geom_sf(map_owner_cost_burden, mapping = aes(fill = bin_share_30), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) +
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) +
  scale_fill_manual(
    values = c(
      "less than 20%" = palette_urbn_red[1],
      "20-40%" = palette_urbn_red[2],
      "40-60%" = palette_urbn_red[5],
      "60-80%" = palette_urbn_red[7],
      "80-100%" = palette_urbn_red[8],
      "No data" = "gray80"
    ),
    name = "Share of homeowners spending more than 30% of income on housing"
  ) +
  guides(fill = guide_legend(override.aes = list(color = NA))) +
  theme_urbn_map()+
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))


print(plot)

ggsave(file.path(file_path, "Pico/Outputs/owner_share_inc_housing_map_pico.png"), width = 14, height = 6, dpi = 300)

## repeating for share of people spending more than 50% of income on housing costs - owner occupied
# categorize into bins and assign "No data" to NA values
map_owner_cost_burden <- owner_burden %>%
  mutate(
    bin_share_50 = cut(share_50_plus, breaks = c(0, .20, .40, .60, .80, 1),
                       labels = c("less than 20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                       include.lowest = TRUE),
    bin_share_50 = as.character(bin_share_50),  # convert to character to add custom label
    bin_share_50 = ifelse(is.na(bin_share_50), "No data", bin_share_50),  # replace NA with label
    bin_share_50 = factor(bin_share_50, levels = c("less than 20%", "20-40%", "40-60%", "60-80%", "80-100%", "No data"))
  )

# plotting
plot <- ggplot() +
  geom_sf(map_owner_cost_burden, mapping = aes(fill = bin_share_50), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) +
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) +
  scale_fill_manual(
    values = c(
      "less than 20%" = palette_urbn_red[1],
      "20-40%" = palette_urbn_red[2],
      "40-60%" = palette_urbn_red[5],
      "60-80%" = palette_urbn_red[7],
      "80-100%" = palette_urbn_red[8],
      "No data" = "gray80"
    ),
    name = "Share of homeowners spending more than 50% of income on housing"
  ) +
  guides(fill = guide_legend(override.aes = list(color = NA))) +
  theme_urbn_map()+
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))


print(plot)

ggsave(file.path(file_path, "Pico/Outputs/owner_share_severe_rent_burden_map_pico.png"), width = 14, height = 6, dpi = 300)



####----Disadvantaged Communities----####

#load in CES disadvantaged community data
ces_disadvantaged <- read.xlsx(file.path(file_path, "Data/Disadvantaged/sb535_tract_all_data.xlsx")) %>%
  rename(GEOID = Census.Tract) %>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))%>% #need to add a leading zero to match GEOID structure
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

### 40 of 46 Pico tracts are disadvantaged


###load in displacement risk
displacement_risk <- read.csv(file.path(file_path, "Data/la_displacement_index.csv"))%>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))%>%
  filter(GEOID %in% pico_tracts$GEOID) #limiting to pico tracts

# adding geometry for map
st_geometry(displacement_risk) <- st_geometry(pico_tracts[match(displacement_risk$GEOID, pico_tracts$GEOID), ])
class(displacement_risk)

## WCG: do these have a natural order? If so, perhaps convert to an ordered factor
## and plot in the relevant order

displacement_risk %>%
  count(Typology) %>%
  ggplot(aes(x = reorder(Typology, -n), y = n)) +
  geom_col(mapping = aes(x=Typology, y = n), position = "dodge") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 12), breaks = c(0,2,4,6,8,10,12))+
  labs(y = "Number of census tracts") +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 9.5, family = "Lato"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 9.5, family = "Lato"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8.5, family = "Lato"),
    axis.text.y = element_text(size = 8.5, family = "Lato"),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  )
#saving
ggsave(file.path(file_path, "Pico/Outputs/displacement_risk_bar_pico.png"),  width = 8, height = 2.5) 

#now map out displacement risk using a heat map -ignore for now
plot <- ggplot() +
  geom_sf(displacement_risk, mapping = aes(fill = Typology, show.legend = TRUE)) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) +
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) +
  scale_fill_manual(
    values = c(
      "Advanced Gentrification" = palette_urbn_diverging[2],
      "At Risk of Gentrification" = palette_urbn_diverging[4],
      "Early/Ongoing Gentrification" = palette_urbn_diverging[3],
      "Low-Income/Susceptible to Displacement" = palette_urbn_diverging[5],
      "Ongoing Displacement" = palette_urbn_diverging[1],
      "Stable Moderate/Mixed Income" = palette_urbn_diverging[6]
    ),
    name = "Displacement Risk"
  ) +
  theme_urbn_map()+
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))


print(plot)

####----Transportation Access and Safety----####
##Car free and one car##                
#pull car data
vehicles <-   
  get_acs(
    geography = "tract",
    variables = c(
      total = "B25044_001",
      no_car_o = "B25044_003",
      one_car_o = "B25044_004",
      no_car_r = "B25044_010",
      one_car_r = "B25044_011"
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )%>%
    select(GEOID, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>% # pivoting
    mutate(
      no_car = (no_car_o + no_car_r),
      one_car = (one_car_o + one_car_r),
      share_no_car = (no_car/total),
      share_one_car = (one_car/total),
      multiple_car = (total - no_car - one_car)
    )%>%
    select(GEOID, total, no_car, one_car, share_no_car, share_one_car, multiple_car) %>% # reducing df to necessary variables
    filter(GEOID %in% pico_tracts$GEOID) # limiting to pico tracts

#adding geometry for map
st_geometry(vehicles) <- st_geometry(pico_tracts[match(vehicles$GEOID, pico_tracts$GEOID), ])
class(vehicles)

#creating custom bins for a map of share of occupied HU that are renter
map_no_car <- vehicles%>% 
  mutate(bin_no_car = cut(share_no_car, breaks=c(0, 0.1, 0.2,0.3,0.4,0.5,1),
                          labels  = c("Less than 10%", "10-20%", "20-30%", "30-40%", "40-50%", "50% or more"),
                          include.lowest = TRUE))
#making each bin a factor
map_no_car$bin_no_car <- factor(map_no_car$bin_no_car, 
                                levels =c("Less than 10%", "10-20%", "20-30%", "30-40%", "40-50%", "50% or more"))

#plot map
plot <-ggplot()+
  geom_sf(map_no_car, mapping = aes(fill = bin_no_car), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,2,4,6,7,8)], #can adjust the palette or color scheme as necessary
    name = "Share of households without car access",
    breaks = c("Less than 10%", "10-20%", "20-30%", "30-40%", "40-50%", "50% or more"),
  )+
  theme_urbn_map()+
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))


print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/share_no_car_map_pico.png"), width = 14, height = 6, dpi = 300)


## Method for commuting to work ##
#pull means of transportation data
transportation_means <-   
  get_acs(
    geography = "tract",
    variables = c(
      total = "B08301_001",
      car = "B08301_002",
      car_alone = "B08301_003",
      car_carpool = "B08301_004",
      total_public_transit = "B08301_010",
      public_transit_bus = "B08301_011",
      public_transit_subway = "B08301_012",
      public_transit_commuter_rail = "B08301_013",
      public_transit_street_car = "B08301_014",
      taxi = "B08301_016",
      motorcycle = "B08301_017",
      bike = "B08301_018",
      walk = "B08301_019",
      other = "B08301_020",
      wfh = "B08301_021"
    ),
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  )%>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>% # pivoting
  mutate(
    total_car = car + motorcycle
  )%>%
  filter(GEOID %in% pico_tracts$GEOID) # limiting to pico tracts

##WCG: an alternate approach to create percentages across many count variables
transportation_means %>%
  transmute(
    GEOID,
    total_commute = total - wfh,
    across(.cols = car:other, .fns = ~ .x / total_commute, .names = "share_{col}")) %>%
  select(-total_commute) %>%
  pivot_longer(-GEOID) %>%
  ggplot() +
  geom_point(aes(y = name, x = value))

#creating a new dataframe of totals across all tracts and compute shares of each mode of transit
transportation_means_sums <- colSums(transportation_means[, 2:17], na.rm = TRUE)
transportation_means_sums <- data.frame(t(transportation_means_sums))
#generating shares of transport types
for (i in 2:16) {
  col_name <- names(transportation_means_sums)[i]
  new_col_name <- paste0("share_", col_name)
  transportation_means_sums[[new_col_name]] <- transportation_means_sums[[i]] / transportation_means_sums[[1]]
}
#generate/combine some variables for ease of data visualization
transportation_means_sums <- transportation_means_sums%>%
  mutate(
    share_walk_or_bike = (share_walk + share_bike)
  )

## create a bar chart of shares of transportation by different types of means
# first pivot to long
transportation_long <- transportation_means_sums %>%
  select(30,31,20,32,29) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")%>%
  mutate(Value = ifelse(Variable == "share_other", 0.017, Value)) #the other category is computed as a residual (i.e. 1 minus selected categories)

# Define custom labels for each variable
custom_labels <- c(
  "share_walk_or_bike" = "Walk or Bike", 
  "share_total_car" = "Car",
  "share_total_public_transit" = "Public Transportation",  
  "share_other" = "Other",
  "share_wfh" = "Work From Home"
)
#custom order of bars
custom_order <- c("share_total_car", "share_total_public_transit", "share_walk_or_bike", "share_wfh", "share_other")
transportation_long$Variable <- factor(transportation_long$Variable, levels = custom_order)

# Produce bar chart with custom x-axis labels
plot <- ggplot(transportation_long) +
  geom_col(mapping = aes(x = Variable, y = Value), position = "dodge") +
  geom_text(mapping = aes(x = Variable, y = Value, label = scales::percent(Value, accuracy = 1)),
            vjust = -0.5, size = 3.5, family = "Lato") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    limits = c(0, 0.75),
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1)  # Percent labels on y-axis
  ) +
  scale_x_discrete(labels = custom_labels) +  # Use custom labels for the x-axis
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12, family = "Lato"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10, family = "Lato"),
    axis.text.y = element_text(size = 10, family = "Lato")
  )

print(plot) 

#saving
ggsave(file.path(file_path, "Pico/Outputs/means_of_transt_bar_pico.png"),  width = 8, height = 3)  


### pull travel time data
travel_time <-   
  get_acs(
    geography = "tract",
    table = "B08303",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE,
  ) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>% # pivoting
  rename(
    total = B08303_001,
    commute_5_less = B08303_002,
    commute_5_9 = B08303_003,
    commute_10_14 = B08303_004,
    commute_15_19 = B08303_005,
    commute_20_24 = B08303_006,
    commute_25_29 = B08303_007,
    commute_30_34 = B08303_008,
    commute_35_39 = B08303_009,
    commute_40_44 = B08303_010,
    commute_45_59 = B08303_011,
    commute_60_89 = B08303_012,
    commute_90_plus = B08303_013
  )%>%
  filter(GEOID %in% pico_tracts$GEOID)%>% # limiting to pico tracts
  mutate(
    commute_5_14 = commute_5_9 + commute_10_14,
    commute_15_24 = commute_15_19 + commute_20_24,
    commute_25_34 = commute_25_29 + commute_30_34,
    commute_35_44 = commute_35_39 + commute_40_44
  )%>%
  select(
    GEOID, total, commute_5_less, commute_5_14, commute_15_24, commute_25_34, commute_35_44, commute_45_59,
    commute_60_89, commute_90_plus
  )



#create a df of totals across all tracts and compute shares of each commute bin
travel_time_sums <- colSums(travel_time[, 1:9], na.rm = TRUE)
travel_time_sums <- data.frame(t(travel_time_sums))
#generating shares of transport types
for (i in 2:9) {
  col_name <- names(travel_time_sums)[i]
  new_col_name <- paste0("share_", col_name)
  travel_time_sums[[new_col_name]] <- travel_time_sums[[i]] / travel_time_sums[[1]]
}

#pivoting to long
travel_time_sums_long <- travel_time_sums %>%
  select(10:17) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

### producing a bar chart of commute times
#custom labels
custom_labels <- c(
  "share_commute_5_less" = "Less than 5",
  "share_commute_5_14" = "5 to 14",
  "share_commute_15_24" = "15 to 24",
  "share_commute_25_34" = "25 to 34",
  "share_commute_35_44" = "35 to 44",
  "share_commute_45_59" = "45 to 59",
  "share_commute_60_89" = "60 to 89",
  "share_commute_90_plus" = "90 or more"
)
#custom order of bars
custom_order <- c("share_commute_5_less", "share_commute_5_14", "share_commute_15_24",
                  "share_commute_25_34","share_commute_35_44","share_commute_45_59",
                  "share_commute_60_89","share_commute_90_plus")
travel_time_sums_long$Variable <- factor(travel_time_sums_long$Variable, levels = custom_order)

#bar chart
ggplot(travel_time_sums_long) +
  geom_col(mapping = aes(x = Variable, y = Value), position = "dodge") +
  geom_text(mapping = aes(x = Variable, y = Value, label = scales::percent(Value, accuracy = 1)),
            vjust = -0.5, size = 3.5, family = "Lato") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    limits = c(0, 0.40),
    breaks = seq(0, 1, by = 0.05),
    labels = scales::percent_format(accuracy = 1)  # Percent labels on y-axis
  ) +
  scale_x_discrete(labels = custom_labels) +  # using custom labels for the x axis
  labs(
    x = "Commute time (minutes)",
    y = "Share of residents in Pico census tracts"
  ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12, family = "Lato"),
    axis.text.x = element_text(size = 12, family = "Lato"),
    axis.text.y = element_text(size = 12, family = "Lato"),
    axis.title.x = element_text(size = 10, family = "Lato"),
    axis.title.y = element_text(size = 10, family = "Lato")
  )

#saving
ggsave(file.path(file_path, "Pico/Outputs/commute_time_bar_pico.png"),  width = 8, height = 3)    


### Also going to make a map of the share of residents with longer than an hour commute
travel_time <- travel_time%>%
  mutate(
    commute_hour_plus = commute_60_89 + commute_90_plus,
    share_commute_hour_plus = commute_hour_plus/total 
  )

#add geometry for map
st_geometry(travel_time) <- st_geometry(pico_tracts[match(travel_time$GEOID, pico_tracts$GEOID), ])
class(travel_time)

#custom bins for share of residents with an hour + commute
map_travel_time <- travel_time%>% 
  mutate(bin_hour_plus = cut(share_commute_hour_plus, breaks=c(0,0.05,0.1,0.15,0.2,0.25,1),
                                labels  = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20-25%", "25% or more"),
                                include.lowest = TRUE))
#making each bin a factor
map_travel_time$bin_hour_plus <- factor(map_travel_time$bin_hour_plus, 
                                            levels = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20-25%", "25% or more"))

#plot map
plot <-ggplot()+
  geom_sf(map_travel_time, mapping = aes(fill = bin_hour_plus), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,2,3,6, 7,8)], #can adjust the palette or color scheme as necessary
    name = "Share of individuals with 60+ minute commute",
    breaks = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20-25%", "25% or more")
  )+
  theme_urbn_map()+
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))


print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/commute_map_pico.png"), width = 14, height = 6, dpi = 300)

#load in data from TIMS
crashes_pico <- read.csv(file.path(file_path, "Pico/Outputs/crash_data_pico_blvd.csv"))
##Collisions - not sure about best data here
#convert to an SF
crashes_pico_sf <- st_as_sf(
  crashes_pico,
  coords = c("POINT_X", "POINT_Y"),
  crs = 4326,  # EPSG:4326 = WGS 84 (standard lat/lon)
  remove = FALSE  # keeps Point_X and Point_Y columns too
)



#all transit performance score - skipping for now

# Bike score
### not sure if this exists by tract, but overall the scores are as follows:
# bike score: 72
# walk score: 85
# transit score: 59

#create map with points overlaid
crash_map_pico <- ggplot() +
  geom_sf(data = pico_buffer_tracts, fill = "lightgray", color = "gray40", size = 0.3) +
  geom_sf(data = major_streets_clipped, color = "orange", size = 0.6) +
  geom_sf(data = crashes_pico_sf, color = "red", size = 1, alpha = 0.7) +
  # labs(title = "Crash Locations in Pico Area",
  #      subtitle = "Overlaid on Buffer Tracts and Major Streets",
  #      caption = "Source: TIMS") +
  theme_urbn_map()+
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12)
  )

print(crash_map_pico) #view map

#save
ggsave(file.path(file_path, "Pico/Outputs/crash_map_pico.png"), width = 14, height = 6, dpi = 300)

####----H+T Index----####

#pull H+T data
ht_index <- read.csv(file.path(file_path, "Data/htaindex2022_data_tracts_06.csv"))%>%
  rename(GEOID = tract)%>%
  mutate(GEOID = gsub('"', '', GEOID))%>%
  mutate(GEOID = str_pad(GEOID, width = 11, pad = "0"))%>% #need to add a leading zero to match GEOID structure
  filter(GEOID %in% pico_tracts$GEOID) # limiting to pico tracts

#add geometry for map
st_geometry(ht_index) <- st_geometry(pico_tracts[match(ht_index$GEOID, pico_tracts$GEOID), ])
class(ht_index)


### housing and transportation costs as a percentage of income - map ###

#H+T index for AMI
#custom bins
map_ht_percent_income <- ht_index%>% 
  mutate(bin_ht_ami = cut(ht_ami, breaks=c(0,30,40,50,100),
                             labels  = c("Less than 30%", "30-40%", "40-50%", "50% or more"),
                             include.lowest = TRUE))
#making each bin a factor
map_ht_percent_income$bin_ht_ami <- factor(map_ht_percent_income$bin_ht_ami, 
                                        levels = c("Less than 30%", "30-40%", "40-50%", "50% or more"))

#plot map
plot <-ggplot()+
  geom_sf(map_ht_percent_income, mapping = aes(fill = bin_ht_ami), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_cyan[c(2,4,6,8)], #can adjust the palette or color scheme as necessary
    name = "Share of income spent on H & T costs - area median income",
    breaks = c("Less than 30%", "30-40%", "40-50%", "50% or more")
  )+
  theme_urbn_map()+
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))


print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/ht_ami_map_pico.png"), width = 14, height = 6, dpi = 300)


#Repeating using H+T index for 80% AMI
map_ht_80percent_income <- ht_index%>% 
  mutate(bin_ht_80ami = cut(ht_80ami, breaks=c(0,30,40,50,60,100),
                          labels  = c("Less than 30%", "30-40%", "40-50%", "50-60%", "60% or more"),
                          include.lowest = TRUE))
#making each bin a factor
map_ht_80percent_income$bin_ht_80ami <- factor(map_ht_80percent_income$bin_ht_80ami, 
                                           levels = c("Less than 30%", "30-40%", "40-50%", "50-60%", "60% or more"))

#plot map
plot <-ggplot()+
  geom_sf(map_ht_80percent_income, mapping = aes(fill = bin_ht_80ami), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_manual(
    values = palette_urbn_cyan[c(2,3,5,7,8)], #can adjust the palette or color scheme as necessary
    name = "Share of income spent on H & T costs - 80% area median income",
    breaks = c("Less than 30%", "30-40%", "40-50%", "50-60%", "60% or more")
  )+
  theme_urbn_map()+
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))

ggsave(file.path(file_path, "Pico/Outputs/ht_80ami_map_pico.png"), width = 14, height = 6, dpi = 300)

####----Measure Voting Data---####
#load the data
hla_vote <- st_read(file.path(file_path, "Data/Measure HLA Vote/HLA.shp"))

#set tracts crs to match that of voting data
pico_buffer_tracts_1 <- st_transform(pico_buffer_tracts, crs = 4326)
hla_vote <- st_transform(hla_vote, crs = 4326)


#find intersection
voting_intersection <- st_intersection(pico_buffer_tracts_1, hla_vote)

install.packages("lwgeom")
library(lwgeom)

voting_map_pico <- ggplot() +
  geom_sf(data = pico_buffer_tracts_1) +
  geom_sf(data = voting_intersection, aes(fill = V3_LOS_A_5), color = NA) +
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  scale_fill_gradientn(
    colors = palette_urbn_cyan[c(2, 4, 6, 8)], 
    name = "Share of Votes in Favor",
    labels = scales::percent_format()
  ) +
  theme_urbn_map()+
  labs(title = NULL)+
  theme (legend.title = element_text(size = 16),
         legend.text = element_text(size = 16), 
         legend.key.height = unit(1.2, "cm"),
         legend.key.width = unit(0.6, "cm"))

ggsave(file.path(file_path, "Pico/Outputs/voting_map_pico.png"), width = 14, height = 6, dpi = 300)




       