#Install all the below packages if not already installed, using install.packages("package")

#used for pulling census data
library(tidycensus)
#contains analysis packages
library(dplyr)
#contains analysis packages
library(tidyr)
#used for loading/saving excel files
library(readxl)
#used for loading/saving excel files
library(openxlsx)
#contains analysis packages
library(tidyverse)
#used for maps and charts
library(ggplot2)
#used for maps
library(geofacet)
#used for shapefile functions
library(sf)
#used for street lines
library(osmdata)
#used to pull urban theme template
library(urbnthemes)
options(scipen=999)
set_urbn_defaults(style="print")


####Load and Clean####
#set personal file path to make it easier to pull data

#Gabe
#file_path <- file.path("C:/Users/GSamuels/Box/LA Transit project/Social Climate Analysis")

#Teddy
file_path <- file.path("C:/Users/TMaginn/Box/LA Transit project/Social Climate Analysis")

###load in shapefiles###

##census tracts##
CA_tracts <- st_read(file.path(file_path, "Hoover/Mapping/CA_census_Tracts.shp"))
la_tracts <- CA_tracts %>% filter(COUNTYFP == "037")
la_shp <- st_read(file.path(file_path, "Data/tl_2023_06_tract/tl_2023_06_tract.shp"))

#pico tracts
pico_buffer_tracts <- st_read(file.path(file_path, "Pico/Maps/pico_buffer_tracts.shp"))
pico_tracts <- la_shp%>%
  filter(GEOID %in% pico_buffer_tracts$GEOID)

#streets
bbox <- st_bbox(pico_tracts)
streets <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway") %>%  # 'highway' includes roads, streets, etc.
  osmdata_sf()
#extract just streets
streets_lines <- streets$osm_lines
#filter to main streets
major_streets <- streets_lines[streets_lines$highway %in% c("primary", "secondary", "tertiary"), ]
major_streets <- st_transform(major_streets, st_crs(pico_tracts)) #transform CRS
major_streets_clipped <- st_intersection(major_streets, st_union(st_geometry(pico_tracts))) #restricting streets to census tract boundaries

#load in census api key
#note - you will need to get a census api key to access this#
Sys.getenv("CENSUS_API_KEY")

####Data Analysis####
#set fips codes
state_fips <- "06"
county_fips <- "037"

###load list of 2023 acs variables
v23 <- load_variables(2023, "acs5", cache = TRUE)

########################Demographic Profile#########################

##Population##
#pull population table from ACS
sex_by_age <- get_acs(
  geography = "tract",
  table = "B01001",
  state = state_fips,
  county = county_fips,
  year = 2023,
  survey = "acs5",
  geometry = FALSE, #change to TRUE for maps
  cache_table = TRUE
)


##Age##
#see sex_by_age

##Race and Ethnicity##
#pull race  from ACS
race <- get_acs(
  geography = "tract",
  table = "B02001",
  state = state_fips,
  county = county_fips,
  year = 2023,
  survey = "acs5",
  geometry = FALSE #change to TRUE for maps
)

#pull ethnicity  from ACS
ethnicity <- 
  get_acs(
    geography = "tract",
    table = "B03002",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE #change to TRUE for maps
  )

##Median Income/Poverty##
#pull income from ACS
median_income <- 
  get_acs(
    geography = "tract",
    table = "B19013",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE #change to TRUE for maps
  )

#pull poverty level from ACS
poverty_level <-
  get_acs(
    geography = "tract",
    table = "B17001",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE, #change to TRUE for maps
    )
  
##Employment Status##
#pull employment status from acs
employment_status <-
  get_acs(
    geography = "tract",
    table = "B23025",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE, #change to TRUE for maps
  )

##Gender##
#see sex_by_age


##Family structure##
#pull family structure
family_structure <-
  get_acs(
    geography = "tract",
    table = "B11003",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE, #change to TRUE for maps
  )

##################Language Access and Cultural Considerations#######################

##Limited English##
#pull from acs
language <-
  get_acs(
    geography = "tract",
    table = "B16004",
    state = state_fips,
    county = county_fips,
    year = 2023,
    survey = "acs5",
    geometry = FALSE, ##change to TRUE for map of other languages spoken
  )

##Other languages spoken in the area##
#see language

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
    geometry = FALSE,
  )

########################Other Accessibility Considerations########################

##Disability Status##
#pull disability status 
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
    geometry = FALSE
  ) %>%
  select(GEOID, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)%>% #pivoting so each tract is one row and each variable is one column
  mutate(
    total_disability =v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12, #summing diability by sex by age to get total population with disability by tract
    share_disability = total_disability/total_pop)%>% #generating share of tract population with disability
  select(GEOID,total_pop,total_disability,share_disability) #reducing df to necessary variables

#binding with pico tracts to limit our data to the study area
pico_disability <- disability %>%
  filter(GEOID %in% pico_tracts$GEOID)

#add geometry from census tracts to create a map
st_geometry(pico_disability) <- st_geometry(pico_tracts[match(pico_disability$GEOID, pico_tracts$GEOID), ])
class(pico_disability)


#creating custom bins for a map of %pop w/ disability
map_disability_pico <- pico_disability%>% 
  mutate(bin_disability = cut(total_disability, breaks=c(0,200, 300, 400, 500,800),
                              labels  = c("Less than 200", "200-300", "300-400", "400-500", "500 or more"),
                              include.lowest = TRUE))
#next, make each of the bins a factor
map_disability_pico$bin_disability <- factor(map_disability_pico$bin_disability, 
                                       levels = c("Less than 200", "200-300", "300-400", "400-500", "500 or more"))

#making the map
plot <-ggplot()+
  geom_sf(map_disability_pico, mapping = aes(fill = bin_disability), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,5,6,8)], #can adjust the palette or color scheme as necessary
    name = "Number of individuals with a disability",
    breaks = c("Less than 200", "200-300", "300-400", "400-500", "500 or more")
  )+
  theme_urbn_map()

print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/disability_map_pico.png"), width = 14, height = 6, dpi = 300)


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

#plot map
plot <-ggplot()+
  geom_sf(map_hearing_diff, mapping = aes(fill = bin_hearing_diff), show.legend = TRUE) +
  geom_sf(data = pico_buffer_tracts, color = "yellow", alpha = 0, lwd = 1) + # Adding the buffer zone as a transparent overlay
  geom_sf(data = major_streets_clipped, color = "gray20", size = 0.5) + #adding streets to map
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,6,8)], #can adjust the palette or color scheme as necessary
    name = "Number of individuals with hearing difficulty",
    breaks = c("Less than 50", "50-100", "100-150", "150 or more")
  )+
  theme_urbn_map()

print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/hearing_diff_map_pico.png"), width = 14, height = 6, dpi = 300)


####pull vision difficulty status####
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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,6,8)], #can adjust the palette or color scheme as necessary
    name = "Number of individuals with a vision difficulty",
    breaks = c("Less than 50", "50-100", "100-150", "150 or more")
  )+
  theme_urbn_map()

print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/vision_diff_map_pico.png"), width = 14, height = 6, dpi = 300)

## ambulatory difficulty ##
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

#write.csv(ambulatory_diff, file.path(file_path, "Fileshare", "ambulatory_diff.csv"), row.names = FALSE) ###ignore this line

#add geometry for map
st_geometry(ambulatory_diff) <- st_geometry(pico_tracts[match(ambulatory_diff$GEOID, pico_tracts$GEOID), ])
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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,5,7,8)], #can adjust the palette or color scheme as necessary
    name = "Number of individuals with ambulatory difficulty",
    breaks = c("Fewer than 50", "50-100", "100-200", "200-300", "300 or more")
  )+
  theme_urbn_map()

print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/ambulatory_diff_map_pico.png"), width = 14, height = 6, dpi = 300)



#### Irregular work hours #### - skipping for now
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

#### Internet/Computer access ####
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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,2,4,6,8)], #can adjust the palette or color scheme as necessary
    name = "Share of households without internet access",
    breaks = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20% or more")
  )+
  theme_urbn_map()

print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/internet_access_map_pico.png"), width = 14, height = 6, dpi = 300)






### pull computer access data
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
    geometry = FALSE, #change back to TRUE to make a map
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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,3,6,8)], #can adjust the palette or color scheme as necessary
    name = "Share of households without access to a device",
    breaks = c("Less than 1%", "1-5%", "5-10%", "10% or higher")
  )+
  theme_urbn_map()

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
ggplot(tech_sums_long) +
  geom_col(mapping = aes(x=device_type, y = share), position = "dodge") +
  geom_text(aes(x = device_type, y = share, label = scales::percent(share, accuracy = 0.01)), 
            vjust = -0.5, size = 3, family = "Lato") +  # Add labels above the bars
  labs(
    x = "Device Access Type",
    y = "Share of Total Population") +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 1), breaks = c(.25, .5, .75)) +
  scale_x_discrete(labels = c(
    "share_computer" = "Access to laptop or desktop computer", 
    "share_no_computer" = "No access to any device", 
    "share_smartphone" = "Access to smartphone",
    "share_tablet" = "Access to tablet or other mobile device"
  )) +
  theme(
    legend.position="top",
    legend.text = element_text(size=9.5, family="Lato"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size=8.5, family="Lato"),
    axis.text.y = element_text(size=8.5, family="Lato")
  )

#saving
ggsave(file.path(file_path, "Pico/Outputs/tech_access_bar_pico.png"),  width = 8, height = 2.5)                       
  
  



##################### Housing & Displacement #############################

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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_quintile[], #can adjust the palette or color scheme as necessary
    name = "Share of occupied housing units that are rented",
    breaks = c("50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
  )+
  theme_urbn_map()

print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/share_renter_occupied_map_pico.png"), width = 14, height = 6, dpi = 300)



##Housing Cost Burden##
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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_diverging[c(5,4,3,2,1)], #can adjust the palette or color scheme as necessary
    name = "Median share of income spent on housing",
    breaks = c("less than 30%", "30-35%", "35-40%", "40-45%", "45-50%")
  )+
  theme_urbn_map()

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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_red[c(1,2,4,6,8)], #can adjust the palette or color scheme as necessary
    name = "Share of renters spending more than 30% of income on housing",
    breaks = c("less than 40%", "40-50%", "50-60%", "60-70%", "70-80%")
  )+
  theme_urbn_map()

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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_red[c(1,2,4,6,7,8)], #can adjust the palette or color scheme as necessary
    name = "Share of renters spending more than 50% of income on housing",
    breaks = c("less than 25%", "25-30%", "30-35%", "35-40%", "40% or higher")
  )+
  theme_urbn_map()

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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
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
  theme_urbn_map()

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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = c(
      "less than 20%" = palette_urbn_red[1],
      "20-40%" = palette_urbn_red[2],
      "40-60%" = palette_urbn_red[5],
      "60-80%" = palette_urbn_red[7],
      "80-100%" = palette_urbn_red[8],
      "No data" = "gray80"
    ),
    name = "Share of households spending more than 30% of income on housing - owner-occupied units"
  ) +
  guides(fill = guide_legend(override.aes = list(color = NA))) +
  theme_urbn_map()

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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = c(
      "less than 20%" = palette_urbn_red[1],
      "20-40%" = palette_urbn_red[2],
      "40-60%" = palette_urbn_red[5],
      "60-80%" = palette_urbn_red[7],
      "80-100%" = palette_urbn_red[8],
      "No data" = "gray80"
    ),
    name = "Share of households spending more than 50% of income on housing - owner-occupied units"
  ) +
  guides(fill = guide_legend(override.aes = list(color = NA))) +
  theme_urbn_map()

print(plot)

ggsave(file.path(file_path, "Pico/Outputs/owner_share_severe_rent_burden_map_pico.png"), width = 14, height = 6, dpi = 300)



####Disadvantaged communities ####

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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
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
  theme_urbn_map()

print(plot)


###Transportation and Commuting###

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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,2,4,6,7,8)], #can adjust the palette or color scheme as necessary
    name = "Share of households without car access",
    breaks = c("Less than 10%", "10-20%", "20-30%", "30-40%", "40-50%", "50% or more"),
  )+
  theme_urbn_map()

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
    total_car = car+ motorcycle
  )%>%
  filter(GEOID %in% pico_tracts$GEOID) # limiting to pico tracts

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
ggplot(transportation_long) +
  geom_col(mapping = aes(x = Variable, y = Value), position = "dodge") +
  geom_text(mapping = aes(x = Variable, y = Value, label = scales::percent(Value, accuracy = 0.1)),
            vjust = -0.5, size = 3.5, family = "Lato") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1)  # Percent labels on y-axis
  ) +
  scale_x_discrete(labels = custom_labels) +  # Use custom labels for the x-axis
  theme(
    legend.position = "top",
    legend.text = element_text(size = 9.5, family = "Lato"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8.5, family = "Lato"),
    axis.text.y = element_text(size = 8.5, family = "Lato")
  )
#saving
ggsave(file.path(file_path, "Pico/Outputs/means_of_transt_bar_pico.png"),  width = 8, height = 2.5)       


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
  )%>%
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
  filter(GEOID %in% pico_tracts$GEOID) # limiting to pico tracts



#create a df of totals across all tracts and compute shares of each commute bin
travel_time_sums <- colSums(travel_time[, 2:14], na.rm = TRUE)
travel_time_sums <- data.frame(t(travel_time_sums))
#generating shares of transport types
for (i in 2:14) {
  col_name <- names(travel_time_sums)[i]
  new_col_name <- paste0("share_", col_name)
  travel_time_sums[[new_col_name]] <- travel_time_sums[[i]] / travel_time_sums[[1]]
}
#pivoting to long
travel_time_sums_long <- travel_time_sums %>%
  select(14:25) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

### producing a bar chart of commute times
#custom labels
custom_labels <- c(
  "share_commute_5_less" = "Less than 5",
  "share_commute_5_9" = "5 to 9",
  "share_commute_10_14" = "10 to 14",
  "share_commute_15_19" = "15 to 19",
  "share_commute_20_24" = "20 to 24",
  "share_commute_25_29" = "25 to 29",
  "share_commute_30_34" = "30 to 34",
  "share_commute_35_39" = "35 to 39",
  "share_commute_40_44" = "40 to 44",
  "share_commute_45_59" = "45 to 59",
  "share_commute_60_89" = "60 to 89",
  "share_commute_90_plus" = "90 or more"
)
#custom order of bars
custom_order <- c("share_commute_5_less", "share_commute_5_9", "share_commute_10_14", "share_commute_15_19",  "share_commute_20_24",
                  "share_commute_25_29","share_commute_30_34","share_commute_35_39","share_commute_40_44","share_commute_45_59",
                  "share_commute_60_89","share_commute_90_plus")
travel_time_sums_long$Variable <- factor(travel_time_sums_long$Variable, levels = custom_order)

#bar chart
ggplot(travel_time_sums_long) +
  geom_col(mapping = aes(x = Variable, y = Value), position = "dodge") +
  geom_text(mapping = aes(x = Variable, y = Value, label = scales::percent(Value, accuracy = 0.1)),
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
    legend.text = element_text(size = 9.5, family = "Lato"),
    axis.text.x = element_text(size = 8.5, family = "Lato"),
    axis.text.y = element_text(size = 8.5, family = "Lato"),
    axis.title.x = element_text(size = 10, family = "Lato"),
    axis.title.y = element_text(size = 10, family = "Lato")
  )

#saving
ggsave(file.path(file_path, "Pico/Outputs/commute_time_bar_pico.png"),  width = 8, height = 2.5)    


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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(1,2,3,6, 7,8)], #can adjust the palette or color scheme as necessary
    name = "Share of individuals with a commute of an hour or longer",
    breaks = c("Less than 5%", "5-10%", "10-15%", "15-20%", "20-25%", "25% or more")
  )+
  theme_urbn_map()

print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/commute_map_pico.png"), width = 14, height = 6, dpi = 300)




######## H+T Index metrics #######
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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(2,4,6,8)], #can adjust the palette or color scheme as necessary
    name = "Share of income spent on housing and transportation costs for area median income",
    breaks = c("Less than 30%", "30-40%", "40-50%", "50% or more")
  )+
  theme_urbn_map()

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
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16, hjust = .5)) +
  scale_fill_manual(
    values = palette_urbn_cyan[c(2,3,5,7,8)], #can adjust the palette or color scheme as necessary
    name = "Share of income spent on housing and transportation costs for 80% area median income",
    breaks = c("Less than 30%", "30-40%", "40-50%", "50-60%", "60% or more")
  )+
  theme_urbn_map()

print(plot) #view map

ggsave(file.path(file_path, "Pico/Outputs/ht_80ami_map_pico.png"), width = 14, height = 6, dpi = 300)



#all transit performance score - skipping for now

############# Bike score ##########
### not sure if this exists by tract, but overall the scores are as follows:
# bike score: 72
#walk score: 85
# transit score: 59

##Collisions - not sure about best data here


