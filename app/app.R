# R packages to be loaded
# packages = c('sf','spdep','rgdal','tmap','tidyverse', 'shiny', 'rsconnect',
#              'shinycssloaders', 'shinythemes', 'shinyjs', 'DT', 'factoextra',
#              'cluster', 'corrplot', 'ggpubr', 'heatmaply', 'markdown', 
#              'leaflet', 'plotly', 'ClustGeo', 'NbClust', 'geodist')

# for (p in packages){
#     if(!require(p, character.only = T)){
#         install.packages(p)
#     }
#     library(p, character.only = T)
# }


library(sf)
library(spdep)
library(rgdal)
library(tmap)
library(tidyverse)
library(shiny)
library(rsconnect)
library(shinycssloaders)
library(shinythemes)
library(shinyjs)
library(DT)
library(factoextra)
library(cluster)
library(corrplot)
library(ggpubr)
library(heatmaply)
library(markdown)
library(leaflet)
library(plotly)
library(ClustGeo)
library(NbClust)
library(geodist)



# Import Data
# Geospatial data (shapefile)
area <- st_read(dsn = "data/geospatial/StudyArea.shp", layer = "StudyArea")
area <- area %>%
    rename(
        PROVINCE_ID = PROVNO,
        CITY_ID = KABKOTNO,
        DISTRICT_ID = KECNO,
        VILLAGE_ID = DESANO,
        PROVINCE = PROVINSI,
        CITY = KABKOT,
        DISTRICT =KECAMATAN,
        VILLAGE = DESA,
        REGION_CODE = IDDESA,
        YEAR = TAHUN,
        SOURCE = SUMBER,
        GEOMETRY = geometry
    )


# Rda file
load(file = "data/geospatial/city_border.Rda")
load(file = "data/geospatial/district_border.Rda")

# Aspatial data (CSV file)
podes_final <- read_csv("data/aspatial/podes_final.csv") #, sep = ",")
podes_final$PROVINCE_ID <- as.character(podes_final$PROVINCE_ID)
podes_final$CITY_ID <- as.character(podes_final$CITY_ID)
podes_final$DISTRICT_ID <- as.character(podes_final$DISTRICT_ID)
podes_final$VILLAGE_ID <- as.character(podes_final$VILLAGE_ID)
#print(ncol(podes_final))
#print(nrow(podes_final))

# Join spatial & aspatial data
eastkalimantan_podes <- dplyr::inner_join(area, podes_final, by = c("PROVINCE_ID", "CITY_ID", "DISTRICT_ID", "VILLAGE_ID", "PROVINCE", "CITY", "DISTRICT", "VILLAGE")) 
#print(head(eastkalimantan_podes))
#print(nrow(eastkalimantan_podes))

st_crs(eastkalimantan_podes) <- 4326
st_crs(city_border) <- 4326
st_crs(district_border) <- 4326


# EDA Categorical Variables
cat_var <- c(# Governance
    "Presence of village council" = "PRESENCE_VILLAGE_COUNCIL",
    "Presence of village head" = "PRESENCE_VILLAGE_HEAD",
    "Presence of village head office" = "PRESENCE_VILLAGE_HEAD_OFFICE",
    #"Status of village head office" = "STATUS_HEAD_OFFICE",
    "Location of village head office" = "LOCATION_HEAD_OFFICE",
    "Presence of village secretary" = "PRESENCE_VILLAGE_SECRETARY",
    "Computer usage in village head office" = "COMPUTER_IN_VILLAGE_HEADOFFICE",
    "Internet usage in village head office" = "INTERNET_FACILITIES_IN_VILLAGE_HEADOFFICE",
    "Presence of Village Information System" = "VILLAGE_INFORMATION_SYSTEM",
    "Presence of Village Guides" = "PRESENCE_VILLAGE_GUIDES",
    
    # Geography
    "Topography" = "VILLAGE_BY_TOPOGRAPHY",
    "Village adjacent to sea" = "VILLAGE_ADJACENT_TO_SEA",
    "Location to forest" = "VILLAGE_LOCATION_TO_FOREST",
    "Forest function" = "FOREST_FUNCTION",
    "Dependency on forest" = "DEPENDENCY_ON_FOREST",
    "Presence of mangrove trees" = "PRESENCE_MANGROVE_TREES",
    "Main income" = "VILLAGE_MAIN_INCOME",
    "Commodities present" = "TYPE_OF_COMMODITIES",
    
    # Basic need & utilities
    "Electricity Provider" = "ELECTRICITY_PROVIDER",
    "Presence of street Lighting" = "STREET_LIGHTING",
    "Fuel used" = "FUEL_USED_OFTEN",
    "Trash disposal/dump" = "TRASH_DUMP_USED_OFTEN",
    "Toilet facilities" = "TOILET_FACILITIES",
    "Toilet disposal site" = "TOILET_DISPOSAL_SITE",
    "Bath water source" = "SOURCE_BATH_WATER",
    "Drinking water source" = "SOURCE_DRINKING_WATER",
    "Presence of power lines" = "PRESENCE_POWER_LINES",
    "Presence of river" = "PRESENCE_RIVER",
    "Presence of irrigation channel" = "PRESENCE_IRRIGATIONCHANNEL",
    "Presence of reservoir" = "PRESENCE_RESERVOIR",
    "Presence of watersprings" = "PRESENCE_WATERSPRINGS",
    
    # Social & sports & recreation
    "Presence of Cultural Reserve sites" = "CULTURAL_RESERVE_SITES",
    "Presence of Communal spaces" = "COMMUNAL_SPACES",
    "Presence of Communal activities" = "COMMUNAL_ACTIVITIES",
    "Presence of Soccer facilities" = "HAS_SOCCER_FACILITIES",
    "Presence of Futsal facilities" = "HAS_FUTSAL_FACILITIES",
    "Presence of Volleyball facilities" = "HAS_VOLLEYBALL_FACILITIES",
    "Presence of Badminton facilities" = "HAS_BADMINTON_FACILITIES",
    "Presence of Basketball facilities" = "HAS_BASKETBALL_FACILITIES",
    "Presence of Tennis facilities" = "HAS_TENNIS_FACILITIES",
    "Presence of Table tennis facilities" = "HAS_TABLETENNIS_FACILITIES",
    "Presence of Swimming facilities" = "HAS_SWIMMING_FACILITIES",
    "Presence of Martial Arts facilities" = "HAS_MARTIALARTS_FACILITIES",
    "Presence of Billiard facilities" = "HAS_BILLARD_FACILITIES",
    "Presence of Fitness facilities" = "HAS_FITNESS_FACILITIES",
    "Presence of Pubs" = "PUBS",
    "Presence of ATM" = "ATM",
    "Presence of Car shop" = "CAR_SHOP",
    "Presence of Pawn shop" = "PAWNSHOPS",
    "Presence of Salon" = "BEAUTY_SALON",
    "Presence of Travel agency" = "TRAVEL_AGENCY",
    
    # Transportation
    "Transportation methods available" = "TRANSPORTATION_METHODS",
    "Presence of Public transport & route" = "PUBLIC_TRANSPORT",
    "Daily operation of Public transport" = "PUBLIC_TRANSPORT_DAILY_OPERATION",
    "Operation time of Public transport" = "PUBLIC_TRANSPORT_OPERATION_TIME",
    
    # Communication
    "Population with cellphones" = "CITIZENS_OWN_CELLPHONES",
    "Strength of cellphone signal" = "STRENGTH_OF_CELLPHONE_SIGNALS",
    "Presence of Internet cafes" = "INTERNET_CAFES",
    "Type of Internet signal" = "TYPE_OF_INTERNET_SIGNAL",
    "Presence of Post office" = "POST_OFFICE",
    "Presence of Mobile post office" = "MOBILE_POSTOFFICE",
    "Presence of private delivery courier" = "PRIVATE_DELIVERY_COURIER",
    "Receives National TV service" = "RECEIVE_NATIONAL_TV",
    "Receives Regional TV service" = "RECEIVE_REGIONAL_TV",
    "Receives Private TV service" = "RECEIVE_PRIVATE_TV",
    "Receives Overseas TV service" = "RECEIVE_OVERSEAS_TV",
    "Receives National Radio service" = "RECEIVE_NATIONAL_RADIO",
    "Receives Regional Radio service" = "RECEIVE_REGIONAL_RADIO",
    "Receives Private Radio service" = "RECEIVE_PRIVATE_RADIO",
    
    # Economic Development/Industry
    "Presence of Transportation Infrastructure development" = "TRANSPORTATION_INFRASTRUCTURE_DEVELOPMENT",
    "Source of fund for transportation development" = "SOURCE_FUND_TRANSPORT",
    "Presence of Energy Infrastructure development" = "ENERGY_INFRASTRUCTURE",
    "Source of fund for energy development" = "SOURCE_FUND_ENERGY",
    "Presence of Info Comm Infrastructure development" = "INFO_COMM_INFRASTRUCTURE",
    "Source of fund for info comm development" = "SOURCE_FUND_INFO_COMM",
    "Presence of Sanitation Infrastructure development" = "SANITATION_INFRASTRUCTURE",
    "Source of fund for sanitation development" = "SOURCE_FUND_SANITATION",
    "Presence of Education, Culture & Health Infrastructure development" = "EDUCATION_CULTURE_HEALTH_INFRASTRUCTURE",
    "Source of fund for education, culture & health development" = "SOURCE_FUND_EDUCATIONCULTURE",
    "Presence of Trading Infrastructure development" = "TRADING_SERVICES_INFRASTRUCTURE",
    "Source of fund for trading services development" = "SOURCE_FUND_TRADING",
    "Presence of Farming Infrastructure development" = "FARMING_PRODUCTION_SMALL_INDUSTRY_INFRASTRUCTURE",
    "Source of fund for farming development" = "SOURCE_FUND_FARMING_SMALL_INDUSTRY",
    "Presence of Recreation & Tourism Infrastructure development" = "RECREATION_TOURISM_INFRASTRUCTURE",
    "Source of fund for recreation & tourism development" = "SOURCE_FUND_RECREATION",
    "Presence of Disaster Mitigation & Environmental Conservation Infrastructure development" = "DISASTER_MITIGATION_ENV_CONSERVATION_INFRASTRUCTURE",
    "Source of fund for disaster mitigation & environmental conservation development" = "SOURCE_FUND_DISASTER_MITIGATION",
    
    # Pollution & natural disasters
    "Water pollution source" = "SOURCE_WATER_POLLUTION",
    "Land pollution source" = "SOURCE_LAND_POLLUTION",
    "Air pollution source" = "SOURCE_AIR_POLLUTION",
    "Environmental conservation: Planting activity" = "PLANTING_ENVIRONMENTAL_CONSERVATION",
    "Environmental conservation: Recycling activity" = "RECYCLE_ENVIRONMENTAL_CONSERVATION",
    "Slash & Burn tendency" = "SLASH_AND_BURN",
    "Presence of natural disaster early warning system" = "WARNING_SYSTEM_NATURAL_DISASTER",
    "Presence of tsunami early warning system" = "WARNING_SYSTEM_TSUNAMI",
    
    # Safety & security
    "Presence of Theft" = "THEFT",
    "Trend of Theft" = "TREND_THEFT",
    "Presence of Robbery" = "ROBBERY",
    "Trend of Robbery" = "TREND_ROBBERY",
    "Presence of Fraud/Embezzlement" = "FRAUD_EMBEZZLEMENT",
    "Trend of Fraud/Embezzlement" = "TREND_FRAUD_EMBEZZLEMENT",
    "Presence of Persecution" = "PERSECUTION",
    "Trend of Persecution" = "TREND_PERSECUTION",
    "Presence of Arson" = "ARSON",
    "Trend of Arson" = "TREND_ARSON",
    "Presence of Rape crime" = "RAPE_CRIME",
    "Trend of Rape crime" = "TREND_RAPE_CRIME",
    "Presence of Drug abuse trafficking" = "DRUG_ABUSE_TRAFFICKING",
    "Trend of Drug abuse trafficking" = "TREND_DRUG_ABUSE_TRAFFICKING",
    "Presence of Gambling" = "GAMBLING",
    "Trend of Gambling" = "TREND_GAMBLING",
    "Presence of Murder" = "MURDER",
    "Trend of Murder" = "TREND_MURDER",
    "Presence of Human trafficking" = "HUMAN_TRAFFICKING",
    "Trend of Human trafficking" = "TREND_HUMAN_TRAFFICKING",
    "Presence of Corruption" = "CORRUPTION",
    "Trend of Corruption" = "TREND_CORRUPTION",
    "Most frequent crime" = "MOST_FREQUENT_CRIME",
    "Building security posts" = "BUILDING_MAINTENANCE_SECURITY_POST",
    "Establishing security guards" = "ESTABLISHING_SECURITY_GUARD",
    "Raising civil defense initiatives" = "RAISING_CIVIL_DEFENSE",
    "Reporting guest stay (more than 24 hours)" = "REPORTING_GUEST_STAY_MORE_THAN_24_HOURS",
    "Activation of citizen security system" = "ACTIVATION_OF_CITIZEN_SECURITY_SYSTEM",
    "Presence of Police Station" = "PRESENCE_POLICE_STATION_OFFICE"
)


# EDA Continuous variables
con_var <- c(# Governance
    "No. of Local units (smaller than village)" = "LOCAL_UNITS_NUM", # no NA values
    "No. of Council members" = "COUNCIL_MEMBER_NUM", # NA values, change to 0 numeric
    "Council Activity in 2017" = "COUNCIL_ACTIVITY_2017", # NA values, change to 0 numeric
    "No. of Village officials" = "VILLAGE_OFFICIALS", # no NA values
    "No. of Village Regulations in 2017" = "VILLAGE_REGULATION_2017", # NA values, change to 0 numeric
    "No. of Civil Defence officer" = "NUM_CIVIL_DEFENSE_OFFICER", # no NA values
    
    
    # Basic needs & utilities
    "Households with public electricity provider" = "HOUSEHOLDS_PLN_ELECTRICITY", # no NA values
    "Households with private electricity provider" = "HOUSEHOLDS_NONPLN_ELECTRICITY", # no NA values
    "Households with no electricity" = "HOUSEHOLDS_NO_ELECTRICITY", # no NA values
    "Households with landlines" = "NUM_FAMILIES_WITH_LANDLINES", # no NA values
    "No. of Government Banks" = "NUM_GOVT_BANKS", # no NA values
    "Distance to nearest govt bank" = "DISTANCE_TO_NEAREST_GOVT_BANKS", # NA values, change to 0 numeric
    "No. of Commercial Banks" = "NUM_COMMERCIAL_BANKS", # no NA values
    "No. of Government Banks" = "NUM_GOVT_BANKS", # no NA values
    "Distance to nearest commercial bank" = "DISTANCE_TO_NEAREST_COMMERCIAL_BANKS", # NA values, change to 0 numeric
    "No. of Rural Credit Banks" = "NUM_RURAL_CREDIT_BANKS", # no NA values
    "Distance to nearest rural credit bank" = "DISTANCE_TO_NEAREST_RURAL_BANKS", # NA values, change to 0 numeric
    
    # Poverty
    "No. of Slums" = "NUM_OF_SLUMS", # NA values, change to 0 numeric
    "Population in poverty" = "STATEMENT_OF_POVERTY", # no NA values
    
    # Education
    "Early childhood centre (public)" = "EARLYCHILDHOOD_PUBLIC_INSTITUTION_NUM", # no NA values
    "Early childhood centre (private)" = "EARLYCHILDHOOD_PRIVATE_INSTITUTION_NUM", # no NA values
    "Distance to nearest childhood centre" = "DISTANCE_TO_NEAREST_CHILDHOOCCENTRE", # NA values, change to 0 numeric
    "Kindergarten (public)" = "KINDERGARTEN_PUBLIC_INSTITUTION_NUM", # no NA values
    "Kindergarten (private)" = "KINDERGARTEN_PRIVATE_INSTITUTION_NUM", # no NA values
    "Distance to nearest kindergarten" = "DISTANCE_TO_NEAREST_KINDERGARTEN", # NA values, change to 0 numeric
    "Primary school (public)" = "PRIMARY_PUBLIC_INSTITUTION_NUM", # no NA values
    "Primary school (private)" = "PRIMARY_PRIVATE_INSTITUTION_NUM", # no NA values
    "Distance to nearest primary school" = "DISTANCE_TO_NEAREST_PRIMARYSCHOOL", # NA values, change to 0 numeric
    "Middle school (public)" = "MIDDLE_PUBLIC_INSTITUTION_NUM", # no NA values
    "Middle school (private)" = "MIDDLE_PRIVATE_INSTITUTION_NUM", # no NA values
    "Distance to nearest middle school" = "DISTANCE_TO_NEAREST_MIDDLESCHOOL", # NA values, change to 0 numeric
    "High school (public)" = "HIGH_PUBLIC_INSTITUTION_NUM", # no NA values
    "High school (private)" = "HIGH_PRIVATE_INSTITUTION_NUM", # no NA values
    "Distance to nearest high school" = "DISTANCE_TO_NEAREST_HIGHSCHOOL", # NA values, change to 0 numeric
    "Polytechnic (public)" = "POLYTECHNIC_PUBLIC_INSTITUTION_NUM", # no NA values
    "Polytechnic (private)" = "POLYTECHNIC_PRIVATE_INSTITUTION_NUM", # no NA values
    "Distance to nearest polytechnic" = "DISTANCE_TO_NEAREST_POLYTECHNIC", # NA values, change to 0 numeric
    "College (public)" = "COLLEGE_PUBLIC_INSTITUTION_NUM", # no NA values
    "College (private)" = "COLLEGE_PRIVATE_INSTITUTION_NUM", # no NA values
    "Distance to nearest college" = "DISTANCE_TO_NEAREST_COLLEGE", # NA values, change to 0 numeric
    "Islamic schools (private)" = "ISLAMIC_SCHOOL_PRIVATE_INSTITUTION_NUM", # no NA values
    
    # Healthcare
    "No. of Hospitals" = "HOSPITAL_NUM", # no NA values
    "Distance to nearest hospital" = "DISTANCE_TO_NEAREST_HOSPITAL", # NA values, change to 0 numeric
    #"Convenience to nearest hospital" = "CONVENIENCE_TO_HOSPITAL",
    "No. of Maternity Hospitals" = "MATERNITY_HOSPITAL_NUM", # no NA values
    "Distance to nearest maternity hospital" = "DISTANCE_TO_NEAREST_MATERNITY_HOSPITAL", # NA values, change to 0 numeric
    #"Convenience to reach nearest maternity hospital" = "CONVENIENCE_TO_MATERNITY_HOSPITAL",
    "No. of Public Health Clinic (inpatient care)" = "PUBLIC_HEALTH_CLINIC_INPATIENT_CARE_NUM", # no NA values
    "Distance to nearest inpatient care clinic" = "DISTANCE_TO_NEAREST_CLINIC_INPATIENT_CARE", # NA values, change to 0 numeric
    #"Convenience to nearest inpatient care clinic" = "CONVENIENCE_TO_CLINIC_INPATIENT_CARE",
    "No. of Public Health Clinic (outpatient care)" = "PUBLIC_HEALTH_CLINIC_OUTPATIENT_CARE_NUM", # no NA values
    "Distance to nearest outpatient care clinic" = "DISTANCE_TO_NEAREST_CLINIC_OUTPATIENT_CARE", # NA values, change to 0 numeric
    #"Convenience to nearest outpatient care clinic" = "CONVENIENCE_TO_CLINIC_OUTPATIENT_CARE",
    "No. of Polyclinic" = "POLYCLINIC_NUM", # no NA values
    "Distance to nearest polyclinic" = "DISTANCE_TO_NEAREST_POLYCLINIC", # NA values, change to 0 numeric
    #"Convenience to nearest polyclincic" = "CONVENIENCE_TO_POLYCLINIC",
    "No. of General Practitioner (GP)" = "GP_NUM", # no NA values
    "Distance to nearest GP" = "DISTANCE_TO_NEAREST_GP", # NA values, change to 0 numeric
    #"Convenience to nearest GP" = "CONVENIENCE_TO_GP",
    "No. of Village Health Post" = "VILLAGE_HEALTH_POST_NUM", # no NA values
    "Distance to nearest health post" = "DISTANCE_TO_NEAREST_HEALTH_POST", # NA values, change to 0 numeric
    #"Convenience to nearest health post" = "CONVENIENCE_TO_HEALTH_POST",
    "No. of Pharmacy" = "PHARMACY_NUM", # no NA values
    "Distance to nearest pharmacy" = "DISTANCE_TO_NEAREST_PHARMACY", # NA values, change to 0 numeric
    #"Convenience to nearest pharmacy" = "CONVENIENCE_TO_PHARMACY",
    "No. of Drugstore" = "DRUGSTORE_NUM", # no NA values
    "Distance to nearest drugstore" = "DISTANCE_TO_NEAREST_DRUGSTORE", # NA values, change to 0 numeric
    #"Convenience to nearest drugstore" = "CONVENIENCE_TO_DRUGSTORE",
    "No. of Dentist" = "DENTIST_NUM", # no NA values
    "No. of Midwife" = "MIDWIFE_NUM", # no NA values
    "No. of Male Doctors" = "MALE_DOCTOR_NUM", # no NA values
    "No. of Female Doctors" = "FEMALE_DOCTOR_NUM", # no NA values
    "Population covered under universal healthcare programme 2017" = "UNIVERSAL_HEALTHCARE_PROGRAMME_POP_2017", # no NA values
    
    # Communication
    "No. of Base Transceiver stations" = "BASE_TRANSCEIVER_STATIONS", # no NA values
    "No. of Mobile phone service operators" = "MOBILE_PHONE_SERVICE_OPERATORS", # no NA values
    
    # Economic Development/Industry
    "No. of Wood Industry" = "NUM_WOOD_INDUSTRY", # no NA values
    "No. of Metal Industry" = "NUM_METAL_INDUSTRY", # no NA values
    "No. of Fabric Industry" = "NUM_FABRIC_INDUSTRY", # no NA values
    "No. of Pottery Industry" = "NUM_POTTERY_INDUSTRY", # no NA values
    "No. of Bamboo Rattan Industry" = "NUM_BAMBOORATTAN_INDUSTRY", # no NA values
    "No. of Food Industry" = "NUM_FOOD_INDUSTRY", # no NA values
    "No. of Village Cooperatives" = "VILLAGE_COOPERATIVE", # no NA values
    "No. of Shophouses" = "NUM_SHOPHOUSE", # no NA values
    "No. of Permanent markets" = "NUM_PERMANENT_MARKET", # no NA values
    "No. of Semi permanent markets" = "NUM_SEMIPERMANENT_MARKET", # no NA values
    "No. of Markets with no building" = "NUM_MARKET_NOBUILDINGS", # no NA values
    "No. of Minimarts" = "NUM_MINIMART", # no NA values
    "No. of Bigger warung (small family-owned business)" = "NUM_BIGGER_WARUNG", # no NA values
    "No. of Smaller warung (small family-owned business)" = "NUM_SMALLER_WARUNG", # no NA values
    "No. of Restaurants" = "NUM_RESTAURANTS", # no NA values
    "No. of Food stalls" = "NUM_FOODSTALLS", # no NA values
    "No. of Hotels" = "NUM_HOTELS", # no NA values
    "No. of Lodging places" = "NUM_LODGING_PLACES", # no NA values
    
    
    # Pollution & natural disasters
    "Mudslides Incidents 2015" = "MUDSLIDES_2015", # NA values, change to 0 numeric
    "Mudslides Incidents 2016" = "MUDSLIDES_2016", # NA values, change to 0 numeric
    "Mudslides Incidents 2017" = "MUDSLIDES_2017", # NA values, change to 0 numeric
    "Flood Incidents 2015" = "FLOOD_2015", # NA values, change to 0 numeric
    "Flood Incidents 2016" = "FLOOD_2016", # NA values, change to 0 numeric
    "Flood Incidents 2017" = "FLOOD_2017", # NA values, change to 0 numeric
    # "Flashflood Incidents 2015" = "FLASHFLOOD_2015",
    # "Flashflood Incidents 2016" = "FLASHFLOOD_2016",
    # "Flashflood Incidents 2017" = "FLASHFLOOD_2017",
    "Earthquake Incidents 2015" = "EARTHQUAKE_2015", # NA values, change to 0 numeric
    "Earthquake Incidents 2016" = "EARTHQUAKE_2016", # NA values, change to 0 numeric
    "Earthquake Incidents 2017" = "EARTHQUAKE_2017", # NA values, change to 0 numeric
    "Tsunami Incidents 2015" = "TSUNAMI_2015", # NA values, change to 0 numeric
    "Tsunami Incidents 2016" = "TSUNAMI_2016", # NA values, change to 0 numeric
    "Tsunami Incidents 2017" = "TSUNAMI_2017", # NA values, change to 0 numeric
    "Tidal Waves Incidents 2015" = "TIDALWAVES_2015", # NA values, change to 0 numeric
    "Tidal Waves Incidents 2016" = "TIDALWAVES_2016", # NA values, change to 0 numeric
    "Tidal Waves Incidents 2017" = "TIDALWAVES_2017", # NA values, change to 0 numeric
    "Tornado Incidents 2015" = "TORNADO_2015", # NA values, change to 0 numeric
    "Tornado Incidents 2016" = "TORNADO_2016", # NA values, change to 0 numeric
    "Tornado Incidents 2017" = "TORNADO_2017", # NA values, change to 0 numeric
    "Volcanic Eruptions 2015" = "VOLCANIC_ERUPTIONS_2015", # NA values, change to 0 numeric
    "Volcanic Eruptions 2016" = "VOLCANIC_ERUPTIONS_2016", # NA values, change to 0 numeric
    "Volcanic Eruptions 2017" = "VOLCANIC_ERUPTIONS_2017", # NA values, change to 0 numeric
    "Forest Fires 2015" = "FORESTFIRE_2015", # NA values, change to 0 numeric
    "Forest Fires 2016" = "FORESTFIRE_2016", # NA values, change to 0 numeric
    "Forest Fires 2017" = "FORESTFIRE_2017", # NA values, change to 0 numeric
    "Drought Incidents 2015" = "DROUGHT_2015", # NA values, change to 0 numeric
    "Drought Incidents 2016" = "DROUGHT_2016", # NA values, change to 0 numeric
    "Drought Incidents 2017" = "DROUGHT_2017", # NA values, change to 0 numeric
    "Diarrhoea cases" = "DIARRHEA_COUNT", # NA values, change to 0 numeric
    "Dengue cases" = "DENGUE_COUNT", # NA values, change to 0 numeric
    "Measles cases" = "MEASLES_COUNT", # NA values, change to 0 numeric
    "Malaria cases" = "MALARIA_COUNT" # NA values, change to 0 numeric
)

# Data wrangling for clustering continuous variables
var_to_recode <- c("COUNCIL_MEMBER_NUM", 
                   "COUNCIL_ACTIVITY_2017", 
                   "VILLAGE_REGULATION_2017",
                   "DISTANCE_TO_NEAREST_GOVT_BANKS",
                   "DISTANCE_TO_NEAREST_COMMERCIAL_BANKS",
                   "DISTANCE_TO_NEAREST_RURAL_BANKS",
                   "NUM_OF_SLUMS",
                   "DISTANCE_TO_NEAREST_CHILDHOOCCENTRE",
                   "DISTANCE_TO_NEAREST_KINDERGARTEN",
                   "DISTANCE_TO_NEAREST_PRIMARYSCHOOL",
                   "DISTANCE_TO_NEAREST_MIDDLESCHOOL",
                   "DISTANCE_TO_NEAREST_HIGHSCHOOL",
                   "DISTANCE_TO_NEAREST_POLYTECHNIC",
                   "DISTANCE_TO_NEAREST_COLLEGE",
                   "DISTANCE_TO_NEAREST_HOSPITAL",
                   "DISTANCE_TO_NEAREST_MATERNITY_HOSPITAL",
                   "DISTANCE_TO_NEAREST_CLINIC_INPATIENT_CARE",
                   "DISTANCE_TO_NEAREST_CLINIC_OUTPATIENT_CARE",
                   "DISTANCE_TO_NEAREST_POLYCLINIC",
                   "DISTANCE_TO_NEAREST_GP",
                   "DISTANCE_TO_NEAREST_HEALTH_POST",
                   "DISTANCE_TO_NEAREST_PHARMACY",
                   "DISTANCE_TO_NEAREST_DRUGSTORE",
                   "MUDSLIDES_2015",
                   "MUDSLIDES_2016",
                   "MUDSLIDES_2017",
                   "FLOOD_2015",
                   "FLOOD_2016",
                   "FLOOD_2017",
                   "EARTHQUAKE_2015",
                   "EARTHQUAKE_2016",
                   "EARTHQUAKE_2017",
                   "TSUNAMI_2015",
                   "TSUNAMI_2016",
                   "TSUNAMI_2017",
                   "TIDALWAVES_2015",
                   "TIDALWAVES_2016",
                   "TIDALWAVES_2017",
                   "TORNADO_2015",
                   "TORNADO_2016",
                   "TORNADO_2017",
                   "VOLCANIC_ERUPTIONS_2015",
                   "VOLCANIC_ERUPTIONS_2016",
                   "VOLCANIC_ERUPTIONS_2017",
                   "FORESTFIRE_2015",
                   "FORESTFIRE_2016",
                   "FORESTFIRE_2017",
                   "DROUGHT_2015",
                   "DROUGHT_2016",
                   "DROUGHT_2017",
                   "DIARRHEA_COUNT",
                   "DENGUE_COUNT",
                   "MEASLES_COUNT",
                   "MALARIA_COUNT")

# for (var in var_to_recode){
#   print(var)
#   eastkalimantan_podes$var[is.na(eastkalimantan_podes$var)] <- 0
# }

eastkalimantan_podes$COUNCIL_MEMBER_NUM[is.na(eastkalimantan_podes$COUNCIL_MEMBER_NUM)] <- 0
eastkalimantan_podes$COUNCIL_ACTIVITY_2017[is.na(eastkalimantan_podes$COUNCIL_ACTIVITY_2017)] <- 0
eastkalimantan_podes$VILLAGE_REGULATION_2017[is.na(eastkalimantan_podes$VILLAGE_REGULATION_2017)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_GOVT_BANKS[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_GOVT_BANKS)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_COMMERCIAL_BANKS[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_COMMERCIAL_BANKS)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_RURAL_BANKS[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_RURAL_BANKS)] <- 0
eastkalimantan_podes$NUM_OF_SLUMS[is.na(eastkalimantan_podes$NUM_OF_SLUMS)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_CHILDHOOCCENTRE[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_CHILDHOOCCENTRE)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_KINDERGARTEN[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_KINDERGARTEN)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_PRIMARYSCHOOL[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_PRIMARYSCHOOL)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_MIDDLESCHOOL[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_MIDDLESCHOOL)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_HIGHSCHOOL[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_HIGHSCHOOL)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_POLYTECHNIC[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_POLYTECHNIC)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_COLLEGE[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_COLLEGE)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_HOSPITAL[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_HOSPITAL)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_MATERNITY_HOSPITAL[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_MATERNITY_HOSPITAL)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_CLINIC_INPATIENT_CARE[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_CLINIC_INPATIENT_CARE)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_CLINIC_OUTPATIENT_CARE[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_CLINIC_OUTPATIENT_CARE)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_POLYCLINIC[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_POLYCLINIC)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_GP[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_GP)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_HEALTH_POST[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_HEALTH_POST)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_PHARMACY[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_PHARMACY)] <- 0
eastkalimantan_podes$DISTANCE_TO_NEAREST_DRUGSTORE[is.na(eastkalimantan_podes$DISTANCE_TO_NEAREST_DRUGSTORE)] <- 0
eastkalimantan_podes$MUDSLIDES_2015[is.na(eastkalimantan_podes$MUDSLIDES_2015)] <- 0
eastkalimantan_podes$MUDSLIDES_2016[is.na(eastkalimantan_podes$MUDSLIDES_2016)] <- 0
eastkalimantan_podes$MUDSLIDES_2017[is.na(eastkalimantan_podes$MUDSLIDES_2017)] <- 0
eastkalimantan_podes$FLOOD_2015[is.na(eastkalimantan_podes$FLOOD_2015)] <- 0
eastkalimantan_podes$FLOOD_2016[is.na(eastkalimantan_podes$FLOOD_2016)] <- 0
eastkalimantan_podes$FLOOD_2017[is.na(eastkalimantan_podes$FLOOD_2017)] <- 0
eastkalimantan_podes$EARTHQUAKE_2015[is.na(eastkalimantan_podes$EARTHQUAKE_2015)] <- 0
eastkalimantan_podes$EARTHQUAKE_2016[is.na(eastkalimantan_podes$EARTHQUAKE_2016)] <- 0
eastkalimantan_podes$EARTHQUAKE_2017[is.na(eastkalimantan_podes$EARTHQUAKE_2017)] <- 0
eastkalimantan_podes$TSUNAMI_2015[is.na(eastkalimantan_podes$TSUNAMI_2015)] <- 0
eastkalimantan_podes$TSUNAMI_2016[is.na(eastkalimantan_podes$TSUNAMI_2016)] <- 0
eastkalimantan_podes$TSUNAMI_2017[is.na(eastkalimantan_podes$TSUNAMI_2017)] <- 0
eastkalimantan_podes$TIDALWAVES_2015[is.na(eastkalimantan_podes$TIDALWAVES_2015)] <- 0
eastkalimantan_podes$TIDALWAVES_2016[is.na(eastkalimantan_podes$TIDALWAVES_2016)] <- 0
eastkalimantan_podes$TIDALWAVES_2017[is.na(eastkalimantan_podes$TIDALWAVES_2017)] <- 0
eastkalimantan_podes$TORNADO_2015[is.na(eastkalimantan_podes$TORNADO_2015)] <- 0
eastkalimantan_podes$TORNADO_2016[is.na(eastkalimantan_podes$TORNADO_2016)] <- 0
eastkalimantan_podes$TORNADO_2017[is.na(eastkalimantan_podes$TORNADO_2017)] <- 0
eastkalimantan_podes$VOLCANIC_ERUPTIONS_2015[is.na(eastkalimantan_podes$VOLCANIC_ERUPTIONS_2015)] <- 0
eastkalimantan_podes$VOLCANIC_ERUPTIONS_2016[is.na(eastkalimantan_podes$VOLCANIC_ERUPTIONS_2016)] <- 0
eastkalimantan_podes$VOLCANIC_ERUPTIONS_2017[is.na(eastkalimantan_podes$VOLCANIC_ERUPTIONS_2017)] <- 0
eastkalimantan_podes$FORESTFIRE_2015[is.na(eastkalimantan_podes$FORESTFIRE_2015)] <- 0
eastkalimantan_podes$FORESTFIRE_2016[is.na(eastkalimantan_podes$FORESTFIRE_2016)] <- 0
eastkalimantan_podes$FORESTFIRE_2017[is.na(eastkalimantan_podes$FORESTFIRE_2017)] <- 0
eastkalimantan_podes$DROUGHT_2015[is.na(eastkalimantan_podes$DROUGHT_2015)] <- 0
eastkalimantan_podes$DROUGHT_2016[is.na(eastkalimantan_podes$DROUGHT_2016)] <- 0
eastkalimantan_podes$DROUGHT_2017[is.na(eastkalimantan_podes$DROUGHT_2017)] <- 0
eastkalimantan_podes$DIARRHEA_COUNT[is.na(eastkalimantan_podes$DIARRHEA_COUNT)] <- 0
eastkalimantan_podes$DENGUE_COUNT[is.na(eastkalimantan_podes$DENGUE_COUNT)] <- 0
eastkalimantan_podes$MEASLES_COUNT[is.na(eastkalimantan_podes$MEASLES_COUNT)] <- 0
eastkalimantan_podes$MALARIA_COUNT[is.na(eastkalimantan_podes$MALARIA_COUNT)] <- 0



# Clustering variables

agglo_method <- c("ward.D" = "ward.D",
                  "complete" = "complete",
                  "average" = "average",
                  "single" = "single",
                  "centroid" = "centroid")

dist_method <- c("euclidean" = "euclidean",
                 "maximum" = "maximum",
                 "manhattan" = "manhattan",
                 "canberra" = "canberra",
                 "minkowski" = "minkowski",
                 "binary" = "binary") 



# Define UI
ui <- fluidPage(theme = shinytheme("simplex"),
                useShinyjs(),
                # Navigation Bar
                navbarPage(
                    title = div(img(src = 'logo_john.jpg', style = "margin-top: 0px; padding-right:6px;padding-bottom:20px", height = 55)),
                    windowTitle = "East Kalimantan as The New J-Town",
                    collapsible = TRUE,
                    
                    # About Panel
                    tabPanel("About", fluid = TRUE, icon=icon("info-circle"),
                             # h2(tags$strong("East Kalimantan as the New J-Town"), style = "color: #3f7300;"),
                             # tags$hr(),
                             sidebarLayout(
                                 sidebarPanel(h3(tags$strong("East Kalimantan as the New J-Town"), style = "color: #3f7300;"),
                                              #h4("IS415 Geospatial Analytics and Application"),
                                              tags$br(),
                                              h4("A Group Project by:"),
                                              tags$ul(
                                                  tags$li(tags$a(href = "https://www.linkedin.com/in/erikaaldisa/", "Erika Aldisa Gunawan"), style = "font-size: 18px;"),
                                                  tags$li(tags$a(href = "http://www.linkedin.com/in/xunjieng53b81m", "Ng Xun Jie"), style = "font-size: 18px;"),
                                                  tags$li(tags$a(href = "http://www.linkedin.com/in/ljhsean", "Sean Lee Jun Hui"), style = "font-size: 18px;")
                                              ),
                                              tags$br(),
                                              h4("Guided by Professor Kam Tin Seong for IS415 Geospatial Analytics and Application"),
                                              #div(img(src = 'smu_logo.png', style = "margin-top:0px; padding-right:0px;padding-bottom:0px", height = 130)),
                                              imageOutput("smu_logo"),
                                              width = 3
                                              #tags$style(
                                              #    HTML('.well {background-color: #ffffff;}'))
                                 ),
                                 mainPanel(h3("Project Motivation"),
                                           tags$hr(),
                                           h4("On 26th August 2019, Indonesia announced new capital city site would be moved to East Kalimantan"),
                                           fluidRow(
                                               # column(6,
                                               #        imageOutput("news_map"),
                                               #        tags$a(href = "https://www.theguardian.com/world/2019/aug/26/indonesia-new-capital-city-borneo-forests-jakarta", "Source: The Guardian"
                                               #        )),
                                               column(5,
                                                      h4("The intention of the Indonesian government was to move away from the heavily populated Java island to slow down the 
                                               environmental degradation of Jakarta and to unify the archipelago by developing Kalimantan 
                                                  which is geographically the central of Indonesia.", style = "margin-top:5px;"),
                                                      tags$br(),
                                                      h4("The move garnered criticisms from urban planning experts and environmentalists 
                                               because the move was not strategic, potentially causing more environmental damage and it 
                                                  does not mean that Jakarta’s problems will be significantly resolved."),
                                                      tags$br(),
                                                      h4("Our project aims to conduct geospatial analysis on the 
                                               proposed area of the new capital city in East Kalimantan to learn more 
                                               about the area’s socio-economical potential to be considered as the 
                                                  new capital city of Indonesia.")
                                               ),
                                               column(7,
                                                      imageOutput("news_map"),
                                                      tags$a(href = "https://www.theguardian.com/world/2019/aug/26/indonesia-new-capital-city-borneo-forests-jakarta", "Source: The Guardian"
                                                      ))
                                           ),
                                           h3("Project Objectives"),
                                           tags$hr(),
                                           tags$ul(
                                               tags$li("To explore the proposed area of the new capital city site by conducting Exploratory Data Analysis on East Kalimantan PODES 2018 data", style = "font-size: 17px; font-weight: 500;"),
                                               tags$li("To conduct geographical segmentation and profiling to learn about different villages' suitability to be the new capital city site", style = "font-size: 17px; font-weight: 500;"),
                                               tags$li("To analyse and recommend the most suitable site/area for the new capital city based on chosen variables", style = "font-size: 17px; font-weight: 500;")
                                           ),
                                           h3("Application"),
                                           tags$hr(),
                                           tags$ol(
                                               tags$li("Exploratory Data Analysis (EDA), to visualize the boxplot & choropleth map of the chosen categorical/continuous variable of PODES 2018 data", style = "font-size: 17px; font-weight: 500;"),
                                               tags$li("Spatially Constrained Clustering using SKATER method, to visualize the clusters formed & correlation analysis graph by choosing the input variables, number of clusters and distance method used", style = "font-size: 17px; font-weight: 500;"),
                                               tags$li("Spatially Constrained Clustering using ClustGeo method, to visualize the clusters formed & correlation analysis graph by choosing the input variables, number of clusters, hierarchical cluster agglomeration method and clustgeo alpha value", style = "font-size: 17px; font-weight: 500;")
                                           ),
                                           h3("R Blogdown Page"),
                                           tags$hr(),
                                           uiOutput("blogdown"),
                                           tags$br(),
                                           width = 9
                                 )
                             )
                    ),
                    
                    # EDA Panel
                    tabPanel("EDA", fluid = TRUE, icon=icon("bar-chart-o"),
                             #h3(tags$strong("Geographical Profiling & Segmentation of the new proposed capital city in East Kalimantan"), style = "color: #3f7300;"),
                             #tags$hr(),
                             #h5("Create Choropleth Maps based on Indonesia's village potential (podes) data collected in 2018."),
                             #tags$br(),
                             sidebarLayout(
                                 sidebarPanel(fluid = TRUE, width = 3,
                                              
                                              # If Categorical Var. tabPanel is clicked, these sidebarpanel below will be shown
                                              conditionalPanel(
                                                  'input.EDA_var === "Categorical Var."',
                                                  tags$strong("Categorical Variable Inputs"),
                                                  #helpText("Click the Categorical Var. Tab to see the changes"),
                                                  selectInput(inputId = "var_cat",
                                                              label = "Choose a categorical var to display:",
                                                              choices = cat_var,
                                                              selected = "Topography"),
                                                  selectInput(inputId = "border_cat",
                                                              label = "Choose the Level of Detail border:",
                                                              choices = c("None" = "none",
                                                                          "City/Regencies" = "city_border",
                                                                          "District" = "district_border",
                                                                          "Village/Sub-District" = "village_border"),
                                                              selected = "None"),
                                                  selectInput(inputId = "palette_cat",
                                                              label = "Palette:",
                                                              choices = c("Accent" = "Accent",
                                                                          "Pastel 1" = "Pastel1", 
                                                                          "Pastel 2" = "Pastel2",
                                                                          "Set 3" = "Set3"),
                                                              selected = "Accent")
                                              ),
                                              
                                              # If Continuous Var. tabPanel is clicked, these sidebarpanel below will be shown
                                              conditionalPanel(
                                                  'input.EDA_var === "Continuous Var."',
                                                  tags$strong("Continuous Variable Inputs"),
                                                  #helpText("Click the Continuous Var. Tab to see the changes"),
                                                  selectInput(inputId = "var_con", 
                                                              label = "Choose a continuous var to display:",
                                                              choices = con_var,
                                                              selected = "Slums"),
                                                  radioButtons(inputId = "style",
                                                               label = "Style for continuous variable:",
                                                               choices = c("Pretty" = "pretty",
                                                                           "Quantile" = "quantile", 
                                                                           "Jenks" = "jenks",
                                                                           "Equal" = "equal"),
                                                               selected = "pretty"),
                                                  sliderInput(inputId = "bin",
                                                              label = "Number of classes:",
                                                              min = 1,
                                                              max = 10,
                                                              step = 1,
                                                              value = 3),
                                                  selectInput(inputId = "border_con",
                                                              label = "Choose the Level of Detail border:",
                                                              choices = c("None" = "none",
                                                                          "City/Regencies" = "city_border",
                                                                          "District" = "district_border",
                                                                          "Village/Sub-District" = "village_border"),
                                                              selected = "None"),
                                                  selectInput(inputId = "palette_con",
                                                              label = "Palette:",
                                                              choices = c("Blues" = "Blues",
                                                                          "Greens" = "Greens", 
                                                                          "Reds" = "Reds",
                                                                          "Yellow & Green" = "YlGn",
                                                                          "Red & Blue" = "RdBu",
                                                                          "Blue & Red" = "-RdBu",
                                                                          "Purple & Blue" = "PuBu"),
                                                              selected = "Blues")
                                              )
                                 ),
                                 mainPanel(
                                     tabsetPanel(
                                         id = "EDA_var",
                                         tabPanel("Categorical Var.",
                                                  column(6,
                                                         tmapOutput("podes_cat_map")
                                                  ),
                                                  column(6, align="center",
                                                         h6("Double click on the map to zoom out & unselect"),
                                                         plotly::plotlyOutput("bar_plot"),
                                                         column(6,
                                                                checkboxInput("flip_bar", "Flip the axis of the plot")
                                                         ),
                                                         column(6,
                                                                checkboxInput("legend_bar", "Hide the legend"))
                                                  )
                                         ),
                                         tabPanel("Continuous Var.", 
                                                  # crosstalk::bscols(tmapOutput("podes_con_map"),
                                                  #                   plotly::plotlyOutput("box_plot")),
                                                  column(6,
                                                         # h6("test")
                                                         tmapOutput("podes_con_map")
                                                  ),
                                                  column(6, align="center",
                                                         h6("Double click on the map to zoom out & unselect"),
                                                         plotly::plotlyOutput("box_plot"),
                                                         
                                                         column(6,
                                                                checkboxInput("flip", "Flip the axis of the plot")
                                                         ),
                                                         column(6,
                                                                checkboxInput("logY", "Transform Continuous scale to log10 scale", FALSE)
                                                         )
                                                         #checkboxInput("logX", "show x-axis in log10", FALSE),
                                                  )
                                         )
                                     )
                                 )
                             )
                    ),
                    
                    #Clustering Panel
                    tabPanel("Clustering", fluid = TRUE, icon=icon("chart-area"),
                             #h3(tags$strong("SKATER method"), style = 'color: #3f7300;')
                             sidebarLayout(
                                 sidebarPanel(fluid = TRUE, width = 3,
                                              
                                              # If Hierarchical method tabPanel is clicked, these sidebarpanel below will be shown
                                              conditionalPanel(
                                                  'input.clustering_var === "Hierarchical method"',
                                                  tags$strong("Hierarchical Clustering (Non-spatial)"),
                                                  #helpText("Please avoid using 0/1/2 variables only, it would sometimes return an error"),
                                                  selectInput("hiera_var",
                                                              label = "Choose your clustering variables:",
                                                              choices = con_var,
                                                              selected = c(#"No. of Council members" = "COUNCIL_MEMBER_NUM",
                                                                  #"No. of Slums" = "NUM_OF_SLUMS",
                                                                  "Population in poverty" = "STATEMENT_OF_POVERTY",
                                                                  "Households with no electricity" = "HOUSEHOLDS_NO_ELECTRICITY",
                                                                  "Households with landlines" = "NUM_FAMILIES_WITH_LANDLINES",
                                                                  "No. of Government Banks" = "NUM_GOVT_BANKS"),
                                                              
                                                              multiple = TRUE),
                                                  sliderInput("hiera_no_cluster",
                                                              label = "No. of clusters",
                                                              min = 3,
                                                              max = 10,
                                                              value = 5,
                                                              step = 1),
                                                  checkboxInput("hiera_sugg_cluster", "Show suggested no. of clusters"),
                                                  conditionalPanel(condition = "input.hiera_sugg_cluster",
                                                                   plotOutput("hiera_cluster_graph", height = 300)),
                                                  # helpText("gap statistic method will not work on variables marked with (NA)"),
                                                  selectInput(inputId = "agglo_method_hiera",
                                                              label = "Hierarchical agglomeration method",
                                                              choices = agglo_method,
                                                              selected = "ward.D",
                                                              multiple = FALSE)
                                                  # checkboxInput("hiera_sugg_method", "Show suggested method"),
                                                  # conditionalPanel(condition = "input.hiera_sugg_method",
                                                  #                  tableOutput("hiera_method"))
                                              ),
                                              
                                              # If SKATER method tabPanel is clicked, these sidebarpanel below will be shown
                                              conditionalPanel(
                                                  'input.clustering_var === "SKATER method"',
                                                  tags$strong("SKATER spatially constrained clustering method"),
                                                  selectInput("skater_var",
                                                              label = "Choose your clustering variables:",
                                                              choices = con_var,
                                                              selected = c("No. of Council members" = "COUNCIL_MEMBER_NUM",
                                                                           "No. of Slums" = "NUM_OF_SLUMS",
                                                                           "Population in poverty" = "STATEMENT_OF_POVERTY",
                                                                           "Households with no electricity" = "HOUSEHOLDS_NO_ELECTRICITY"
                                                              ),
                                                              multiple = TRUE),
                                                  sliderInput("skater_no_cluster",
                                                              label = "No. of clusters",
                                                              min = 3,
                                                              max = 10,
                                                              value = 5,
                                                              step = 1),
                                                  checkboxInput("skater_sugg_cluster", "Show suggested no. of clusters"),
                                                  conditionalPanel(condition = "input.skater_sugg_cluster",
                                                                   plotOutput("skater_cluster_graph", height = 300)),
                                                  selectInput(inputId = "skater_method",
                                                              label = "Distance method",
                                                              choices = dist_method,
                                                              selected = "euclidean")
                                              ),
                                              
                                              # If ClustGeo method tabPanel is clicked, these sidebarpanel below will be shown
                                              conditionalPanel(
                                                  'input.clustering_var === "ClustGeo method"',
                                                  tags$strong("ClustGeo spatially constrained clustering method"),
                                                  selectInput("clustgeo_var",
                                                              label = "Choose your clustering variables:",
                                                              choices = con_var,
                                                              selected = c("No. of Hospitals" = "HOSPITAL_NUM", 
                                                                           "Distance to nearest hospital" = "DISTANCE_TO_NEAREST_HOSPITAL",
                                                                           "No. of Hotels" = "NUM_HOTELS", 
                                                                           "No. of Lodging places" = "NUM_LODGING_PLACES"),
                                                              multiple = TRUE),
                                                  # helpText("Select more variables than chosen no. of cluster?"),
                                                  sliderInput("clustgeo_no_cluster",
                                                              label = "No. of clusters",
                                                              min = 3,
                                                              max = 10,
                                                              value = 5,
                                                              step = 1),
                                                  checkboxInput("clustgeo_sugg_cluster", "Show suggested no. of clusters"),
                                                  conditionalPanel(condition = "input.clustgeo_sugg_cluster",
                                                                   plotOutput("clustgeo_cluster_graph", height = 300)),
                                                  # selectInput(inputId = "agglo_method_clustgeo",
                                                  #             label = "Agglomeration method",
                                                  #             choices = agglo_method,
                                                  #             selected = "ward.D",
                                                  #             multiple = FALSE),
                                                  
                                                  sliderInput("clustgeo_alpha",
                                                              label = "alpha value",
                                                              min = 0.1,
                                                              max = 1.0,
                                                              value = 0.5,
                                                              step = 0.1),
                                                  checkboxInput("clustgeo_sugg_alpha", "Show suggested alpha value for known no. of clusters"),
                                                  conditionalPanel(condition = "input.clustgeo_sugg_alpha",
                                                                   plotOutput("clustgeo_sugg_alpha", height = 300),
                                                                   h6("Choose the intersection of D0 & D1 as the alpha value"))
                                              )
                                 ),
                                 
                                 mainPanel(
                                     tabsetPanel(
                                         id = "clustering_var",
                                         tabPanel("Hierarchical method",
                                                  column(6,
                                                         tmapOutput("hiera_cluster_map")
                                                  ),
                                                  column(6, 
                                                         plotOutput("hiera_corr_plot"),
                                                         column(12, align="right",
                                                                actionButton(inputId = "hiera_corr_plot_help", label = "About Correlation Matrix/Plot"),
                                                                hidden(
                                                                    tags$div(id = "corr_plot_text_hiera", h5("The correlation matrix below shows that 1 means highly correlated variables and -1 means variables are not correlated at all, do choose your variables wisely because highly correlated variables will give inaccurate clustering results.
                                                                                                           You can set your own threshold to choose which variables are worth keeping.")
                                                                    )
                                                                )
                                                         )
                                                  ),
                                                  tabsetPanel(
                                                      id = "hiera_info",
                                                      tabPanel("About Hierarchical Clustering",
                                                               column(12,
                                                                      h2("What is Hierarchical Clustering?"),
                                                                      tags$br(),
                                                                      h5("The most common type of hierarchichal clustering is AGNES (Agglomerative Nesting),
                                                            the method group objects in clusters based on their similarity."),
                                                                      h5("The object is first considered as a single-element cluster and at each step of the algorithm,
                                                            the two most similar clusters are combined and this is repeated until there is only one big cluster."),
                                                                      h5("The result of the clustering can be represented as a dendogram or cluster heatmap."),
                                                                      h5("These are agglomeration/linkage functions that groups the objects into hierarchical cluster tree based on the proximity matrix (similarity) generated:"),
                                                                      tags$ul(
                                                                          tags$li("Ward: minimizes total within-cluster variance, the pair of clusters with minimum between-cluster distance are merged"),
                                                                          tags$li("Complete: computes all pairwise dissimilarities between elements two clusters, and considers the maximum value as the distance between the two clusters, tend to produce compact clusters"),
                                                                          tags$li("Average: computes all pairwise dissimilarities between elements two clusters, and considers the average value as the distance between the two clusters"),
                                                                          tags$li("Single: computes all pairwise dissimilarities between elements two clusters, and considers the minimum value as the distance between the two clusters, tend to produce loose clusters"),
                                                                          tags$li("Centroid: computes the dissimilarity between the centroid for cluster 1 and centroid for cluster 2 and considers it as the distance")
                                                                      ),
                                                                      h5("Take note that hierarchical clustering is a non-spatial clustering method, hence the cluster results are fragmented.")
                                                               )
                                                      ),
                                                      tabPanel("Cluster Heatmap",
                                                               column(12,
                                                                      plotly::plotlyOutput("hiera_cluster_heatmap")
                                                               )
                                                      )
                                                  )
                                         ),
                                         tabPanel("SKATER method",
                                                  column(6,
                                                         tmapOutput("skater_cluster_map")
                                                  ),
                                                  column(6, 
                                                         plotOutput("skater_corr_plot"),
                                                         column(12, align="right",
                                                                actionButton(inputId = "skater_corr_plot_help", label = "About Correlation Matrix/Plot"),
                                                                hidden(
                                                                    tags$div(id = "corr_plot_text_skater", h5("The correlation matrix below shows that 1 means highly correlated variables and -1 means variables are not correlated at all, do choose your variables wisely because highly correlated variables will give inaccurate clustering results.
                                                                                                            You can set your own threshold to choose which variables are worth keeping.")
                                                                    )
                                                                )
                                                         )
                                                  ),
                                                  tabsetPanel(
                                                      id = "skater_info",
                                                      tabPanel("About SKATER",
                                                               column(12,
                                                                      h2("What is SKATER?"),
                                                                      tags$br(),
                                                                      h5("Spatial Kluster Analysis is a contiguity-constrained clustering implemented in the spdep package in R.
                                                                       The SKATER algorithm from Assuncao et al (2006) is based on pruning (cut edges) a minimum spanning tree constructed from the adjacency graph or contiguity structure of the spatial units that need to be grouped."),
                                                                      h5("Steps required involve: "),
                                                                      tags$ul(
                                                                          tags$li("Building neighbour/contiguity list graph"),
                                                                          tags$li("Computing the minimum spanning tree which is a tree that minimizes a cost function to minimize sum of dissimilarities over all nodes"),
                                                                          tags$li("Pruning/cutting edges of the tree for desired number of clusters")
                                                                      ),
                                                                      # h5("The distance method used for skater function are euclidean, maximum, manhattan, canberra, binary, minkowski and the definition can be checked here."),
                                                                      # h5(tags$a(href = "https://www.google.com/", "here."))
                                                                      tags$div(
                                                                          h5("The distance method used for skater function are euclidean, maximum, manhattan, canberra, binary, minkowski and the definition can be checked", style = "display: inline;"),
                                                                          h5(tags$a(href = "https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/dist", "here."), style = "display: inline;")
                                                                      )
                                                               )
                                                      ),
                                                      tabPanel("Cluster Heatmap",
                                                               column(12,
                                                                      plotly::plotlyOutput("skater_cluster_heatmap")
                                                               )
                                                      )
                                                  )
                                         ),
                                         tabPanel("ClustGeo method",
                                                  column(6,
                                                         tmapOutput("clustgeo_cluster_map")
                                                  ),
                                                  column(6, 
                                                         plotOutput("clustgeo_corr_plot"),
                                                         column(12, align="right",
                                                                actionButton(inputId = "clustgeo_corr_plot_help", label = "About Correlation Matrix/Plot"),
                                                                hidden(
                                                                    tags$div(id = "corr_plot_text_clustgeo", h5("The correlation matrix below shows that 1 means highly correlated variables and -1 means variables are not correlated at all, do choose your variables wisely because highly correlated variables will give inaccurate clustering results.
                                                                                                            You can set your own threshold to choose which variables are worth keeping.")
                                                                    )
                                                                )
                                                         )
                                                  ),
                                                  tabsetPanel(
                                                      id = "clustgeo_info",
                                                      tabPanel("About ClustGeo Clustering",
                                                               column(12,
                                                                      h2("What is ClustGeo clustering?"),
                                                                      tags$br(),
                                                                      h5("ClustGeo is an implementation of ward hierarchical clustering when the dissimilarities are not necessary euclidean and the weights are not uniform.
                                                                    The hclustgeo function of ClustGeo package takes two dissimilarity matrices D0 & D1 and a mixing parameter alpha between 0 and 1."),
                                                                      tags$ul(
                                                                          tags$li("D0: euclidean distance matrix performed with socio-demographic/socio-economic continuous variables, obtained through dist() function"),
                                                                          tags$li("D1: second dissimilarity matrix to compute the geographical proximity between places in the study area, obtained through geodist() function")
                                                                      ),
                                                                      h5("Here, D0 gives the feature space while D1 gives the contiguity constraints, in order to get more geographically compact clusters and not fragmented clusters."),
                                                                      h5("The alpha value sets the importance of the contiguity constraint (distance matrix) in the clustering process."),
                                                                      h5("The idea is to determine a alpha value which increases the spatial contiguity without deteriorating the quality of the solution based on the D0 variables (socio-economic variables)."),
                                                                      h5("The hclustgeo function will take in the 2 matrices as input and cut the hclust tree into desired no. of clusters/groups.")
                                                               )
                                                      ),
                                                      tabPanel("Cluster Heatmap",
                                                               column(12,
                                                                      plotly::plotlyOutput("clustgeo_cluster_heatmap")
                                                               )
                                                      )
                                                  )
                                         )
                                     )
                                 )
                             )
                    ),
                    
                    
                    
                    # Data table Panel
                    tabPanel("Data", fluid = TRUE, icon=icon("table"),
                             h3(tags$strong("Data frame of PODES 2018 Data"), style = "color: #3f7300;"),
                             tags$hr(),
                             h5("Some of the variables of PODES data, Potensi Desa (village potential data), are displayed in the data table below."),
                             h5("Village is Indonesia's smallest administrative area and there are two types of villages, Desa (rural village) and Kelurahan (urban village),"),
                             h5("According to the 2019 report by the Ministry of Home Affairs, there are 8,488 urban villages and 74,953 rural villages in Indonesia."),
                             tags$div(
                                 h5("PODES data is obtained from Professor Kam Tin Seong & produced by", style = "display: inline;"),
                                 h5(tags$a(href = "https://www.bps.go.id/subject/168/potensi-desa.html#subjekViewTab3", " Badan Pusat Statistik, Central Statistics Body Indonesia."), style = "display: inline;")
                             ),
                             tags$br(),
                             tags$br(),
                             DT::dataTableOutput(outputId = "podes")),
                    tags$head(tags$style(HTML('.navbar-brand {width: 130px;
                             text-align:center;
                             padding-top: 0px;
                             padding-bottom: -10px;}',
                                              '.navbar { background-color: #ffffff;
                             font-family: Arial;
                             font-size: 15px;
                             min-height: 45px;
                             padding-top: 5px;
                             padding-bottom: 0px;
                             color: #FF0000; }',
                                              '.navbar-dropdown {
                             font-family: Arial;
                             font-size: 15px;
                             color: #FF0000; }')))
                )
                
)




server <- function(input, output, session) {
    # EDA categorical leaflet map
    output$podes_cat_map <- renderLeaflet({
        data <- eastkalimantan_podes
        x <- input$var_cat
        plot_cat <- tm_shape(data) +
            tm_fill(col = x,
                    style = "cat",
                    palette = input$palette_cat,
                    id = "VILLAGE",
                    popup.vars = c("Village" = "VILLAGE", "District" = "DISTRICT", "City" = "CITY", "var" = x),
                    legend.show = TRUE,
                    alpha = 0.9) +
            tm_view(view.legend.position=c("right","top"),
                    control.position=c("left","bottom"),
                    colorNA="Black") +
            tmap_options(basemaps = c("Esri.WorldGrayCanvas","OpenStreetMap", "Stamen.TonerLite"),
                         basemaps.alpha = c(0.8, 0.8, 0.8))
        
        if (input$border_cat == "city_border"){
            plot_cat <- plot_cat +
                tm_shape(city_border)+
                tm_borders(lwd = 1)
            return(tmap_leaflet(plot_cat, in.shiny = TRUE))
        }
        
        if (input$border_cat == "district_border"){
            plot_cat <- plot_cat +
                tm_shape(district_border)+
                tm_borders(lwd = 1)
            return(tmap_leaflet(plot_cat, in.shiny = TRUE))
        }
        
        if (input$border_cat == "village_border"){
            plot_cat <- plot_cat +
                tm_shape(area)+
                tm_borders(lwd = 1)
            return(tmap_leaflet(plot_cat, in.shiny = TRUE))
        }
        
        
        tmap_leaflet(plot_cat, in.shiny = TRUE)
    })
    
    # EDA categorical bar chart/plot
    output$bar_plot <- plotly::renderPlotly({
        plot_bar <- ggplot(eastkalimantan_podes, 
                           aes_string(x = input$var_cat, fill = "CITY"))+
            #aes_string(x = "CITY", y = y, colour = "CITY"))+
            geom_bar(alpha = 0.8, position="stack") + 
            labs(title= input$var_cat, subtitle = "in the village level", y = "Count")
        
        
        if (input$flip_bar & input$legend_bar){
            plot_bar <- plot_bar + 
                coord_flip() +
                theme(legend.position = "none")
            # plot_bar <- ggplotly(plot_box) %>%
            #   highlight(on="plotly_selected", off="plotly_deselect",
            #             opacityDim=0.4)
            return(ggplotly(plot_bar))
        }
        
        if (input$legend_bar){
            plot_bar <- plot_bar + 
                theme(legend.position = "none")
            # plot_bar <- ggplotly(plot_box) %>%
            #   highlight(on="plotly_selected", off="plotly_deselect",
            #             opacityDim=0.4)
            return(ggplotly(plot_bar))
        }
        
        if (input$flip_bar){
            plot_bar <- plot_bar + 
                coord_flip()
            # plot_bar <- ggplotly(plot_box) %>%
            #   highlight(on="plotly_selected", off="plotly_deselect",
            #             opacityDim=0.4)
            return(ggplotly(plot_bar))
        }
        
        plot_bar <- ggplotly(plot_bar) #%>%
        # highlight(on="plotly_selected", off="plotly_deselect",
        #           opacityDim=0.4)
        return(plot_bar)
        
    })
    
    # EDA continuous leaflet map
    output$podes_con_map <- renderLeaflet({
        data <- eastkalimantan_podes
        y <- input$var_con
        plot_con <- tm_shape(data) +
            tm_fill(col = y,
                    style = input$style,
                    n = input$bin,
                    palette = input$palette_con,
                    id = "VILLAGE",
                    popup.vars = c("Village" = "VILLAGE", "District" = "DISTRICT", "City" = "CITY", "var" = y),
                    legend.show = TRUE,
                    alpha = 0.9) +
            tm_view(view.legend.position=c("right","top"),
                    control.position=c("left","bottom"),
                    colorNA="Black") +
            tmap_options(basemaps = c("Esri.WorldGrayCanvas","OpenStreetMap", "Stamen.TonerLite"),
                         basemaps.alpha = c(0.8, 0.8, 0.8))
        
        
        if (input$border_con == "city_border"){
            plot_con <- plot_con +
                tm_shape(city_border)+
                tm_borders(lwd = 1)
            return(tmap_leaflet(plot_con, in.shiny = TRUE))
            
        }
        
        if (input$border_con == "district_border"){
            plot_con <- plot_con +
                tm_shape(district_border)+
                tm_borders(lwd = 1)
            return(tmap_leaflet(plot_con, in.shiny = TRUE))
        }
        
        if (input$border_con == "village_border"){
            plot_con <- plot_con +
                tm_shape(area)+
                tm_borders(lwd = 1)
            return(tmap_leaflet(plot_con, in.shiny = TRUE))
        }
        
        
        tmap_leaflet(plot_con, in.shiny = TRUE)
    })
    
    # EDA continuous variable box plot
    output$box_plot <- plotly::renderPlotly({
        data <- eastkalimantan_podes
        y <- input$var_con
        plot_box <- ggplot(data, 
                           #aes(x = CITY, y = input$var_con, colour = CITY))+
                           aes_string(x = "CITY", y = y, colour = "CITY"))+
            geom_boxplot(fill = NA, alpha = 0.5) +
            geom_jitter(aes(text = paste("VILLAGE: ", VILLAGE)), width = 0.25, cex = 0.6, shape = 2)+
            labs(title= input$var_con, subtitle = "in the village level")+
            theme(legend.position = "none")
        #labs(title = paste0(c("Plot of ",input$var_con, "in the village level")))
        
        if (input$flip & input$logY){
            plot_box <- plot_box + scale_y_log10() + coord_flip() 
            plot_box <- ggplotly(plot_box) %>%
                highlight(on="plotly_selected", off="plotly_deselect",
                          opacityDim=0.4)
            
            return(plot_box)
        }
        
        if (input$logY){
            plot_box <- plot_box + scale_y_log10()
            plot_box <- ggplotly(plot_box) %>%
                highlight(on="plotly_selected", off="plotly_deselect",
                          opacityDim=0.4)
            return(plot_box)
        }
        
        if (input$flip){
            plot_box <- plot_box + coord_flip()
            plot_box <- ggplotly(plot_box) %>%
                highlight(on="plotly_selected", off="plotly_deselect",
                          opacityDim=0.4)
            return(plot_box)
        }
        
        plot_box <- ggplotly(plot_box) %>%
            highlight(on="plotly_selected", off="plotly_deselect",
                      opacityDim=0.4)
        return(plot_box)
        
    })
    
    # Hierarchical gap statistic graph (for number of clusters)
    output$hiera_cluster_graph <- renderPlot({
        
        cluster_df <- eastkalimantan_podes[,input$hiera_var]
        cluster_df <- st_set_geometry(cluster_df, NULL) 
        #row.names(cluster_df) <- eastkalimantan_podes$REGION_CODE
        
        # Normalize df using Min-Max standardization method
        cluster_df <- normalize(cluster_df)
        # print(cluster_df)
        # Suggested number of clusters using gap statistic method
        set.seed(12345)
        gap_stat <- clusGap(cluster_df, FUN = hcut, nstart = 20, K.max = 10, B =50)
        
        # print(gap_stat, method = "firstmax")
        cluster_graph <- fviz_gap_stat(gap_stat)
        
        return(cluster_graph)
        
    })
    
    # Hierarchical method
    # output$hiera_method <- renderTable({
    #   
    #   cluster_df <- eastkalimantan_podes[,input$hiera_var]
    #   cluster_df <- st_set_geometry(cluster_df, NULL) 
    #   row.names(cluster_df) <- eastkalimantan_podes$REGION_CODE
    #  
    #   m <- c( "ward.D", "complete", "average", "single", "centroid")
    #   names(m) <- c( "ward.D", "complete", "average", "single", "centroid")
    # 
    #   # ac <- function(x) {
    #   #   agnes(cluster_df, method = x)$ac
    #   # }
    #   
    #   ac <- agnes(cluster_df, method = m)$ac
    #   table <- map_dbl(m, ac)
    #   return(table)
    #   
    # })
    
    
    
    # Hierarchical clustering map
    output$hiera_cluster_map <- leaflet::renderLeaflet({
        
        cluster_df <- eastkalimantan_podes[,input$hiera_var]
        cluster_df <- st_set_geometry(cluster_df, NULL) 
        
        row.names(cluster_df) <- eastkalimantan_podes$REGION_CODE
        
        # Normalize df using Min-Max standardization method
        cluster_df <- normalize(cluster_df)
        
        # Computing proximity matrix using euclidean distance method
        proxmat <- dist(cluster_df, method = "euclidean")
        
        # Perform hierarchical clustering on the proximity matrix
        hclust_result <- hclust(proxmat, method = input$agglo_method_hiera)
        
        
        # Cutting the dendogram into cluster groups
        groups <- as.factor(cutree(hclust_result, k = input$hiera_no_cluster))
        
        # Combine the cluster groups to the original dataframe
        new_podes <- cbind(eastkalimantan_podes, as.matrix(groups)) %>%
            rename(`CLUSTER` = `as.matrix.groups.`)
        
        cluster_plot <- tm_shape(new_podes) +
            tm_fill(col = "CLUSTER",
                    id = "VILLAGE",
                    popup.vars = c("Village" = "VILLAGE", 
                                   "District" = "DISTRICT", 
                                   "City" = "CITY",
                                   "Cluster" = "CLUSTER"),
                    legend.show = TRUE,
                    alpha = 0.9) +
            tm_borders(lwd = 1)
        
        tmap_leaflet(cluster_plot, in.shiny = TRUE)
        
    })
    
    # Hierarhical clustering - correlation plot
    output$hiera_corr_plot <- renderPlot({
        cluster_df <- eastkalimantan_podes[,input$hiera_var]
        cluster_df <- st_set_geometry(cluster_df, NULL) 
        #row.names(cluster_df) <- eastkalimantan_podes$REGION_CODE
        
        # Normalize df using Min-Max standardization method
        cluster_df <- normalize(cluster_df)
        
        var_cor <- cor(cluster_df)
        corr_plot <- corrplot.mixed(var_cor,
                                    lower = "ellipse",
                                    upper = "number",
                                    tl.pos = "lt",
                                    diag = "l",
                                    tl.col = "black",
                                    tl.cex = 0.5,
                                    sig.level = 0.05,
                                    na.label = "NA"
        )
        
        return(corr_plot)
    })
    
    # Hierarchical clustering - cluster heatmap
    output$hiera_cluster_heatmap <- plotly::renderPlotly({
        cluster_df <- eastkalimantan_podes[,input$hiera_var]
        cluster_df <- st_set_geometry(cluster_df, NULL) 
        row.names(cluster_df) <- paste("District:", eastkalimantan_podes$DISTRICT,",", "Village:", eastkalimantan_podes$VILLAGE)
        
        # Normalize df using Min-Max standardization method
        cluster_df_mat <- data.matrix(cluster_df)
        heatmap <- heatmaply(normalize(cluster_df_mat),
                             Colv=NA,
                             hclust_method = "ward.D",
                             seriate = "OLO",
                             colors = Blues,
                             k_row = 7,
                             margins = c(NA,200,60,NA),
                             fontsize_row = 2,
                             fontsize_col = 5)
        #main="Geographic Segmentation of East Kalimantan by Socio-economic Indicators")
        return(heatmap)
    })
    
    # SKATER - cluster heatmap
    output$skater_cluster_heatmap <- plotly::renderPlotly({
        cluster_df <- eastkalimantan_podes[,input$skater_var]
        cluster_df <- st_set_geometry(cluster_df, NULL) 
        row.names(cluster_df) <- paste("District:", eastkalimantan_podes$DISTRICT,",", "Village:", eastkalimantan_podes$VILLAGE)
        
        # Normalize df using Min-Max standardization method
        cluster_df_mat <- data.matrix(cluster_df)
        heatmap <- heatmaply(normalize(cluster_df_mat),
                             Colv=NA,
                             hclust_method = "ward.D",
                             seriate = "OLO",
                             colors = Blues,
                             k_row = 7,
                             margins = c(NA,200,60,NA),
                             fontsize_row = 2,
                             fontsize_col = 5)
        #main="Geographic Segmentation of East Kalimantan by Socio-economic Indicators")
        return(heatmap)
    })
    
    # SKATER - correlation plot
    output$skater_corr_plot <- renderPlot({
        cluster_df <- eastkalimantan_podes[,input$skater_var]
        cluster_df <- st_set_geometry(cluster_df, NULL) 
        #row.names(cluster_df) <- eastkalimantan_podes$REGION_CODE
        
        # Normalize df using Min-Max standardization method
        cluster_df <- normalize(cluster_df)
        
        var_cor <- cor(cluster_df)
        corr_plot <- corrplot.mixed(var_cor,
                                    lower = "ellipse",
                                    upper = "number",
                                    tl.pos = "lt",
                                    diag = "l",
                                    tl.col = "black",
                                    tl.cex = 0.5,
                                    sig.level = 0.05,
                                    na.label = "NA"
        )
        
        return(corr_plot)
    })
    
    # SKATER gap statistic graph (for number of clusters)
    output$skater_cluster_graph <- renderPlot({
        
        cluster_df <- eastkalimantan_podes[,input$skater_var]
        cluster_df <- st_set_geometry(cluster_df, NULL) 
        #row.names(cluster_df) <- eastkalimantan_podes$REGION_CODE
        
        # Normalize df using Min-Max standardization method
        cluster_df <- normalize(cluster_df)
        # print(cluster_df)
        # Suggested number of clusters using gap statistic method
        set.seed(12345)
        gap_stat <- clusGap(cluster_df, FUN = hcut, nstart = 20, K.max = 10, B =50)
        
        # print(gap_stat, method = "firstmax")
        cluster_graph <- fviz_gap_stat(gap_stat)
        
        return(cluster_graph)
        
    })
    
    # SKATER clustering map
    output$skater_cluster_map <- leaflet::renderLeaflet({
        cluster_df <- eastkalimantan_podes[,input$skater_var]
        cluster_df <- st_set_geometry(cluster_df, NULL) 
        # row.names(cluster_df) <- eastkalimantan_podes$REGION_CODE
        
        # Generate neighbour list
        # st_zm needed since GIS data is in multipolygons
        ekps_sp <- as_Spatial(st_zm(eastkalimantan_podes))
        ekps.nb <- poly2nb(ekps_sp, queen = TRUE)
        
        # Resolve any no-neighbour nodes
        cnt <- card(ekps.nb)
        island_ind <- match(0, cnt) #returns the node with 0 neighbours 
        nearest <- knearneigh(coordinates(ekps_sp))$nn
        ekps.nb[[island_ind]] <- nearest[island_ind]
        
        # We need to add this island to the neighbors of its connected municipality as well
        ekps.nb[[nearest[island_ind]]] <- c(as.integer(island_ind), as.integer(ekps.nb[[nearest[island_ind]]]))
        
        
        # Generate Weights
        lcosts <- nbcosts(ekps.nb, cluster_df)
        # We need to specify the style as B to make sure the cost values are not row-standardized
        ekps.weights <- nb2listw(ekps.nb, lcosts, style="B")
        #summary(ekps.w)
        
        # Compute Minimum Spanning Tree
        ekps.mst <- mstree(ekps.weights)
        print(dim(ekps.mst)) # check if the mst has the correct number of edges
        
        # Plot MST
        # plot(ekps_sp, border = gray(0.5))
        # plot.mst(ekps.mst, coordinates(ekps_sp),
        #          col="blue", cex.lab=0.3, cex.circles=0.005, add=TRUE)
        # 
        
        # Use Skater function to create spatially constrained clusters
        clust <- skater(ekps.mst[,1:2], 
                        cluster_df, 
                        method = input$skater_method, 
                        input$skater_no_cluster-1)
        
        # Plot SKATER MST
        # plot(ekps_sp, border = gray(0.5))
        # plot.mst(clust, coordinates(area_sp),
        #          groups.colors=c("red","green","purple", "yellow", "blue", "brown", "pink", "light blue", "orange"),
        #          cex.lab=0.3, cex.circles=0.005, add=TRUE)
        
        # Combine the cluster groups to the original dataframe
        groups_mat <- as.matrix(clust$groups)
        new_podes <- cbind(eastkalimantan_podes, as.factor(groups_mat)) %>%
            rename(`SP_CLUSTER`=`as.factor.groups_mat.`)
        
        
        cluster_plot <- tm_shape(new_podes) +
            tm_fill(col = "SP_CLUSTER",
                    id = "VILLAGE",
                    popup.vars = c("Village" = "VILLAGE", 
                                   "District" = "DISTRICT", 
                                   "City" = "CITY",
                                   "Cluster" = "SP_CLUSTER"),
                    legend.show = TRUE,
                    alpha = 0.9) +
            tm_borders(lwd = 1)
        
        tmap_leaflet(cluster_plot, in.shiny = TRUE)
        
    })
    
    # ClustGeo gap statistic graph (for number of clusters)
    output$clustgeo_cluster_graph <- renderPlot({
        cluster_df <- eastkalimantan_podes[,input$clustgeo_var]
        cluster_df <- st_set_geometry(cluster_df, NULL) 
        #row.names(cluster_df) <- eastkalimantan_podes$REGION_CODE
        
        # Normalize df using Min-Max standardization method
        cluster_df <- normalize(cluster_df)
        set.seed(12345)
        gap_stat <- clusGap(cluster_df, FUN = hcut, nstart = 20, K.max = 10, B =50)
        
        # print(gap_stat, method = "firstmax")
        cluster_graph <- fviz_gap_stat(gap_stat)
        
        return(cluster_graph)
    })
    
    # Clustgeo - cluster heatmap
    output$clustgeo_cluster_heatmap <- plotly::renderPlotly({
        cluster_df <- eastkalimantan_podes[,input$clustgeo_var]
        cluster_df <- st_set_geometry(cluster_df, NULL) 
        row.names(cluster_df) <- paste("District:", eastkalimantan_podes$DISTRICT,",", "Village:", eastkalimantan_podes$VILLAGE)
        
        # Normalize df using Min-Max standardization method
        cluster_df_mat <- data.matrix(cluster_df)
        heatmap <- heatmaply(normalize(cluster_df_mat),
                             Colv=NA,
                             hclust_method = "ward.D",
                             seriate = "OLO",
                             colors = Blues,
                             k_row = 7,
                             margins = c(NA,200,60,NA),
                             fontsize_row = 2,
                             fontsize_col = 5)
        #main="Geographic Segmentation of East Kalimantan by Socio-economic Indicators")
        return(heatmap)
    })
    
    
    # Clustgeo - correlation plot
    output$clustgeo_corr_plot <- renderPlot({
        cluster_df <- eastkalimantan_podes[,input$clustgeo_var]
        cluster_df <- st_set_geometry(cluster_df, NULL) 
        #row.names(cluster_df) <- eastkalimantan_podes$REGION_CODE
        
        # Normalize df using Min-Max standardization method
        cluster_df <- normalize(cluster_df)
        
        var_cor <- cor(cluster_df)
        corr_plot <- corrplot.mixed(var_cor,
                                    lower = "ellipse",
                                    upper = "number",
                                    tl.pos = "lt",
                                    diag = "l",
                                    tl.col = "black",
                                    tl.cex = 0.5,
                                    sig.level = 0.05,
                                    na.label = "NA"
        )
        
        return(corr_plot)
    })
    
    # Clustgeo clustering map
    output$clustgeo_cluster_map <- leaflet::renderLeaflet({
        D0 <- eastkalimantan_podes[,input$clustgeo_var]
        D0 <- st_set_geometry(D0, NULL) 
        # row.names(cluster_df) <- eastkalimantan_podes$REGION_CODE
        # row.names(cluster_df) <- paste("District:", eastkalimantan_podes$DISTRICT,",", "Village:", eastkalimantan_podes$VILLAGE)
        
        # Normalize df using Min-Max standardization method
        D0 <- normalize(D0)
        
        # Deriving D0 - euclidean distance matrix
        D0 <- dist(D0, method = "euclidean") # returns d0 distance matrix
        # tree <- hclustgeo(D0)
        # Plot D0 dendogram/tree
        # plot(tree,hang=-1,label=FALSE, xlab="",sub="",
        #     main="Ward dendrogram with D0 only",cex.main=0.8,cex=0.8,cex.axis=0.8,cex.lab=0.8)
        # rect.hclust(tree,k=7,border=c(4,5,3,2,1,6,7))
        # legend("topright", legend= paste("cluster",1:7), fill=1:7, cex=0.8,bty="n",border="white")
        
        # Plot D0 on map
        # P7 <- cutree(tree, 7)
        # eastkalimantan_podes$cgd0 <- P7
        # qtm(eastkalimantan_podes, "cgd0")
        
        # The clusters appear geographically fragmented so we will introduce 
        # the geographical distances between VILLAGEs into the clustering method
        
        # Deriving D1 - geographical distance matrix
        # D1 matrix obtained by extracting coordinates of VILLAGE
        # Convert map to latlong projection instead of UTM
        map_ll <- spTransform(as_Spatial(st_zm(eastkalimantan_podes)), CRS("+proj=longlat +datum=WGS84"))
        
        # Extract coordinates of VILLAGE
        coords <- coordinates(map_ll)
        row.names(coords) <- map_ll$VILLAGE
        colnames(coords) <- c("lon", "lat")
        D1 <- geodist(coords, measure = "vincenty") # vincenty measure to reduce error as distance gets beyond 100km
        # D1 <- D1/1000 # convert from m to km
        D1 <- as.dist(D1)
        
        
        # This performs the geoclustering. D0 is the created matrix from getD0(), D1 is a constant; which can either be D1 (from above) or D1_n (if we take into account neighbours for the geoclustering)
        # geoclustering <- function(D0, D1, alpha, K) {
        #   tree <- hclustgeo(D0, D1, alpha)
        #   clust <- cutree(tree, K)
        #   eastkalimantan_podes$`GEO_CLUSTER` <- as.factor(clust)
        #   
        #   #eastkalimantan_podes <- cbind(eastkalimantan_podes, as.factor(clust)) 
        #   return(eastkalimantan_podes)
        # }
        
        tree <- hclustgeo(D0, D1, input$clustgeo_alpha)
        geo_clust <- cutree(tree, input$clustgeo_no_cluster)
        eastkalimantan_podes <- eastkalimantan_podes %>%
            mutate(GEO_CLUSTER = as.factor(geo_clust))
        
        cluster_plot <- tm_shape(eastkalimantan_podes) +
            tm_fill(col = "GEO_CLUSTER",
                    id = "VILLAGE",
                    popup.vars = c("Village" = "VILLAGE", 
                                   "District" = "DISTRICT", 
                                   "City" = "CITY",
                                   "Cluster" = "GEO_CLUSTER"),
                    legend.show = TRUE,
                    alpha = 0.9) +
            tm_borders(lwd = 1)
        
        tmap_leaflet(cluster_plot, in.shiny = TRUE)
        
    })
    
    # Clustgeo alpha plot
    output$clustgeo_sugg_alpha <- renderPlot({
        D0 <- eastkalimantan_podes[,input$clustgeo_var]
        D0 <- st_set_geometry(D0, NULL) 
        D0 <- normalize(D0)
        
        # Deriving D0 - euclidean distance matrix
        D0 <- dist(D0, method = "euclidean")
        map_ll <- spTransform(as_Spatial(st_zm(eastkalimantan_podes)), CRS("+proj=longlat +datum=WGS84"))
        
        # Extract coordinates of VILLAGE
        coords <- coordinates(map_ll)
        row.names(coords) <- map_ll$VILLAGE
        colnames(coords) <- c("lon", "lat")
        D1 <- geodist(coords, measure = "vincenty") # vincenty measure to reduce error as distance gets beyond 100km
        # D1 <- D1/1000 # convert from m to km
        D1 <- as.dist(D1)
        
        # This function plots the graph to pick the best alpha parameter. D1 is the distance matrix (real geographical distance)
        # alpha_geo <- function(D0,D1,K) {
        #   range.alpha <- seq(0,1,0.05)
        #   cr <- choicealpha(D0, D1, range.alpha, K, graph = TRUE)
        #   print(cr$Q)
        #   print(cr$Qnorm)
        # }
        range.alpha <- seq(0,1,0.05)
        cr <- choicealpha(D0, D1, range.alpha, input$clustgeo_no_cluster, graph = TRUE)
        #return(plot.choicealpha(cr, cex = 0.8, xlab = "alpha value/mixing parameter"))
        return(cr)
    })
    
    observeEvent(input$hiera_corr_plot_help, {
        toggle("corr_plot_text_hiera")
    })
    
    observeEvent(input$skater_corr_plot_help, {
        toggle("corr_plot_text_skater")
    })
    
    observeEvent(input$clustgeo_corr_plot_help, {
        toggle("corr_plot_text_clustgeo")
    })
    
    
    # DT data table 
    output$podes <- DT::renderDataTable({
        data <- eastkalimantan_podes %>%
            select(PROVINCE, CITY, DISTRICT, VILLAGE, 
                   STATUS_HEAD_OFFICE, CONDITION_HEAD_OFFICE, LOCATION_HEAD_OFFICE, 
                   VILLAGE_ADJACENT_TO_SEA,
                   PRESENCE_MANGROVE_TREES, VILLAGE_LOCATION_TO_FOREST, FOREST_FUNCTION,
                   VILLAGE_MAIN_INCOME, TYPE_OF_COMMODITIES, STREET_LIGHTING,
                   ELECTRICITY_PROVIDER, FUEL_USED_OFTEN, TRASH_DUMP_USED_OFTEN,
                   TOILET_FACILITIES, TOILET_DISPOSAL_SITE, SOURCE_DRINKING_WATER,
                   PRESENCE_POWER_LINES, NUM_OF_SLUMS, SOURCE_WATER_POLLUTION,
                   SOURCE_LAND_POLLUTION, SOURCE_AIR_POLLUTION, PLANTING_ENVIRONMENTAL_CONSERVATION,
                   RECYCLE_ENVIRONMENTAL_CONSERVATION, SLASH_AND_BURN
            )
        DT::datatable(data = data,
                      options = list(pageLength = 10),
                      rownames = FALSE)
    })
    
    # About news image
    output$news_map <- renderImage({
        width <- session$clientData$output_news_map_width
        height <- session$clientData$output_news_map_height
        
        list(
            src = "www/news.jpg",
            contentType = "image/png",
            width = width,
            height = height,
            alt = "New Proposed Capital City of Indonesia"
        )
    }, deleteFile = FALSE)
    
    # About blogdown text
    output$blogdown <- renderUI({
        div(
            h4("Please check out our R blogdown page for the full report", style = "display: inline;"),
            h4(tags$a(href = "https://www.google.com/", "here."), style = "display: inline;")
        )
    })
    
    # About logo 
    output$logo <- renderImage({
        width <- (session$clientData$output_news_map_width)
        height <- (session$clientData$output_news_map_height)
        
        list(
            src = "www/logo_john.jpg",
            contentType = "image/png",
            width = width,
            height = height,
            alt = "Logo"
        )
    })
    
    # About SMU logo
    output$smu_logo <- renderImage({
        width <- 0.38*(session$clientData$output_news_map_width)
        height <- 0.32*(session$clientData$output_news_map_height)
        
        list(
            src = "www/smu_logo.png",
            contentType = "image/png",
            width = width,
            height = height,
            alt = "SMU Logo"
        )
    }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
#rsconnect::deployApp()
