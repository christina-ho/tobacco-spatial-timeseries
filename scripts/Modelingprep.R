# Read in Retailer dataset
ny2011 <- read.csv('census data/ny2011.csv')
ny2015 <- read.csv('census data/ny2015.csv')

# Create variable count in  ny2011 data set to count number of tobacco retailers per tract
ny2011 <- ddply(ny2011,.(census_tract),transform,count2011=length(census_tract))

# Load in 5 year estimates cencus data for years 2007 - 2011 with tracts that have median income less than the average median income for all tracts
acs_2011_low <- read_csv('acs_2011_low.csv')

# Change name of column GEOID to cencus_tract (consistent with ny 2011 data set) to combine the two data sets
acs_2011_low <- acs_2011_low %>% mutate( census_tract = GEOID   ) %>% select(-GEOID)

# Create a data set ny2011_ that only contains distincts rows (each row representing a distinct tract) and count of retailers per tract
ny2011_ <-  ny2011 %>% select(census_tract, count2011) %>% distinct()

# Create a new data set called data2011 that joins cencus data with ny2011_ by the variable census tract
data2011 <- left_join(acs_2011_low,ny2011_, by = "census_tract")

# Drop missing values for the variable count i.e. drop all rows for tracts that have no tobacco retailers
data2011 <- data2011 %>%  drop_na(count2011)

# Values range from 1 - 8
table(data2011$count2011)

# Create variable count in  ny2015 data set to count number of tobacco retailers per tract
ny2015 <- ddply(ny2015,.(census_tract),transform,count2015=length(census_tract))

# Create a data set ny2015_ that only contains distincts rows (each row representing a distinct tract) and count of retailers per tract
ny2015_ <-  ny2015 %>% select(census_tract, count2015) %>% distinct()
acs_2015 <- read_csv('census_2011_2015.csv')

# Change name of column GEOID to cencus_tract (consistent with ny 2015 data set) to combine the two data sets
acs_2015 <- acs_2015 %>% mutate( census_tract = GEOID) %>% select(-GEOID)

# Create final data set that joins subsetted data for 2011 with corresponding tract information for 2015
data_final <- left_join(data2011, ny2015_, by = "census_tract")

# Replace missing values in count2015 with 0
data_final$count2015 <-   data_final$count2015 %>% replace_na(0)
colnames(data_final) <- c("NAME","totpop2011","propwhite2011" , "medincome2011" , "propdegree2011",  "geometry" ,    "census_tract", "count2011", "count2015")
colnames(acs_2015) <- c("NAME","totpop2015","propwhite2015" , "medincome2015" , "propdegree2015",  "geometry" ,    "census_tract")

data_final <- left_join(data_final, acs_2015, by = "census_tract") 