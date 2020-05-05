### Census Data Analysis for NYC based on 2015 American Community Survey 5-year estimates.
## Ref: https://walkerke.github.io/tidycensus/articles/spatial-data.html

# Dependencies

library(tidycensus)
library(tidyverse)
library(mapview)
library(leaflet)
library(stringr)
library(sf)
options(tigris_use_cache = TRUE)

# API Key
# census_api_key("4a97bf83f424b28881bbcb2615511bc9c7ec2238", install=T)

# total population in NYS by county
total_pop_nys <- get_acs(year=2011,
                         geography = 'county',
                         variables = c(total_population = 'P001001'),
                         state = "NY") %>% 
  arrange(-value)
total_pop_nys

## Proportion of Whites 2007-2011 and 2011-2015
# 2007-2011
bronx_white<- get_acs(geography = "tract", variables = 'DP05_0032PE', 
                      state = "NY", county = "Bronx", geometry = TRUE,
                      summary_var = "B01001_001", year = 2011, survey = 'acs5')

kings_white <- get_acs(geography = "tract", variables = 'DP05_0032PE', 
                       state = "NY", county = "Kings", geometry = TRUE,
                       summary_var = "B01001_001", year = 2011, survey = 'acs5')

richmond_white <- get_acs(geography = "tract", variables = 'DP05_0032PE', 
                          state = "NY", county = "Richmond", geometry = TRUE,
                          summary_var = "B01001_001", year = 2011, survey = 'acs5')

newyork_white <- get_acs(geography = "tract", variables = 'DP05_0032PE', 
                         state = "NY", county = "New York", geometry = TRUE,
                         summary_var = "B01001_001", year = 2011, survey = 'acs5')

queens_white <- get_acs(geography = "tract", variables = 'DP05_0032PE', 
                        state = "NY", county = "Queens", geometry = TRUE,
                        summary_var = "B01001_001", year = 2011, survey = 'acs5')

# 2011-2015
bronx_white2 <- get_acs(geography = "tract", variables = 'DP05_0032PE', 
                        state = "NY", county = "Bronx", geometry = TRUE,
                        summary_var = "B01001_001", year = 2015, survey = 'acs5')

kings_white2 <- get_acs(geography = "tract", variables = 'DP05_0032PE', 
                        state = "NY", county = "Kings", geometry = TRUE,
                        summary_var = "B01001_001", year = 2015, survey = 'acs5')

richmond_white2 <- get_acs(geography = "tract", variables = 'DP05_0032PE', 
                           state = "NY", county = "Richmond", geometry = TRUE,
                           summary_var = "B01001_001", year = 2015, survey = 'acs5')

newyork_white2 <- get_acs(geography = "tract", variables = 'DP05_0032PE', 
                          state = "NY", county = "New York", geometry = TRUE,
                          summary_var = "B01001_001", year = 2015, survey = 'acs5')

queens_white2 <- get_acs(geography = "tract", variables = 'DP05_0032PE', 
                         state = "NY", county = "Queens", geometry = TRUE,
                         summary_var = "B01001_001", year = 2015, survey = 'acs5')

## 2011 Graphs by Borough of White Population
bronx_white %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") +
  ggtitle('Bronx 2011')
ggsave('figures/bronx_white.png')
dev.off()

kings_white %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") +
  ggtitle('Kings 2011')
ggsave('figures/kings_white.png')


richmond_white %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") +
  ggtitle('Richmond 2011')
ggsave('figures/richmond_white.png')

newyork_white %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") +
  ggtitle('New York 2011')
ggsave('figures/newyork_white.png')

queens_white %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") +
  ggtitle('Queens 2011')
ggsave('figures/queens_white.png')
```
2015 Graphs by Borough of White Population
```{r}
bronx_white2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") +
  ggtitle('Bronx 2015')
ggsave('figures/bronx_white2.png')

kings_white2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") +
  ggtitle('Kings 2015')
ggsave('figures/kings_white2.png')

richmond_white2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") +
  ggtitle('Richmond 2015')
ggsave('figures/richmond_white2.png')

newyork_white2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") +
  ggtitle('New York 2015')
ggsave('figures/ny_white2.png')

queens_white2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "magma") +
  ggtitle('Queens 2015')
ggsave('figures/queens_white2.png')

Median Income 2007-2011 and 2011-2015

# 2007-2011
bronx_income<- get_acs(geography = "tract", variables = 'DP03_0062E', 
                       state = "NY", county = "Bronx", geometry = TRUE,
                       summary_var = "B01001_001", year = 2011, survey = 'acs5')

kings_income <- get_acs(geography = "tract", variables = 'DP03_0062E', 
                        state = "NY", county = "Kings", geometry = TRUE,
                        summary_var = "B01001_001", year = 2011, survey = 'acs5')

richmond_income <- get_acs(geography = "tract", variables = 'DP03_0062E', 
                           state = "NY", county = "Richmond", geometry = TRUE,
                           summary_var = "B01001_001", year = 2011, survey = 'acs5')

newyork_income <- get_acs(geography = "tract", variables = 'DP03_0062E', 
                          state = "NY", county = "New York", geometry = TRUE,
                          summary_var = "B01001_001", year = 2011, survey = 'acs5')

queens_income <- get_acs(geography = "tract", variables = 'DP03_0062E', 
                         state = "NY", county = "Queens", geometry = TRUE,
                         summary_var = "B01001_001", year = 2011, survey = 'acs5')

# 2011-2015
bronx_income2 <- get_acs(geography = "tract", variables = 'DP03_0062E', 
                         state = "NY", county = "Bronx", geometry = TRUE,
                         summary_var = "B01001_001", year = 2015, survey = 'acs5')

kings_income2 <- get_acs(geography = "tract", variables = 'DP03_0062E', 
                         state = "NY", county = "Kings", geometry = TRUE,
                         summary_var = "B01001_001", year = 2015, survey = 'acs5')

richmond_income2 <- get_acs(geography = "tract", variables = 'DP03_0062E', 
                            state = "NY", county = "Richmond", geometry = TRUE,
                            summary_var = "B01001_001", year = 2015, survey = 'acs5')

newyork_income2 <- get_acs(geography = "tract", variables = 'DP03_0062E', 
                           state = "NY", county = "New York", geometry = TRUE,
                           summary_var = "B01001_001", year = 2015, survey = 'acs5')

queens_income2 <- get_acs(geography = "tract", variables = 'DP03_0062E', 
                          state = "NY", county = "Queens", geometry = TRUE,
                          summary_var = "B01001_001", year = 2015, survey = 'acs5')

bronx_income %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "viridis") +
  ggtitle('Bronx 2011')
ggsave('figures/bronx_income.png')

kings_income %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "viridis") +
  ggtitle('Kings 2011')
ggsave('figures/kings_income.png')

richmond_income %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "viridis") +
  ggtitle('Richmond 2011')
ggsave('figures/richmond_income.png')

newyork_income %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "viridis") +
  ggtitle('New York 2011')
ggsave('figures/ny_income.png')

queens_income %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "viridis") +
  ggtitle('Queens 2011')
ggsave('figures/queens_income.png')

bronx_income2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "viridis") +
  ggtitle('Bronx 2015')
ggsave('figures/bronx_income2.png')

kings_income2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "viridis") +
  ggtitle('Kings 2015')
ggsave('figures/kings_income2.png')

richmond_income2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "viridis") +
  ggtitle('Richmond 2015')
ggsave('figures/richmond_income2.png')

newyork_income2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "viridis") +
  ggtitle('New York 2015')
ggsave('figures/ny_income2.png')

queens_income2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "viridis") +
  ggtitle('Queens 2015')
ggsave('figures/queens_income2.png')

## School Enrollment 2007-2011 and 2011-2015
# 2007-2011
bronx_sch <- get_acs(geography = "tract", variables = 'DP02_0057PE', 
                     state = "NY", county = "Bronx", geometry = TRUE,
                     summary_var = "B01001_001", year = 2011, survey = 'acs5')

kings_sch <- get_acs(geography = "tract", variables = 'DP02_0057PE', 
                     state = "NY", county = "Kings", geometry = TRUE,
                     summary_var = "B01001_001", year = 2011, survey = 'acs5')

richmond_sch <- get_acs(geography = "tract", variables = 'DP02_0057PE', 
                        state = "NY", county = "Richmond", geometry = TRUE,
                        summary_var = "B01001_001", year = 2011, survey = 'acs5')

newyork_sch <- get_acs(geography = "tract", variables = 'DP02_0057PE', 
                       state = "NY", county = "New York", geometry = TRUE,
                       summary_var = "B01001_001", year = 2011, survey = 'acs5')

queens_sch <- get_acs(geography = "tract", variables = 'DP02_0057PE', 
                      state = "NY", county = "Queens", geometry = TRUE,
                      summary_var = "B01001_001", year = 2011, survey = 'acs5')

# 2011-2015
bronx_sch2 <- get_acs(geography = "tract", variables = 'DP02_0057PE', 
                      state = "NY", county = "Bronx", geometry = TRUE,
                      summary_var = "B01001_001", year = 2015, survey = 'acs5')

kings_sch2 <- get_acs(geography = "tract", variables = 'DP02_0057PE', 
                      state = "NY", county = "Kings", geometry = TRUE,
                      summary_var = "B01001_001", year = 2015, survey = 'acs5')

richmond_sch2 <- get_acs(geography = "tract", variables = 'DP02_0057PE', 
                         state = "NY", county = "Richmond", geometry = TRUE,
                         summary_var = "B01001_001", year = 2015, survey = 'acs5')

newyork_sch2 <- get_acs(geography = "tract", variables = 'DP02_0057PE', 
                        state = "NY", county = "New York", geometry = TRUE,
                        summary_var = "B01001_001", year = 2015, survey = 'acs5')

queens_sch2 <- get_acs(geography = "tract", variables = 'DP02_0057PE', 
                       state = "NY", county = "Queens", geometry = TRUE,
                       summary_var = "B01001_001", year = 2015, survey = 'acs5')

bronx_sch %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "cividis") +
  ggtitle('Bronx 2011')
ggsave('figures/bronx_sch.png')

kings_sch %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "cividis") +
  ggtitle('Kings 2011')
ggsave('figures/kings_sch.png')

richmond_sch %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "cividis") +
  ggtitle('Richmond 2011')
ggsave('figures/richmond_sch.png')

newyork_sch %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "cividis") +
  ggtitle('New York 2011')
ggsave('figures/ny_sch.png')

queens_sch %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "cividis") +
  ggtitle('Queens 2011')
ggsave('figures/queens_sch.png')

bronx_sch %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "cividis") +
  ggtitle('Bronx 2015')
ggsave('figures/bronx_sch2.png')

kings_sch2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "cividis") +
  ggtitle('Kings 2015')
ggsave('figures/kings_sch2.png')

richmond_sch2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "cividis")  +
  ggtitle('Richmond 2015')
ggsave('figures/rich_sch2.png')

newyork_sch2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "cividis")  +
  ggtitle('New York 2015')
ggsave('figures/ny_sch2.png')

queens_sch2 %>%
  ggplot(aes(fill = estimate)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis_c(option = "cividis")  +
  ggtitle('Queens 2015')
ggsave('figures/queens_sch2.png')

## Overall
ny <- get_acs(geography = "tract", 
              variables = "B19013_001", 
              state = "NY", 
              county = "New York", 
              geometry = TRUE,
              cb = FALSE)

#  Remove the water area from Manhattan’s Census tracts
st_erase <- function(x, y) {
  st_difference(x, st_union(st_combine(y)))
}

ny_water <- area_water("NY", "New York", class = "sf")

ny_erase <- st_erase(ny, ny_water)

mapview(ny_erase, zcol = "estimate", legend = TRUE)

## Writing to Shape Files
library(sf)
st_write(manhattan, "manhattan.shp")

## Get 5 year estimates of variables for 2007-2011 & 2011-2015
census_2007_2011 <- get_acs(geography = 'tract',
                            variables = c('DP05_0001E', # Total Population Estimate
                                          'DP05_0032PE', # Proportion of White
                                          'DP03_0062E', # Median Income in the Past 12 Months
                                          'DP02_0057PE' # Proportion of School Enrollment College/Grad School
                            ), summary_var = "B01001_001",
                            state = 'NY', year = 2011, geometry = TRUE, output = 'wide')

census_2007_2011 = census_2007_2011 %>% 
  mutate(totpop = DP05_0001E, propwhite = DP05_0032PE/100, medincome = DP03_0062E, propdegree = DP02_0057PE/100) %>% 
  select(GEOID, NAME, totpop, propwhite, medincome, propdegree, geometry)


census_2011_2015 <- get_acs(geography = "tract",
                            variables = c( 'DP05_0001E', # Total Population Estimate
                                           'DP05_0032PE', # Proportion of White
                                           'DP03_0062E', # Median Income in the Past 12 Months
                                           'DP02_0057PE' # Proportion of School Enrollment College/Grad School
                            ),summary_var = "B01001_001",
                            
                            state = 'NY', year = 2015, geometry = TRUE, output = "wide")

census_2011_2015 = census_2011_2015 %>% 
  mutate(totpop = DP05_0001E, propwhite = DP05_0032PE/100, medincome = DP03_0062E, propdegree = DP02_0057PE/100) %>% 
  select(GEOID, NAME, totpop, propwhite, medincome, propdegree, geometry)
