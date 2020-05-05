# This is the R file to use google api to plot the latitutde and longitude of all the tobacco retailers

# Dependencies
library('readxl')
require('plyr')
require('sas7bdat')
require('tidyr')
require('sf')
require('mapview')
require('lubridate')
require('ggplot2')
require('tidyr')
require('ggmap')

foil_2007_2015  <- read_excel('tobacco data/FOIL Case No. 83-2017_List of Cigarette Retail Dealers from 2007 to 2015.xlsx')

foil_2007_2015 <-  foil_2007_2015  %>% mutate (Year = year(foil_2007_2015$CREATE_DATE)) 
foil_2010_2015 <- subset(foil_2007_2015, Year %in% c(2010,2011,2012,2013,2014,2015))
foil_2010_2015$BORO <- toupper(foil_2010_2015$BORO)
foil_2010_2015 <- subset(foil_2010_2015 , BORO %in% c('BRONX', 'BROOKLYN', 'MANHATTAN', 'QUEENS', 'STATEN ISLAND'))
foil_2010_2015 <- subset(foil_2010_2015, STATE == 'NY')
foil_2010_2015$CITY[which(foil_2010_2015$CITY == "BAYSIDE HILLS")] = "BAYSIDE"
foil_2010_2015$CITY[which(foil_2010_2015$CITY == "CAMBRIA HTS")] = "CAMBRIA HEIGHTS"
foil_2010_2015 <- unite(foil_2010_2015,"address", c(BUILDING, STREET, CITY, STATE, ZIP), sep = " ")
foil_2010_2015$STREET_2 <- NULL
foil_2010_2015 <-  foil_2010_2015  %>% mutate (Year2 = year(foil_2010_2015$EXPIRATION_DATE)) 
foil_2011 <- subset(foil_2010_2015, Year == 2011)

register_google(key = 'AIzaSyA8y8FgxCH4WamYldyyBZyIg1TFUTLGUo0')

manhattan2011 <- subset(foil_2011, BORO == 'MANHATTAN')
geocode_manhattan2011 <- geocode(manhattan2011$address)

brooklyn2011 <- subset(foil_2011, BORO == 'BROOKLYN')
geocode_brooklyn2011 <- geocode(brooklyn2011$address)

bronx2011 <- subset(foil_2011, BORO == 'BRONX')
gecode_bronx2011 <- geocode(bronx2011$address)

queens2011 <- subset(foil_2011, BORO == 'QUEENS')
geocode_queens2011 <- geocode(queens2011$address)

statenisland2011 <- subset(foil_2011, BORO == 'STATEN ISLAND')
geocode_statenisland2011 <- geocode(statenisland2011$address)

foil_2015 <- subset(foil_2010_2015, Year == 2015)
manhattan2015 <- subset(foil_2015, BORO == 'MANHATTAN')
geocode_manhattan2015 <- geocode(manhattan2015$address)

brooklyn2015 <- subset(foil_2015, BORO == 'BROOKLYN')
geocode_brooklyn2015 <- geocode(brooklyn2015$address)

bronx2015 <- subset(foil_2015, BORO == 'BRONX')
gecode_bronx2015 <- geocode(bronx2015$address)

queens2015 <- subset(foil_2015, BORO == 'QUEENS')
geocode_queens2015 <- geocode(queens2015$address)





```{r}
statenisland2015 <- subset(foil_2015, BORO == 'STATEN ISLAND')
```


```{r}
geocode_statenisland2015 <- geocode(statenisland2015$address)
```


```{r}
as.data.frame(rbind(geocode_brooklyn2011,geocode_manhattan2011))
```

geocode_manhattan2011 <- read.csv('tobacco data/manhattan2011.csv')
geocode_manhattan2015 <- read.csv('tobacco data/manhattan2015.csv')
geocode_brooklyn2011 <- read.csv('tobacco data/brooklyn2011.csv')
geocode_brooklyn2015 <- read.csv('tobacco data/brooklyn2015.csv')
geocode_queens2011 <- read.csv('tobacco data/queens2011.csv')
geocode_queens2015 <- read.csv('tobacco data/queens2015.csv')
geocode_statenisland2011  <- read.csv('tobacco data/statenisland2011.csv')
geocode_statenisland2015 <- read.csv('tobacco data/statenisland2015.csv')
gecode_bronx2011 <-  read.csv('tobacco data/bronx2011.csv')
gecode_bronx2015 <- read.csv('tobacco data/bronx2015.csv')

manhattan_map <- get_map(location = c(lon = -73.96625, lat = 40.78343), maptype = "terrain", zoom = 11)
p_manhattan = ggmap(manhattan_map)
p_manhattan + geom_point(aes(x = lon, y = lat), data = geocode_manhattan2011, size = 0.1) + 
  theme(legend.position="bottom")
p_manhattan + geom_point(aes(x = lon, y = lat), data = geocode_manhattan2015, size = 0.1) + 
  theme(legend.position="bottom")



brooklyn_map <- get_map(location = c(lon = -73.9442, lat = 40.6782 ), maptype = "terrain", zoom = 11)
p_brooklyn = ggmap(brooklyn_map)
p_brooklyn + geom_point(aes(x = lon, y = lat), data = geocode_brooklyn2011, size = 0.1) + 
  theme(legend.position="bottom")
p_brooklyn + geom_point(aes(x = lon, y = lat), data = geocode_brooklyn2015, size = 0.1) + 
  theme(legend.position="bottom")



queens_map <- get_map(location = c(lon = -73.7949, lat = 40.7282), maptype = "terrain", zoom = 11)
p_queens = ggmap(queens_map)

p_queens + geom_point(aes(x = lon, y = lat), data = geocode_queens2011, size = 0.1) + 
  theme(legend.position="bottom")
p_queens + geom_point(aes(x = lon, y = lat), data = geocode_queens2015, size = 0.1) + 
  theme(legend.position="bottom")



statenisland_map <-  get_map(location = c(lon = -74.1502, lat = 40.5795), maptype = "terrain", zoom = 12)
p_statenisland = ggmap(statenisland_map)

p_statenisland + geom_point(aes(x = lon, y = lat), data = geocode_statenisland2011, size = 0.1) + 
  theme(legend.position="bottom")
p_statenisland + geom_point(aes(x = lon, y = lat), data = geocode_statenisland2015, size = 0.1) + 
  theme(legend.position="bottom")


bronx_map <- get_map(location = c(lon = -73.8648, lat = 40.8448), maptype = "terrain", zoom = 12)
p_bronx = ggmap(bronx_map)

p_bronx + geom_point(aes(x = lon, y = lat), data = gecode_bronx2011, size = 0.1) + 
  theme(legend.position="bottom")
p_bronx + geom_point(aes(x = lon, y = lat), data = gecode_bronx2015, size = 0.1) + 
  theme(legend.position="bottom")
  
  
  

