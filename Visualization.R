library(ggplot2)
library(readr)
library(leaflet)
library(knitr)
library(highcharter)
library(plyr)
library(dplyr)
library(tidyr)
library(viridis)
library(plotly)
library(tidyr)
library(tidygraph)
library(maps)
library(ggmap)
library(gridExtra)
library(countrycode)
library(stringr)
library(tidyverse)
library(tidygraph)
library(geosphere)
library(maptools)
library(networkD3)
library(d3Network)

setwd ("your path")

TS <- read.csv("time_series.csv", sep=";",header = T)

AS <- read.csv("asylum_seekers_monthly.csv", sep=";",header = T)

# converting to numeric and removing NA values
TS$Value<-as.numeric(as.character(TS$Value))
TS<- na.omit(TS)

AS$Value<-as.numeric(as.character(AS$Value))
AS<- na.omit(AS)

kable((TS[1:10,]))
kable((AS[1:10,]))
glimpse(AS)

### Visualizing Number Refugees by Origin ### 

TS %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value, na.rm = T)) -> TS_byorg

TS_byorg<- arrange(TS_byorg, -Total)

hchart(TS_byorg[1:20,], "column", hcaes('Origin', y = Total, color = Total)) %>%
  hc_title(text = "Where do Refugees come from (1951 - 2016)") %>%
  hc_credits(enabled = TRUE, text = "Data Source: UNHCR ", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)

# Visualizing Refugees Migration Trend from 1951 to 2016  

TS %>% 
  select(Year,Value) %>% 
  group_by(Year) %>% 
  summarise(Total = sum(Value)) ->Total_Year

hchart(Total_Year, "line", hcaes(x = Year, y = Total)) %>%
  hc_title(text = "Total Number of Refugees per Year (1951 - 2016) ") %>%
  hc_credits(enabled = TRUE, text = "Data Source: UNHCR ", style = list(fontSize = "12px")) %>%
  hc_legend(enabled = FALSE)

### Map of Refugees per Country (1999)

#Selecting year to build the map
AS %>% 
  filter(Year == 1999) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country1999

#Matching the country name with country code
names(AS_country1999) <- c("country.name", "total")
country.name <- as.character(AS_country1999$country.name)
AS_country1999$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")

#building the map
l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm1999<-AS_country1999
names(wm1999) <- c("COUNTRY","TOTAL" ,"CODE")
wm1999$TOTAL<-as.numeric(as.character(wm1999$TOTAL))

plot_geo(wm1999) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (1999)",
    geo = g
  )


### Map of Refugees per Country (2000)

AS %>% 
  filter(Year == 2000) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2000

names(AS_country2000) <- c("country.name", "total")
country.name <- as.character(AS_country2000$country.name)
AS_country2000$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2000<-AS_country2000
names(wm2000) <- c("COUNTRY","TOTAL" ,"CODE")
wm2000$TOTAL<-as.numeric(as.character(wm2000$TOTAL))

plot_geo(wm2000) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2000)",
    geo = g
  )

### Map of Refugees per Country (2001)

AS %>% 
  filter(Year == 2001) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2001

names(AS_country2001) <- c("country.name", "total")
country.name <- as.character(AS_country2001$country.name)
AS_country2001$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2001<-AS_country2001
names(wm2001) <- c("COUNTRY","TOTAL" ,"CODE")
wm2001$TOTAL<-as.numeric(as.character(wm2001$TOTAL))

plot_geo(wm2001) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2001)",
    geo = g
  )

### Map of Refugees per Country (2002)

AS %>% 
  filter(Year == 2002) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2002

names(AS_country2002) <- c("country.name", "total")
country.name <- as.character(AS_country2002$country.name)
AS_country2002$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2002<-AS_country2002
names(wm2001) <- c("COUNTRY","TOTAL" ,"CODE")
wm2002$TOTAL<-as.numeric(as.character(wm2002$TOTAL))

plot_geo(wm2002) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2002)",
    geo = g
  )

### Map of Refugees per Country (2003)

AS %>% 
  filter(Year == 2003) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2003

names(AS_country2003) <- c("country.name", "total")
country.name <- as.character(AS_country2003$country.name)
AS_country2003$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2003<-AS_country2003
names(wm2003) <- c("COUNTRY","TOTAL" ,"CODE")
wm2003$TOTAL<-as.numeric(as.character(wm2003$TOTAL))

plot_geo(wm2003) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2003)",
    geo = g
  )

### Map of Refugees per Country (2004)

AS %>% 
  filter(Year == 2004) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2004

names(AS_country2004) <- c("country.name", "total")
country.name <- as.character(AS_country2004$country.name)
AS_country2004$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2004<-AS_country2004
names(wm2004) <- c("COUNTRY","TOTAL" ,"CODE")
wm2004$TOTAL<-as.numeric(as.character(wm2004$TOTAL))

plot_geo(wm2004) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2004)",
    geo = g
  )

### Map of Refugees per Country (2005)

AS %>% 
  filter(Year == 2005) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2005

names(AS_country2005) <- c("country.name", "total")
country.name <- as.character(AS_country2005$country.name)
AS_country2005$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2005<-AS_country2005
names(wm2005) <- c("COUNTRY","TOTAL" ,"CODE")
wm2005$TOTAL<-as.numeric(as.character(wm2005$TOTAL))

plot_geo(wm2005) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2005)",
    geo = g
  )

### Map of Refugees per Country (2006)

AS %>% 
  filter(Year == 2006) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2006

names(AS_country2006) <- c("country.name", "total")
country.name <- as.character(AS_country2006$country.name)
AS_country2006$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2006<-AS_country2006
names(wm2006) <- c("COUNTRY","TOTAL" ,"CODE")
wm2006$TOTAL<-as.numeric(as.character(wm2006$TOTAL))

plot_geo(wm2006) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2006)",
    geo = g
  )

### Map of Refugees per Country (2007)

AS %>% 
  filter(Year == 2007) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2007

names(AS_country2007) <- c("country.name", "total")
country.name <- as.character(AS_country2007$country.name)
AS_country2007$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2007<-AS_country2007
names(wm2007) <- c("COUNTRY","TOTAL" ,"CODE")
wm2007$TOTAL<-as.numeric(as.character(wm2007$TOTAL))

plot_geo(wm2007) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2007)",
    geo = g
  )

### Map of Refugees per Country (2008)

AS %>% 
  filter(Year == 2008) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2008

names(AS_country2008) <- c("country.name", "total")
country.name <- as.character(AS_country2008$country.name)
AS_country2008$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2008<-AS_country2008
names(wm2008) <- c("COUNTRY","TOTAL" ,"CODE")
wm2008$TOTAL<-as.numeric(as.character(wm2008$TOTAL))

plot_geo(wm2008) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2008)",
    geo = g
  )

### Map of Refugees per Country (2009)

AS %>% 
  filter(Year == 2009) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2009

names(AS_country2009) <- c("country.name", "total")
country.name <- as.character(AS_country2009$country.name)
AS_country2009$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2009<-AS_country2009
names(wm2009) <- c("COUNTRY","TOTAL" ,"CODE")
wm2009$TOTAL<-as.numeric(as.character(wm2009$TOTAL))

plot_geo(wm2009) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2009)",
    geo = g
  )

### Map of Refugees per Country (2010)

AS %>% 
  filter(Year == 2010) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2010

names(AS_country2010) <- c("country.name", "total")
country.name <- as.character(AS_country2010$country.name)
AS_country2010$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2010<-AS_country2010
names(wm2010) <- c("COUNTRY","TOTAL" ,"CODE")
wm2010$TOTAL<-as.numeric(as.character(wm2010$TOTAL))

plot_geo(wm2010) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2010)",
    geo = g
  )


### Map of Refugees per Country (2011)

AS %>% 
  filter(Year == 2011) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2011

names(AS_country2011) <- c("country.name", "total")
country.name <- as.character(AS_country2011$country.name)
AS_country2011$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2011<-AS_country2011
names(wm2011) <- c("COUNTRY","TOTAL" ,"CODE")
wm2011$TOTAL<-as.numeric(as.character(wm2011$TOTAL))

plot_geo(wm2011) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2011)",
    geo = g
  )

### Map of Refugees per Country (2012)

AS %>% 
  filter(Year == 2012) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2012

names(AS_country2012) <- c("country.name", "total")
country.name <- as.character(AS_country2012$country.name)
AS_country2012$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2012<-AS_country2012
names(wm2012) <- c("COUNTRY","TOTAL" ,"CODE")
wm2012$TOTAL<-as.numeric(as.character(wm2012$TOTAL))

plot_geo(wm2012) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2012)",
    geo = g
  )

### Map of Refugees per Country (2013)

AS %>% 
  filter(Year == 2013) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2013

names(AS_country2013) <- c("country.name", "total")
country.name <- as.character(AS_country2013$country.name)
AS_country2013$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2013<-AS_country2013
names(wm2013) <- c("COUNTRY","TOTAL" ,"CODE")
wm2013$TOTAL<-as.numeric(as.character(wm2013$TOTAL))

plot_geo(wm2013) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2013)",
    geo = g
  )

### Map of Refugees per Country (2014)

AS %>% 
  filter(Year == 2014) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2014

names(AS_country2014) <- c("country.name", "total")
country.name <- as.character(AS_country2014$country.name)
AS_country2014$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2014<-AS_country2014
names(wm2014) <- c("COUNTRY","TOTAL" ,"CODE")
wm2014$TOTAL<-as.numeric(as.character(wm2014$TOTAL))

plot_geo(wm2014) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2014)",
    geo = g
  )

### Map of Refugees per Country (2015)

AS %>% 
  filter(Year == 2015) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2015

names(AS_country2015) <- c("country.name", "total")
country.name <- as.character(AS_country2015$country.name)
AS_country2015$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2015<-AS_country2015
names(wm2015) <- c("COUNTRY","TOTAL" ,"CODE")
wm2015$TOTAL<-as.numeric(as.character(wm2015$TOTAL))

plot_geo(wm2015) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2015)",
    geo = g
  )

### Map of Refugees per Country (2016)

AS %>% 
  filter(Year == 2016) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2016

names(AS_country2016) <- c("country.name", "total")
country.name <- as.character(AS_country2016$country.name)
AS_country2016$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2016<-AS_country2016
names(wm2016) <- c("COUNTRY","TOTAL" ,"CODE")
wm2016$TOTAL<-as.numeric(as.character(wm2016$TOTAL))

plot_geo(wm2016) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2016)",
    geo = g
  )

### Map of Refugees per Country (2017)

AS %>% 
  filter(Year == 2017) %>% 
  select(Origin, Value) %>% 
  group_by(Origin) %>% 
  summarise(Total = sum(Value,na.rm = T)) -> AS_country2017

names(AS_country2017) <- c("country.name", "total")
country.name <- as.character(AS_country2017$country.name)
AS_country2017$iso3 <- countrycode(country.name, "country.name","iso3c")
data(worldgeojson, package = "highcharter")


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

wm2017<-AS_country2017
names(wm2017) <- c("COUNTRY","TOTAL" ,"CODE")
wm2017$TOTAL<-as.numeric(as.character(wm2017$TOTAL))

plot_geo(wm2017) %>%
  add_trace(
    z = ~TOTAL, color = ~TOTAL, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  colorbar(title = 'Number of Refugees', tickprefix = '') %>%
  layout(
    title = "Refugees' Origin (2017)",
    geo = g
  )
