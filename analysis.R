library(dplyr)
library(plotly)
library(tidyr)
library(stringr)
library(countrycode)
library(ggplot2)

# Load and cleanup data ----

# CO2 and GHG dataset from
# https://www.kaggle.com/srikantsahu/co2-and-ghg-emission-data/download

data <- read.csv('./emission data.csv', stringsAsFactors = FALSE)
colnames(data)[-1] <- str_replace(colnames(data)[-1], 'X', '')
data <- gather(data, key = 'Year', value = 'GHG.Emissions.Tonnes', -Country)
data <- mutate(data,
               Year = as.numeric(Year),
               GHG.Emissions.Tonnes = as.numeric(GHG.Emissions.Tonnes),
               region = countrycode(Country, 'country.name', 'region'),
               continent = countrycode(Country, 'country.name', 'continent'))
data <- filter(data, !(Country %in% c('World', 'Asia and Pacific (other)', 'Americas (other)', 'EU-28', 'Europe (other)')))

# Visualize data with geom_point ----

ggplot(data, aes(x = Year, y = GHG.Emissions.Tonnes)) +
  geom_point() +
  ggtitle('GHG Emissions (Tonnes) over time')

# dplyr + ggplot for visualization ----

data %>% 
  filter(Country %in% c('United States', 'China', 'India', 'South Africa', 'Somalia', 'Niger')) %>% 
  ggplot(aes(x = Year, y = GHG.Emissions.Tonnes, color = Country)) +
  geom_point() +
  ggtitle('GHG Emissions (Tonnes) over time by country')

# Visualize data with geom_line ----

data %>% 
  filter(Country %in% c('United States', 'China', 'India', 'South Africa', 'Somalia', 'Niger')) %>% 
  ggplot(aes(x = Year, y = GHG.Emissions.Tonnes, color = Country)) +
  geom_line() +
  ggtitle('GHG Emissions (Tonnes) over time by country')

# Visualize data with multiple layers: geom_point and geom_line ----

data %>% 
  filter(Country %in% c('United States', 'China', 'India', 'South Africa', 'Somalia', 'Niger')) %>% 
  ggplot(aes(x = Year, y = GHG.Emissions.Tonnes, color = Country)) +
  geom_point() +
  geom_line() +
  ggtitle('GHG Emissions (Tonnes) over time by country')

# Visualize data with geom_point and facets ----

data %>% 
  filter(!is.na(continent)) %>% 
  ggplot(aes(x = Year, y = GHG.Emissions.Tonnes)) +
  geom_point() +
  facet_wrap(~ continent) +
  ggtitle('GHG Emissions (Tonnes) over time by continent')

# Visualize data with histogram ----

data %>% 
  filter(region == 'South-Eastern Asia', Year == 2016) %>% 
  ggplot(aes(x = Country, y = GHG.Emissions.Tonnes)) +
  geom_histogram(stat = 'identity') +
  ggtitle('GHG Emissions (Tonnes) by country in SE Asia (2016)')

# Visualize data with pie chart ----

data %>% 
  filter(region == 'South-Eastern Asia', Year == 2016) %>% 
  ggplot(aes(x = '', y = Country)) +
  geom_bar(aes(fill = GHG.Emissions.Tonnes), width = 1, stat = 'identity') +
  coord_polar('y', start = 0) +
  ggtitle('GHG Emissions (Tonnes) by country in SE Asia (2016)')


# Visualize pie chart interactively ----
# Note: we do not have to filter, but I am filtering because
# ggplotly takes a while with huge datasets :)

line_plot <- data %>% 
  filter(Year > 2005) %>% 
  ggplot(aes(x = Year, y = GHG.Emissions.Tonnes, color = Country)) +
  geom_point() +
  geom_line() +
  ggtitle('GHG Emissions (Tonnes) over time by country')

ggplotly(line_plot)
