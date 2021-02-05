require(OECD)
library(tidyverse)
library(plyr)
# library(dplyr)
library(ggplot2)
library(gganimate)
library(ggthemes)
library(transformr)
library(scales)
library(gifski)
library(zoo)
library(lubridate)
library(png)
library(RColorBrewer)
library(AER)
library(dynlm)
library(forecast)
library(stargazer)
library(scales)
library(quantmod)
library(urca)
library(plotly)
# devtools::install_github('thomasp85/gganimate')
# devtools::install_github("ropensci/plotly")


# Get data
## GDP growth rates
dataset_list <- get_datasets()
search_dataset("Quarterly", data = dataset_list)
dataset <- "QNA"
dstruc <- get_data_structure(dataset)
str(dstruc, max.level = 1)
GDP_growth <- get_dataset("QNA", 
                  filter = c(list(slice_head(dstruc$LOCATION, n=37)$id),
                             "B1_GE", 
                             "GPSA", 
                             "Q"), 
                  start_time = 2006, 
                  end_time = 2020)

## GDP component growth rates
COMPONENT_growth <- get_dataset("QNA", 
                                filter = list(c(slice_head(dstruc$LOCATION, n=37)$id),
                                           c("P31S14_S15", "P51", "P3S13", "P6", "P7"),
                                           "GPSA", 
                                           "Q"), 
                                start_time = 2006, 
                                end_time = 2020)

# Clean data
GDP_growth = GDP_growth[, c("LOCATION", "obsTime", "obsValue")]
COMPONENT_growth = COMPONENT_growth[, c("LOCATION", "SUBJECT", "obsTime", "obsValue")]

names(GDP_growth)[1:3] <- c("COUNTRY", "QUARTER", "GDP_GROWTH_RATE")
names(COMPONENT_growth)[1:4] <- c("COUNTRY", "COMPONENT", "QUARTER", "COMPONENT_GROWTH_RATE")

GDP_growth$QUARTER <- as.yearqtr(GDP_growth$QUARTER, format='%Y-Q%q')
GDP_growth$YEAR <- year(GDP_growth$QUARTER)

COMPONENT_growth$QUARTER <- as.yearqtr(COMPONENT_growth$QUARTER, format='%Y-Q%q')
COMPONENT_growth$YEAR <- year(COMPONENT_growth$QUARTER)

COMPONENT_growth$COMPONENT[COMPONENT_growth$COMPONENT == "P31S14_S15"] <- "CONSUMPTION"
COMPONENT_growth$COMPONENT[COMPONENT_growth$COMPONENT == "P3S13"] <- "INVESTMENT"
COMPONENT_growth$COMPONENT[COMPONENT_growth$COMPONENT == "P51"] <- "GOVERNMENT_SPENDING"
COMPONENT_growth$COMPONENT[COMPONENT_growth$COMPONENT == "P6"] <- "EXPORTS"
COMPONENT_growth$COMPONENT[COMPONENT_growth$COMPONENT == "P7"] <- "IMPORTS"

# Label data by region
GDP_growth$REGION <- NA

GDP_growth$REGION[GDP_growth$COUNTRY == "JPN"] <- "Asia"
GDP_growth$REGION[GDP_growth$COUNTRY == "KOR"] <- "Asia"

GDP_growth$REGION[GDP_growth$COUNTRY == "AUS"] <- "Pacific Islands"
GDP_growth$REGION[GDP_growth$COUNTRY == "NZL"] <- "Pacific Islands"

GDP_growth$REGION[GDP_growth$COUNTRY == "ISR"] <- "Middle East"
GDP_growth$REGION[GDP_growth$COUNTRY == "TUR"] <- "Middle East"

GDP_growth$REGION[GDP_growth$COUNTRY == "CAN"] <- "North America"
GDP_growth$REGION[GDP_growth$COUNTRY == "MEX"] <- "North America"
GDP_growth$REGION[GDP_growth$COUNTRY == "USA"] <- "North America"

GDP_growth$REGION[GDP_growth$COUNTRY == "COL"] <- "South America"
GDP_growth$REGION[GDP_growth$COUNTRY == "CHL"] <- "South America"

GDP_growth$REGION[GDP_growth$COUNTRY == "CZE"] <- "Eastern Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "HUN"] <- "Eastern Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "LVA"] <- "Eastern Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "LTU"] <- "Eastern Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "POL"] <- "Eastern Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "SVK"] <- "Eastern Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "SVN"] <- "Eastern Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "EST"] <- "Eastern Europe"

GDP_growth$REGION[GDP_growth$COUNTRY == "AUT"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "BEL"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "DNK"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "FIN"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "FRA"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "DEU"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "GRC"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "ISL"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "IRL"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "ITA"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "LUX"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "NLD"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "NOR"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "PRT"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "ESP"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "SWE"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "CHE"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "GBR"] <- "Western Europe"
GDP_growth$REGION[GDP_growth$COUNTRY == "AUT"] <- "Western Europe"

# Merge data sets
GROWTHRATES <- merge(COMPONENT_growth, GDP_growth, by= c("COUNTRY", "QUARTER", "YEAR"))

# Find average growth rate per quarter for each component per region
## Sum up countries' individual components s <- consumption of asia + japan
REGIONAL_COMPONENT_GROWTH <- GROWTHRATES %>% group_by(QUARTER, COMPONENT, REGION)%>%
  dplyr::summarize(MEAN_GROWTH_RATE = mean(COMPONENT_GROWTH_RATE, na.rm=TRUE))

REGIONAL_COMPONENT_GROWTH <- REGIONAL_COMPONENT_GROWTH %>%
ungroup(QUARTER, COMPONENT)

# Merge respective GDP growth rates
COMBINEDGROWTH <- merge(REGIONAL_COMPONENT_GROWTH, GROWTHRATES, by= c("QUARTER", "COMPONENT", "REGION"))
names(COMBINEDGROWTH)[4] <- "REGIONAL_GROWTH_RATE"

# Create STX matrix for time series regression
# Do Korea alone, then compare to Japan alone, etc
# US, china, germany, japan
## Split into countries
### (I'm going to write a function to do this with every country if this example is accurate)

KOREA_GDP <- subset(GDP_growth, GDP_growth$COUNTRY == "KOR")
KOREA_GDP <- KOREA_GDP[, c("QUARTER", "GROWTH_RATE")]
KOREA_COMPONENTS <- subset(COMBINEDGROWTH, COMBINEDGROWTH$COUNTRY == "KOR")
KOREA_COMPONENTS <- KOREA_COMPONENTS[, c("QUARTER", "COMPONENT", "COMPONENT_GROWTH_RATE")]
KOREA_COMBINED <- merge(KOREA_COMPONENTS, KOREA_GDP, by = "QUARTER")

## Split into GDP components
KOREA_CONSUMPTION <- subset(KOREA_COMBINED, KOREA_COMBINED$COMPONENT == "CONSUMPTION")
KOREA_CONSUMPTION <- KOREA_CONSUMPTION %>% 
  filter(!is.na(GDP_GROWTH_RATE))
KOREA_INVESTMENT <- subset(KOREA_COMBINED, KOREA_COMBINED$COMPONENT == "INVESTMENT")
KOREA_GOVERNMENT_SPENDING <- subset(KOREA_COMBINED, KOREA_COMBINED$COMPONENT == "GOVERNMENT_SPENDING")
KOREA_EXPORTS <- subset(KOREA_COMBINED, KOREA_COMBINED$COMPONENT == "EXPORTS")
KOREA_IMPORTS <- subset(KOREA_COMBINED, KOREA_COMBINED$COMPONENT == "IMPORTS")


# Regress against GDP on components
require(stargazer)
KOREA_CONSUMPTION_REGRESSION <- lm(GDP_GROWTH_RATE~COMPONENT_GROWTH_RATE, data = KOREA_CONSUMPTION)
stargazer(KOREA_CONSUMPTION_REGRESSION, type = "text")

KOREA_CONSUMPTION_REGRESSION <- KOREA_CONSUMPTION %>% filter(!is.na(GDP_GROWTH_RATE)) %>% lm(GDP_GROWTH_RATE ~ COMPONENT_GROWTH_RATE,.) %>% fitted.values()

KOREA_INVESTMENT_REGRESSION <- lm(GDP_GROWTH_RATE~COMPONENT_GROWTH_RATE, data = KOREA_INVESTMENT)
stargazer(KOREA_INVESTMENT_REGRESSION, type = "text")

KOREA_GOVERNMENT_SPENDING_REGRESSION <- lm(GDP_GROWTH_RATE~COMPONENT_GROWTH_RATE, data = KOREA_GOVERNMENT_SPENDING)
stargazer(KOREA_GOVERNMENT_SPENDING_REGRESSION, type = "text")

KOREA_EXPORTS_REGRESSION <- lm(GDP_GROWTH_RATE~COMPONENT_GROWTH_RATE, data = KOREA_EXPORTS)
stargazer(KOREA_EXPORTS_REGRESSION, type = "text")

KOREA_IMPORTS_REGRESSION <- lm(GDP_GROWTH_RATE~COMPONENT_GROWTH_RATE, data = KOREA_IMPORTS)
stargazer(KOREA_IMPORTS_REGRESSION, type = "text")


### PLEASE HELP ME HERE
## This line is not a regression line and no matter what I do I can't make it one
require(plotly)
KOREA <- plot_ly() %>%
  add_trace(data=KOREA_CONSUMPTION, x = ~GDP_GROWTH_RATE, y = ~COMPONENT_GROWTH_RATE, text = ~COMPONENT, name = "Consumption", type="scatter", mode="markers")  %>%
  add_trace(data=KOREA_CONSUMPTION, x = ~GDP_GROWTH_RATE, y = fitted.values(try), name="regression line", type="scatter", mode="lines")
KOREA

## I tried sorting by x axis here to account for why the line was all over the place when I tried to graph
## the regression line, here the line is still not straight

try <- KOREA_CONSUMPTION %>% arrange(GDP_GROWTH_RATE)
try_regression <- lm(GDP_GROWTH_RATE~COMPONENT_GROWTH_RATE, data = try)
stargazer(try_regression, type = "text")
## Graph relationship
require(plotly)
KOREA <- plot_ly() %>%
  add_trace(data=try, x = ~GDP_GROWTH_RATE, y = ~COMPONENT_GROWTH_RATE, text = ~COMPONENT, name = "Consumption", type="scatter", mode="markers")  %>%
  add_trace(data=try, x = ~GDP_GROWTH_RATE, y = fitted.values(try_regression), name="regression line", type="scatter", mode="lines")
KOREA








  # require(plotly)
  # KOREA <- plot_ly(x = KOREA_CONSUMPTION$GDP_GROWTH_RATE,
  #             y = KOREA_CONSUMPTION$COMPONENT_GROWTH_RATE, 
  #             name = "Consumption", type="scatter", mode = "markers")  %>%
  #   add_trace(x = sort(KOREA_CONSUMPTION$GDP_GROWTH_RATE), 
  #             y = fitted(KOREA_CONSUMPTION_REGRESSION), name="regression line", 
  #             mode="lines")
  # KOREA
