# Import library
library(tidyverse)
# install.packages('plyr')
library(plyr)
# install.packages('shiny')
library(shiny)
# install.packages('gapminder')
library(gapminder)
# install.packages('plotly')
library(plotly)
# install.packages('AMR')
library(AMR)
library(data.table)
# install.packages('DT')
library(DT)
# install.packages('ggridges')
library(ggridges)
# install.packages('lubridate')
library(lubridate)
# install.packages('qicharts2')
library(qicharts2)
# install.packages('rintrojs')
library(rintrojs)
# install.packages('shinyBS', 'shinydashboard', 'shinycssloaders', 'shinyjs', 'shinyWidgets', 'survival', 'ggpubr', 'survminer')
library(shinyBS)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(survival)
library(ggpubr)
library(survminer)
# install.packages('dplyr')
library(dplyr)
# install.packages('viridis')
library(viridis)
# install.packages('zoo')
library(zoo)
# install.packages('gganimate')
library(gganimate)
# install.packages('transformr')
library(transformr)
# install.packages('readr')
library(readr)
# install.packages('gifski')
library(gifski)
# install.packages('forecast')
library(forecast) # time series analysis

# Setup environment variables
static_path <- 'D:/main/static'
plot_title <- "Life Expectancy"
legend_title <- "Life Expectancy"
attr <- "lifeExp"

# Loading data
## Population_processed: https://data.worldbank.org/indicator/SP.POP.TOTL
## GreenHouse_processed.csv: https://data.worldbank.org/indicator/EN.ATM.GHGT.KT.CE
## Continents.csv: https://www.kaggle.com/datasets/deepthi21/continents2
world <- read.csv(paste(static_path, "/Population_processed.csv", sep=""), na.strings="")
GH_data <- read.csv(paste(static_path, "/GreenHouse_processed.csv", sep=""), na.strings="")
continent <- read.csv(paste(static_path, "/Continents.csv", sep=""), na.strings="")

# Merging green house emission data
world <-rbind(world,GH_data)
world<-world %>% drop_na(Country.Name) # Removing rows with country as 'NA'
names(world)<-c("Indicator","Ind_Code","Country","CODE",rep(1960:2020,each=1)) # Changing Column Name
world <- left_join(world,continent[,c( "alpha.3","region")], by = c("CODE"= "alpha.3")) %>% distinct()

world<-world %>%
  gather(Year,value,-c(Indicator,Ind_Code,Country,CODE,region)) # Wide to long format

world<-subset(world,select=-Ind_Code)
world$value <-as.numeric(world$value) # Removing irrelevant column

df<-world %>%
  pivot_wider(names_from = Indicator,values_from = value) # Long to wide format

df<- df %>% 
  rename(pop = `Population, total`,
         lifeExp = `Life expectancy at birth, total (years)`,
         imports = `Imports of goods and services (% of GDP)`,
         exports = `Exports of goods and services (% of GDP)`,
         alcohol_consumption = `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`,
         tobacco_consumption = `Prevalence of current tobacco use (% of adults)`
  ) # Rename dataframe


write_csv(df,paste(static_path, "/Merged_processed.csv", sep=""))

df

# Shiny App
source('custom/ui.r')
source('custom/server.r')
shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0"))
