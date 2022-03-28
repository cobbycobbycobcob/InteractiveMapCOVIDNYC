########################################################################################################################################
# Interactive Map
# Choropleth COVID-19 Data from the Last Week
# Data from NYC GitHub
# Adapted from the webinar 'Geospatial Analysis in R' from NASA (https://www.youtube.com/watch?v=Ul5Ly0266fU)
# https://github.com/jessnicwelch/edwebinar_mar19/blob/master/edwebinar_mar19_ornldaac_tutorial.md
########################################################################################################################################

library(tidyverse) # data wrangling
library(vroom) # import csv
library(sf) # spatial analysis
library(tigris) # geojoin
library(leaflet) # interactive maps
library(htmlwidgets) # interactive map labels
library(shiny)

### CREATE and SET WD
if (!dir.exists('~/Desktop/R/Covid19')) {
  dir.create('~/Desktop/R/Covid19')
}
setwd('~/Desktop/R/Covid19')

### IMPORT DATA
download.file(url = 'https://github.com/nychealth/coronavirus-data/archive/master.zip',
              destfile = 'coronavirus-data-master.zip')

unzip('coronavirus-data-master.zip')

## Select Health Data
percentposi <- vroom('coronavirus-data-master/trends/percentpositive-by-modzcta.csv') # Number of molecular tests that were positive
caserate <- vroom('coronavirus-data-master/trends/caserate-by-modzcta.csv') # Number of cases per 100k people, normalize for diverse population
testrate <- vroom('coronavirus-data-master/trends/testrate-by-modzcta.csv') # Number of tests per 100k people
# The data is broken down by borough and MODSCTA (Modified ZIP Code Tabulation Area), incorporating the regional information

## Select Spatial Data
modzcta <- st_read('~/Desktop/R/Covid19/coronavirus-data-master/Geography-resources/MODZCTA_2010.shp')
zcta_conv <- vroom('~/Desktop/R/Covid19/coronavirus-data-master/Geography-resources/ZCTA-to-MODZCTA.csv', delim = ',')

### CLEAN DATA 1

## Reformat case data by MODZCTA 
# into a 'long' tidy format, i.e. column for modzcta
# remove borough data and city wide data, i.e. cols 2-7
caserates <- caserate %>% select(-c(2:7))
# reshape data, aggregate across modzcta
caserates_long <- caserates %>%
  pivot_longer(2:178, names_to = 'modzcta',
               names_prefix = 'CASERATE_', values_to = 'caserate')

## Reformat percent positive by MODZCTA
pctpos <- percentposi %>% select(-c(2:7))
pctpos_long <- pctpos %>%
  pivot_longer(2:178, names_to = 'modzcta',
               names_prefix = 'PCTPOS_', values_to = 'pctpos')

## Reformat testrate data
testrates <- testrate %>% select(-c(2:7))
testrate_long <- testrates %>%
  pivot_longer(2:178, names_to = 'modzcta',
               names_prefix = 'TESTRATE_', values_to = 'testrate')

### MERGE DATA
## Merge COVID data
# 3 dfs left joined by week and MODZCTA
allcovid <- caserates_long %>%
  left_join(pctpos_long, by = c('week_ending', 'modzcta')) %>%
  left_join(testrate_long, by = c('week_ending', 'modzcta'))

## Merge COVID data with Spatial data
covidgeo <- geo_join(modzcta, allcovid,
                     'MODZCTA', 'modzcta',
                     how = 'inner')

### CLEAN DATA 2
covidgeo$week_ending <- as.Date(covidgeo$week_ending, format = '%m/%d/%Y')

saveRDS(covidgeo, 'NYCCovidMap/covidgeo.RDS')

### SUBSET DATA
## To display
# from last week
covidgeo_lastweek <- subset(covidgeo, subset = (week_ending == max(week_ending)))

### DATA INSPECTION

## Case rate frequency distribution
# over time
covidgeo %>%
  ggplot(aes(x = week_ending, y = as.numeric(caserate))) +
  geom_point(color ='#69b3a2')

## last week
covidgeo_lastweek %>%
  ggplot(aes(x = as.numeric(caserate))) +
  geom_histogram(bins = 20, color = ' white', fill = '#69b3a2')

### INTERACTIVE MAP

## Labels

labels <- sprintf(
  '<strong>%s</strong><br/>%g cases per 100k', # Bold formatting, mapping MODZCTA and caserate
  covidgeo_lastweek$MODZCTA, covidgeo_lastweek$caserate) %>%
  lapply(htmltools::HTML)

## Colour Palette
colpal <- colorBin(palette = 'viridis', domain = covidgeo_lastweek$caserate)

## Map
intmap <- covidgeo_lastweek %>%
  st_transform(crs = '+init=epsg:4326') %>%
  leaflet() %>%
  addProviderTiles(provider = 'CartoDB.Positron') %>%
  addPolygons(
    label = labels,
    stroke = FALSE,
    smoothFactor = .5,
    opacity = 1,
    fillOpacity = 0.7,
    fillColor = ~ colpal(caserate), # The colour is a function of caserate number
    highlightOptions = highlightOptions(weight = 5, # Greyscale highlight
                                        fillOpacity = 1,
                                        color = 'black',
                                        opacity = 1,
                                        bringToFront = TRUE)) %>%
  addLegend('bottomright',
            pal = colpal,
            values = caserate,
            title = 'Cases / 100k',
            opacity = 0.7)

## Save the map
saveWidget(intmap, 'toy_nyc_covid_caserate_map.html')

### SHINY APP

library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets) 

setwd("~/Desktop/R/Covid19/NYCCovidMap")

### IMPORT DATA
covidgeo_shiny <- readRDS('covidgeo.RDS')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # TITLE
  titlePanel("COVID-19 NYC Trends in Modified ZCTA Geography by Week"),
  
  # SIDEBAR
  sidebarLayout(
    sidebarPanel(
      h5('All data metrics are aggregated by week (as the Sunday of each week).
             Percent positive indicates, of the people tested for COVID-19, the percentage who yeilded positive molecular test results.
             All data is from the NYC Department of Health'),
      tags$a(href='http://www.github.com/nychealth/coronavirus-data', 'Data Repository', target = '_blank'),
      ### USER INPUT
      ## Drop down selection of timeframe from which data will be displayed (as the week-ending date)
      
      selectInput("date", # Refer to Reactive function below
                  "Select a date (week ending in):",
                  choices = unique(covidgeo_shiny$week_ending)
      )
    ),
    
    ### DISPLAY
    ## Main Panel
    ## Tabs for maps
    mainPanel(
      tabsetPanel(
        tabPanel('Case Rate', leafletOutput('cases')),
        tabPanel('Test Rate', leafletOutput('tests')),
        tabPanel('Percent Positive', leafletOutput('pctpos'))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ### REACTIVE FUNCTION TO SELECT/FILTER DATA
  ## Based on user input of date
  # define a function that takes the user input and returns a filtered df of that week's data
  week_zcta <- reactive({
    w <- covidgeo_shiny %>%
      filter(week_ending == input$date) # input tells server to what has been named earlier in ui
    return(w)
  })
  
  ### OUTPUT
  ## Based on reactive function
  # cases
  output$cases <- renderLeaflet({
    pal <- colorBin(palette = 'OrRd', 9, domain = week_zcta()$caserate)
    
    labels = sprintf(
      '<strong>%s</strong><br/>%g cases per 100k',
      week_zcta()$MODZCTA, week_zcta()$caserate) %>%
      lapply(htmltools::HTML)
    
    hilite <- highlightOptions(weight = 5,
                               fillOpacity = 1,
                               color = 'black',
                               opacity = 1,
                               bringToFront = TRUE)
    week_zcta() %>%
      st_transform(crs = '+init=epsg:4326') %>%
      leaflet() %>%
      addProviderTiles(provider = 'CartoDB.Positron') %>%
      setView(-73.9, 40.7, zoom = 10) %>% # trial and error, based on Google search
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = .5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(week_zcta()$caserate),
                  highlightOptions = hilite) %>%
      
      addLegend('bottomright',
                pal = pal,
                values = ~ caserate,
                title = 'Cases / 100k',
                opacity = 0.7)
    
  })
  # tests
  output$tests <- renderLeaflet({
    pal <- colorBin(palette = 'OrRd', 9, domain = week_zcta()$testrate)
    
    labels = sprintf(
      '<strong>%s</strong><br/>%g tests per 100k',
      week_zcta()$MODZCTA, week_zcta()$testrate) %>%
      lapply(htmltools::HTML)
    
    hilite <- highlightOptions(weight = 5,
                               fillOpacity = 1,
                               color = 'black',
                               opacity = 1,
                               bringToFront = TRUE)
    week_zcta() %>%
      st_transform(crs = '+init=epsg:4326') %>%
      leaflet() %>%
      addProviderTiles(provider = 'CartoDB.Positron') %>%
      setView(-73.9, 40.7, zoom = 10) %>% # trial and error, based on Google search
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = .5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(week_zcta()$testrate),
                  highlightOptions = hilite) %>%
      
      addLegend('bottomright',
                pal = pal,
                values = ~ testrate,
                title = 'Tests / 100k',
                opacity = 0.7)
    
  })
  # percent positive
  output$pctpos <- renderLeaflet({
    pal <- colorBin(palette = 'OrRd', 9, domain = week_zcta()$pctpos)
    
    labels = sprintf(
      '<strong>%s</strong><br/>%g Percent tests resulting positive',
      week_zcta()$MODZCTA, week_zcta()$pctpos) %>%
      lapply(htmltools::HTML)
    
    hilite <- highlightOptions(weight = 5,
                               fillOpacity = 1,
                               color = 'black',
                               opacity = 1,
                               bringToFront = TRUE)
    week_zcta() %>%
      st_transform(crs = '+init=epsg:4326') %>%
      leaflet() %>%
      addProviderTiles(provider = 'CartoDB.Positron') %>%
      setView(-73.9, 40.7, zoom = 10) %>% # trial and error, based on Google search
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = .5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(week_zcta()$pctpos),
                  highlightOptions = hilite) %>%
      
      addLegend('bottomright',
                pal = pal,
                values = ~ pctpos,
                title = '% Positive',
                opacity = 0.7)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

  
  
  
