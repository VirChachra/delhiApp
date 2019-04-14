
library(shiny)
library(leaflet)
library(rgdal)
library(dplyr)
library(readxl)
library(geojsonio)
library(leaflet)
library(tidyverse)



#Delhi data
states.url <- "http://bl.ocks.org/HarryStevens/raw/69da410602d8ca6b9f41f72024dba791/d3baa4ff0b411c53309c1f828a821e298ddff96f/map.json"
states <-geojson_read(states.url, what = "sp")
Delhi <- subset(states, states$st_nm %in% c("NCT of Delhi"))
delhi_stats <- read.csv("Delhi Data.csv")

# Pollution Data

pollution.data <- read_xlsx("Aggregate Pollution Data.xlsx")
colnames(pollution.data) <- c("District", "Station", "Year", "SO2", "NO2", "PM10", "PM2.5")
pollution.data[pollution.data == 0] <- NA
pollution.data <- within(pollution.data, District[District == 'West' & Station == 'Mayapuri'] <- 'South West')
pollution.data <- within(pollution.data, District[District == 'North East' & Station == 'Shahzada Bagh'] <- 'West')
pollution.data <- within(pollution.data, District[District == 'West' & Station == 'Janakpuri'] <- 'South West')



# Pollution Averages
pollution.avg <- pollution.data %>%
  group_by(District) %>%
  summarize (SO2 = mean(`SO2`, na.rm =TRUE), 
             NO2 = mean(`NO2`, na.rm = TRUE), 
             PM10 = mean(`PM10`, na.rm = TRUE ), 
             PM2.5 = mean(`PM2.5`, na.rm = TRUE))

pollution.avg <- within(pollution.avg, PM2.5[District == 'North' | District == 'East'] <- NA)

colnames(pollution.avg)[1] <- "district"

Delhi@data$id <- Delhi@data$district

# Integrate Both
Delhi@data <- inner_join(Delhi@data, delhi_stats, by = "district")
Delhi@data <- left_join(Delhi@data, pollution.avg, by = "district")

?addTiles

ui <- fluidPage(
  titlePanel("Correlations between Pollution and Identity (Delhi, India)"),
  tabsetPanel(
    tabPanel("Maps",selectInput(inputId = "choice",
                                   label = "Characteristics",
                                   selected = "TotalPop",
                                   choices = colnames(Delhi@data)[6:12]), 
             selectInput(inputId = "pollutant",
                         label = "Choose Pollutant",
                         choices = colnames(pollution.data)[4:ncol(pollution.data)]),
      fluidRow(
        column(6, leafletOutput("delhiMap")), column(6, leafletOutput("pollutionMap")) 
      ), plotOutput("plot2")),
    tabPanel("Pollution Graphs", plotOutput("plot")),
    tabPanel("Density Graphs",  plotOutput("density"), sliderInput("Years",
                                                                   label = "Years",
                                                                   min = 2004, 
                                                                   max = 2015, 
                                                                   value = 2004, 
                                                                   animate = TRUE))
  )
)


server <- function(input, output) {
  #Polygon colors
  colors <- reactive({
    colorBin(palette = "Reds", 
             domain = Delhi@data[ ,input$choice],
             bins = 5)
    
  })
  colors2 <- reactive({
    colorBin(palette = "Blues", 
             domain = Delhi@data[ ,input$pollutant],
             bins = 5)
    
  })
  # Graphs
  output$plot <- renderPlot({
     pollution.data %>%
      group_by(Year, District) %>%
      summarise(mean.con = mean((eval(as.symbol(input$pollutant))), na.rm = TRUE)) %>%
      na.omit() %>%
      ggplot(aes(x = Year, y = mean.con, color = "red")) + geom_line(size = 2) + ylab(paste("Mean", input$pollutant, "Concentration")) +
       facet_wrap(~District) + theme(axis.text=element_text(size=8),   axis.title=element_text(size=14,face="bold"), 
                                     panel.spacing = unit(.75, "lines"), 
                                     legend.position = "none", 
                                     plot.title = element_text(size=22), 
                                     strip.text = element_text(size = 15)) + geom_point(size = 3) +  ggtitle(paste("Concentration of", input$pollutant, 
                                                                                                                   "Over Time")) + 
      theme(plot.title = element_text(size=18), axis.text=element_text(size=12), strip.text = element_text(size = 15)) + 
      scale_x_discrete(name = "Year", 
                       limits = seq(ifelse(input$pollutant != "PM2.5", 2005, 2008),2015, 1))   
                                                                                                                            
                                                                                                                                                                                  
    
  })
  
  # Density Graphs
  healthlines <- function(x) {
    if(x == "SO2") {
      return(78.61)
    }
    else if( x == "NO2") {
      return(99.73)
    }
    else if(x == "PM10") {
      return(50)
    }
    return(15)
  }
  output$density <- renderPlot({ 
    pollution.data %>%
      group_by(District, Year) %>%
      # mutate(legitDistricts = ifelse(District == "North" | District == "East", NA, District)) %>%
      # na.omit %>%
      # filter(Year == input$Years) %>%
      ggplot(aes(x = eval(as.symbol(input$pollutant)) , fill = District)) + 
      geom_histogram(stat = "density", alpha = .8) + xlab(as.character(input$pollutant)) +
      geom_vline(xintercept = healthlines(input$pollutant), linetype = "dashed", show_guide = TRUE) + 
      ggtitle(paste("Density Graph of", input$pollutant, 
                    "by District Compared To Healthy Levels (", 
                    as.character(healthlines(input$pollutant)),"ug/m^3 )" )) + theme( axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
                                                                                      panel.spacing = unit(.75, "lines"), 
                                                                                      legend.position = "none", 
                                                                                      plot.title = element_text(size=22), 
                                                                                      strip.text = element_text(size = 15)) 
                                                                                 
                                                                                 
    
      })
  
  # The Characteristic map
  output$delhiMap <- renderLeaflet({
    map <- Delhi %>%
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      addControl("Census Map", position = "bottomright")
    
    map %>%
      addPolygons(fillColor = ~colors()(Delhi@data[ ,input$choice]), opacity = 1,
                  fillOpacity = .7, weight = 2, highlightOptions = 
                    highlightOptions(color = "white", weight = 2,
                                     bringToFront = TRUE), label= ~as.character(district), layerId = ~as.character(district) 
      ) %>%
      addLegend(pal = colors(),
                values = ~input$choice, title = ~input$choice, position = "bottomleft")
    
  })
  # Pollution Map
  output$pollutionMap <- renderLeaflet({
    map <- Delhi %>%
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>% 
      addControl("Pollution Map", position = "bottomright")
    
    map %>%
      addPolygons(fillColor = ~colors2()(Delhi@data[ ,input$pollutant]), opacity = 1,
                  fillOpacity = .7, weight = 2, highlightOptions = 
                    highlightOptions(color = "white", weight = 2,
                                     bringToFront = TRUE), label= ~as.character(district), 
                  layerId = ~as.character(district) 
      ) %>%
      addLegend(pal = colors2(),
                values = ~input$pollutant, title = paste(input$pollutant, "ug/m^3"), position = "bottomleft") 

    
  })
  # Click the map to show individual graphs
  observeEvent(input$pollutionMap_shape_click, { # update the location selectInput on map clicks
    click <- input$pollutionMap_shape_click
    
    output$plot2 <- renderPlot({
    pollution.data2 <- pollution.data %>%
      group_by(Year, District) %>%
      filter(District == click$id) %>%
      summarise(mean.con = mean(eval(as.symbol(input$pollutant)), na.rm = TRUE))
    
    if(nrow(pollution.data2) == 0){
      return()
    }
      else{
      return(
      pollution.data2 %>%  
      na.omit() %>%
      ggplot(aes(x = Year, y = mean.con)) + geom_line(size = 2, color = "Red") + 
      ylab("Mean Concentration") + theme(legend.position = "none") + geom_point(size = 3) + 
        ggtitle(paste("Concentration of", input$pollutant, "in", click$id)) +  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
                                                                                                             panel.spacing = unit(.75, "lines"), 
                                                                                                             legend.position = "none", 
                                                                                                             plot.title = element_text(size=22), 
                                                                                                             strip.text = element_text(size = 15)) + geom_point(size = 3) + 
        scale_x_discrete(name = "Year", limits = seq(ifelse(input$pollutant != "PM2.5", 2005, 2008), 
                                                     2015, 1)) 
      )
    }
    })
     
  })
}
shinyApp(ui = ui, server = server)


