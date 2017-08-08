library(reshape2)
library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(maps)
library(mapproj)

## Load the data
data <- read.csv("data.csv")
states_map <- map_data("state")
states_map$state <- state.abb[match(states_map$region, tolower(state.name))]
data <- filter(data, STATE %in% states_map$state)
evtypes <- sort(unique(data$Event))

sortBy <- function(data, minYear, maxYear){
  result <- data %>% filter(year >= minYear, year <= maxYear)
  return(result)
}
sortByEvt <- function(data, minYear, maxYear, evtypes){
  result <- sortBy(data, minYear, maxYear)
  result <- result %>% filter(Event %in% evtypes) %>% rename(Year = year, Property.Damage = PROPDMG.Num, Crop.Damage = CROPDMG.Num, State = STATE, Injuries = INJURIES, Fatalities = FATALITIES)
  return(result)
}
sortByState <- function(data, minYear, maxYear, evtypes){
  result <- sortByEvt(data, minYear, maxYear, evtypes)
  result <- result %>% group_by(State) %>% summarise_each(funs(sum), c(2,3,6,7))
  return(result)
}

sortByYear <- function(data, minYear, maxYear, evtypes){
  result <- sortByEvt(data, minYear, maxYear, evtypes)
  result <- result %>% group_by(Year) %>% summarise_each(funs(sum),c(2,3,6,7))
  return(result)
}

plotByStateInj <- function(data, minYear, maxYear, title){
  title <- sprintf(title, minYear, maxYear)
  states_map$Injuries <- data$Injuries[match(states_map$state, data$State)]
  map <- ggplot(states_map, aes(x = long, y = lat, fill = Injuries, group = group)) + geom_polygon()
  map <- map + labs(x = "Long", y = "Lat", title = title)
  map <- map + scale_fill_gradient(low = "white", high = "orange")
}

plotByStateFat <- function(data, minYear, maxYear, title){
  title <- sprintf(title, minYear, maxYear)
  states_map$Fatalities <- data$Fatalities[match(states_map$state, data$State)]
  map <- ggplot(states_map, aes(x = long, y = lat, fill = Fatalities, group = group)) + geom_polygon()
  map <- map + labs(x = "Long", y = "Lat", title = title)
  map <- map + scale_fill_gradient(low = "white", high = "blue")
}

plotByStateCrop <- function(data, minYear, maxYear, title){
  title <- sprintf(title, minYear, maxYear)
  states_map$Crop.Damage <- data$Crop.Damage[match(states_map$state, data$State)]
  map <- ggplot(states_map, aes(x = long, y = lat, fill = Crop.Damage, group = group)) + geom_polygon()
  map <- map + labs(x = "Long", y = "Lat", title = title)
  map <- map + scale_fill_gradient(low = "white", high = "red")
}

plotByStatePro <- function(data, minYear, maxYear, title){
  title <- sprintf(title, minYear, maxYear)
  states_map$Property.Damage <- data$Property.Damage[match(states_map$state, data$State)]
  map <- ggplot(states_map, aes(x = long, y = lat, fill = Property.Damage, group = group)) + geom_polygon()
  map <- map + labs(x = "Long", y = "Lat", title = title)
  map <- map + scale_fill_gradient(low = "white", high = "green")
}

plotByYearInj <- function(data, minYear, maxYear){
  p <- ggplot(data, aes(x = Year, y = Injuries))
  p <- p + geom_smooth(method = "loess")
  p <- p + theme_bw()
  p <- p + labs(x = "Year", y = "Injuries", title = paste(minYear, "to", maxYear, ": Injuries"))
}

plotByYearFat <- function(data, minYear, maxYear){
  p <- ggplot(data, aes(x = Year, y = Fatalities))
  p <- p + geom_smooth(method = "loess")
  p <- p + theme_bw()
  p <- p + labs(x = "Year", y = "Fatalities", title = paste(minYear, "to", maxYear, ": Fatalities"))
}

plotByYearCrop <- function(data, minYear, maxYear){
  p <- ggplot(data, aes(x = Year, y = Crop.Damage))
  p <- p + geom_smooth(method = "loess")
  p <- p + theme_bw()
  p <- p + labs(x = "Year", y = "Crop.Damage", title = paste(minYear, "to", maxYear, ": Crop.Damage"))
}

plotByYearPro <- function(data, minYear, maxYear){
  p <- ggplot(data, aes(x = Year, y = Property.Damage))
  p <- p + geom_smooth(method = "loess")
  p <- p + theme_bw()
  p <- p + labs(x = "Year", y = "Property.Damage", title = paste(minYear, "to", maxYear, ": Property.Damage"))
}

shinyServer(function(input, output) {
  
  values <- reactiveValues()
  values$evtypes <- evtypes
  
  output$evtypeControls <- renderUI({
    checkboxGroupInput("evtypes", "Event Types", evtypes, selected = values$evtypes)
  })
  
  observe({
    if(input$clear_all == 0) return()
    values$evtypes <- c()
  })
  observe({
    if(input$select_all == 0) return()
    values$evtypes <- evtypes
  })
  
  dataset <- reactive({
    sortByEvt(data, input$range[1], input$range[2], input$evtypes)
  })
  dataState <- reactive({
    sortByState(data, input$range[1], input$range[2], input$evtypes)
  })
  dataYear <- reactive({
    sortByYear(data, input$range[1], input$range[2], input$evtypes)
  })
  
  output$dataTable <- renderDataTable({
    dataset()
  })
  output$InjuryByState <- renderPlot({
    print(plotByStateInj(dataState(), 
                         minYear = input$range[1], 
                         maxYear = input$range[2], 
                         title = "Injuries %d - %d"))
  })
  output$FatalityByState <- renderPlot({
    print(plotByStateFat(dataState(), 
                         minYear = input$range[1], 
                         maxYear = input$range[2], 
                         title = "Fatalities %d - %d"))
  })
  output$CropByState <- renderPlot({
    print(plotByStateCrop(dataState(), 
                          minYear = input$range[1], 
                          maxYear = input$range[2], 
                          title = "Crop Damage %d - %d"))
  })
  output$ProByState <- renderPlot({
    print(plotByStatePro(dataState(), 
                         minYear = input$range[1], 
                         maxYear = input$range[2], 
                         title = "Production Damage %d - %d"))
  })
  output$InjuryByYear <- renderPlot({
    print(plotByYearInj(dataYear(), 
                     minYear = input$range[1], 
                     maxYear = input$range[2]))
  })
  output$FatalityByYear <- renderPlot({
    print(plotByYearFat(dataYear(), 
                     minYear = input$range[1], 
                     maxYear = input$range[2]))
  })
  output$CropByYear <- renderPlot({
    print(plotByYearCrop(dataYear(), 
                     minYear = input$range[1], 
                     maxYear = input$range[2]))
  })
  output$ProByYear <- renderPlot({
    print(plotByYearPro(dataYear(), 
                     minYear = input$range[1], 
                     maxYear = input$range[2]))
  })
  output$download <- downloadHandler(
    filename = "data_selected.csv",
    content = function(file){
      write.csv(dataset(), file, row.names = FALSE)
    }
  )
})