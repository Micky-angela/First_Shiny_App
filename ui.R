library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(maps)
library(data.table)
library(mapproj)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Data Visualizer",
                   tabPanel("Explore the Data",
                            sidebarPanel(
                              sliderInput("range",
                                          "Range:",
                                          min = 1950,
                                          max = 2011,
                                          value = c(1992,2010)),
                              uiOutput("evtypeControls"),
                              actionButton(inputId = "clear_all", label = "Clear Selection", icon = icon("check-square")),
                              actionButton(inputId = "select_all", label = "Select All", icon = icon("check-square-o"))
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel(p(icon("table"), "Dataset"),
                                         dataTableOutput(outputId = "dataTable"),
                                         downloadButton("download", "Download")),
                                tabPanel(p(icon("line-chart"), "Plot by State"),
                                         plotOutput("InjuryByState"),
                                         plotOutput("FatalityByState"),
                                         plotOutput("CropByState"),
                                         plotOutput("ProByState")),
                                tabPanel(p(icon("line-chart"), "Plot by Year"),
                                         plotOutput("InjuryByYear"),
                                         plotOutput("FatalityByYear"),
                                         plotOutput("CropByYear"),
                                         plotOutput("ProByYear"))
                              )
                            )
                   )
))
