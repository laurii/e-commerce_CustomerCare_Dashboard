library(shiny)
library(shinythemes)
library(shinydashboard)
#For Linechart
library(dygraphs)
#For Dates
library(lubridate)
#For Data Manipulation
library(dplyr)
#For converting to ZOO object similar to timeseries object(ts)
#ZOO is a dependency to dygraphs
library(zoo)
#library(ggplot2)
library(rCharts)
#library(metricsgraphics)
#library for %
#library(scales)
#For DataTables
library(DT)
#xts is a dependency to dygraphs
#library(xts)
library(leaflet)

dashboardPage(skin="blue",
              dashboardHeader(title = "Ecommerce dashboard"),
              
              dashboardSidebar(
                #Menu Items to the left of the dashboard body.
                sidebarMenu(
                  menuItem("Call Order Summary", tabName = "linechart", icon = icon("phone")),
                  
                  menuItem("Tickets", tabName = "staffing", icon = icon("ticket"),
                           menuSubItem("Complaints","Complaints",icon = icon("users"))),
                  
                  menuItem("Tables",tabName = "tables",icon = icon("table"),
                           menuSubItem("Forecast",tabName ="Forecast",icon = icon("bolt"))
                           )
                )),
              
              dashboardBody(
                
              tags$head(
              tags$link(rel="stylesheet",type="text/css",href="custom.css")
              ),
                  
              fluidRow(
                
                valueBoxOutput("mean_calls"),valueBoxOutput("mean_orders"),valueBoxOutput("call_per_order")),

              fluidRow(
                
                
                
                tabBox(id = "forecast",title = "Calls-Order Summary",width = 12,height = "500px",
                  tabPanel(title ="Forecast Summary",
                            
                           fluidRow(column(4,
                           selectInput("call_order_year","Choose Year",selected=2015,
                                          choices=list(2014,2015))),
                           column(8,
                            selectInput("call_order_month","Choose Month",selected=b$Month[1],
                                              choices=unique(b$Month))      
                           )),
                           #Actual Plot is plotted here
                            dygraphOutput(outputId = "lineplot",width = 1050,height = 300),
                           
                           br(),
                           br(),
                           br(),
                           
                            DT::dataTableOutput("forecasttable"),
                            width = 1200,height = 350),

                  tabPanel(title ="Deviation",
                           
                           fluidRow(column(4,
                                           selectInput("call_order_dev_year","Choose Year",selected=2015,
                                                       choices=list(2014,2015))),
                                    column(8,
                                           selectInput("call_order_dev_month","Choose Month",selected=b$Month[1],
                                                       choices=unique(b$Month))      
                                    )),
                           
                           #Actual Plot is plotted here
                           dygraphOutput(outputId = "line_percent",width = 1050,height = 300),width = 1100,height = 350),

                  tabPanel(title ="Map",
                           #Actual Plot is plotted here
                           leafletOutput(outputId = "map",width = 1050,height = 800),width = 1100,height = 850),
                  
                  tabPanel(title ="Barplot",
                           selectInput("call_order_day_year","Choose Year",selected=2015,
                                       choices=list(2014,2015)),
                           #Actual Plot is plotted here
                           showOutput(outputId = "barplot",lib="nvd3"),width = 1050,height = 850)
                  
                  )
              ))
              )
