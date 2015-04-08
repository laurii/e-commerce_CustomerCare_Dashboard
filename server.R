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

b<-read.csv("data/forecastcomplete.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
b$Year<-year(b$Date)
#b$Call.Per.Order<-round(b$Call.Per.Order*100,0)
#b$Deviation.<-round(b$Deviation.*100,0)

#For ValueBox
df<-as.data.frame(b[,c(1,5,7,14,8)])

shinyServer(function(input, output) {
  
  #Lineplot DATA with Numbers------------------------------------------------------------------
  timeS<-reactive({
    df<-as.data.frame(b[,c(1,2,5,8,7,14)])
    df<-as.data.frame(df[df$Year==input$call_order_year,])
    df<-as.data.frame(df[df$Month==input$call_order_month,])
    zoodata<-zoo(df[,c(3,5)],order.by = as.Date(as.character(df$Date),format="%Y/%m/%d"))
    zoodata
  })
  
  #Lineplot data with %.------------------------------------------------------------------------
  
  timeS_pcent<-reactive({
    df_pcent<-as.data.frame(b[,c(1,2,8,13,14)])
    df_pcent<-as.data.frame(df_pcent[df_pcent$Year==input$call_order_dev_year,])
    df_pcent<-as.data.frame(df_pcent[df_pcent$Month==input$call_order_dev_month,])
    zoo_pcent<-zoo(df_pcent[,c(3,4)],order.by = as.Date(as.character(df_pcent$Date),format="%Y/%m/%d"))
    zoo_pcent
  })
  
  #ValueBox Reactive data------------------------------------------------------------------------
  
  valuebox_ord<-reactive({
    vb<-as.data.frame(b[,c(1,2,5,8,7,14)])
    vb<-as.data.frame(vb[vb$Year==input$call_order_year,])
    vb<-as.data.frame(vb[vb$Month==input$call_order_month,3])
    colnames(vb)<-c("Orders")
    vb$Orders
    })
  
  valuebox_call<-reactive({
    vb<-as.data.frame(b[,c(1,2,5,8,7,14)])
    vb<-as.data.frame(vb[vb$Year==input$call_order_year,])
    vb<-as.data.frame(vb[vb$Month==input$call_order_month,5])
    colnames(vb)<-c("Calls")
    vb$Calls
  })
  
  valuebox_callperord<-reactive({
    vb<-as.data.frame(b[,c(1,2,5,8,7,14)])
    vb<-as.data.frame(vb[vb$Year==input$call_order_year,])
    vb<-as.data.frame(vb[vb$Month==input$call_order_month,])
    round((sum(vb$Calls)/sum(vb$Orders)),2)*100
    })
  
  
  
  #PLOTS and TABLES
  #----------------------------------------------------------------------------------------------
  
  #ValueBox-----------------------------------------------------------------------------------
  output$mean_orders<-renderValueBox({
    valueBox(paste(round(mean(valuebox_ord()),0)),"Average Orders",icon = icon("inbox"),
           color = "purple") 
  })
  
  output$mean_calls<-renderValueBox({
    valueBox(paste(round(mean(valuebox_call()),0)),"Average Calls",icon = icon("phone"),
             color = "yellow")
  })
  
  output$call_per_order<-renderValueBox({
    valueBox(paste(round(mean(valuebox_callperord()),0)),"Calls for every 100 Orders",icon = icon("phone"),
             color = "yellow")
  })
  
  
  #Plotting Lineplot Forecast Summary-------------------------------------------------------------------------
  output$lineplot<- renderDygraph({  
    dygraph(timeS(),main="Calls and Order Summary")%>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE)%>%
      dyAxis("x", pixelsPerLabel = 60, drawGrid = TRUE,axisLabelFontSize = 13) %>%
      dyAxis("y", label = "Calls and Orders (Thousands)",axisLabelFontSize = 13) %>%
        dyLegend(width=635)%>%
      dyEvent(date = "2014-12-10", "GOOGLE SHOPPING FESTIVAL BEGINS", labelLoc = "bottom") %>%
      dyEvent(date = "2014-12-12", "GOOGLE SHOPPING FESTIVAL ENDS", labelLoc = "bottom")%>%
      dyOptions(axisLineWidth = 1.5,fillGraph = TRUE, drawGrid = FALSE,drawPoints = TRUE )
                
  })

  
  output$forecasttable<-DT::renderDataTable({
    DT::datatable(b,filter = 'bottom',extensions =c('ColReorder','TableTools'),options =list(
      dom='T<"clear">Rlfrtip',scrollX=TRUE,
      tableTools = (list=sSwfPath=copySWF())
    ))
#     ))%>%
#       formatPercentage("Call.per.Order",0)%>% formatPercentage("Deviation.",0)
      })
  
  
  #Plotting Percentages-------------------------------------------------------------------------
  output$line_percent<- renderDygraph({  
    
    dygraph(timeS_pcent(),main="Calls% and Forecast Deviation%")%>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE)%>%
      dyLegend(width=670)%>%
      dyAxis("x", pixelsPerLabel = 60, drawGrid = TRUE,axisLabelFontSize = 13) %>%
      dyAxis("y", label = "Deviation %",axisLabelFontSize = 13) %>%
      dyAxis("y",axisLabelFormatter = JS("function(d) {return Math.round(d*100)+'%'}"))%>%
      dyEvent(date = "2014-12-10", "GOOGLE SHOPPING FESTIVAL BEGINS", labelLoc = "bottom") %>%
      dyEvent(date = "2014-12-12", "GOOGLE SHOPPING FESTIVAL ENDS", labelLoc = "bottom")%>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE,drawPoints = TRUE)
  })
  
  #Plotting Map
  
  output$map<-renderLeaflet({
      leaflet()%>%
        addTiles()%>%
        setView(77.209021,28.613939,10)%>%
        addPopups(77.209021,28.613939,'<b>NEW DELHI 
                  56,000 Orders</b>')
    
  })

  #Barplot data with------------------------------------------------------------------------
  
  barplot_nvd3_day<-reactive({
    daydf<-as.data.frame(b[,c(2,3,7,14)])
    daydf<-as.data.frame(daydf[daydf$Year==input$call_order_day_year,c(1,2,3)])
    colnames(daydf)<-c("Month","Days","Freq")
    
    daydf<-daydf%>%
          group_by(Month,Days)%>%
          summarise(Freq=sum(Freq))
        daydf$Days<-factor(daydf$Days,labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
        daydf
    })

#SAME SUBSET of DATA as above in reactive object-barplot_nvd3_day
#BARPLOT DATA
 # c<-as.data.frame(b[,c(2,3,7)])
 # colnames(c)<-c("Month","Days","Freq")
  #cc<-c%>%
   # group_by(Month,Days)%>%
  #  summarise(Freq=sum(Freq))
  #cc$Days<-factor(cc$Days,labels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
  #cc
  
  output$barplot<- renderChart({
    nvplot <- nPlot(Freq ~ Days,group="Month",dom="barplot", 
                 data = barplot_nvd3_day(),
                 type = 'multiBarChart')
  nvplot$templates$script <- "http://timelyportfolio.github.io/rCharts_nvd3_templates/chartWithTitle.html"
    nvplot$set(title = "Daywise Distribution")
    nvplot
  })  
  
})
