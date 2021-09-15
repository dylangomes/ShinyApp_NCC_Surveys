#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggmap)
library("ggplot2")
theme_set(theme_bw())
library("sf")
# library("rnaturalearth")
# library("rnaturalearthdata")
library(shiny)
library(marmap)
library(dplyr)

dat<-read.csv("https://raw.githubusercontent.com/dylangomes/ShinyApp_NCC_Surveys/main/Spatial_Temporal_Extent_AllSurveys.csv",row.names = 1)
# dat<-read.csv("../Spatial_Temporal_Extent_AllSurveys.csv",row.names = 1)
temp<-as.POSIXlt(dat$Date,format="%m/%d/%Y")
dat$yday<-temp$yday
rm(temp)

# new ui ####
ui <- fluidPage(
  fluidRow(
    column(4,"---------- Map parameters ----------",
           sliderInput("daterange",
                                         "Day of year:",
                                         min = min(dat$yday),
                                         max = max(dat$yday),
                                         value = c(121,151)),
           fluidRow(
             column(12, ## full row within above col = 2
                          sliderInput("Lat",
                                      "Latitude:",
                                      min = 31,
                                      max = 50,
                                      value = c(40,49))
                    )
           ),
           br(),
           "---------- All plot parameters ----------",
           fluidRow(
             column(12, checkboxGroupInput("check","Surveys",choices=unique(dat$Survey),selected=c("JSOES"))
             )
                    )
            ),
    column(8,plotOutput("distPlot",height="550px"))
    ),
  br(),
  fluidRow(
    column(6,plotOutput("Plot2",height="225px",width="100%")),
    column(6,plotOutput("Plot3",height="225px",width="100%"))
           )
)

## server ####
server <- function(input, output) { ####
  
  output$distPlot <- renderPlot({

    
    ## define lat long
    long=c(-127, -120)
    lati=input$Lat
    
    ## define time period
    min=input$daterange[1]
    max=input$daterange[2]
    days<-seq(from=min,to=max,by=1)
    
    ## which data to include
    dat<-dat[dat$Survey%in%input$check,]
    dat$date<-gsub(pattern = "/[0-9]{4}$","", dat$Date)
    dat<-dat[dat$yday%in%days,]
    ## OPTION 1 (without bathymetry) ###
    # ggmap(
    #   get_map(
    #     location=make_bbox(
    #       lon=long,
    #       lat=lati,
    #       f=.1
    #     )
    #   )
    # )+
    ## END OPTION 1 ###
    
    ## OPTION 2 (with Bathymetry) ###
    # states <- map_data("state")
    
    lon1 = -122
    lon2 = -126.25
    ## first bath version
    NCC <- getNOAA.bathy(lon1 = lon1, lon2 = lon2,
                         lat1 = min(lati), lat2 = max(lati), resolution = 1,keep=T)
    ggplot(data=NCC, aes(x=x, y=y)) + coord_quickmap() +
      geom_raster(aes(fill=z),show.legend = F) +
      scale_fill_etopo()+
      geom_contour(aes(z=z),
                   breaks=c(-100, -200, -1280),
                   colour="black", size=0.2, show.legend = F
      )+
      # geom_polygon(data=states,aes(x = long, y = lat), alpha=0,color = "black")+
      coord_sf(xlim = c(lon2,lon1), ylim = c(min(lati), max(lati)), expand = FALSE)+
    
    ## END OPTION 2 ###
    
    
      geom_point(aes(x=Lon,y=Lat,color=Survey),
                 data=dat,
                 alpha=.6)+
      scale_color_manual(values = c("black", "gray100","magenta", "cyan","darkorange3","chartreuse","yellow"))+
      labs(x="Longitude",y="Latitude",
           title=paste(min(dat$date[which(dat$yday==min(dat$yday))]),
                       "-",
                       max(dat$date[which(dat$yday==max(dat$yday))])))+
      theme(
        legend.position="bottom",
        legend.key = element_rect(fill = "lightblue")
      )
    
  }#, height = 500, width = 250 ## change size of map
  )
  
  output$Plot2 <- renderPlot({
    ## which data to include
    dat2<-dat[dat$Survey%in%input$check,]
    maxV<-dat2 %>% group_by(Survey) %>% summarise(d=max(density(yday)$y))
    
    ggplot(data=dat2,aes(x=yday,fill=Survey))+
      geom_density(alpha=0.3)+
      coord_cartesian(xlim=c(min(dat2$yday),max(dat2$yday)),
                      ylim=c(0,max(maxV$d))
                      )+
      scale_fill_manual(values = c("black", "gray100","magenta", "cyan","darkorange3","chartreuse","yellow"))+
      labs(x="Day of the year")
      
    })
  
  output$Plot3 <- renderPlot({
    dat2<-dat[dat$Survey%in%input$check,]
    dat2$Year<-substr(dat2$Date,nchar(dat2$Date)-3,nchar(dat2$Date))
    ggplot(data=dat2,aes(x=Survey,y=Year))+
      stat_bin2d()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
