#Install Packages if Necessary
# list.of.packages <- c("shiny", "remotes", "tidyverse","sf","leaflet","shinythemes","DT","bslib",
#                       "dataRetrieval","lubridate","magrittr","owmr","shinyjs")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

#Library Packages and install github packages
library(shiny)
library(remotes)
library(tidyverse)
library(sf)
library(leaflet)
library(shinythemes)
library(DT)
# remotes::install_github("ColinFay/geoloc")
library(geoloc)
library(bslib)
library(dataRetrieval)
library(lubridate)
library(magrittr)
library(owmr)
library(shinyjs)



#Run this on your first time be sure to change the file path first
#setwd("C:/Users/josh.kocik/Documents/RProjects/NearestRVM/ClosestRiverMile/MissouriRiverWebsite")

RVM<-read.csv("RiverMilesTenths.csv")

BoatRamps<-read.csv("BoatRamp.csv")
BoatRamps2<-BoatRamps|>
  rename("Boat Ramp"=Boat.Ramp, "River Mile"=River.Mile)
MasterAngler<-read.csv("MasterAngler.csv")
MasterAngler$Weight<-as.numeric(MasterAngler$Weight)
MasterAngler2<-MasterAngler|>
  mutate_if(is.character, ~na_if(.,""))

sketch= htmltools::withTags(table(
  class= "display",
  thead(
    tr(
      th(colspan = 1, ""),
      th(colspan = 2, "Nebraska Master Angler"),
      th(colspan = 2, "Nebraska State Record")
    ),
    tr(
      lapply(rep(c("Species", "Weight (lbs)", "Length (in)", "Rod and Reel", "Bow and Arrow"), 1), th)
    )
  )
)
)

NearestRvm<-function(x, y){
  RVM$PointLat<-x
  RVM$PointLong<-y
  RVM|>
    select(Lat, Long)|>
    sf::st_as_sf(coords=c(2,1), crs=4326)->RVM$RVM
  MPoint<-cbind(x,y)
  MyPoint<-as.data.frame(MPoint)
  colnames(MyPoint)<-c("Long","Lat")
  MyPoint|>
    sf::st_as_sf(coords=c(1,2), crs=4326)->Point
  RVM$Distance<-as.data.frame(st_distance(x=RVM$RVM, y=Point))
  RVM|>
    slice_min(Distance)->Min
  FinalRVM<-Min[1,3]
  DistFromRVM<-Min[1,10]
  BendName<-Min[1,5]
  BendNumber<-Min[1,6]
  Results<-list(Min)
}
#Date Calculations
StDate<-Sys.Date()-6
EnDate<-Sys.Date()+6

#weatherAPI key
Sys.setenv(OWM_API_KEY = "9be4d2b81c98a2b3c1aa1e8548987a8e")
#SiouxCity Gage Height
SiouxCityGageheight<-readNWISuv(siteNumbers="06486000", parameterCd="00065", StDate, Sys.Date(), tz="America/Chicago")
labelLoc<-SiouxCityGageheight$dateTime[1]
SiouxCityGageMin<-floor(min(SiouxCityGageheight$X_00065_00000))
SiouxCityGageMax<-ceiling(max(SiouxCityGageheight$X_00065_00000))
SiouxCityGageRange<-c(SiouxCityGageMin, SiouxCityGageMax)

SiouxCityGage<-ggplot(SiouxCityGageheight, aes(x=dateTime, y=X_00065_00000))+
  geom_rect(ymin=25, ymax=30,
            xmin= -Inf, xmax= Inf, fill="#fffcf3", colour="#feeb9d")+
  annotate("text", x=labelLoc, y=25, hjust=0, vjust=0, label="Action Stage", colour="#bfa437")+
  geom_rect(ymin=30, ymax=33,
            xmin= -Inf, xmax= Inf, fill="#fcf5e5", colour="#eab333")+
  annotate("text", x=labelLoc, y=30, hjust=0, vjust=0, label="Minor Flood", colour= "#b38416")+
  geom_rect(ymin=33, ymax=36,
            xmin= -Inf, xmax= Inf, fill="#fce8e6", colour="#ea4e38")+
  annotate("text", x=labelLoc, y=33, hjust=0, vjust=0, label="Moderate Flood", colour="#902616")+
  geom_rect(ymin=36, ymax=800,
            xmin= -Inf, xmax= Inf, fill="#fbeefd", colour="#e07cf2")+
  annotate("text", x=labelLoc, y= 36, hjust=0, vjust=0, label="Major Flood", colour="#731a83")+
  geom_point(size=0.5)+
  theme_classic()+
  labs(title = "SiouxCity, IA USGS Site: 06486000", y="Gage Height (ft)" , x="Date")+
  scale_y_continuous(limits = SiouxCityGageRange)

#Sioux City Weather
SiouxCityForecast<-get_forecast("Sioux City, IA, USA", units="imperial")|>
  owmr_as_tibble()|>
  mutate(
    DateTimeUTC=as_datetime(dt_txt, tz="UTC"))
SiouxCityForecast$DateTimeCentral<-with_tz(SiouxCityForecast$DateTimeUTC, "America/Chicago")
SiouxCityNextDay<-SiouxCityForecast|>
  mutate(
    IconPrefix=c("<img src=\"https://openweathermap.org/img/wn/")
  )|>
  mutate(
    IconSuffix=c("@2x.png\" height=\"52\"></img>")
  )|>
  unite(
    Icon, c("IconPrefix","weather_icon","IconSuffix"), sep = ""
  )|>
  mutate(
    Time=format(as.POSIXct(DateTimeCentral), format="%m/%d %H:%M")
  )|>
  select(Time, Icon, weather_description,temp, feels_like, wind_speed, wind_gust, rain_3h)|>
  rename(" "="Icon",Weather="weather_description", Temp="temp", "Feels Like"="feels_like",
         "Wind Speed"="wind_speed", "Gust Speed"= "wind_gust", Rainfall="rain_3h")

#Decatur Gage Height
DecaturGageheight<-readNWISuv(siteNumbers="06601200", parameterCd="00065", StDate, Sys.Date(), tz="America/Chicago")
labelLoc<-DecaturGageheight$dateTime[1]
DecaturGageMin<-floor(min(DecaturGageheight$X_00065_00000))
DecaturGageMax<-ceiling(max(DecaturGageheight$X_00065_00000))
DecaturGageRange<-c(DecaturGageMin, DecaturGageMax)

DecaturGage<-ggplot(DecaturGageheight, aes(x=dateTime, y=X_00065_00000))+
  geom_rect(ymin=33, ymax=35,
            xmin= -Inf, xmax= Inf, fill="#fffcf3", colour="#feeb9d")+
  annotate("text", x=labelLoc, y=33, hjust=0, vjust=0, label="Action Stage", colour="#bfa437")+
  geom_rect(ymin=35, ymax=38,
            xmin= -Inf, xmax= Inf, fill="#fcf5e5", colour="#eab333")+
  annotate("text", x=labelLoc, y=35, hjust=0, vjust=0, label="Minor Flood", colour= "#b38416")+
  geom_rect(ymin=38, ymax=41,
            xmin= -Inf, xmax= Inf, fill="#fce8e6", colour="#ea4e38")+
  annotate("text", x=labelLoc, y=38, hjust=0, vjust=0, label="Moderate Flood", colour="#902616")+
  geom_rect(ymin=41, ymax=800,
            xmin= -Inf, xmax= Inf, fill="#fbeefd", colour="#e07cf2")+
  annotate("text", x=labelLoc, y= 41, hjust=0, vjust=0, label="Major Flood", colour="#731a83")+
  geom_point(size=0.5)+
  theme_classic()+
  labs(title = "Decatur, NE USGS Site: 06601200", y="Gage Height (ft)" , x="Date")+
  scale_y_continuous(limits = DecaturGageRange)

#Decatur Weather
DecaturForecast<-get_forecast("Decatur, NE, USA", units="imperial")|>
  owmr_as_tibble()|>
  mutate(
    DateTimeUTC=as_datetime(dt_txt, tz="UTC"))
DecaturForecast$DateTimeCentral<-with_tz(DecaturForecast$DateTimeUTC, "America/Chicago")
DecaturNextDay<-DecaturForecast|>
  mutate(
    IconPrefix=c("<img src=\"https://openweathermap.org/img/wn/")
  )|>
  mutate(
    IconSuffix=c("@2x.png\" height=\"52\"></img>")
  )|>
  unite(
    Icon, c("IconPrefix","weather_icon","IconSuffix"), sep = ""
  )|>
  mutate(
    Time=format(as.POSIXct(DateTimeCentral), format="%m/%d %H:%M")
  )|>
  select(Time, Icon, weather_description,temp, feels_like, wind_speed, wind_gust, rain_3h)|>
  rename(" "="Icon",Weather="weather_description", Temp="temp", "Feels Like"="feels_like",
         "Wind Speed"="wind_speed", "Gust Speed"= "wind_gust", Rainfall="rain_3h")

#Nebraska City Gage height
NECityGageheight<-readNWISuv(siteNumbers="06807000", parameterCd="00065", StDate, Sys.Date(), tz="America/Chicago")
labelLoc<-NECityGageheight$dateTime[1]
NECityGageMin<-floor(min(NECityGageheight$X_00065_00000))
NECityGageMax<-ceiling(max(NECityGageheight$X_00065_00000))
NEGageRange<-c(NECityGageMin, NECityGageMax)

NECityGage<-ggplot(NECityGageheight, aes(x=dateTime, y=X_00065_00000))+
  geom_rect(ymin=17, ymax=18,
            xmin= -Inf, xmax= Inf, fill="#fffcf3", colour="#feeb9d")+
  annotate("text", x=labelLoc, y=17, hjust=0, vjust=0, label="Action Stage", colour="#bfa437")+
  geom_rect(ymin=18, ymax=23,
            xmin= -Inf, xmax= Inf, fill="#fcf5e5", colour="#eab333")+
  annotate("text", x=labelLoc, y=18, hjust=0, vjust=0, label="Minor Flood", colour= "#b38416")+
  geom_rect(ymin=23, ymax=27,
            xmin= -Inf, xmax= Inf, fill="#fce8e6", colour="#ea4e38")+
  annotate("text", x=labelLoc, y=23, hjust=0, vjust=0, label="Moderate Flood", colour="#902616")+
  geom_rect(ymin=27, ymax=800,
            xmin= -Inf, xmax= Inf, fill="#fbeefd", colour="#e07cf2")+
  annotate("text", x=labelLoc, y= 27, hjust=0, vjust=0, label="Major Flood", colour="#731a83")+
  geom_point(size=0.5)+
  theme_classic()+
  labs(title = "Nebraska City, NE USGS Site: 06807000", y="Gage Height (ft)" , x="Date")+
  scale_y_continuous(limits = NEGageRange)

#Nebraska City Weather
NECityForecast<-get_forecast("Nebraska City, NE, USA", units="imperial")|>
  owmr_as_tibble()|>
  mutate(
    DateTimeUTC=as_datetime(dt_txt, tz="UTC"))
NECityForecast$DateTimeCentral<-with_tz(NECityForecast$DateTimeUTC, "America/Chicago")
NECityNextDay<-NECityForecast|>
  mutate(
    IconPrefix=c("<img src=\"https://openweathermap.org/img/wn/")
  )|>
  mutate(
    IconSuffix=c("@2x.png\" height=\"52\"></img>")
  )|>
  unite(
    Icon, c("IconPrefix","weather_icon","IconSuffix"), sep = ""
  )|>
  mutate(
    Time=format(as.POSIXct(DateTimeCentral), format="%m/%d %H:%M")
  )|>
  select(Time, Icon, weather_description,temp, feels_like, wind_speed, wind_gust, rain_3h)|>
  rename(" "="Icon",Weather="weather_description", Temp="temp", "Feels Like"="feels_like",
         "Wind Speed"="wind_speed", "Gust Speed"= "wind_gust", Rainfall="rain_3h")

#Rulo Gage height
RuloGageheight<-readNWISuv(siteNumbers="06813500", parameterCd="00065", StDate, Sys.Date(), tz="America/Chicago")
labelLoc<-RuloGageheight$dateTime[1]
RuloGageMin<-floor(min(RuloGageheight$X_00065_00000))
RuloGageMax<-ceiling(max(RuloGageheight$X_00065_00000))
RuloGageRange<-c(RuloGageMin, RuloGageMax)

RuloGage<-ggplot(RuloGageheight, aes(x=dateTime, y=X_00065_00000))+
  geom_rect(ymin=16, ymax=17,
            xmin= -Inf, xmax= Inf, fill="#fffcf3", colour="#feeb9d")+
  annotate("text", x=labelLoc, y=16, hjust=0, vjust=0, label="Action Stage", colour="#bfa437")+
  geom_rect(ymin=17, ymax=21,
            xmin= -Inf, xmax= Inf, fill="#fcf5e5", colour="#eab333")+
  annotate("text", x=labelLoc, y=17, hjust=0, vjust=0, label="Minor Flood", colour= "#b38416")+
  geom_rect(ymin=21, ymax=26,
            xmin= -Inf, xmax= Inf, fill="#fce8e6", colour="#ea4e38")+
  annotate("text", x=labelLoc, y=21, hjust=0, vjust=0, label="Moderate Flood", colour="#902616")+
  geom_rect(ymin=26, ymax=800,
            xmin= -Inf, xmax= Inf, fill="#fbeefd", colour="#e07cf2")+
  annotate("text", x=labelLoc, y= 26, hjust=0, vjust=0, label="Major Flood", colour="#731a83")+
  geom_point(size=0.5)+
  theme_classic()+
  labs(title = "Rulo, NE USGS Site: 06813500", y="Gage Height (ft)" , x="Date")+
  scale_y_continuous(limits = RuloGageRange)

#Rulo Weather
RuloForecast<-get_forecast("Rulo, NE, USA", units="imperial")|>
  owmr_as_tibble()|>
  mutate(
    DateTimeUTC=as_datetime(dt_txt, tz="UTC"))
RuloForecast$DateTimeCentral<-with_tz(RuloForecast$DateTimeUTC, "America/Chicago")
RuloNextDay<-as.data.frame(RuloForecast)|>
  mutate(
    IconPrefix=c("<img src=\"https://openweathermap.org/img/wn/")
  )|>
  mutate(
    IconSuffix=c("@2x.png\" height=\"52\"></img>")
  )|>
  unite(
    Icon, c("IconPrefix","weather_icon","IconSuffix"), sep = ""
  )|>
  mutate(
    Time=format(as.POSIXct(DateTimeCentral), format="%m/%d %H:%M")
  )|>
  select(Time, Icon, weather_description,temp, feels_like, wind_speed, wind_gust, rain_3h)|>
  rename(" "="Icon",Weather="weather_description", Temp="temp", "Feels Like"="feels_like",
         "Wind Speed"="wind_speed", "Gust Speed"= "wind_gust", Rainfall="rain_3h")

#Platte River at Louisville
LouisvilleGageheight<-readNWISuv(siteNumbers="06805500", parameterCd="00065", StDate, Sys.Date(), tz="America/Chicago")
labelLoc<-LouisvilleGageheight$dateTime[1]
LouisvilleGageMin<-floor(min(LouisvilleGageheight$X_.Primary.Stage.Sensor._00065_00000))
LouisvilleGageMax<-ceiling(max(LouisvilleGageheight$X_.Primary.Stage.Sensor._00065_00000))
LouisvilleGageRange<-c(LouisvilleGageMin, LouisvilleGageMax)

LouisvilleGage<-ggplot(LouisvilleGageheight, aes(x=dateTime, y=X_.Primary.Stage.Sensor._00065_00000))+
  geom_rect(ymin=8, ymax=9,
            xmin= -Inf, xmax= Inf, fill="#fffcf3", colour="#feeb9d")+
  annotate("text", x=labelLoc, y=8, hjust=0, vjust=0, label="Action Stage", colour="#bfa437")+
  geom_rect(ymin=9, ymax=11,
            xmin= -Inf, xmax= Inf, fill="#fcf5e5", colour="#eab333")+
  annotate("text", x=labelLoc, y=9, hjust=0, vjust=0, label="Minor Flood", colour= "#b38416")+
  geom_rect(ymin=11, ymax=12,
            xmin= -Inf, xmax= Inf, fill="#fce8e6", colour="#ea4e38")+
  annotate("text", x=labelLoc, y=11, hjust=0, vjust=0, label="Moderate Flood", colour="#902616")+
  geom_rect(ymin=12, ymax=800,
            xmin= -Inf, xmax= Inf, fill="#fbeefd", colour="#e07cf2")+
  annotate("text", x=labelLoc, y= 12, hjust=0, vjust=0, label="Major Flood", colour="#731a83")+
  geom_point(size=0.5)+
  theme_classic()+
  labs(title = "Platte River at Louisville, NE USGS Site: 06805500", y="Gage Height (ft)" , x="Date")+
  scale_y_continuous(limits = LouisvilleGageRange)

#Louisville Weather
LouisvilleForecast<-get_forecast("Louisville, NE, USA", units="imperial")|>
  owmr_as_tibble()|>
  mutate(
    DateTimeUTC=as_datetime(dt_txt, tz="UTC"))
LouisvilleForecast$DateTimeCentral<-with_tz(LouisvilleForecast$DateTimeUTC, "America/Chicago")
LouisvilleNextDay<-LouisvilleForecast|>
  mutate(
    IconPrefix=c("<img src=\"https://openweathermap.org/img/wn/")
  )|>
  mutate(
    IconSuffix=c("@2x.png\" height=\"52\"></img>")
  )|>
  unite(
    Icon, c("IconPrefix","weather_icon","IconSuffix"), sep = ""
  )|>
  mutate(
    Time=format(as.POSIXct(DateTimeCentral), format="%m/%d %H:%M")
  )|>
  select(Time, Icon, weather_description,temp, feels_like, wind_speed, wind_gust, rain_3h)|>
  rename(" "="Icon",Weather="weather_description", Temp="temp", "Feels Like"="feels_like",
         "Wind Speed"="wind_speed", "Gust Speed"= "wind_gust", Rainfall="rain_3h")

#Links for Boat Ramps
Test<-datatable(BoatRamps2, style="bootstrap4",rowname=FALSE, options=list(
  pageLength=50
), escape = FALSE)


# Define UI for application that draws a histogram
ui <- fluidPage(useShinyjs(),
                theme = bs_theme(version = 5,
                                 preset = "sandstone",
                                 primary = "#8f7553",
                                 secondary = "#1175b8",
                                 success = "#1175b8",
                                 fg="#000000",
                                 bg = "#F9F9F9",
                                 base_font = font_google("Roboto Condensed"),
                                 heading_font = font_google("Roboto")),
                
                # Application title
                mainPanel(
                  navbarPage("MO RVM", id="navBar",collapsible=TRUE,
                             
                             
                             tabPanel("Current RVM",
                                      icon = icon("location-dot"),
                                      h2("What is the closest river mile?"),
                                      tags$p("Click the button to get your location"),
                                      geoloc::button_geoloc("myBtn", "Get my Location"),
                                      tags$br(),
                                      textOutput("GPSRVM"),
                                      textOutput("GPSBend"),
                                      textOutput("GPSLoc"),
                                      leafletOutput("lf")),
                             navbarMenu("Location",
                                        icon = icon("map"),
                                        tabPanel("Calculate RVM",
                                                 icon = icon("map-pin"),
                                                 h2("What is the closest river mile?"),
                                                 numericInput("Lat", "Latitude", value=40.84615),
                                                 numericInput("Long", "Longitude", value=-95.84198),
                                                 textOutput("ClosestRvm"),
                                                 leafletOutput("mymap"),
                                                 tags$style(type="text/css",
                                                            ".shiny-output-error{visibility: hidden;}",
                                                            ".shiny-output-error:before{visibility: hidden;")),
                                        tabPanel("Boat Ramps",
                                                 icon = icon("anchor"),
                                                 h2("Missouri River Boat Ramps"),
                                                 DTOutput("BoatRamps"))),
                             navbarMenu("River Conditions",
                                        icon = icon("water"),
                                        tabPanel(title="Sioux City",
                                                 navset_card_underline(title="Sioux City",
                                                                       nav_panel("Gage Height", plotOutput("SiouxCityPlot")),
                                                                       nav_panel("Weather", DTOutput("SiouxCityWeather")))),
                                        tabPanel(title="Decatur",
                                                           navset_card_underline(title="Decatur",
                                                                                 nav_panel("Gage Height", plotOutput("DecaturPlot")),
                                                                                 nav_panel("Weather", DTOutput("DecaturWeather")))),
                                        tabPanel(title="Nebraska City",
                                                 navset_card_underline(title="Nebraska City",
                                                                       nav_panel("Gage Height", plotOutput("NECityPlot")),
                                                                       nav_panel("Weather", DTOutput("NECityWeather")))),
                                        tabPanel(title="Rulo",
                                                 navset_card_underline(title="Rulo",
                                                              nav_panel("Gage Height", plotOutput("RuloPlot")),
                                                              nav_panel("Weather", DTOutput("RuloWeather")))),
                                        tabPanel(title="Louisville",
                                                 navset_card_underline(title="Louisville",
                                                                       nav_panel("Gage Height", plotOutput("LouisvillePlot")),
                                                                       nav_panel("Weather", DTOutput("LouisvilleWeather"))))),
                             navbarMenu("Fish",
                                        icon = icon("fish"),
                                        tabPanel("Master Angler",
                                                 icon = icon("award"),
                                                 h2("Nebraska Master Angler and State Records"),
                                                 "This information is based on the ",
                                                 tags$a(href="https://outdoornebraska.gov/guides-maps/fishing-guides-reports/fishing-guide/",
                                                        "2023 NGPC Fishing Guide", target="_blank"),
                                                 DTOutput("MasterAngler"))),
                             tabPanel("Online Resources",
                                      icon = icon("book"),
                                      h2("Helpful Online Resources"),
                                      tags$a(href="https://outdoornebraska.gov/", "Nebraska Game and Parks Website", target="_blank"),
                                      tags$br(),
                                      tags$a(href="https://www.nwd-mr.usace.army.mil/rcc/reports/forecast.html", "Missouri River Dam Release Forecast", target="_blank"),
                                      tags$br(),
                                      tags$a(href="https://waterdata.usgs.gov/mo/nwis/current?type=flow&group_key=basin_cd&search_site_no_station_nm", "Basin Wide USGS Gauges", target="_blank")),
                             nav_item(input_dark_mode(id="dark_mode", mode="light")))),
                
                tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               bottom: 300px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 500%;
               color: #F9F9F9;
               background-color: #1175b8;
               z-index: 105;
             }
          "),
          conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                           tags$div("Loading...",id="loadmessage"))
                ), )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$ClosestRvm<-renderText({
    paste0("The closest rivermile is ", NearestRvm(input$Long, input$Lat)[[1]][[3]], " and is ", round(NearestRvm(input$Long, input$Lat)[[1]][[10]][[1]], 3), " meters away from this point.")
  })
  
  
  output$GPSRVM<-renderText({
    req(input$myBtn_lon)
    req(input$myBtn_lat)
    paste0("The closest rivermile is ", NearestRvm(input$myBtn_lon, input$myBtn_lat)[[1]][3], " and is ", round(NearestRvm(input$myBtn_lon, input$myBtn_lat)[[1]][[10]][[1]], 3), " meters away.")
  })
  
  output$GPSLoc<-renderText({
    req(input$myBtn_lon)
    req(input$myBtn_lat)
    paste0( "(",as.numeric(input$myBtn_lat),", ",as.numeric(input$myBtn_lon), ")")
  }) 
  
  output$GPSBend<-renderText({
    req(input$myBtn_lon)
    req(input$myBtn_lat)
    paste0("This is located on ", NearestRvm(input$myBtn_lon, input$myBtn_lat)[[1]][5], " (", NearestRvm(input$myBtn_lon, input$myBtn_lat)[[1]][[6]],")")}) 
  
  output$mymap<-renderLeaflet({
    leaflet()|>
      addTiles()|>
      addMarkers(lng = input$Long, lat = input$Lat)
  })
  output$lf<-renderLeaflet({
    req(input$myBtn_lon)
    req(input$myBtn_lat)
    leaflet()|>
      addTiles()|>
      setView(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), zoom=17)|>
      addMarkers(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat))
  })
  
  output$BoatRamps<-renderDT({
    Test
    }, escape = FALSE)
  
  output$SiouxCityPlot<-renderPlot({
    SiouxCityGage
  })
  
  output$SiouxCityWeather<-renderDT({
    datatable(SiouxCityNextDay, escape=FALSE,extensions="FixedColumns", style="bootstrap4",
              rowname=FALSE, options=list(pageLength=5, dom="tp",fixedColumns=list(leftColumns=1)))
  })
  
  output$DecaturPlot<-renderPlot({
    DecaturGage
  })
  
  output$DecaturWeather<-renderDT({
    datatable(DecaturNextDay, escape=FALSE, extensions="FixedColumns", style="bootstrap4",
              rowname=FALSE, options=list(pageLength=5, dom="tp", fixedColumns=list(leftColumns=1)))
  })
  
  output$NECityPlot<-renderPlot({
    NECityGage
  })
  
  output$NECityWeather<-renderDT({
    datatable(NECityNextDay, escape=FALSE, extensions="FixedColumns", style="bootstrap4",
              rowname=FALSE, options=list(pageLength=5, dom="tp", fixedColumns=list(leftColumns=1)))
  })
  
  output$RuloPlot<-renderPlot({
    RuloGage
  })
  
  output$RuloWeather<-renderDT({
    datatable(RuloNextDay, escape=FALSE, extensions="FixedColumns", style="bootstrap4",
              rowname=FALSE, options=list(pageLength=5, dom="tp", fixedColumns=list(leftColumns=1)))
    })
  
  output$LouisvillePlot<-renderPlot({
    LouisvilleGage
  })
  
  output$LouisvilleWeather<-renderDT({
    datatable(LouisvilleNextDay, escape=FALSE, extensions="FixedColumns", style="bootstrap4",
              rowname=FALSE, options=list(pageLength=5, dom="tp", fixedColumns=list(leftColumns=1)))
  })
  
  output$MasterAngler<-renderDT({
    datatable(MasterAngler2, style="bootstrap4", rowname=FALSE, container = sketch, options=list(
      pageLength=75, order = list(0, "asc"), columnDefs = list(list(
        targets = 1:4,
        render = JS(
          "function(data, type, row, meta) {",
          "return data === null ? 'NA' : data;",
          "}")
      )
      ))
    )})
  observeEvent(input$navBar, {
    runjs('
      var elem = document.getElementsByClassName("navbar-collapse")[0]
      elem.setAttribute("aria-expanded", "false");
      elem.setAttribute("class", "navbar-collapse collapse");
    ')
  })
  
  
  observeEvent(input$navBar, {
    runjs('
      var elem = document.getElementsByClassName("navbar-collapse")[0]
      elem.setAttribute("aria-expanded", "false");
      elem.setAttribute("class", "navbar-collapse collapse");
    ')
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)