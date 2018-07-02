# Version 2 introduces check boxes and filter bars
# Version 3 was broken
# Version 4 adds graph below map
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#identify variables to allow for check boxes
check.vars<-c("avg_coalesced_commercial_age"
              ,"avg_age_pr_18_34_commercial"
              ,"avg_race_white_commercial"
              ,"avg_ts_married"
              ,"avg_children_in_hh"
              ,"avg_education_collegegrad"
              ,"avg_head_hh_salary_amt"
              ,"avg_travel_domestic_foreign"
              ,"avg_hh_has_credit_card"
              ,"avg_online_is_online"
              ,"avg_foodatlas12_fast_food_restaurants"
              ,"avg_foodatlas12_full_service_restaurants"
              ,"avg_last_6_months_dunkin_donuts"
              ,"avg_last_6_months_lenscrafters"
              ,"avg_last_6_months_pearlevision"
              ,"total_count"
              ,"avg_coalesced_commercial_age_index"
              ,"avg_age_pr_18_34_commercial_index"
              ,"avg_race_white_commercial_index"
              ,"avg_ts_married_index"
              ,"avg_children_in_hh_index"
              ,"avg_education_collegegrad_index"
              ,"avg_head_hh_salary_amt_index"
              ,"avg_travel_domestic_foreign_index"
              ,"avg_hh_has_credit_card_index"
              ,"avg_online_is_online_index"
              ,"avg_foodatlas12_fast_food_restaurants_index"
              ,"avg_foodatlas12_full_service_restaurants_index"
              ,"avg_last_6_months_dunkin_donuts_index"
              ,"avg_last_6_months_lenscrafters_index"
              ,"avg_last_6_months_pearlevision_index"
              ,"total_count_index")
              
library(shiny)
library(civis)
library(leaflet)
library(tigris)
library(tidyverse)
library(ggplot2)

# Pull in Data
cluster.data<-read_civis("visionworks.visionworks_store_clusters")
index.data<-read_civis("visionworks.all_stores_indexed")
lenscrafters.stores<-read_civis("visionworks.lenscrafters_with_full_geo", stringsAsFactors = FALSE)
pearlevision.stores<-read_civis("visionworks.pearlevision_with_geo", stringsAsFactors = FALSE)
lenscrafters.stores<-lenscrafters.stores[lenscrafters.stores$statefp10 == 48, ]
pearlevision.stores<-pearlevision.stores[pearlevision.stores$statefp10 == 48, ]

visionworks.data<-merge(x = cluster.data, y = index.data, by = "address", all.y = FALSE, all.x = FALSE)
visionworks.data<-unique(visionworks.data)
lenscrafters.data<-merge(x = lenscrafters.stores, y = index.data, by = "address", all.y = FALSE, all.x = FALSE)
lenscrafters.data<-unique(lenscrafters.data)
pearlevision.data<-merge(x = pearlevision.stores, y = index.data, by = "address", all.y = FALSE, all.x = FALSE)
pearlevision.data<-unique(pearlevision.data)

data<-visionworks.data
cluster.choices <- data.frame(
  var = levels(data$clustername),
  num = 1:length(levels(data$clustername))
)
index.choices <- check.vars


lenscraftersIcon <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = 'black'
)

pearlevisionIcon <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'gray',
  library = 'ion',
  markerColor = 'gray'
)

visionworksIcon <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'red',
  library = 'ion',
  markerColor = 'red'
)

zip.to.geoid<-read_civis("visionworks.zip_to_geoid", stringsAsFactors = FALSE)

summary.file<-read_civis("dunkin.census_tract_summary_file", stringsAsFactors = FALSE)
tx.data<-summary.file[summary.file$state_code == 'TX', ]
tx.zip.data <- left_join(tx.data, zip.to.geoid[ , c("geoid","zcta5")], by=c("census_tract"="geoid"))

sum(is.na(tx.zip.data$zcta5))

tx.zip.summary <- tx.zip.data %>%
  group_by(zcta5) %>%
  summarise_all(funs(mean))

tx.zip.counts <- tx.zip.data[ , c("total_count", "zcta5")] %>%
  group_by(zcta5) %>%
  summarise_all(funs(sum))

tx.zip.summary$zip.code<-as.character(tx.zip.summary$zcta5)
tx.zip.summary$total_count<-tx.zip.counts$total_count

tx.zips<- zctas(year = 2010, state = "TX")

tx.data$geoid<-as.character(tx.data$census_tract)

tmpd.zips <- left_join(tx.zips@data, tx.zip.summary, by=c("ZCTA5CE10"="zip.code"))

tmpd.zips <- tmpd.zips %>%
  mutate(Lat = as.numeric(INTPTLAT10), Long = as.numeric(INTPTLON10))

tx.zips@data <- tmpd.zips


pal <- colorNumeric(
  palette = "YlGnBu",
  domain = tx.zips@data$total_count,
  #na.color = "#808080"
  na.color = "#00000000"
)

# List of choices for selectInput and checkboxGroupInput
cluster.list <- as.vector(cluster.choices$var)
index.list <- as.vector(index.choices)
# Name it
names(cluster.list) <- cluster.choices$var

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Visionworks Retail Map"),
   
   fluidRow(
   # Left sidebars 
   column(2,
      selectInput("cluster",
                     label = h3("Store Cluster:"),
                     choices = c("All", cluster.list),
                     #selected = NULL,
                     multiple = FALSE)
      ,
      sliderInput("lenscrafters_in_one_mile", "Lenscrafters Stores Within One Mile:",
                  min = 0, max = max(data$lenscrafters_in_one_mile),
                  value = c(0,max(data$lenscrafters_in_one_mile)))
      ,
      sliderInput("lenscrafters_in_five_miles", "Lenscrafters Stores Within Five Miles:",
                  min = 0, max = max(data$lenscrafters_in_five_miles),
                  value = c(0,max(data$lenscrafters_in_five_miles)))
      ,
      sliderInput("pearlevision_in_one_mile", "Pearle Vision Stores Within One Mile:",
                  min = 0, max = max(data$pearlevision_in_one_mile),
                  value = c(0,max(data$pearlevision_in_one_mile)))
      ,
      sliderInput("pearlevision_in_five_miles", "Pearle Vision Stores Within Five Miles:",
                  min = 0, max = max(data$pearlevision_in_five_miles),
                  value = c(0,max(data$pearlevision_in_five_miles)))
      ,
      style="height: 1000px; overflow-x: scroll; overflow-y: scroll")
   ,
      # Show a plot of the generated distribution
   column(8,
        leafletOutput("map", height="600px"),
         plotOutput("visionworksPlot", height = "400px")
      ),
   column(2,
          checkboxGroupInput("variables",
                      label = h3("Variables to Display:"),
                      choices = c(index.list))
          ,
          style="height: 1000px; overflow-x: scroll; overflow-y: scroll"
   )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$map <- renderLeaflet({
      # generate bins based on input$bins from ui.R
      #x    <- data[data$clustername == input$cluster, ] 
  
    
        
     us.leaflet<-leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
       addPolygons(data= tx.zips, 
                   fillColor = ~pal(tx.zips@data$total_count),
                   color = "#b2aeae",
                   fillOpacity = .4,
                   weight = .3,
                   smoothFactor = .2,
                   label=tx.zips@data$census_tract,
                   popup=paste0( '<p>', "<b>Zip Code: ",tx.zips@data$ZCTA5CE10,"</b>", '<p></p>', 
                                 "Total Population: ",tx.zips@data$total_count, '</p><p>'),
                   popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE))%>%
                addAwesomeMarkers(lng=pearlevision.stores$civis_longitude, lat = pearlevision.stores$civis_latitude, icon = pearlevisionIcon)  %>%
                addAwesomeMarkers(lng=lenscrafters.stores$civis_longitude, lat = lenscrafters.stores$civis_latitude, icon = lenscraftersIcon)
   })
   
   observeEvent(input$cluster,{
     visionworks.popup.code<- "paste(sep = '<br/>', paste0('<b>Visionworks Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         visionworks.popup.code<-paste0(visionworks.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     visionworks.popup.code<- paste0(visionworks.popup.code, ")")
     
     lenscrafters.popup.code<- "paste(sep = '<br/>', paste0('<b>Lenscrafters Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         lenscrafters.popup.code<-paste0(lenscrafters.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     lenscrafters.popup.code<- paste0(lenscrafters.popup.code, ")")
     
     pearlevision.popup.code<- "paste(sep = '<br/>', paste0('<b>Pearle Vision Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         pearlevision.popup.code<-paste0(pearlevision.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     pearlevision.popup.code<- paste0(pearlevision.popup.code, ")")
     
     if (input$cluster == "All"){
       x <- data[input$lenscrafters_in_one_mile[1] <= data$lenscrafters_in_one_mile & input$lenscrafters_in_one_mile[2] >= data$lenscrafters_in_one_mile &
                 input$lenscrafters_in_five_miles[1] <= data$lenscrafters_in_five_miles & input$lenscrafters_in_five_miles[2] >= data$lenscrafters_in_five_miles &
                 input$pearlevision_in_one_mile[1] <= data$pearlevision_in_one_mile & input$pearlevision_in_one_mile[2] >= data$pearlevision_in_one_mile &
                 input$pearlevision_in_five_miles[1] <= data$pearlevision_in_five_miles & input$pearlevision_in_five_miles[2] >= data$pearlevision_in_five_miles, ]
     } else{
       #hold <- reactive(data[which(data$cluster %in% input$cluster),])
       #x <- hold()
       x<-data[data$clustername == input$cluster & input$lenscrafters_in_one_mile[1] <= data$lenscrafters_in_one_mile & input$lenscrafters_in_one_mile[2] >= data$lenscrafters_in_one_mile &
                 input$lenscrafters_in_five_miles[1] <= data$lenscrafters_in_five_miles & input$lenscrafters_in_five_miles[2] >= data$lenscrafters_in_five_miles &
                 input$pearlevision_in_one_mile[1] <= data$pearlevision_in_one_mile & input$pearlevision_in_one_mile[2] >= data$pearlevision_in_one_mile &
                 input$pearlevision_in_five_miles[1] <= data$pearlevision_in_five_miles & input$pearlevision_in_five_miles[2] >= data$pearlevision_in_five_miles,]
     }
     
     us.leaflet<-leafletProxy("map") %>% addProviderTiles("CartoDB.Positron")  %>% clearMarkers() %>%
       addAwesomeMarkers(lng=x$civis_longitude, lat = x$civis_latitude, icon = visionworksIcon,
                  popup =      eval(parse(text = visionworks.popup.code))
                  )%>%
       addAwesomeMarkers(lng=pearlevision.stores$civis_longitude, lat = pearlevision.stores$civis_latitude, icon = pearlevisionIcon,
                         popup =      eval(parse(text = pearlevision.popup.code)))  %>%
       addAwesomeMarkers(lng=lenscrafters.stores$civis_longitude, lat = lenscrafters.stores$civis_latitude, icon = lenscraftersIcon,
                         popup =      eval(parse(text = lenscrafters.popup.code)))
     print(us.leaflet)
     
   })

   observeEvent(input$variables,{
     visionworks.popup.code<- "paste(sep = '<br/>', paste0('<b>Visionworks Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         visionworks.popup.code<-paste0(visionworks.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     visionworks.popup.code<- paste0(visionworks.popup.code, ")")
     
     lenscrafters.popup.code<- "paste(sep = '<br/>', paste0('<b>Lenscrafters Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         lenscrafters.popup.code<-paste0(lenscrafters.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     lenscrafters.popup.code<- paste0(lenscrafters.popup.code, ")")
     
     pearlevision.popup.code<- "paste(sep = '<br/>', paste0('<b>Pearle Vision Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         pearlevision.popup.code<-paste0(pearlevision.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     pearlevision.popup.code<- paste0(pearlevision.popup.code, ")")
     if (input$cluster == "All"){
       x <- data[input$lenscrafters_in_one_mile[1] <= data$lenscrafters_in_one_mile & input$lenscrafters_in_one_mile[2] >= data$lenscrafters_in_one_mile &
                   input$lenscrafters_in_five_miles[1] <= data$lenscrafters_in_five_miles & input$lenscrafters_in_five_miles[2] >= data$lenscrafters_in_five_miles &
                   input$pearlevision_in_one_mile[1] <= data$pearlevision_in_one_mile & input$pearlevision_in_one_mile[2] >= data$pearlevision_in_one_mile &
                   input$pearlevision_in_five_miles[1] <= data$pearlevision_in_five_miles & input$pearlevision_in_five_miles[2] >= data$pearlevision_in_five_miles, ]
     } else{
       #hold <- reactive(data[which(data$cluster %in% input$cluster),])
       #x <- hold()
       x<-data[data$clustername == input$cluster & input$lenscrafters_in_one_mile[1] <= data$lenscrafters_in_one_mile & input$lenscrafters_in_one_mile[2] >= data$lenscrafters_in_one_mile &
                 input$lenscrafters_in_five_miles[1] <= data$lenscrafters_in_five_miles & input$lenscrafters_in_five_miles[2] >= data$lenscrafters_in_five_miles &
                 input$pearlevision_in_one_mile[1] <= data$pearlevision_in_one_mile & input$pearlevision_in_one_mile[2] >= data$pearlevision_in_one_mile &
                 input$pearlevision_in_five_miles[1] <= data$pearlevision_in_five_miles & input$pearlevision_in_five_miles[2] >= data$pearlevision_in_five_miles,]
     }
     us.leaflet<-leafletProxy("map") %>% addProviderTiles("CartoDB.Positron")  %>% clearMarkers() %>%
       addAwesomeMarkers(lng=x$civis_longitude, lat = x$civis_latitude, icon = visionworksIcon,
                         popup =      eval(parse(text = visionworks.popup.code))
       )%>%
       addAwesomeMarkers(lng=pearlevision.stores$civis_longitude, lat = pearlevision.stores$civis_latitude, icon = pearlevisionIcon,
                         popup =      eval(parse(text = pearlevision.popup.code)))  %>%
       addAwesomeMarkers(lng=lenscrafters.stores$civis_longitude, lat = lenscrafters.stores$civis_latitude, icon = lenscraftersIcon,
                         popup =      eval(parse(text = lenscrafters.popup.code)))
     print(us.leaflet)
     
   })
   
   observeEvent(input$lenscrafters_in_one_mile,{
     visionworks.popup.code<- "paste(sep = '<br/>', paste0('<b>Visionworks Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         visionworks.popup.code<-paste0(visionworks.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     visionworks.popup.code<- paste0(visionworks.popup.code, ")")
     
     lenscrafters.popup.code<- "paste(sep = '<br/>', paste0('<b>Lenscrafters Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         lenscrafters.popup.code<-paste0(lenscrafters.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     lenscrafters.popup.code<- paste0(lenscrafters.popup.code, ")")
     
     pearlevision.popup.code<- "paste(sep = '<br/>', paste0('<b>Pearle Vision Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         pearlevision.popup.code<-paste0(pearlevision.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     pearlevision.popup.code<- paste0(pearlevision.popup.code, ")")
     
     if (input$cluster == "All"){
       x <- data[input$lenscrafters_in_one_mile[1] <= data$lenscrafters_in_one_mile & input$lenscrafters_in_one_mile[2] >= data$lenscrafters_in_one_mile &
                   input$lenscrafters_in_five_miles[1] <= data$lenscrafters_in_five_miles & input$lenscrafters_in_five_miles[2] >= data$lenscrafters_in_five_miles &
                   input$pearlevision_in_one_mile[1] <= data$pearlevision_in_one_mile & input$pearlevision_in_one_mile[2] >= data$pearlevision_in_one_mile &
                   input$pearlevision_in_five_miles[1] <= data$pearlevision_in_five_miles & input$pearlevision_in_five_miles[2] >= data$pearlevision_in_five_miles, ]
     } else{
       #hold <- reactive(data[which(data$cluster %in% input$cluster),])
       #x <- hold()
       x<-data[data$clustername == input$cluster & input$lenscrafters_in_one_mile[1] <= data$lenscrafters_in_one_mile & input$lenscrafters_in_one_mile[2] >= data$lenscrafters_in_one_mile &
                 input$lenscrafters_in_five_miles[1] <= data$lenscrafters_in_five_miles & input$lenscrafters_in_five_miles[2] >= data$lenscrafters_in_five_miles &
                 input$pearlevision_in_one_mile[1] <= data$pearlevision_in_one_mile & input$pearlevision_in_one_mile[2] >= data$pearlevision_in_one_mile &
                 input$pearlevision_in_five_miles[1] <= data$pearlevision_in_five_miles & input$pearlevision_in_five_miles[2] >= data$pearlevision_in_five_miles,]
     }
     us.leaflet<-leafletProxy("map") %>% addProviderTiles("CartoDB.Positron")  %>% clearMarkers() %>%
       addAwesomeMarkers(lng=x$civis_longitude, lat = x$civis_latitude, icon = visionworksIcon,
                         popup =      eval(parse(text = visionworks.popup.code))
       )%>%
       addAwesomeMarkers(lng=pearlevision.stores$civis_longitude, lat = pearlevision.stores$civis_latitude, icon = pearlevisionIcon,
                         popup =      eval(parse(text = pearlevision.popup.code)))  %>%
       addAwesomeMarkers(lng=lenscrafters.stores$civis_longitude, lat = lenscrafters.stores$civis_latitude, icon = lenscraftersIcon,
                         popup =      eval(parse(text = lenscrafters.popup.code)))
     print(us.leaflet)
     
   })   
   
   observeEvent(input$lenscrafters_in_five_miles,{
     visionworks.popup.code<- "paste(sep = '<br/>', paste0('<b>Visionworks Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         visionworks.popup.code<-paste0(visionworks.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     visionworks.popup.code<- paste0(visionworks.popup.code, ")")
     
     lenscrafters.popup.code<- "paste(sep = '<br/>', paste0('<b>Lenscrafters Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         lenscrafters.popup.code<-paste0(lenscrafters.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     lenscrafters.popup.code<- paste0(lenscrafters.popup.code, ")")
     
     pearlevision.popup.code<- "paste(sep = '<br/>', paste0('<b>Pearle Vision Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         pearlevision.popup.code<-paste0(pearlevision.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     pearlevision.popup.code<- paste0(pearlevision.popup.code, ")")
     
     if (input$cluster == "All"){
       x <- data[input$lenscrafters_in_one_mile[1] <= data$lenscrafters_in_one_mile & input$lenscrafters_in_one_mile[2] >= data$lenscrafters_in_one_mile &
                   input$lenscrafters_in_five_miles[1] <= data$lenscrafters_in_five_miles & input$lenscrafters_in_five_miles[2] >= data$lenscrafters_in_five_miles &
                   input$pearlevision_in_one_mile[1] <= data$pearlevision_in_one_mile & input$pearlevision_in_one_mile[2] >= data$pearlevision_in_one_mile &
                   input$pearlevision_in_five_miles[1] <= data$pearlevision_in_five_miles & input$pearlevision_in_five_miles[2] >= data$pearlevision_in_five_miles, ]
     } else{
       #hold <- reactive(data[which(data$cluster %in% input$cluster),])
       #x <- hold()
       x<-data[data$clustername == input$cluster & input$lenscrafters_in_one_mile[1] <= data$lenscrafters_in_one_mile & input$lenscrafters_in_one_mile[2] >= data$lenscrafters_in_one_mile &
                 input$lenscrafters_in_five_miles[1] <= data$lenscrafters_in_five_miles & input$lenscrafters_in_five_miles[2] >= data$lenscrafters_in_five_miles &
                 input$pearlevision_in_one_mile[1] <= data$pearlevision_in_one_mile & input$pearlevision_in_one_mile[2] >= data$pearlevision_in_one_mile &
                 input$pearlevision_in_five_miles[1] <= data$pearlevision_in_five_miles & input$pearlevision_in_five_miles[2] >= data$pearlevision_in_five_miles,]
     }
     us.leaflet<-leafletProxy("map") %>% addProviderTiles("CartoDB.Positron")  %>% clearMarkers() %>%
       addAwesomeMarkers(lng=x$civis_longitude, lat = x$civis_latitude, icon = visionworksIcon,
                         popup =      eval(parse(text = visionworks.popup.code))
       )%>%
       addAwesomeMarkers(lng=pearlevision.stores$civis_longitude, lat = pearlevision.stores$civis_latitude, icon = pearlevisionIcon,
                         popup =      eval(parse(text = pearlevision.popup.code)))  %>%
       addAwesomeMarkers(lng=lenscrafters.stores$civis_longitude, lat = lenscrafters.stores$civis_latitude, icon = lenscraftersIcon,
                         popup =      eval(parse(text = lenscrafters.popup.code)))
     print(us.leaflet)
     
   }) 
   
   observeEvent(input$pearlevision_in_one_mile,{
     visionworks.popup.code<- "paste(sep = '<br/>', paste0('<b>Visionworks Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         visionworks.popup.code<-paste0(visionworks.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     visionworks.popup.code<- paste0(visionworks.popup.code, ")")
     
     lenscrafters.popup.code<- "paste(sep = '<br/>', paste0('<b>Lenscrafters Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         lenscrafters.popup.code<-paste0(lenscrafters.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     lenscrafters.popup.code<- paste0(lenscrafters.popup.code, ")")
     
     pearlevision.popup.code<- "paste(sep = '<br/>', paste0('<b>Pearle Vision Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         pearlevision.popup.code<-paste0(pearlevision.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     pearlevision.popup.code<- paste0(pearlevision.popup.code, ")")
     
     if (input$cluster == "All"){
       x <- data[input$lenscrafters_in_one_mile[1] <= data$lenscrafters_in_one_mile & input$lenscrafters_in_one_mile[2] >= data$lenscrafters_in_one_mile &
                   input$lenscrafters_in_five_miles[1] <= data$lenscrafters_in_five_miles & input$lenscrafters_in_five_miles[2] >= data$lenscrafters_in_five_miles &
                   input$pearlevision_in_one_mile[1] <= data$pearlevision_in_one_mile & input$pearlevision_in_one_mile[2] >= data$pearlevision_in_one_mile &
                   input$pearlevision_in_five_miles[1] <= data$pearlevision_in_five_miles & input$pearlevision_in_five_miles[2] >= data$pearlevision_in_five_miles, ]
     } else{
       #hold <- reactive(data[which(data$cluster %in% input$cluster),])
       #x <- hold()
       x<-data[data$clustername == input$cluster & input$lenscrafters_in_one_mile[1] <= data$lenscrafters_in_one_mile & input$lenscrafters_in_one_mile[2] >= data$lenscrafters_in_one_mile &
                 input$lenscrafters_in_five_miles[1] <= data$lenscrafters_in_five_miles & input$lenscrafters_in_five_miles[2] >= data$lenscrafters_in_five_miles &
                 input$pearlevision_in_one_mile[1] <= data$pearlevision_in_one_mile & input$pearlevision_in_one_mile[2] >= data$pearlevision_in_one_mile &
                 input$pearlevision_in_five_miles[1] <= data$pearlevision_in_five_miles & input$pearlevision_in_five_miles[2] >= data$pearlevision_in_five_miles,]
     }
     us.leaflet<-leafletProxy("map") %>% addProviderTiles("CartoDB.Positron")  %>% clearMarkers() %>%
       addAwesomeMarkers(lng=x$civis_longitude, lat = x$civis_latitude, icon = visionworksIcon,
                         popup =      eval(parse(text = visionworks.popup.code))
       )%>%
       addAwesomeMarkers(lng=pearlevision.stores$civis_longitude, lat = pearlevision.stores$civis_latitude, icon = pearlevisionIcon,
                         popup =      eval(parse(text = pearlevision.popup.code)))  %>%
       addAwesomeMarkers(lng=lenscrafters.stores$civis_longitude, lat = lenscrafters.stores$civis_latitude, icon = lenscraftersIcon,
                         popup =      eval(parse(text = lenscrafters.popup.code)))
     print(us.leaflet)
     
   }) 
   
   observeEvent(input$pearlevision_in_five_miles,{
     visionworks.popup.code<- "paste(sep = '<br/>', paste0('<b>Visionworks Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         visionworks.popup.code<-paste0(visionworks.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     visionworks.popup.code<- paste0(visionworks.popup.code, ")")
     
     lenscrafters.popup.code<- "paste(sep = '<br/>', paste0('<b>Lenscrafters Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         lenscrafters.popup.code<-paste0(lenscrafters.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     lenscrafters.popup.code<- paste0(lenscrafters.popup.code, ")")
     
     pearlevision.popup.code<- "paste(sep = '<br/>', paste0('<b>Pearle Vision Address: ', x$address, '</b>')"
     if (length(input$variables)>0){
       for (i in 1:length(input$variables)){
         
         var.code<-paste0("x$", input$variables[i])
         pearlevision.popup.code<-paste0(pearlevision.popup.code, ",paste0('",input$variables[i], ": ', ", var.code, ")")
       }
     }
     pearlevision.popup.code<- paste0(pearlevision.popup.code, ")")
     
     if (input$cluster == "All"){
       x <- data[input$lenscrafters_in_one_mile[1] <= data$lenscrafters_in_one_mile & input$lenscrafters_in_one_mile[2] >= data$lenscrafters_in_one_mile &
                   input$lenscrafters_in_five_miles[1] <= data$lenscrafters_in_five_miles & input$lenscrafters_in_five_miles[2] >= data$lenscrafters_in_five_miles &
                   input$pearlevision_in_one_mile[1] <= data$pearlevision_in_one_mile & input$pearlevision_in_one_mile[2] >= data$pearlevision_in_one_mile &
                   input$pearlevision_in_five_miles[1] <= data$pearlevision_in_five_miles & input$pearlevision_in_five_miles[2] >= data$pearlevision_in_five_miles, ]
     } else{
       #hold <- reactive(data[which(data$cluster %in% input$cluster),])
       #x <- hold()
       x<-data[data$clustername == input$cluster & input$lenscrafters_in_one_mile[1] <= data$lenscrafters_in_one_mile & input$lenscrafters_in_one_mile[2] >= data$lenscrafters_in_one_mile &
                 input$lenscrafters_in_five_miles[1] <= data$lenscrafters_in_five_miles & input$lenscrafters_in_five_miles[2] >= data$lenscrafters_in_five_miles &
                 input$pearlevision_in_one_mile[1] <= data$pearlevision_in_one_mile & input$pearlevision_in_one_mile[2] >= data$pearlevision_in_one_mile &
                 input$pearlevision_in_five_miles[1] <= data$pearlevision_in_five_miles & input$pearlevision_in_five_miles[2] >= data$pearlevision_in_five_miles,]
     }
     us.leaflet<-leafletProxy("map") %>% addProviderTiles("CartoDB.Positron")  %>% clearMarkers() %>%
       addAwesomeMarkers(lng=x$civis_longitude, lat = x$civis_latitude, icon = visionworksIcon,
                         popup =      eval(parse(text = visionworks.popup.code))
       )%>%
       addAwesomeMarkers(lng=pearlevision.stores$civis_longitude, lat = pearlevision.stores$civis_latitude, icon = pearlevisionIcon,
                         popup =      eval(parse(text = pearlevision.popup.code)))  %>%
       addAwesomeMarkers(lng=lenscrafters.stores$civis_longitude, lat = lenscrafters.stores$civis_latitude, icon = lenscraftersIcon,
                         popup =      eval(parse(text = lenscrafters.popup.code)))
     print(us.leaflet)
     
   }) 
   output$visionworksPlot <- renderPlot({
     # generate bins based on input$bins from ui.R
     #x    <- data[data$clustername == input$cluster, ] 
     colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')
     
     if (input$cluster == "All"){
       x <- data
       colour <- factor(x$clustername)
     } else{
       #hold <- reactive(data[which(data$cluster %in% input$cluster),])
       #x <- hold()
       x<-data[which(data$clustername %in% input$cluster),]
       index<- match(input$cluster, cluster.list)
       colors<- colors[index]
     }
     
     #bins <- seq(min(x), max(x), length.out = input$bins + 1)
     x.min <- 0
     x.max <- max(data$total_count)
     y.min <- min(data$avg_last_6_months_dunkin_donuts_index)
     y.max <- max(data$avg_last_6_months_dunkin_donuts_index)
     
     # draw the histogram with the specified number of bins
     cluster_graph <- ggplot(x, aes(x = total_count, y = avg_last_6_months_dunkin_donuts_index)) + xlim(x.min, x.max) + ylim(y.min, y.max)
     cluster_graph <- cluster_graph + geom_point(aes(colour = factor(x$clustername)))
     
     cluster_graph <- cluster_graph + scale_colour_manual(name = "Cluster Group", values=colors)
     cluster_graph <- cluster_graph + xlab("Number of People in Census Tract")
     cluster_graph <- cluster_graph + ylab("Index of Likelihood to Visit Visionworks in Last 6 Months")
     print(cluster_graph)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

