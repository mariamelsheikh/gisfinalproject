#load libraries

library(shiny)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(tidyr)
library(sf)
library(stringr)
library(viridis)

## load data

#neighbourhoods in toronto
nbh_to <- read_sf("data/nbh.shp") %>% 
    st_transform(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#remove number of neighbourhoods from the name
nbh_to$AREA_NA <- str_replace(nbh_to$AREA_NA, " \\(.*\\)", "")


# results of 3FSCA, access ratio by neighbourhood
accessibility <- read_sf("data/ratio.shp") %>% 
    st_transform(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

accessibility <- accessibility %>% 
    rename("avg_ratio_1000" = "a__1000")

# data from census with different socio demographic variables
sociodemo <- read_sf("data/sociodemo.shp") %>% 
    st_transform(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

names(sociodemo) <- c("Shape Area","Type","Dwellings","Households","GeoUID","Population",
                      "Adjusted Population (previous Census)","PR_UID","CMA_UID","CSD_UID",
                      "CD_UID","province_code","cms_code","census_tract","population",
                      "population_densitykm2","average_age",
                      "average household size","knowledge_eng",
                      "knowledge_french","kowledge_both","knowledge_neither",
                      "first_english","first_french",
                      "first_both","first_neither","median income of households in 2015 ($)",
                      "median after-tax income of households in 2015",
                      "number household in 2015",
                      "under 5000","5000 to 9999","10000 to 14999","15000 to 19999",
                      "20000 to 24999","25000 to 29999","30000 to 34999","35000 to 39999",
                      "40000 to 44999","45000 to 49999","50000 to 59999","60000 to 69999",
                      "60000 to 79999","80000 to 89999","90000 to 99999","100000 and over",
                      "average after-tax income of households in 2015","low income LIM-AT", 
                      "prevalence of low income LIM-AT","low income LICO-AT",
                      "prevalence LICO-AT",
                      "knowledge of languages for the population in private households",
                      "knowledge_official language_private households",
                      "knowledge_eng_private households",
                      "knowledge_french_private households", 
                      "knowledge_non-official language_private households",
                      "Total - Citizenship for the population in private households",
                      "canadian citizens","Not Canadian citizens", 
                      "Total - Immigrant status for the population in private households",
                      "non-immigrants", "immigrants", "Non-permanent residents",
                      "Immigrants_2011 to 2016", 
                      "Immigrants_2006 to 2010", "Immigrants_2001 to 2005",
                      "Immigrants_2001 to 2010", 
                      "Immigrants_1991 to 2000", "Immigrants_1981 to 1990",
                      "Immigrants_Before 1981", 
                      "Total - Generation status for the population in private households", 
                      "First generation", "Second generation", "Third generation or more", 
                      "Admission category for the immigrant population in private households", 
                      "Economic immigrants", "Immigrants sponsored by family", "Refugees", 
                      "other immigrants", 
                      "Total - Aboriginal identity for the population in private households", 
                      "Non-Aboriginal identity", "Aboriginal identity", 
                      "Total - Visible minority for the population in private households", 
                      "Total visible minority population","Not a visible minority",
                      "Total - Private households by tenure",
                      "Total - diploma or degree for the population aged 15 years and over in private households", "No certificate, diploma or degree", "Secondary",
                      "Postsecondary", "Employment rate",
                      "Unemployment rate",
                      "Total - Population aged 15 years and over by Labour force status", 
                      "In the labour force", "Not in the labour force","geometry")

sociodemo <- sociodemo %>% 
    select("Shape Area","Type","Dwellings","Households","GeoUID","Population",
           "PR_UID","CMA_UID","CSD_UID",
           "CD_UID","province_code","cms_code","census_tract","population",
           "population_densitykm2","average_age","number household in 2015","100000 and over",
           "average after-tax income of households in 2015", 
           "prevalence of low income LIM-AT",
           "Total - Immigrant status for the population in private households",
           "immigrants","Total - Visible minority for the population in private households",
           "Total visible minority population",
           "Total - diploma or degree for the population aged 15 years and over in private households",
           "No certificate, diploma or degree","Postsecondary","Unemployment rate","Employment rate")

sociodemo <- sociodemo %>% 
    rename("total_immigrants_vm" = "Total - Immigrant status for the population in private households",
           "visibleminority" = "Total visible minority population",
           "total_edu" = "Total - diploma or degree for the population aged 15 years and over in private households",
           "nodegree" = "No certificate, diploma or degree",
           "avg_aftertax_income_hh" = "average after-tax income of households in 2015",
           "prevlence_low_income_limat" = "prevalence of low income LIM-AT") %>% 
    select(-"Total - Visible minority for the population in private households")

sociodemo <- sociodemo %>% 
    mutate(per_immigrants = (immigrants/total_immigrants_vm)*100,
           per_visibleminority = (visibleminority/total_immigrants_vm)*100,
           per_postsecondary = (Postsecondary/total_edu)*100,
           per_nodegree = (nodegree/total_edu)*100,
           per_highincome = (`100000 and over`/`number household in 2015`)*100)

# health centers (for markers)
healthcenters <- read_sf("data/healthcenters.shp") %>% 
    st_transform(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
longlat <- as.data.frame(st_coordinates(healthcenters))


healthcenters <- healthcenters %>% mutate(long = longlat$X,
                                                  lat = longlat$Y)



bounds<-st_bbox(nbh_to)



pal_ratio <- colorNumeric(
    palette = "viridis",
    domain = accessibility$avg_ratio_1000,
    reverse = T)

access_map <- leaflet() %>%
    addTiles() %>% 
    setView(mean(bounds[c(1,3)]),
            mean(bounds[c(2,4)]),
            zoom=10) %>% 
    addPolygons(data = accessibility,
                fillColor = ~pal_ratio(accessibility$avg_ratio_1000),
                color = "gray25",
                fillOpacity = 1,
                weight = 2) %>%
    addPolygons(data = nbh_to,
                weight = 3,
                color = "grey",
                highlight = highlightOptions(weight = 5,
                                             color = "black",
                                             fillOpacity = 0.01,
                                             bringToFront = TRUE),
                label = sprintf(
                    "<strong>%s</strong>",
                    nbh_to$AREA_NA
                ) %>% lapply(htmltools::HTML),
                labelOptions = 
                    labelOptions(style = list("font-weight" = "normal", 
                                              padding = "3px 8px"),
                                 textsize = "15px",
                                 direction = "auto")) %>%
    addLegend(pal = pal_ratio,
              values = (accessibility$avg_ratio_1000),
              position = "bottomright",
              title = "Healthcare access ratio per 1000 people") %>% 
    addCircleMarkers(data = healthcenters[healthcenters$type == "Hospital",],
                     group = "Hospital",
                     color = "green",
                     stroke = T, radius = 5) %>%
    addCircleMarkers(data = healthcenters[healthcenters$type == "Community Health Center",],
                     group = "Community Health Center",
                     color = "turquoise",
                     stroke = T,radius = 5) %>%
    addCircleMarkers(data = healthcenters[healthcenters$type == "Walk-in Clinic",],
                     group = "Walk-in Clinic",
                     color = "blue",
                     stroke = T,radius = 5) %>%
    addCircleMarkers(data = healthcenters[healthcenters$type == "Family Medical Center",],
                     group = "Family Medical Center",
                     color = "lightblue", 
                     stroke = T,radius = 5) %>%
    addCircleMarkers(data = healthcenters[healthcenters$type == "Family Medical Teams",],
                     group = "Family Medical Teams", 
                     color = "springgreen4",
                     stroke = T,radius = 5) %>%
    addCircleMarkers(data = healthcenters[healthcenters$type == "Nurse Practitioner Clinic",],
                     group = "Nurse Practitioner Clinic",
                     color = "yellow", 
                     stroke = T,radius = 5) %>%
    addLayersControl(overlayGroups = c("Hospital", "Community Health Center","Walk-in Clinic",
                                       "Family Medical Center","Family Medical Teams",
                                       "Nurse Practitioner Clinic"), 
                     options = layersControlOptions(collapsed = F)) %>% 
    addProviderTiles("CartoDB.DarkMatter")


#Define UI for data upload app
ui <- dashboardPage(
    
    #App title
    dashboardHeader(title = "Health care services in Toronto",
                    titleWidth = 300),
    
    dashboardSidebar(disable = T),
   
     dashboardBody(
         tags$head(tags$style(HTML('.skin-blue .main-header .logo {
          background-color: #3c8dbc;}
        .skin-blue .main-header .logo:hover {background-color: #3c8dbc;
        }
      '))),
         fluidRow(
         box(selectizeInput("variable", "Choose One Variable",
                         choices = c("population density",
                                     "immigrants",
                                     "visible minority",
                                     "average income after-tax",
                                     "households with income >=100k",
                                     "prevalence of low income households",
                                     "postsecondary degree","no degree",
                                     "employment rate","unemployment rate"),
                         multiple = F),
             width = 4,solidHeader = TRUE, height = 110)),
         fluidRow(
             box(leafletOutput(outputId = "firstmap"), width = 12 ,height = 430)
         ),
         fluidRow(
             box(leafletOutput(outputId = "secondmap"), width = 12 ,height = 430)
         )
     )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$firstmap <- renderLeaflet({
        
        access_map
        
    })
    output$secondmap <- renderLeaflet({
        
        if(input$variable == "unemployment rate"){
            var <- reactive({sociodemo$`Unemployment rate`})
        } else if (input$variable == "immigrants"){
            var <- reactive({sociodemo$per_immigrants})
        } else if (input$variable == "visible minority"){
            var <- reactive({sociodemo$per_visibleminority})
        } else if (input$variable == "average income after-tax"){
            var <- reactive({sociodemo$avg_aftertax_income_hh})
        } else if (input$variable == "households with income >=100k"){
            var <- reactive({sociodemo$per_highincome})
        } else if (input$variable == "prevalence of low income households"){
            var <- reactive({sociodemo$prevlence_low_income_limat})
        } else if (input$variable == "postsecondary degree"){
            var <- reactive({sociodemo$per_postsecondary})
        } else if (input$variable == "no degree"){
            var <- reactive({sociodemo$per_nodegree})
        } else if (input$variable == "employment rate"){
            var <- reactive({sociodemo$`Employment rate`})
        } else {var <- reactive({sociodemo$population_densitykm2})}
        
        
        colorpal <- colorNumeric(
            palette = "viridis",
            domain = var(),
            reverse = T)
        
        
        leaflet() %>%
            addTiles() %>% 
            setView(mean(bounds[c(1,3)]),
                    mean(bounds[c(2,4)]),
                    zoom=10) %>% 
            addPolygons(data = sociodemo,
                        fillColor = ~colorpal(var()),
                        color = "gray25",
                        fillOpacity = 1,
                        weight = 2) %>%
            addPolygons(data = nbh_to,
                        weight = 3,
                        color = "grey",
                        highlight = highlightOptions(weight = 5,
                                                     color = "black",
                                                     fillOpacity = 0.01,
                                                     bringToFront = TRUE),
                        label = sprintf("<strong>%s</strong>",
                            nbh_to$AREA_NA
                        ) %>% lapply(htmltools::HTML),
                        labelOptions = 
                            labelOptions(style = list("font-weight" = "normal", 
                                                      padding = "3px 8px"),
                                         textsize = "15px",
                                         direction = "auto")) %>%
            addLegend(pal = colorpal,
                      values = (var()),
                      position = "bottomright") %>%  
            addCircleMarkers(data = healthcenters[healthcenters$type == "Hospital",],
                             group = "Hospital",
                             color = "green",
                             stroke = T, radius = 5) %>%
            addCircleMarkers(data = healthcenters[healthcenters$type == "Community Health Center",],
                             group = "Community Health Center",
                             color = "turquoise",
                             stroke = T,radius = 5) %>%
            addCircleMarkers(data = healthcenters[healthcenters$type == "Walk-in Clinic",],
                             group = "Walk-in Clinic",
                             color = "blue",
                             stroke = T,radius = 5) %>%
            addCircleMarkers(data = healthcenters[healthcenters$type == "Family Medical Center",],
                             group = "Family Medical Center",
                             color = "lightblue", 
                             stroke = T,radius = 5) %>%
            addCircleMarkers(data = healthcenters[healthcenters$type == "Family Medical Teams",],
                             group = "Family Medical Teams", 
                             color = "springgreen4",
                             stroke = T,radius = 5) %>%
            addCircleMarkers(data = healthcenters[healthcenters$type == "Nurse Practitioner Clinic",],
                             group = "Nurse Practitioner Clinic",
                             color = "yellow", 
                             stroke = T,radius = 5) %>%
            addLayersControl(overlayGroups = c("Hospital", "Community Health Center","Walk-in Clinic",
                                               "Family Medical Center","Family Medical Teams",
                                               "Nurse Practitioner Clinic"), 
                             options = layersControlOptions(collapsed = F)) %>% 
            addProviderTiles("CartoDB.DarkMatter")
        
    })
    
   
    
    
 
    # observe({
    #     
    #     colorpal <- colorNumeric(
    #         palette = "viridis",
    #         domain = var(),
    #         reverse = T)
    # 
    #     leafletProxy("secondmap") %>%
    #         addPolygons(data = sociodemo,
    #                     fillColor = ~colorpal(var()),
    #                     color = "gray25",
    #                     fillOpacity = 0.9,
    #                     weight = 2) %>%
    #         addPolygons(data = nbh_to,
    #                     weight = 3,
    #                     color = "grey",
    #                     highlight = highlightOptions(weight = 5,
    #                                                  color = "black",
    #                                                  fillOpacity = 0.1,
    #                                                  bringToFront = TRUE),
    #                     label = sprintf(
    #                         "<strong>%s</strong>",
    #                         nbh_to$AREA_NA
    #                     ) %>% lapply(htmltools::HTML),
    #                     labelOptions = 
    #                         labelOptions(style = list("font-weight" = "normal", 
    #                                                   padding = "3px 8px"),
    #                                      textsize = "15px",
    #                                      direction = "auto")) %>%
    #         addLegend(pal = colorpal,
    #                   values = (var()),
    #                   position = "bottomright")
    # })
    
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
