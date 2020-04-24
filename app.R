library(shiny)
library(dplyr)
library(shinydashboard)
library(tidyr)


#Define UI for data upload app
ui <- dashboardPage(
    
    #App title
    dashboardHeader(title = "Health care services in Toronto",
                    titleWidth = 300),
    
    #Sidebar contents (tabs)
    dashboardSidebar(width = 170,
                     sidebarMenu(
                         menuItem("Introduction",tabName = "intro",icon=icon("book-open")))),
    
    
    
    #Main panel for displaying outputs
    dashboardBody(
        #organizing what will go in the different tabs
        tabItems(
            #First tab "intro" and connecting it to the readME file in the directory
            tabItem(tabName = "intro"),
            #Second tab "code" and connecting it to this code file
            tabItem(tabName = "code")
            #third tab content "HIV" and organizing the different sections that will go in
            
        ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
