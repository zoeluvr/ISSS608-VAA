library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(leaflet)
library(leaflet.extras)

ec_geo2 <- read_xlsx("E:/semester1/ISSS616-Applied Statistical Analysis with R-G1/Workshop shiny/Workshop2/ASAR_Workshop2/data/ec_geo2.xlsx")
ec_data <- read_csv('E:/semester1/ISSS616-Applied Statistical Analysis with R-G1/Workshop shiny/Workshop2/ASAR_Workshop2/data/ec_data_workshop.csv')

ec_data$Floor_Level <- factor(ec_data$Floor_Level, labels = c("Ground Floor", "Bottom Half", "Top Half", "Penthouse"))
ec_data$mrt_distance_grp <- factor(ec_data$mrt_distance_grp, labels = c("< 1km", "1-2 km", "> 2km"))

#########################################################Shiny STARTS FROM HERE
ui <- dashboardPage(skin = 'yellow',
                    dashboardHeader(title = 'Executive Condominium Sales Analysis', titleWidth = 400),
                    dashboardSidebar(width = 400,
                                     sidebarMenu(id = 'sbm',
                                                 menuItem('Introduction', tabName = 'Introduction', icon = NULL),
                                                 menuItem('Descriptive', tabName = 'Descriptive', icon = NULL)
                                     )
                    ),
                    dashboardBody(
                        tabItems(
                            tabItem(tabName = 'Introduction',
                                    fluidPage(
                                        titlePanel("All Executive Condominiums in Singapore"),
                                        sidebarLayout(
                                            sidebarPanel( 
                                                selectInput("region", "Region:",
                                                            choices = 
                                                                list('All', "North East Region", "North Region", "East Region", "West Region", "Central Region"
                                                                                  )),
                                                            
                                                                            
                                                hr(),
                                                helpText("Executive condominiums by Region."),
                                                Position = "top"
                                        ),
                                            mainPanel(
                                                leafletOutput("map", height = 500)
                                           )
                                        ),
                                                   
                                        fluidRow(
                                            column(width = 12,
                                                   box(
                                                       width = 12,
                                                       height = 800,
                                                       solidHeader = TRUE,
                                                       collapsible = FALSE,
                                                       collapsed = FALSE,
                                                       plotOutput('regionanalysis', height = 750)
                                                   )
                                            )
                                        )
                                        )
                                    ),
                                    ###end of introduction
                            tabItem(tabName = 'Descriptive',
                                    fluidPage(
                                        titlePanel("Descriptive Statistics"),
                                        fluidRow(
                                            column(width = 12,
                                                tabsetPanel(
                                                tabPanel("Price by Factors",
                                                         box(
                                                           radioButtons('xcol',
                                                                        label = tags$strong('Analyse Sales By:'),
                                                                        choices = c('Floor level' = 'Floor_Level',
                                                                                    'Disctance from MRT' = 'mrt_distance_grp',
                                                                                    'Completion date' = '`Completion Date`'),
                                                                        inline = TRUE)
                                                         ),
                                                         box(
                                                           width = 12,
                                                           height = 800,
                                                           solidHeader = TRUE,
                                                           collapsible = FALSE,
                                                           collapsed = FALSE,
                                                           plotOutput('descriptiveAnalysis', height = 750)
                                                         )
                                                         ),                                                
                                                tabPanel("Sales Count by Region", 
                                                         box(
                                                           width = 12,
                                                           height = 800,
                                                           solidHeader = TRUE,
                                                           collapsible = FALSE,
                                                           collapsed = FALSE,
                                                           selectInput(
                                                             inputId = "Year", 
                                                             label = "Select years:", 
                                                             choices = 2000:2019),
                                                           plotOutput(outputId  = "piechart", 
                                                                      height = "600")
                                                         )
                                                         )
                                                ),
                                            )
                                        )
                                        #end of tabname "Descriptive" 
                                    ))),
                                ))
##########################################################################################Server code starts here
                                
server <- function(input, output) {
    
    pal <- colorFactor(palette = c("red", "green", "blue", "orange", "purple"),
                                           levels = c("East Region", "North East Region", 
                                                      "West Region", "North Region", "Central Region"))

    output$map <- renderLeaflet({
      if (input$region=="All"){
        display_map<-ec_geo2 %>%
        leaflet() %>%
        setView(lng = 103.8522, lat = 1.347510, zoom = 11) %>%
        addTiles() %>%
        addCircleMarkers(label = ~ pjname, color = ~ pal(`Planning Region`), radius = 3, fillOpacity = 0.5) %>%
        addSearchOSM() %>%
        addLegend(
          "bottomleft",
          pal = pal,
          values = ~`Planning Region`,
          opacity = 1, #color transparency
          title = "Regions")}
      
      if(input$region!="All") {
            display_map<-ec_geo2 %>%
            filter(`Planning Region` == input$region) %>%
            leaflet() %>%
            setView(lng = 103.8522, lat = 1.347510, zoom = 11) %>%
            addTiles() %>%
            addCircleMarkers(label = ~ pjname, color = ~ pal(input$region), radius = 3, fillOpacity = 0.5) %>%
            addLegend(
                "bottomleft",
                pal = pal,
                values = ~`Planning Region`,
                opacity = 1, #color transparency
                title = "Regions")
      }
      display_map
    })
    
    output$regionanalysis <- renderPlot({
        data_rv <- ec_data %>%
            group_by(`Planning Region`, Sale_Year) %>%
            summarise(region_value = mean(`Unit Price ($ psm)`, na.rm = T))
        data_rv$Sale_Year <- as.numeric(data_rv$Sale_Year)

        p <- ggplot(data_rv, aes(y = region_value, x = Sale_Year)) +
            geom_smooth(aes(col = data_rv$`Planning Region`), method = 'lm', se = FALSE) + 
            geom_point(aes(col = data_rv$`Planning Region`), size = 2.5) + 
            labs(title = 'Sales Price by Year', subtitle = paste('by', 'Region'),
                 col = 'Planning Region',x = 'Sales Year', y = 'Sales Price ($)',
                 fill = data_rv$`Planning Region`)
        return(p)

    })
    
####################################################end of introduction
    
    output$descriptiveAnalysis <- renderPlot({
        analysis <- ec_data %>%
            group_by_(.dots = input$xcol) %>%
            filter(`Completion Date` != 'Uncompleted', `Completion Date` != 'Unknown') %>%
            summarise(basket_value = mean(`Unit Price ($ psm)`, na.rm = T))
        
        p <- ggplot(analysis, aes_string(y = 'basket_value', x = input$xcol)) +
            geom_bar(aes_string(fill = input$xcol), stat = 'identity') +
            labs(title = 'Average Sales Price', subtitle = paste('by', input$xcol), 
                 x = input$xcol, y = 'Sales Price ($)',
                 fill = input$xcol)
        return(p)
            
        })
    

    output$piechart <- renderPlot({
        data<-ec_data %>%
            filter(Sale_Year == input$Year) %>%
            group_by(`Planning Region`) %>%
            summarise(count = n())
        data<-data %>%
          arrange(desc(`Planning Region`)) %>%
          mutate(prop=round(count/sum(data$count)*100,2)) %>% 
          mutate(ypos=round(cumsum(prop)-0.5*prop, 2))
        
        ggplot(data, aes(x="", y=prop, fill=`Planning Region`)) +
          geom_bar(stat="identity", width=1, color="white") +
          coord_polar("y", start=0)+
          theme_void() +
          geom_text(aes(y=ypos, label=paste0(prop,"%")), color="white")+
          labs(title = 'The proportion of sale counts for different Planning Regions',
                     x = 'Planning Region') +
          theme(plot.title = element_text(color = "Black", size = 14))
    })
#################end of descriptive analysis

}


shinyApp(ui, server)


  
                                
                                