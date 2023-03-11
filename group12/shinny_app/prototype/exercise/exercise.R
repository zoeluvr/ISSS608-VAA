#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)

header <- 
  dashboardHeader( title = HTML("City of Engagament"))

siderbar <- 
  dashboardSidebar(
    sidebarMenu(id = 'sidebarmenu',
                menuItem("Demographics Analysis", 
                         tabName = "demographics_tab", startExpanded = FALSE, icon = icon("tachometer-alt"),
                         menuSubItem("Overall Demographics", tabName = "overall_demo_analysis"),
                         menuSubItem("Wage Analysis", tabName = "wage_analysis"), # TO-DO
                         menuSubItem("Expenditure Analysis", tabName = "expend_analysis") # TO-DO
                         #menuSubItem("Consume analysis", tabName = "consume_analysis")
                ),
                menuItem("Social Activity", tabName = "social_activity_tab", startExpanded = FALSE, icon = icon("users"),
                         menuSubItem("Overall Social Network", tabName = "network_tab"),
                         menuSubItem("Network by Group", tabName = "group_tab"),
                         menuSubItem("Network by Individual", tabName = "vis_tab")
                ),
                menuItem("Predominant Business", tabName = "predominant_business_tab", startExpanded = FALSE, icon = icon("building"),  
                         menuSubItem("Overall Town Map", tabName = "townmap_tab"),
                         menuSubItem("Cost Analysis", tabName = "venuetype_tab"),
                         menuSubItem("Check-in Analysis", tabName = "checkin_tab"),
                         menuSubItem("Revenue Analysis", tabName = "revenue_tab")
                )
    )
  )

ui <- dashboardPage(skin = "green",
                    header, 
                    siderbar,
                    dashboardBody())

server <- function(input, output) { }

shinyApp(ui, server)
