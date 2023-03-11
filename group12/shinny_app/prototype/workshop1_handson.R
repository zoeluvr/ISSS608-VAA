library(shiny)
library(tidyverse)

conservation<-read.csv("E:/semester1/ISSS616-Applied Statistical Analysis with R-G1/Group Project/workshop/Workshop1/conservation_explanation.csv")

#Separating column using separate()
conservation %>%
  separate(`conservation.abbreviation`, 
           into = c("abbreviation", "conservation_status"), sep = " = ")

#Combining data across data frames: 
##toupper() change to uppercase abbreviations 
###Note that if there is no information in the second dataset that matches with the information in the first dataset, left_join() will add NA

conserve <- conservation %>%
  separate(`conservation.abbreviation`, 
           into = c("abbreviation", "conservation_status"), sep = " = ")
sleep_data <-msleep %>%
  mutate(conservation = toupper(conservation)) %>%
  left_join(conserve, by = c("conservation" = "abbreviation"))


unique(sleep_data$vore)

sleep_data<-sleep_data %>% 
  select(name, "diet"=vore, conservation_status, sleep_total:awake) %>% 
  mutate(diet=recode(diet,
                     carni="Carnivore",
                     omni="Omnivore",
                     herbi="Herbivore",
                     insecti="Carnivore"))


ui <- fluidPage(
  titlePanel("Sleepy Animals"),
 
  sidebarLayout(
    sidebarPanel(
      selectInput("diet",
                  label="Choose diet type",
                  choices = list("All","Carnivore", "Herbivore",
                                 "Omnivore"),
                  selected = "All")
      ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Table", tableOutput("table")
          ),
        tabPanel("Plot", plotOutput("plot"))
    )              
  )
)
)


server <- function(input, output, session) {
  output$table <-renderTable({
  data<-sleep_data
  if (input$diet !="All") {
    data <-sleep_data %>%
      filter(diet==input$diet)
  }
  data})
  output$plot <-renderPlot({
    data<-sleep_data
    if (input$diet !="All") {
      data <-sleep_data %>%
        filter(diet==input$diet)
    }
    qplot(data=data, x=diet, y=sleep_total, geom="boxplot")
  })
}

shinyApp(ui, server)
