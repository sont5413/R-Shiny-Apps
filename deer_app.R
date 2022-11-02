library(shiny)
library(grDevices)
library(tidyverse)
library(ggplot2)
library(dplyr)

deer_data <- read.csv("Survey_Data_Collated_ALL_SITES.csv")
ui <- fluidPage(
  
  titlePanel("Herd Composition Trends at TMD Training Site"),
  
  selectInput(inputId = "site",
              label = "Training site:",
              choices = list(Wolters = "WOLTERS",
                             Maxey = "MAXEY",
                             Swift = "SWIFT",
                             Bowie = "BOWIE",
                             Mabry = "MABRY")),
  selectInput(inputId = "y", 
              label = "Y-axis variable:",
              choices = c("Density" = "Density_acres_per_deer", 
                          "Fawn to doe ratio" = "Fawn_to_Doe_Ratio",
                          "Buck to doe ratio" = "Buck_to_Doe_Ratio"), 
              selected = "Density"),
  
  plotOutput(outputId = "plot"))
server <- function(input, output) {
  output$plot <- renderPlot({
    deer_data %>%
      filter(site == input$site) %>%
      ggplot(aes_string(x="year",y= input$y)) +
      geom_path(color =  "#b3697a", size = 1) + 
      geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
      labs(title= "", 
           y=input$y,x="Year")+
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))
    
  }, )
}
shinyApp(ui,server)
