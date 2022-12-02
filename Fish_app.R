library(shiny)
library(Rmisc)
library(tidyverse)
library(ggplot2)
library(scales)

data = read.csv("fishData.csv")
fish_data = summarySE(data, 
                      measurevar = "fishLength", 
                      groupvars = c("size","location"))


ui <- fluidPage(
  
  titlePanel("Salmon Length by Size and Capture Location"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "size",
               label = "Fish Size:",
               choices = list(Fry = "fry",
                              Parr = "parr",
                              Smolt = "smolt"))
    ),
    
mainPanel(
  
  plotOutput(outputId = "plot")
  )
 )
)
server <- function(input, output) {
  output$plot <- renderPlot({
    fish_data %>%
      filter(size == input$size) %>%
    ggplot(aes(x = input$size,
               y = fishLength, 
               fill = location))+
      geom_point(aes(x = input$size,
                     y = fishLength, 
                     colour = location), 
                 color = "black", 
                 shape = 21, size = 7,
                 position = position_dodge(width = 0.25))+
      geom_errorbar(aes(ymin = fishLength - ci,
                        ymax = fishLength + ci, 
                        width = .075, colour = location), 
                    size = 1.25,
                    position = position_dodge(width = 0.25))+ 
      labs(y="Mean Length (mm)
    with 95% Confidence Intervals ",x = "Size")+
      scale_y_continuous(breaks = pretty_breaks(n = 10))+
      theme(axis.text.x = element_text(size = 16, 
                                       margin = margin(0.25, 0, 0, 0, "cm")),
            axis.text.y = element_text(size = 14, 
                                       margin = margin(0,0.25,  0, 0, "cm")),
            axis.title.x = element_text(size = 18, 
                                        margin = margin(0.5, 0, 0, 0, "cm")),
            axis.title.y = element_text(size = 16, 
                                        margin = margin(0, 0.5, 0, 0, "cm")), 
            legend.position = c(.85,.2),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            legend.key.size = unit(3,"line"),
            title = element_text(size = 16))
  }, )
}
shinyApp(ui,server)

