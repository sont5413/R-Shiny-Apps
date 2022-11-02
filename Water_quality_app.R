library(shiny)
library(Rmisc)
library(tidyverse)
library(ggplot2)

data = read.csv("Master_Dataset.csv")
wq_data = data %>%  
  group_by(site,waterbody_name_long) %>%  
  summarise_at(vars("ph_units","spcond_ms_cm","sal_ppt","ldo_sat","turb"),mean)

wq_data

ui <- fluidPage(
  titlePanel("Water-quality Parameters by Training Site"),
  
  sidebarLayout(
    
    sidebarPanel(
  
      selectInput(inputId = "site",
              label = "Site:",
              choices = list(Bowie = "BOWIE",
                             Swift = "SWIFT",
                             Mabry = "MABRY",
                             Wolters = "WOLTERS",
                            Maxey = "MAXEY")),
      selectInput(inputId = "y",
              label = "Parameter:",
              choices = c("pH Units" = "ph_units",
                          "Specific Conductance (mS/cm)" = "spcond_ms_cm",
                          "Salinity (ppt)" = "sal_ppt",
                          "LDO (% saturation)" = "ldo_sat",
                          "Turbidity (NTU)" = "turb"))
  ),
  
mainPanel(
  
  plotOutput(outputId = "plot")
  )
 )
)
server <- function(input, output) {
  output$plot <- renderPlot({
    wq_data %>%
      filter(site == input$site) %>%
      ggplot(aes_string(x = "waterbody_name_long", y = input$y, fill = "waterbody_name_long"))+
      geom_bar(stat="identity")+
      labs(y=input$y, x="Waterbody")+
      theme(axis.text.x = element_text(size = 12, 
                                       margin = margin(0.25, 0, 0, 0, "cm"),
                                       angle = 45, 
                                       vjust = 1, 
                                       hjust=1),
            axis.text.y = element_text(size = 14, 
                                       margin = margin(0,0.25,  0, 0, "cm")),
            axis.title.x = element_text(size = 18, 
                                        margin = margin(0.5, 0, 0, 0, "cm")),
            axis.title.y = element_text(size = 16, 
                                        margin = margin(0, 0.5, 0, 0, "cm")), 
            legend.position = "none")
  }, )
}
shinyApp(ui,server)

