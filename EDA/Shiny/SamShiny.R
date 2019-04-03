library(dplyr)
library(ggplot2)
library(shiny)
library(RColorBrewer)


data = readRDS(file = "data.rds")

ui = fluidPage(
  titlePanel("Demographic Analysis of Sam's Club Members",
             windowTitle = "Demo Insights Dashboard"),
  sidebarLayout(
    sidebarPanel(
      helpText("The Top Graph displays categorical demographic factors
               & the Bottom Graph displays numeric demographic factors"),
      selectInput(inputId = "y_var", label = "Select the Response Variable - Top Graph",
                  choices = c("Total Sales" = "TOTAL_SALES", 
                              "Renew/Non-Renew" ="renew_ind_1or0",
                              "Total Visits" = "TOTAL_VISITS")),
      selectInput(inputId = "x_var1", label = "X Variable - Categorical", 
                  choices =  c("Marital Status" = "marital_status_desc",
                                "Income" = "income_desc",
                               "Membership Type" = "MEMBERSHIP_TYPE_DESC")),
      selectInput(inputId = "x_var2", label = "X Variable - Numeric", 
                  choices =  c("HH Age" = "hhh_age_desc", 
                               "Miles to Club" = "MILES_TO_CLUB",
                               "HH Num of Children" = "nbr_children_desc",
                               "HH Size" = "hh_size_desc"))
    ),
    mainPanel(
      plotOutput(outputId = "plot1"),
      plotOutput(outputId = "plot2")
    )
  )
)


server = function(input, output){
  
  output$plot1 = renderPlot({
    
    data %>% 
      filter(marital_status_desc != 'NA', income_desc != 'NA') %>% 
      ggplot(aes_string(x = input$x_var1, y = (input$y_var), fill = input$x_var1)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_brewer(palette = "Reds") +
      labs(x = input$x_var1, y = input$y_var) +
      theme_classic() +
      theme(legend.position = "bottom")
    
  })
  
  output$plot2 = renderPlot({
    data %>% 
      filter(MILES_TO_CLUB<50) %>% 
      ggplot(aes_string(x = input$x_var2,  colour = "RENEW_IND")) +
      geom_density() +
      labs(x = input$x_var2, y = "Density", title = "Density Graph") +
      theme_classic() +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)






