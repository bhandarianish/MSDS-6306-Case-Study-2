#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tidyverse)

ui <- shinyUI(pageWithSidebar(
  
  headerPanel("Frito Lay Data (Select 1 per Attribute)"),
  
  sidebarPanel(
    checkboxInput("cb_cut", "Attrition(Yes)", FALSE),
    checkboxInput("cb_color", "Attrition(No)", FALSE),
    
    checkboxInput("cb_m", "Gender(Male)", FALSE),
    checkboxInput("cb_f", "Gender(Female)", FALSE),
    checkboxInput("cb_jll", "Job Level(1)", FALSE),
    checkboxInput("cb_jlh", "Job Level (4)", FALSE),
    
    
    
  ),
  
  mainPanel(
    DT::dataTableOutput("data_table"),
    plotOutput("data_plot")
  )
))


server <- shinyServer(function(input, output) {
  
  filtered_data <- reactive({
    dat <- (read.csv("https://raw.githubusercontent.com/bhandarianish/MSDS-6306-Case-Study-2/main/CaseStudy2-data.csv", header = TRUE))
    if (input$cb_cut) { dat <- dat %>% filter(dat$Attrition %in% "Yes") }
    if (input$cb_color) { dat <- dat %>% filter(dat$Attrition %in% "No") }
    if (input$cb_m) { dat <- dat %>% filter(dat$Gender %in% "Male") }
    if (input$cb_f) { dat <- dat %>% filter(dat$Gender %in% "Female") }
    if (input$cb_jll) { dat <- dat %>% filter(dat$JobLevel %in% "1") }
    if (input$cb_jlh) { dat <- dat %>% filter(dat$JobLevel %in% "4") }
    
    dat
  })
  
  output$data_table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$data_plot <- renderPlot({
    boxplot(filtered_data()$MonthlyIncome, main = "Box Plot of Salary", ylab = "Monthly Salary ($)")
    
    output$data_plot <- renderPlot({
      hist(filtered_data()$MonthlyIncome, main = "Histogram of Salary", ylab = "Monthly Salary ($)")
      
      
    })   
  })
})