library(shiny)
library(jsonlite)
library(XML)
library(RCurl)
library(DT)

# Define UI for application that plots random distributions 
shinyUI(fluidPage(

  # Load D3.js
  tags$head(
    tags$script(src = 'http://d3js.org/d3.v3.min.js')
  ),
  
  # Application title
  titlePanel("NAO Parliament Monitor"),
  
  sidebarPanel(
    textInput("searchInput", "Search", value="e.g., free schools"),
    tags$i(textOutput('queryText')),
    checkboxInput("oralQuestionsBox", "Commons Oral Questions", TRUE),
    checkboxInput("writtenQuestionsBox", "Written Questions", TRUE),
    width = 3
  ),
  
  
  # Sidebar with a slider input for number of observations
  mainPanel(
          #htmlOutput('page_head'),
          plotOutput('bar_chart'),
          DT::dataTableOutput('table_data')
  )
    
))