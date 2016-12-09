library(shiny)
library(jsonlite)
library(XML)
library(RCurl)
library(DT)

# http://data.parliament.uk/membersdataplatform/services/mnis/members/query/house=Commons%7CIsEligible=true/http://data.parliament.uk/membersdataplatform/services/mnis/members/query/house=Commons%7CIsEligible=true/
# Members
json_file <- getURL("http://lda.data.parliament.uk/members.json?_pageSize=100", ssl.verifypeer = FALSE)
json_data <- fromJSON(json_file)
members_list <- json_data$result$items

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
    selectInput("select", label = "Members", 
                choices = members_list$fullName$'_value'),
    width = 3
  ),
  
  
  # Sidebar with a slider input for number of observations
  mainPanel(
          #htmlOutput('page_head'),
          plotOutput('bar_chart'),
          DT::dataTableOutput('table_data')
  )
    
))