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
  
  includeCSS("style.css"),

  # Load D3.js
  tags$head(
    tags$script(src = 'http://d3js.org/d3.v3.min.js')
  ),
  
  # Application title
  tags$img(src="https://www.nao.org.uk/wp-content/themes/nao2016/dist/images/nao_logo_400.png", style="float: right; margin: 15px"),
  titlePanel("NAO Parliament Monitor"),
  
  # side bar
  sidebarPanel(
    tags$h3('Search', class="skipMargin"),
    textInput("searchInput", label=NULL, value="e.g., free schools"),
    tags$i(textOutput('queryText')),
    tags$h4('Select database(s)'),
    checkboxInput("commonsOralQuestionsCheckBox", "Commons Oral Questions", TRUE),
    checkboxInput("commonsWrittenQuestionsCheckBox", "Commons Written Questions", TRUE),
    tags$h4('Select member(s)'),
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