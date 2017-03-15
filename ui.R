library(shiny)
library(shinydashboard)
library(jsonlite)
library(XML)
library(RCurl)
library(DT)

# http://data.parliament.uk/membersdataplatform/services/mnis/members/query/house=Commons%7CIsEligible=true/http://data.parliament.uk/membersdataplatform/services/mnis/members/query/house=Commons%7CIsEligible=true/
# Members
#json_file <- getURL("http://lda.data.parliament.uk/members.json?_pageSize=100", ssl.verifypeer = FALSE)
#json_data <- fromJSON(json_file)
#members_list <- json_data$result$items


shinyUI(
  dashboardPage(
    
    #includeCSS("style.css"),
    
    # Load D3.js
    #tags$head(
    #  tags$script(src = 'http://d3js.org/d3.v3.min.js')
    #),
    
    # Application title
    dashboardHeader(title="Parliament Monitor"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Text search", tabName="textSearch"),
        menuItem("PAC members", tabName="pacMembers")
      ),
      
      #flowLayout(
      #inputPanel(
        #tags$i(textOutput('queryText')),
        h4('Select database(s)'),
        checkboxInput("commonsOralQuestionsCheckBox", "Commons Oral Questions", TRUE),
        checkboxInput("commonsWrittenQuestionsCheckBox", "Commons Written Questions", TRUE)
        #tags$h4('Select member(s)'),
        #selectInput("select", label = NULL, choices = members_list$fullName$'_value')
      #)
      #)
    ),
    
    dashboardBody(
      
      tags$img(src="https://www.nao.org.uk/wp-content/themes/nao2016/dist/images/nao_logo_400.png", style="float: right; margin: 15px"),
      #tags$h2("NAO Parliament Monitor", class="leftPadding"),
      
      tabItems(
        tabItem(tabName="textSearch", 
                 #inputPanel(
                   tags$h3('Search'),
                   textInput("searchInput", label=NULL, value="e.g., National Audit Office"),
                 #)
                
                #htmlOutput('page_head'),
                plotOutput('text_search_bars'),
                DT::dataTableOutput('text_search_table')
        ),
        tabItem(tabName="pacMembers",
                 #inputPanel(
                   tags$h3('Select PAC members'),
                   checkboxInput("PAC", "All PAC members", FALSE),
                 #)
                plotOutput('pac_members_bars'),
                DT::dataTableOutput('pac_members_table')
        )#,
        #id = "tabs"
      )
    )
    
  ))