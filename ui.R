library(shiny)
library(shinydashboard) # https://rstudio.github.io/shinydashboard/
library(jsonlite)
library(XML)
library(RCurl)
library(DT) # for data tables http://rstudio.github.io/DT/
library(leaflet) # for plotting maps https://rstudio.github.io/leaflet/

# http://data.parliament.uk/membersdataplatform/services/mnis/members/query/house=Commons%7CIsEligible=true/http://data.parliament.uk/membersdataplatform/services/mnis/members/query/house=Commons%7CIsEligible=true/
# Members
#json_file <- getURL("http://lda.data.parliament.uk/members.json?_pageSize=100", ssl.verifypeer = FALSE)
#json_data <- fromJSON(json_file)
#members_list <- json_data$result$items


shinyUI(
  dashboardPage(skin = "blue",
    
    #includeCSS("style.css"),
    
    # Load D3.js
    #tags$head(
    #  tags$script(src = 'http://d3js.org/d3.v3.min.js')
    #),
    
    # Application title
    dashboardHeader(title="NAO Parliament Monitor", titleWidth = 275),
    
    dashboardSidebar(
      width = 275,
      h4(' Select function:'),
      sidebarMenu(
        menuItem("Text search", tabName="textSearch", icon = icon("search")),
        menuItem("PAC", tabName="pacMembers", icon = icon("institution")),
        menuItem("House of Commons MPs", tabName="MPs", icon = icon("users"),
                 menuSubItem("Search members", tabName="MPs_table", icon = NULL),
                 menuSubItem("Constituency map", tabName="MPs_map", icon = NULL)
                 ),
        menuItem("Select databases", tabName="databases", icon = icon("database"), 
                 menuSubItem(icon = NULL, checkboxInput("commonsOralQuestionsCheckBox", "Commons Oral Questions", TRUE)),
                 menuSubItem(icon = NULL, checkboxInput("commonsWrittenQuestionsCheckBox", "Commons Written Questions", TRUE))
                 ),
        menuItem("Options", tabName="options", icon = icon("cog"), 
                 menuSubItem(icon = NULL, selectInput("text_search_results", "Number of search results per database:",c("10" = 10, "25" = 25, "50" = 50, "100" = 100), selected=50)),
                 menuSubItem(icon = NULL, selectInput("pac_member_results", "Number of results per PAC member:",c("10" = 10, "25" = 25, "50" = 50, "100" = 100), selected=10))
        )
      )
    ),
    
    dashboardBody(
      
      tags$img(src="https://www.nao.org.uk/wp-content/themes/nao2016/dist/images/nao_logo_400.png", style="float: right; margin: 15px"),
      #tags$h2("NAO Parliament Monitor", class="leftPadding"),
      
      tabItems(
        tabItem(tabName="textSearch", 
                tags$h2('Search Commons questions'),
                textInput("searchInput", label=NULL, value="e.g., National Audit Office"),
                fluidRow(
                         box(width=6, plotOutput('text_search_bars')),
                         box(width=6, plotOutput('party_bars'))
                         ),
                DT::dataTableOutput('text_search_table')
        ),
        tabItem(tabName="pacMembers",
                tags$h2('Questions from PAC members'),
                #checkboxInput("PAC", "All PAC members", FALSE),
                fluidRow(
                  box(width=12, plotOutput('pac_members_bars'))
                ),
                DT::dataTableOutput('pac_members_table')
        ),
        tabItem(tabName="MPs_table",
                tags$h2('Search House of Commons MPs'),
                DT::dataTableOutput('MPs')
        ),
        tabItem(tabName="MPs_map",
                tags$h2('Constituency map'),
                leafletOutput('MPs_map', width = "100%", height = 800)
        )
        #id = "tabs"
      )
    )
    
  ))