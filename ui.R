library(shiny)
library(shinydashboard) # https://rstudio.github.io/shinydashboard/
library(jsonlite)
library(XML)
library(RCurl)
library(DT) # for data tables http://rstudio.github.io/DT/
library(leaflet) # for plotting maps https://rstudio.github.io/leaflet/

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
        menuItem("Home", tabName="home", icon = icon("home")),
        menuItem("Search questions", tabName="textSearch", icon = icon("search")),
        menuItem("PAC", tabName="PAC", icon = icon("institution"),
                 menuSubItem("View members", tabName="view_PAC", icon = NULL),
                 menuSubItem("Constituency map", tabName="PAC_map", icon = NULL),
                 menuSubItem("Questions from PAC members", tabName="PAC_questions", icon = NULL)
                 ),
        menuItem("House of Commons MPs", tabName="MPs", icon = icon("users"),
                 menuSubItem("Search members", tabName="MPs_table", icon = NULL),
                 menuSubItem("Constituency map", tabName="MPs_map", icon = NULL)
                 ),
        menuItem("Select question types", tabName="databases", icon = icon("database"), 
                 menuSubItem(icon = NULL, checkboxInput("commonsOralQuestionsCheckBox", "Commons Oral Questions", TRUE)),
                 menuSubItem(icon = NULL, checkboxInput("commonsWrittenQuestionsCheckBox", "Commons Written Questions", TRUE))
                 )
      )
    ),
    
    dashboardBody(
      
      tags$img(src="https://www.nao.org.uk/wp-content/themes/nao2016/dist/images/nao_logo_400.png", style="float: right; margin: 15px"),
      #tags$h2("NAO Parliament Monitor", class="leftPadding"),
      
      tabItems(
        tabItem(tabName="home",
                tags$br(),
                tags$h2('Welcome to NAO Parliament Monitor'),
                tags$p("Use this tool to search live data from Parliament's online database,",
                  tags$a(href="http://data.parliament.uk/", target="_blank", "data.parliament.uk"),"."
                ),
                tags$br(),
                tags$div('The menu on the left allows you to switch between three main functions:',
                  tags$ul(
                    tags$li(tags$b('Keyword search'), " - Search written and oral questions from the House of Commons with keyword(s). The search covers all fields, so you can search on the question, an MPs name, or the body answering the question. You can then filter your results by date, question type and political party."),
                    tags$li(tags$b('PAC'), " - View written and oral questions from members of PAC. As with the text search, you can then filter the results by date, question type and political party."),
                    tags$li(tags$b('House of Commons MPs'), " - Listing of members of the House of Commons, including constituency and political party. You can search the listing as a table or view a constituency map.")
                  )
                ),
                tags$div('All of the main functions are controlled by the',
                    tags$b('Select question types'),
                    'section, which allows you to choose between searching written questions, oral questions or both.'
                )
        ),
        tabItem(tabName="textSearch", 
                tags$h2('Search Commons questions'),
                tags$br(),
                textInput("searchInput", label="Search term", value="e.g., National Audit Office"),
                selectInput("text_search_results", "Number of results per question type:",c("10" = 10, "25" = 25, "50" = 50, "100" = 100), selected=25),
                fluidRow(
                         box(width=7, plotOutput('text_search_bars')),
                         box(width=5, plotOutput('party_bars'))
                         ),
                DT::dataTableOutput('text_search_table')
        ),
        tabItem(tabName="PAC_questions",
                tags$h2('Questions from PAC members'),
                tags$br(),
                selectInput("pac_member_results", "Number of results per PAC member:",c("10" = 10, "25" = 25, "50" = 50, "100" = 100), selected=10),
                tags$p('This page may take a few seconds to load - please be patient!'),
                #checkboxInput("PAC", "All PAC members", FALSE),
                fluidRow(
                  box(width=12, plotOutput('pac_members_bars'))
                ),
                DT::dataTableOutput('pac_members_table')
        ),
        tabItem(tabName="view_PAC",
                tags$h2('Members of PAC'),
                DT::dataTableOutput('PAC_table')
        ),
        tabItem(tabName="PAC_map",
                tags$h2("PAC members' constituency map"),
                tags$br(),
                tags$p('This page may take a few seconds to load - please be patient!'),
                leafletOutput('PAC_map', width = "100%", height = 800)
        ),
        tabItem(tabName="MPs_table",
                tags$h2('Search House of Commons MPs'),
                DT::dataTableOutput('MPs')
        ),
        tabItem(tabName="MPs_map",
                tags$h2('Constituency map'),
                tags$br(),
                tags$p('This page may take a few seconds to load - please be patient!'),
                leafletOutput('MPs_map', width = "100%", height = 800)
        )
        #id = "tabs"
      )
    )
    
  ))