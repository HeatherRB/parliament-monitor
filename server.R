library(shiny)
library(jsonlite)
library(XML)
library(RCurl)
#library(httr) # for GET command https://cran.r-project.org/web/packages/httr/httr.pdf
library(DT) # for data tables http://rstudio.github.io/DT/
library(plyr)
library(dplyr)
library(ggplot2)
library(leaflet) # for plotting maps https://rstudio.github.io/leaflet/
library(rgdal) # for reading shape files (readOGR) https://www.r-bloggers.com/things-i-forget-reading-a-shapefile-in-r-with-readogr/
library(htmltools)

mnisIdsPAC <- c(1524, 1451, 4388, 3971, 4040, 389, 4451, 3929, 4134, 4136, 4249, 4046, 1454, 4444, 4531)
# source: http://www.parliament.uk/business/committees/committees-a-z/commons-select/public-accounts-committee/membership/

# set up data on party colours
Party <- c("Labour", "Labour (Co-op)", "Conservative", 
             "Scottish National Party", "Plaid Cymru", "Green Party",
           "Speaker", "Liberal Democrat", "UK Independence Party", "Independent",
           "Democratic Unionist Party", "Ulster Unionist Party", 
           "Social Democratic & Labour Party", "Sinn Fein")

colour <- c("red", "red", "royalblue",
             "yellow", "forestgreen", "green",
             "gray", "orange", "purple", "gray",
             "darkred", "darkblue",
            "forestgreen", "darkgreen")
partyColours <- data.frame(Party, colour)
partyColoursGB <- partyColours[1:10,]
partyColours$Party <- factor(partyColours$Party)
partyColours$colour <- factor(partyColours$colour)

# list to store info on individual MPs
# http://data.parliament.uk/MembersDataPlatform/memberquery.aspx
MPs_XML <- getURL("http://data.parliament.uk/membersdataplatform/services/mnis/members/query/house=Commons/", ssl.verifypeer = FALSE)
xmlfile <- xmlTreeParse(MPs_XML)
MPs_data <- xmlSApply(xmlRoot(xmlfile), function(x) c(xmlGetAttr(x, "Member_Id"), xmlSApply(x, xmlValue)))
MPs_df <- data.frame(t(MPs_data),row.names=NULL)
#sapply(MPs_df, class)
MPs_df$Party <- do.call("c", lapply(MPs_df$Party, "[[", 1))
MPs_df$Party <- factor(MPs_df$Party)
MPs_df$Gender <- do.call("c", lapply(MPs_df$Gender, "[[", 1))
MPs_df$Gender <- factor(MPs_df$Gender)
MPs_df$MemberFrom <- do.call("c", lapply(MPs_df$MemberFrom, "[[", 1))
MPs_df$DisplayAs <- do.call("c", lapply(MPs_df$DisplayAs, "[[", 1))
MPs_df <- mutate(MPs_df, mnisId=as.character(V1))
MPs_df <- select(MPs_df, -V1)

# function to check for null values and replace with 'NA'
elseNA <- function(x){
  if (is.null(x)) {
    return('NA')
  } else {
    return(x)
  }
}

getMnisId <- function(url) {
  if (is.character(url)) {
    v <- strsplit(url, "/")[[1]]
  } else {
    return('NA')
  }
  if (length(v)>=5) {
    return(v[[5]])
  } else {
    return('NA')
  }
}

collapseList <- function(l) {
  if (!is.list(l)) {
    return('NA')
  } else {
    return(l[[1]])
  }
}

collapseAbout <- function(l) {
  if (!is.list(l)) {
    return('NA')
  } else {
    return(l$'_about')
  }
}

lapply(MPs_df$Party, "[[", 1)

# Get info on individual MPs
getMPInfo <- function(mnisId){
  urlString <- paste("http://lda.data.parliament.uk/members/", mnisId, ".json", sep="")
  json_file <- getURL(urlString, ssl.verifypeer = FALSE)
  json_data <- fromJSON(json_file)
  info <- json_data$result$primaryTopic
  MPInfo <- vector(length = 8)
  #MPInfo.names <- c("name", "Id", "constituency", "constituencyURL", "gender", "website", "party", "twitter")
  MPInfo <- c(
              name = elseNA(info$fullName$'_value'[1]),
              Id = elseNA(mnisId),
              constituency = elseNA(info$constituency$label$'_value'[1]),
              constituencyURL = elseNA(info$constituency$'_about'[1]),
              gender = elseNA(info$gender$'_value'[1]),
              website = elseNA(info$homePage[1]),
              party = elseNA(info$party$'_value'[1]),
              twitter = elseNA(info$twitter$'_value'[1])
              )
  return(MPInfo)
}

getMemberUrl <- function(list) {
  if(!is.null(list$tablingMember$'_about')) {
    return(list$tablingMember$'_about')
  } else {
    return(do.call("c", lapply(list$tablingMember, collapseAbout)))
  }
}

# Clean and format json data for written questions
cleanWrittenQs <- function(json_data){
  list <- json_data$result$items
  list$AnswerDate <- list$AnswerDate$'_value'
  list$dateTabled <- list$dateTabled$'_value'
  list$tablingMemberUrl <- getMemberUrl(list)
  list$tablingMemberPrinted <- do.call("c", lapply(list$tablingMemberPrinted, collapseList))
  list$AnsweringBody <- do.call("c", lapply(list$AnsweringBody, collapseList))
  list$type <- "Commons Written Question"
  list$mnisId <- do.call("c", lapply(list$tablingMemberUrl, getMnisId))
  list <- select(list, About=`_about`, AnswerDate, AnsweringBody, dateTabled, questionText, tablingMemberUrl, tablingMemberPrinted, mnisId, type)
  return(list)
}

# Clean and format json data for oral questions
cleanOralQs <- function(json_data){
  list <- json_data$result$items
  list$AnswerDate <- list$AnswerDate$'_value'
  list$dateTabled <- list$dateTabled$'_value'
  list$tablingMemberUrl <- getMemberUrl(list)
  list$tablingMemberPrinted <- do.call("c", lapply(list$tablingMemberPrinted, collapseList))
  list$AnsweringBody <- do.call("c", lapply(list$AnsweringBody, collapseList))
  list$title <- list$Location$prefLabel$'_value'
  list$type <- "Commons Oral Question"
  list$mnisId <- do.call("c", lapply(list$tablingMemberUrl, getMnisId))
  list <- select(list, About=`_about`, AnswerDate, AnsweringBody, dateTabled, questionText, tablingMemberUrl, tablingMemberPrinted, mnisId, type)
  return(list)
}

# Get json data and append to existing results
getJsonData <- function(urlString, results_list, qType){
  
  if(is.null(urlString)) {
    return(results_list)
  }
  
  #get json data
  json_file <- getURL(urlString, ssl.verifypeer = FALSE)
  json_data <- fromJSON(json_file)
  
  # clean json data according to its type
  if (qType == "oral") {
    new_results <- cleanOralQs(json_data)
  } else if (qType == "written") {
    new_results <- cleanWrittenQs(json_data)
  } else {
    new_results <- NULL
  }
  
  # append to existing data
  if (is.null(results_list)) {
    results_list <- new_results
  } else if (!is.null(new_results)) {
    results_list <- rbind(results_list, new_results)
  }
  
  return(results_list)
}
  
shinyServer(function(input, output, session) {
  
  # search criteria
  queryString <- reactive({
    string <- sub("e.g., ", "", input$searchInput)
    string <- gsub(" ", "+AND+", string)
    paste("&_search=", string, sep="")
  })
  
  # load data for text search
  text_search_data <- reactive({
    results_list <- NULL
    pageSize <- input$text_search_results
    if (input$commonsWrittenQuestionsCheckBox) {
      # urlString <- "http://lda.data.parliament.uk/commonswrittenquestions.json?_view=Written+Questions&_pageSize=25" 
      urlString <- paste("http://lda.data.parliament.uk/commonswrittenquestions.json?_view=Written+Questions&_pageSize=", pageSize, "&_search=", queryString(), sep="")
      results_list <- getJsonData(urlString, results_list, "written")
    }
    if (input$commonsOralQuestionsCheckBox) {
      urlString <- paste("http://lda.data.parliament.uk/commonsoralquestions.json?_view=Commons+Oral+Questions&_pageSize=", pageSize, "&_search=", queryString(), sep="")
      results_list <- getJsonData(urlString, results_list, "oral")
    }
    
    # incorporate data on MPs
    results_list <- merge(results_list, select(MPs_df, mnisId, MemberFrom, Party))
    
    results_list <- results_list[rev(order(as.Date(results_list$dateTabled))),]
    results_list
  })
  
  MPs_data <- reactive({
    v <- unique(text_search_data()$mnisId)
    MPs.names <- v
    for (Id in v) {
      MPs[[Id]] <- getMPInfo(Id)
    }
    t(as.data.frame(MPs))
  })
  
  # load data for PAC members
  pac_members_data <- reactive({
    results_list <- NULL
    if (input$commonsWrittenQuestionsCheckBox) {
      for (mnisId in mnisIdsPAC) {
        pageSize <- input$pac_member_results
        urlString <- paste("http://lda.data.parliament.uk/commonswrittenquestions.json?mnisId=", mnisId, "&_view=Written+Questions&_pageSize=", pageSize, sep="")
        results_list <- getJsonData(urlString, results_list, "written")
      }
    }
    if (input$commonsOralQuestionsCheckBox) {
      for (mnisId in mnisIdsPAC) {
        urlString <- paste("http://lda.data.parliament.uk/commonsoralquestions.json?mnisId=", mnisId, "&_view=Commons+Oral+Questions&_pageSize=", pageSize, sep="")
        results_list <- getJsonData(urlString, results_list, "oral")
      }
    }
    results_list <- results_list[rev(order(as.Date(results_list$dateTabled))),]
    results_list
  })
  
  # bars
  #bars <- reactive({
    #results_list <- data()
    #results_list$fdate <- factor(format(as.Date(results_list$dateTabled),'%Y-%m'))
    #results_list$month <- factor(format(as.Date(results_list$dateTabled),'%b %y'))
    #bars_data <- summarise(group_by(results_list, fdate, month), n=n())
    #date1 <- paste(bars_data$fdate[1],"01", sep="-")
    #date2 <- paste(bars_data$fdate[nrow(bars_data)],"01", sep="-")
    #month_list <- data.frame(date=c(seq(as.Date(date1),to=as.Date(date2),by='month')))
    #month_list$fdate <- factor(format(as.Date(month_list$date),'%Y-%m'))
    #merge(month_list, bars_data, all.x=TRUE)
  #})
  
  #output$bar_chart <- renderPlot({
    #barplot(bars()$n, names.arg = bars()$month)
    #ggplot(bars()$n, aes=(bars()$month))
    #results_list <- text_search_data()
    #results_list$fdate <- as.Date(results_list$dateTabled)
    #summarise(group_by(results_list, fdate), n=n()) %>%
    #ggplot(results_list, aes(fdate)) + geom_histogram(binwidth=7) #+ scale_x_date(date_labels = "%b %Y", limits = c(as.Date("2016-01-01"), Sys.Date()))
  #})

  # put MPs data into a DT data table
  # http://rstudio.github.io/DT/
  output$MPs <- DT::renderDataTable(
    #MPs_data()$linked_Name = paste("<a href=", constituencyURL, ">", name, "</a>")) %>%
    select(MPs_df, DisplayAs, Gender, Party, MemberFrom) %>%
    datatable(escape=TRUE, 
              filter = 'top',
              colnames = c('Name' = 'DisplayAs', 'Constituency' = 'MemberFrom'),
              caption = htmltools::tags$caption("This table lists all current House of Commons MPs, as listed on ", htmltools::a(href="http://data.parliament.uk/MembersDataPlatform/memberquery.aspx", target="_blank", "UK Parliament's Members' Names Data Platform"), "."),
              options = list(pageLength = 50))
  )
  
  output$MPs_map <- renderLeaflet({
    # open and transform constituency boundary file
    # 
    boundaries <- readOGR(dsn="shapefiles", layer="Westminster_Parliamentary_Constituencies_December_2015_Super_Generalised_Clipped_Boundaries_in_Great_Britain")
    boundaries2 <- spTransform(boundaries, CRS("+init=epsg:4326"))
    
    # merge in data on MPs
    # Q: Are there gaps after merging on name?
    boundaries2 <- merge(boundaries2, select(MPs_df, mnisId, DisplayAs, MemberFrom, Party), by.x="pcon15nm", by.y="MemberFrom")
    boundaries2 <- merge(boundaries2, partyColours)
    
    # Q: How to set deafult zoom?
    # labels script https://rpubs.com/bhaskarvk/leaflet-labels
    leaflet(boundaries2) %>%
      addPolygons(stroke=TRUE, color = "#333333", weight=0.5, opacity = 1, fillOpacity = 0.7,
                  label=mapply(function(x, y, z) {
                    htmltools::HTML(sprintf("%s <br>Member: %s, %s", htmlEscape(x), htmlEscape(y), htmlEscape(z)))}, 
                    boundaries2$pcon15nm, boundaries2$DisplayAs, boundaries2$Party, SIMPLIFY = F),
                  fillColor = ~colour) %>%
      addLegend(colors = partyColoursGB$colour, labels = partyColoursGB$Party, opacity = 0.7)
  })
    
  output$text_search_table <- DT::renderDataTable(
    text_search_data() %>% 
      select(-mnisId, -About, -tablingMemberUrl) %>% 
      datatable()
      #datatable(colnames = c('Date' = 'dateTabled', 'Tabling member' = 'tablingMemberPrinted', 'Question text' = 'questionText', 'Answering body' = 'AnsweringBody'), escape=FALSE)
  )
  
  output$text_search_bars <- renderPlot({
    results_list <- text_search_data()
    results_list$fdate <- as.Date(results_list$dateTabled)
    ggplot(results_list, aes(fdate)) + geom_histogram() + labs(title = "Number of questions over time", x="Date", y="Number of questions")
  })
  
  output$pac_members_table <- DT::renderDataTable(
    pac_members_data() %>% 
      select(dateTabled, tablingMemberPrinted, questionText, AnsweringBody, type) %>% 
      datatable(colnames = c('Date' = 'dateTabled', 'Tabling member' = 'tablingMemberPrinted', 'Question text' = 'questionText', 'Answering body' = 'AnsweringBody'), escape=FALSE)
  )
  
  output$pac_members_bars <- renderPlot({
    results_list <- pac_members_data()
    results_list$fdate <- as.Date(results_list$dateTabled)
    ggplot(results_list, aes(fdate)) + geom_histogram(binwidth=7)
  })
  
  output$party_bars <- renderPlot({
    ggplot(data=text_search_data(), aes(factor(Party))) + geom_bar() + coord_flip()
  })
  
  output$page_head <- renderUI({
    HTML(paste("<b>Dimensions:</b><br/><ul><li>"))
  })
  
  output$queryText <- renderText({ 
    queryString()
  })
  
  output$tabText <- renderText({ 
    tab()
  })
  
  
})