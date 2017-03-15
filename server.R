library(shiny)
library(jsonlite)
library(XML)
library(RCurl)
library(DT)
library(plyr)
library(dplyr)
library(ggplot2)

mnisIdsPAC <- c(1524, 1451, 4388, 3971, 4040, 389, 4451, 3929, 4134, 4136, 4249, 4046, 1454, 4444, 4531)
# source: http://www.parliament.uk/business/committees/committees-a-z/commons-select/public-accounts-committee/membership/

# Clean and format json data for written questions
cleanWrittenQs <- function(json_data){
  list <- json_data$result$items
  list$AnswerDate <- list$AnswerDate$'_value'
  list$dateTabled <- list$dateTabled$'_value'
  list$tablingMemberUrl <- list$tablingMember$'_about'
  list$tablingMemberPrinted <- do.call("c", lapply(list$tablingMemberPrinted, "[[", 1))
  list$AnsweringBody <- do.call("c", lapply(list$AnsweringBody, "[[", 1))
  list$type <- "Commons Written Question"
  list <- select(list, About=`_about`, AnswerDate, AnsweringBody, dateTabled, questionText, tablingMemberUrl, tablingMemberPrinted, type)
  return(list)
}

# Clean and format json data for oral questions
cleanOralQs <- function(json_data){
  list <- json_data$result$items
  list$AnswerDate <- list$AnswerDate$'_value'
  list$dateTabled <- list$dateTabled$'_value'
  list$tablingMemberUrl <- list$tablingMember$'_about'
  list$tablingMemberPrinted <- do.call("c", lapply(list$tablingMemberPrinted, "[[", 1))
  list$AnsweringBody <- do.call("c", lapply(list$AnsweringBody, "[[", 1))
  list$title <- list$Location$prefLabel$'_value'
  list$type <- "Commons Oral Question"
  list <- select(list, About=`_about`, AnswerDate, AnsweringBody, dateTabled, questionText, tablingMemberUrl, tablingMemberPrinted, type)
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
    if (input$commonsWrittenQuestionsCheckBox) {
      urlString <- paste("http://lda.data.parliament.uk/commonswrittenquestions.json?_view=Written+Questions&_pageSize=50", queryString(), sep="")
      results_list <- getJsonData(urlString, results_list, "written")
    }
    if (input$commonsOralQuestionsCheckBox) {
      urlString <- paste("http://lda.data.parliament.uk/commonsoralquestions.json?_view=Commons+Oral+Questions&_pageSize=50&_search=", queryString(), sep="")
      results_list <- getJsonData(urlString, results_list, "oral")
    }
    results_list
  })
  
  # load data for PAC members
  pac_members_data <- reactive({
    results_list <- NULL
    if (input$commonsWrittenQuestionsCheckBox) {
      for (mnisId in mnisIdsPAC) {
        urlString <- paste("http://lda.data.parliament.uk/commonswrittenquestions.json?mnisId=", mnisId, "&_view=Written+Questions&_pageSize=10", sep="")
        results_list <- getJsonData(urlString, results_list, "oral")
      }
    }
    if (input$commonsOralQuestionsCheckBox) {
      for (mnisId in mnisIdsPAC) {
        urlString <- paste("http://lda.data.parliament.uk/commonsoralquestions.json?mnisId=", mnisId, "&_view=Commons+Oral+Questions&_pageSize=10", sep="")
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
  
  output$text_search_table <- DT::renderDataTable(
    text_search_data() %>% 
      select(dateTabled, tablingMemberPrinted, questionText, AnsweringBody, type) %>% 
      datatable(colnames = c('Date' = 'dateTabled', 'Tabling member' = 'tablingMemberPrinted', 'Question text' = 'questionText', 'Answering body' = 'AnsweringBody'), escape=FALSE)
  )
  
  output$text_search_bars <- renderPlot({
    results_list <- text_search_data()
    results_list$fdate <- as.Date(results_list$dateTabled)
    ggplot(results_list, aes(fdate)) + geom_histogram(binwidth=7)
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