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

  
shinyServer(function(input, output) {
  
  # search criteria
  queryString <- reactive({
    string <- sub("e.g., ", "", input$searchInput)
    string <- gsub(" ", "+AND+", string)
    paste("&_search=", string, sep="")
  })
  
  # HoC Oral Questions
  writtenQuestions <- reactive({
    if (input$PAC) {
      urlString <- "http://lda.data.parliament.uk/commonswrittenquestions.json?mnisId=1451&_view=Written+Questions&_pageSize=50"
    } else if (input$commonsWrittenQuestionsCheckBox) {
      urlString <- paste("http://lda.data.parliament.uk/commonswrittenquestions.json?_view=Written+Questions&_pageSize=50", queryString(), sep="")
    } else {
      NULL
    }
    if (is.null(urlString)) {
      NULL
    } else {
      json_file <- getURL(urlString, ssl.verifypeer = FALSE)
      json_data <- fromJSON(json_file)
      results_list <- cleanWrittenQs(json_data)
      #print(colnames(results_list))
      results_list
    }
  })
  
  # HoC Written Questions
  oralQuestions <- reactive({
    if (input$PAC) {
      results_list <- NULL
      for (mnisId in mnisIdsPAC) {
        urlString <- paste("http://lda.data.parliament.uk/commonsoralquestions.json?mnisId=", mnisId, "&_view=Commons+Oral+Questions&_pageSize=50", sep="")
        json_file <- getURL(urlString, ssl.verifypeer = FALSE)
        json_data <- fromJSON(json_file)
        if (is.null(results_list)) {
          results_list <- cleanOralQs(json_data)
        } else {
          results_list <- rbind(results_list, cleanOralQs(json_data))
        }
      }
      #print(colnames(results_list))
      results_list <- results_list[rev(order(as.Date(results_list$dateTabled))),]
    } else if (input$commonsOralQuestionsCheckBox) {
      urlString <- paste("http://lda.data.parliament.uk/commonsoralquestions.json?_view=Commons+Oral+Questions&_pageSize=50&_search=", queryString(), sep="")
      json_file <- getURL(urlString, ssl.verifypeer = FALSE)
      json_data <- fromJSON(json_file)
      results_list <- cleanOralQs(json_data)
    } else {
      NULL
    }
    if (is.null(urlString)) {
      NULL
    } else {

      #print(colnames(results_list))
      results_list
    }
  })
  
  # load data
  data <- reactive({
    #print(colnames(oralQuestions()))
    #print(colnames(writtenQuestions()))
    rbind(oralQuestions(), writtenQuestions())
  })
  
  # bars
  bars <- reactive({
    results_list <- data()
    results_list$fdate <- factor(format(as.Date(results_list$dateTabled),'%Y-%m'))
    results_list$month <- factor(format(as.Date(results_list$dateTabled),'%b %y'))
    bars_data <- summarise(group_by(results_list, fdate, month), n=n())
    date1 <- paste(bars_data$fdate[1],"01", sep="-")
    date2 <- paste(bars_data$fdate[nrow(bars_data)],"01", sep="-")
    month_list <- data.frame(date=c(seq(as.Date(date1),to=as.Date(date2),by='month')))
    month_list$fdate <- factor(format(as.Date(month_list$date),'%Y-%m'))
    merge(month_list, bars_data, all.x=TRUE)
  })
  
  output$bar_chart <- renderPlot({
    #barplot(bars()$n, names.arg = bars()$month)
    #ggplot(bars()$n, aes=(bars()$month))
    results_list <- data()
    results_list$fdate <- as.Date(results_list$dateTabled)
    #summarise(group_by(results_list, fdate), n=n()) %>%
    ggplot(results_list, aes(fdate)) + geom_histogram(binwidth=7) #+ scale_x_date(date_labels = "%b %Y", limits = c(as.Date("2016-01-01"), Sys.Date()))
  })
  
  output$table_data <- DT::renderDataTable(
    data() %>% 
      select(dateTabled, tablingMemberPrinted, questionText, AnsweringBody, type) %>% 
      datatable(colnames = c('Date' = 'dateTabled', 'Tabling member' = 'tablingMemberPrinted', 'Question text' = 'questionText', 'Answering body' = 'AnsweringBody'), escape=FALSE)
  )
  
  output$page_head <- renderUI({
    HTML(paste("<b>Dimensions:</b><br/><ul><li>"))
  })
  
  output$queryText <- renderText({ 
    queryString()
  })
  
  
})