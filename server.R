library(shiny)
library(jsonlite)
library(XML)
library(RCurl)
library(DT)
library(plyr)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  
  # search criteria
  queryString <- reactive({
    string <- sub("e.g., ", "", input$searchInput)
    string <- gsub(" ", "+AND+", string)
    paste("&_search=", string, sep="")
  })
  
  # HoC Oral Questions
  writtenQuestions <- reactive({
    if (input$commonsWrittenQuestionsCheckBox) {
      urlString <- paste("http://lda.data.parliament.uk/commonswrittenquestions.json?_view=Written+Questions&_pageSize=50", queryString(), sep="")
      json_file <- getURL(urlString, ssl.verifypeer = FALSE)
      json_data <- fromJSON(json_file)
      results_list <- json_data$result$items
      results_list$AnswerDate <- results_list$AnswerDate$'_value'
      results_list$dateTabled <- results_list$dateTabled$'_value'
      results_list$tablingMemberUrl <- results_list$tablingMember$'_about'
      results_list$tablingMemberPrinted <- do.call("c", lapply(results_list$tablingMemberPrinted, "[[", 1))
      results_list$AnsweringBody <- do.call("c", lapply(results_list$AnsweringBody, "[[", 1))
      results_list$type <- "Commons Written Question"
      results_list <- select(results_list, About=`_about`, AnswerDate, AnsweringBody, dateTabled, questionText, tablingMemberUrl, tablingMemberPrinted, type)
      #print(colnames(results_list))
      results_list
    } else {
      NULL
    }
  })
  
  # HoC Written Questions
  oralQuestions <- reactive({
    if (input$commonsOralQuestionsCheckBox) {
      urlString <- paste("http://lda.data.parliament.uk/commonsoralquestions.json?_view=Commons+Oral+Questions&_pageSize=50&_search=", queryString(), sep="")
      json_file <- getURL(urlString, ssl.verifypeer = FALSE)
      json_data <- fromJSON(json_file)
      results_list <- json_data$result$items
      results_list$AnswerDate <- results_list$AnswerDate$'_value'
      results_list$dateTabled <- results_list$dateTabled$'_value'
      results_list$tablingMemberUrl <- results_list$tablingMember$'_about'
      results_list$tablingMemberPrinted <- do.call("c", lapply(results_list$tablingMemberPrinted, "[[", 1))
      results_list$AnsweringBody <- do.call("c", lapply(results_list$AnsweringBody, "[[", 1))
      results_list$title <- results_list$Location$prefLabel$'_value'
      results_list$type <- "Commons Oral Question"
      results_list <- select(results_list, About=`_about`, AnswerDate, AnsweringBody, dateTabled, questionText, tablingMemberUrl, tablingMemberPrinted, type)
      #print(colnames(results_list))
      results_list
    } else {
      NULL
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