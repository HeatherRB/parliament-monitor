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
library(testthat) # R testing package

# Set-updata on MPs and their parties ------------------------------------------------------

# Members of PAC (by ID)
mnisIdsPAC <- c(1524, 1451, 4388, 3971, 4040, 389, 4451, 3929, 4134, 4136, 4249, 4046, 1454, 4444, 4531)
# source: http://www.parliament.uk/business/committees/committees-a-z/commons-select/public-accounts-committee/membership/

# Obtains an xml list of MPs data and returns a clean dataframe
MPs_XML <- getURL("http://data.parliament.uk/membersdataplatform/services/mnis/members/query/House=Commons%7CMembership=all%7Ccommonsmemberbetween=2010-05-06and2018-03-31/", ssl.verifypeer = FALSE)
xmlfile <- xmlTreeParse(MPs_XML)
  
# Sample output
#<Members>
#  <Member Member_Id="172" Dods_Id="25790" Pims_Id="3572">
#  <DisplayAs>Ms Diane Abbott</DisplayAs>
#  <ListAs>Abbott, Ms Diane</ListAs>
#  <FullTitle>Rt Hon Diane Abbott MP</FullTitle>
#  <LayingMinisterName/>
#  <DateOfBirth>1953-09-27T00:00:00</DateOfBirth>
#  <DateOfDeath xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:nil="true"/>
#  <Gender>F</Gender>
#  <Party Id="15">Labour</Party>
#  <House>Commons</House>
#  <MemberFrom>Hackney North and Stoke Newington</MemberFrom>
#  <HouseStartDate>1987-06-11T00:00:00</HouseStartDate>
#  <HouseEndDate xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:nil="true"/>
#  <CurrentStatus Id="0" IsActive="True">
#  <Name>Current Member</Name>
#  <Reason/>
#  <StartDate>2017-06-08T00:00:00</StartDate>
#  </CurrentStatus>
#  </Member>
  
# extract the Member_Id from the member attributes and put into a dataframe, alongside other fields
MPs_data <- xmlSApply(xmlRoot(xmlfile), function(x) c(xmlGetAttr(x, "Member_Id"), xmlSApply(x, xmlValue))) 
MPs_data <- t(MPs_data)
rownames(MPs_data) <- MPs_data[,1]
MPs_df <- as.data.frame(MPs_data)
  
# clean column entries
MPs_df$HouseEndDate <- lapply(MPs_df$HouseEndDate, function(x) if(length(x)==0) {NULL} else {as.Date.character(x)}) # format HouseEndDate as a date, where applicable
MPs_df$Current <- lapply(MPs_df$HouseEndDate, function(x) if(length(x)==0) {TRUE} else {Sys.Date() < as.Date.character(x)}) # add 'current' field = TRUE/FALSE
MPs_df$Party <- as.factor(unlist(MPs_df$Party, use.names=FALSE)) # convert Party to factor
MPs_df$Gender <- as.factor(unlist(MPs_df$Gender, use.names=FALSE))
MPs_df$MemberFrom <- do.call("c", lapply(MPs_df$MemberFrom, "[[", 1))
MPs_df$DisplayAs <- do.call("c", lapply(MPs_df$DisplayAs, "[[", 1))
MPs_df <- mutate(MPs_df, mnisId=as.character(V1))
MPs_df <- select(MPs_df, -V1)

test_that("MPs_df has the expected number of columns", {
  expect_equal(ncol(MPs_df), 15)
})

# Data on party colours of current MPs
current_MPs <- filter(MPs_df, Current == TRUE)
if(nrow(current_MPs)>0) {
    Party <- levels(factor(current_MPs$Party))
    knownParties <- c("Labour", "Labour (Co-op)", "Conservative", 
               "Scottish National Party", "Plaid Cymru", "Green Party",
               "Liberal Democrat", "UK Independence Party",
               "Democratic Unionist Party", "Ulster Unionist Party", "Sinn Fein")
    prescribedColours <- c("red", "red", "royalblue",
                 "yellow", "forestgreen", "green",
                 "orange", "purple",
                 "darkred", "forestgreen", "yellow",
                 "grey")
    # returns the colour of a known political party, or grey otherwise
    colour <- sapply(Party, function(x) prescribedColours[match(x, knownParties, nomatch = 12)])
    partyColours <- data.frame(Party, colour)
    partyColours$Party <- factor(partyColours$Party)
    partyColours$colour <- factor(partyColours$colour)
    
    test_that("partyColours has the expected number of rows and columns", {
      expect_equal(ncol(partyColours), 2)
      expect_equal(nrow(partyColours), length(levels(factor(current_MPs$Party))))
    })
}

# Cleaning and checking functions ------------------------------------------------------------------

elseNA <- function(x){
  # function to check for null values and replace with 'NA'
  if (is.null(x)) {
    return('NA')
  } else {
    return(x)
  }
}

getMnisId <- function(url) {
  # retrieves an MPs ID from their data.parliament URL
  # e.g. url <- "http://data.parliament.uk/members/3973" returns "3973"
  # invalid URL returns 'NA'
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
  # transforms a list of lists into a list containing the first element of each list
  if (!is.list(l)) {
    return('NA')
  } else {
    return(l[[1]])
  }
}

collapseAbout <- function(l) {
  # transforms a list of lists into a list containing the '_about' element of a each list
  if (!is.list(l)) {
    return('NA')
  } else {
    return(l$'_about')
  }
}

getMemberUrl <- function(list) {
  # Deals with columns with different formats, i.e., column format (where list$tablingMember$'_about' exists)
  # and list of list format (where elements of list$tablingMember are lists, some of which are missing an '_about' element)
  if(!is.null(list$tablingMember$'_about')) {
    return(list$tablingMember$'_about')
  } else {
    return(do.call("c", lapply(list$tablingMember, collapseAbout)))
  }
}

cleanQs <- function(json_data, questionType){
  # Clean and format json data for commons written/oral questions
  # accepts json_data (data.frame) and a questionType (string)
  # returns a data frame (or NULL if no results have been found)
  
  list <- json_data$result$items
  
  if (length(list)==0) {
    return(NULL)
  } else {
    list$AnswerDate <- list$AnswerDate$'_value'
    list$dateTabled <- list$dateTabled$'_value'
    list$tablingMemberUrl <- getMemberUrl(list)
    list$tablingMemberPrinted <- do.call("c", lapply(list$tablingMemberPrinted, collapseList))
    list$AnsweringBody <- do.call("c", lapply(list$AnsweringBody, collapseList))
    list$type <- questionType
    list$mnisId <- do.call("c", lapply(list$tablingMemberUrl, getMnisId))
    list <- select(list, About=`_about`, AnswerDate, AnsweringBody, dateTabled, questionText, tablingMemberUrl, tablingMemberPrinted, mnisId, type)
    return(list)
  }
}

# Functions for retrieving data ------------------------------------------------------------------

getJsonData <- function(urlString, results_list, qType){
  # Get json data and append to existing results
  # accepts urlString (string), results_list (data.frame), qType ("oral" or "written")
  # returns an appended data frame (or NULL if no results have been found)
  
  # ignore null URLs
  if(is.null(urlString)) {
    return(results_list)
  }
  
  #get json data
  json_file <- getURL(urlString, ssl.verifypeer = FALSE)
  json_data <- fromJSON(json_file)
  
  # clean json data according to its type
  if (qType == "oral") {
    new_results <- cleanQs(json_data, "Commons Oral Question")
  } else if (qType == "written") {
    new_results <- cleanQs(json_data, "Commons Written Question")
  } else {
    new_results <- NULL
  }
  
  # append to existing data
  # if new_results is null this section has no effect
  if (is.null(results_list)) {
    results_list <- new_results
  } else if (!is.null(new_results)) {
    results_list <- rbind(results_list, new_results)
  }
  
  return(results_list)
}

  
shinyServer(function(input, output, session) {
  
  # Constituency boundaries ------------------------------------------------------------------
  
  boundaries <- reactive({
    # open and transform constituency boundary file
    # merge in data on MPs
    # Q: Are there gaps after merging on name?
    # not actually reactive but shared between outputs and only loaded when needed
    readOGR(dsn="shapefiles", layer="Westminster_Parliamentary_Constituencies_December_2015_Super_Generalised_Clipped_Boundaries_in_Great_Britain") %>%
    spTransform(CRS("+init=epsg:4326")) %>%
    merge(select(current_MPs, mnisId, DisplayAs, MemberFrom, Party), by.x="pcon15nm", by.y="MemberFrom") %>%
    merge(partyColours)
  })
  
  # Text search ------------------------------------------------------------------
  
  queryString <- reactive({
    # search criteria (string)
    string <- sub("e.g., ", "", input$searchInput)
    string <- gsub(" ", "+AND+", string)
    paste("&_search=", string, sep="")
  })
  
  text_search_data <- reactive({
    # loads results for text search
    # returns NULL if no results have been found
    
    results_list <- NULL
    pageSize <- input$text_search_results
    
    if (input$commonsWrittenQuestionsCheckBox) {
      urlString <- paste("http://lda.data.parliament.uk/commonswrittenquestions.json?_view=Written+Questions&_pageSize=", pageSize, "&_search=", queryString(), sep="")
      # e.g., urlString <- "http://lda.data.parliament.uk/commonswrittenquestions.json?_view=Written+Questions&_pageSize=25"
      results_list <- getJsonData(urlString, results_list, "written")
    }
    if (input$commonsOralQuestionsCheckBox) {
      urlString <- paste("http://lda.data.parliament.uk/commonsoralquestions.json?_view=Commons+Oral+Questions&_pageSize=", pageSize, "&_search=", queryString(), sep="")
      results_list <- getJsonData(urlString, results_list, "oral")
    }
    
    if (!is.null(results_list)) {
      # incorporate data on MPs and sort by date
      results_list <- merge(results_list, select(MPs_df, mnisId, MemberFrom, Party))
      results_list$dateTabled <- as.Date(results_list$dateTabled)
      results_list$type <- factor(results_list$type)
      results_list <- results_list[rev(order(results_list$dateTabled)),]
    }
    results_list
  })
  
  output$text_search_table <- DT::renderDataTable(
    # data table output for text search
    # returns NULL if no results have been found
    text_search_data() %>% 
      select(dateTabled, tablingMemberPrinted, MemberFrom, Party, questionText, AnsweringBody, type) %>%
      #select(-mnisId, -About, -tablingMemberUrl, -AnswerDate) %>% 
      datatable(escape=TRUE, filter = 'top', rownames = FALSE, 
                options = list(dom = 'tp'), #show table only, not search box
                caption = htmltools::tags$caption("The table below lists the most recent results for your search term, as listed on ", htmltools::a(href="http://data.parliament.uk/", target="_blank", "data.parliament.uk"), ". Refine your results using the filters in the column headers."),
                colnames = c('Date'='dateTabled', 'Tabling member'='tablingMemberPrinted', 'Constituency'='MemberFrom', 'Party'='Party', 'Question text'='questionText', 'Answering body'='AnsweringBody', 'Question type' = 'type'))
  )
  
  output$text_search_bars <- renderPlot({
    results_list <- text_search_data()
    #results_list$month <- paste(format(as.Date(results_list$dateTabled), format='%Y-%m'), '-01', sep="")
    #levels <- unique(results_list$month[order(results_list$dateTabled)])
    #results_list$month <- factor(results_list$month, levels = levels)
    #select(-mnisId, -About, -tablingMemberUrl, -AnswerDate) %>% 
    date1 <- paste(format(min(results_list$dateTabled), '%Y-%m'), "-01", sep="")
    date2 <- paste(format(Sys.Date(), '%Y-%m'), "-01", sep="")
    seq <- seq(as.Date(date1),to=as.Date(date2), by='month')
    n <- length(seq)
    m <- n %/% 3
    seq2 <- seq[3*(0:m)]
    labels <- format(as.Date(seq2), '%b %y')
    #ggplot(results_list, aes(month)) + geom_bar() + labs(title = "Number of questions over time", x="Month", y="Number of questions") + scale_x_date(breaks = seq2, labels = labels)
    ggplot(data=results_list, aes(dateTabled)) +
      geom_histogram(binwidth=30.5, boundary = as.Date(date1), colour = "gray28", fill = "gray48") +
      labs(title = "Number of questions per month", x="Date", y="Number of questions") +
      scale_x_date(breaks = seq2, labels = labels)
  })
  
  output$party_bars <- renderPlot({
    bars <- summarise(group_by(text_search_data(), Party), n=n())
    bars$Party <- factor(bars$Party, levels=(bars$Party[order(bars$n)]))
    ggplot(bars, aes(x=Party, y=n)) + 
      geom_bar(stat="identity", colour = "gray28", fill = "gray48") + 
      coord_flip() + 
      labs(title = "Number of questions by party", x="Party", y="Number of questions")
  })
  
  # PAC members ------------------------------------------------------------------
  
  pac_members_data <- reactive({
    # loads results for PAC members
    
    results_list <- NULL
    pageSize <- input$pac_member_results
    
    if (input$commonsWrittenQuestionsCheckBox) {
      for (mnisId in mnisIdsPAC) {
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
    
    if (!is.null(results_list)) {
      # incorporate data on MPs and sort by date
      results_list <- merge(results_list, select(MPs_df, mnisId, MemberFrom, Party, HouseEndDate))
      results_list$dateTabled <- as.Date(results_list$dateTabled)
      results_list$type <- factor(results_list$type)
      results_list <- results_list[rev(order(results_list$dateTabled)),]
    }
    results_list
  })
  
  output$pac_members_table <- DT::renderDataTable(
    # data table output for PAC members' questions
    pac_members_data() %>% 
      select(dateTabled, tablingMemberPrinted, MemberFrom, Party, questionText, AnsweringBody, type) %>% 
      datatable(escape=TRUE, filter = 'top', rownames = FALSE, 
                options = list(dom = 'tp'), #show table only, not search box
                caption = htmltools::tags$caption("The table below lists the most recent results for PAC members, as listed on ", htmltools::a(href="http://data.parliament.uk/", target="_blank", "data.parliament.uk"), ". Refine your results using the filters in the column headers."),
                colnames = c('Date' = 'dateTabled', 'Tabling member' = 'tablingMemberPrinted', 'Constituency'='MemberFrom', 'Party'='Party', 'Question text' = 'questionText', 'Answering body' = 'AnsweringBody', 'Question type' = 'type'))
  )
  
  output$pac_members_bars <- renderPlot({
    #ggplot(pac_members_data(), aes(dateTabled)) + geom_histogram() + labs(title = "Number of questions over time", x="Date", y="Number of questions")
    results_list <- pac_members_data()
    #results_list$month <- format(as.Date(results_list$dateTabled), format='%b %y')
    #levels <- unique(results_list$month[order(results_list$dateTabled)])
    #results_list$month <- factor(results_list$month, levels = levels)
    #select(-mnisId, -About, -tablingMemberUrl, -AnswerDate) %>% 
    #ggplot(results_list, aes(month)) + geom_bar() + labs(title = "Number of questions over time", x="Month", y="Number of questions")
    date1 <- paste(format(min(results_list$dateTabled), '%Y-%m'), "-01", sep="")
    date2 <- paste(format(Sys.Date(), '%Y-%m'), "-01", sep="")
    seq <- seq(as.Date(date1),to=as.Date(date2), by='month')
    n <- length(seq)
    m <- n %/% 3
    seq2 <- seq[3*(0:m)]
    labels <- format(as.Date(seq2), '%b %y')

    ggplot(data=results_list, aes(dateTabled)) +
      geom_histogram(binwidth=30.5, boundary = as.Date(date1), colour = "gray28", fill = "gray48") +
      labs(title = "Number of questions per month", x="Date", y="Number of questions") +
      scale_x_date(breaks = seq2, labels = labels)
  })
  
  
  output$PAC_map <- renderLeaflet({
    
    # Filter to show only PAC members
    PAC <- subset(MPs_df, MPs_df$mnisId %in% as.character(mnisIdsPAC)) %>%
      select(mnisId, DisplayAs, MemberFrom, Party)
    
    #merge PAC members' details into boundary data
    boundaries2 <- merge(boundaries(), PAC, by.x="pcon15nm", by.y="MemberFrom") %>%
      merge(partyColours)
    
    # Q: How to set deafult zoom?
    # labels script https://rpubs.com/bhaskarvk/leaflet-labels
    leaflet() %>%
      addPolygons(data=boundaries(), stroke=TRUE, color = "#333333", weight=0.5, opacity = 1, fillOpacity = 0.7,
                  fillColor = "#888888") %>%
      addPolygons(data=boundaries2, stroke=TRUE, color = "#333333", weight=0.5, opacity = 1, fillOpacity = 0.7,
                  label=mapply(function(x, y, z) {
                    htmltools::HTML(sprintf("%s <br>Member: %s, %s", htmlEscape(x), htmlEscape(y), htmlEscape(z)))}, 
                    boundaries2$pcon15nm, boundaries2$DisplayAs, boundaries2$Party, SIMPLIFY = F),
                  fillColor = ~colour) %>%
      addLegend(colors = partyColours$colour, labels = partyColours$Party, opacity = 0.7)
  })
  
  output$PAC_table <- DT::renderDataTable(
    # data table output for MP data
    #MPs_data()$linked_Name = paste("<a href=", constituencyURL, ">", name, "</a>")) %>%
    # Filter to show only PAC members
    subset(MPs_df, MPs_df$mnisId %in% as.character(mnisIdsPAC)) %>%
      select(DisplayAs, Gender, Party, MemberFrom, HouseEndDate) %>%
      datatable(escape=TRUE, rownames = FALSE, 
                options = list(dom = 't', pageLength = 50), #show table only, not search box
                colnames = c('Name' = 'DisplayAs', 'Constituency' = 'MemberFrom', 'End date' = 'HouseEndDate'),
                caption = htmltools::tags$caption("The table below lists PAC members as at April 2017, using data from ", htmltools::a(href="http://data.parliament.uk/MembersDataPlatform/memberquery.aspx", target="_blank", "UK Parliament's Members' Names Data Platform"), "."))
  )
  
  # Commons members ------------------------------------------------------------------

  output$MPs <- DT::renderDataTable(
    # data table output for MP data
    #MPs_data()$linked_Name = paste("<a href=", constituencyURL, ">", name, "</a>")) %>%
    select(MPs_df, DisplayAs, Gender, Party, MemberFrom, HouseEndDate) %>%
    datatable(escape=TRUE, filter = 'top', rownames = FALSE, 
              options = list(dom = 'tp', pageLength = 50), #show table only, not search box
              colnames = c('Name' = 'DisplayAs', 'Constituency' = 'MemberFrom', 'End date' = 'HouseEndDate'),
              caption = htmltools::tags$caption("The table below lists all House of Commons MPs since May 2010, as listed on ", htmltools::a(href="http://data.parliament.uk/MembersDataPlatform/memberquery.aspx", target="_blank", "UK Parliament's Members' Names Data Platform"), ". Search the list using the filters in the column headers."))
  )
  
  output$MPs_map <- renderLeaflet({
    # Q: How to set deafult zoom?
    # labels script https://rpubs.com/bhaskarvk/leaflet-labels
    
    # set-up leaflet
    if (nrow(current_MPs) == 0) { 
    # no current MPs, so no MP labels
      leaflet(boundaries()) %>%
        addPolygons(stroke=TRUE, color = "#333333", weight=0.5, opacity = 1, fillOpacity = 0.7,
                    label=mapply(function(x) {
                      htmltools::HTML(sprintf("%s", htmlEscape(x)))}, 
                      boundaries()$pcon15nm, SIMPLIFY = F))
    } else { 
    # add MP labels
      leaflet(boundaries()) %>%
      addPolygons(stroke=TRUE, color = "#333333", weight=0.5, opacity = 1, fillOpacity = 0.7,
                  label=mapply(function(x, y, z) {
                    htmltools::HTML(sprintf("%s <br>Member: %s, %s", htmlEscape(x), htmlEscape(y), htmlEscape(z)))}, 
                    boundaries()$pcon15nm, boundaries()$DisplayAs, boundaries()$Party, SIMPLIFY = F),
                  fillColor = ~colour) %>%
      addLegend(colors = partyColours$colour, labels = partyColours$Party, opacity = 0.7)
    }
  })
  
})

