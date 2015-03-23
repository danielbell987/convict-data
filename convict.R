## Download last statements

## Get a table containing metadata regarding each last statement
library(XML)
library(RCurl)
url <- "http://www.tdcj.state.tx.us/death_row/dr_executed_offenders.html"
convictData <- readHTMLTable(url, header = T)
convictData <- convictData[[1]]
convictData <- data.frame(convictData, stringsAsFactors = FALSE)
names(convictData)[2:3] <- c("OffenderInformation", "LastStatement")
convictData[, 1:8] <- lapply(convictData[, 1:8], as.character)


## Use the first and last names to open the link to each statment and enter in the table

## These rows contained names that did not match the required ones in the html file
convictData[convictData$Execution==491, 5] <- "RamonTorres"            
convictData[convictData$Execution==476, 5] <- "FrankM"                 
convictData[convictData$Execution==421, 4] <- "Whitaker"
convictData[convictData$Execution==218, 5] <- "RobertEarl"
convictData[convictData$Execution==151, 5] <- "RobertAnthony"
convictData[convictData$Execution==143, 5] <- "Charles"


## These functions remove elements of the names so the correct html page can be opened
convictData$Last.Name <- gsub(", Jr.", "", convictData$Last.Name)                   ## Handles rows containing juniors
convictData$Last.Name <- gsub(" Jr.", "", convictData$Last.Name)                   ## Handles rows containing juniors
convictData$Last.Name <- gsub(", Sr.", "", convictData$Last.Name)                   ## Handles rows containing juniors
convictData$Last.Name <- gsub("'", "", convictData$Last.Name)                  ## Handles rows containing juniors
convictData$Last.Name <- gsub("-", "", convictData$Last.Name)                  ## Handles rows containing juniors
convictData$Last.Name <- gsub(" ", "", convictData$Last.Name)                  ## Handles rows containing juniors


getStatement <- function(x, last = "", first = "", end = "last.html"){
  if(last=="" & first==""){
    last <- tolower(x[4])
    first <- tolower(x[5])
  }
  print(x[1])
  url <- paste("http://www.tdcj.state.tx.us/death_row/dr_info/", last, first, end, sep = "")
  if(url.exists(url)){
    doc.html = htmlTreeParse(url,useInternal = TRUE)
    doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
    lastSt <- grep("Last Statement:", doc.text)
    doc.text <- doc.text[-(1:lastSt)]
    doc.text = gsub('\\n', ' ', doc.text)
    doc.text = paste(doc.text, collapse = ' ')
    x[3] <- doc.text
  }
}

convictData$LastStatement <- apply(convictData, 1, function(x) getStatement(x))

## This row had the TCDJ number in the html
convictData[convictData$Execution==429, 3] <- getStatement(convictData[convictData$TDCJ.Number==999173,], end = "last999173.html")

## This rows html name was spelt wrong
convictData[convictData$Execution==401, 3] <- getStatement(convictData[convictData$TDCJ.Number==999171,], last = "mosely", first = "daroyce")

## The html file containing these rows data was spelt wrong so must be adjusted in the function
convictData[convictData$Execution==243, 3] <- getStatement(convictData[convictData$Execution==243, ], last = "hernandez", first = "adoph")
convictData[convictData$Execution==58, 3] <- getStatement(convictData[convictData$Execution==58, ], last = "hererra", first = "leonel")


## Set the correct data type for each variable
convictData[,1] <- as.numeric(as.character(convictData[,1]))
convictData[,7] <- as.numeric(as.character(convictData[,7]))
convictData[,3] <- as.character(convictData[,3])
convictData[,8] <- as.Date(convictData[,8], format = "%m/%d/%Y")


## Save the new tidy dataset to the current working directory
write.csv(convictData, file = "ConvictLastStmtData.csv", row.names=FALSE)
