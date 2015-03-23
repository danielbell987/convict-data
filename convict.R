## Download convict's last statements

## Required librarys
library(XML)
library(RCurl)

## Get a table containing metadata regarding each last statement
url <- "http://www.tdcj.state.tx.us/death_row/dr_executed_offenders.html"
convictData <- readHTMLTable(url, header = T)
convictData <- convictData[[1]]
convictData <- data.frame(convictData, stringsAsFactors = FALSE)
convictData[,2] <- NULL
names(convictData)[2] <- "LastStatement"


## Set the correct data type for each variable
## (Cannot set the last statement type until data has been added)
convictData[,1] <- as.numeric(as.character(convictData[,1]))
convictData[,3:5] <- lapply(convictData[,3:5], as.character)
convictData[,6] <- as.numeric(as.character(convictData[,6]))
convictData[,7] <- as.Date(convictData[,7], format = "%m/%d/%Y")


## Use the first and last names to open the link to each statment and enter in the table

## These rows contained names that did not match the required ones in the html file
convictData[convictData$Execution==491, 4] <- "RamonTorres"            
convictData[convictData$Execution==476, 4] <- "FrankM"                 
convictData[convictData$Execution==421, 3] <- "Whitaker"
convictData[convictData$Execution==218, 4] <- "RobertEarl"
convictData[convictData$Execution==151, 4] <- "RobertAnthony"
convictData[convictData$Execution==143, 4] <- "Charles"


## These functions remove elements of the names so the correct html page can be opened
convictData$Last.Name <- gsub(", Jr.", "", convictData$Last.Name)                   ## Handles rows containing juniors
convictData$Last.Name <- gsub(" Jr.", "", convictData$Last.Name)                   ## Handles rows containing juniors
convictData$Last.Name <- gsub(", Sr.", "", convictData$Last.Name)                   ## Handles rows containing juniors
convictData$Last.Name <- gsub("'", "", convictData$Last.Name)                  ## Handles rows containing juniors
convictData$Last.Name <- gsub("-", "", convictData$Last.Name)                  ## Handles rows containing juniors
convictData$Last.Name <- gsub(" ", "", convictData$Last.Name)                  ## Handles rows containing juniors


getStatement <- function(x, last = "", first = "", end = "last.html"){
  if(last=="" & first==""){
    last <- tolower(x[3])
    first <- tolower(x[4])
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
    x[2] <- doc.text
  }
}

convictData$LastStatement <- apply(convictData, 1, function(x) getStatement(x))

## This row had the TCDJ number in the html
convictData[convictData$Execution==429, 2] <- getStatement(convictData[convictData$TDCJ.Number==999173,], end = "last999173.html")

## This rows html name was spelt wrong
convictData[convictData$Execution==401, 2] <- getStatement(convictData[convictData$TDCJ.Number==999171,], last = "mosely", first = "daroyce")

## The html file containing these rows data was spelt wrong so must be adjusted in the function
convictData[convictData$Execution==243, 2] <- getStatement(convictData[convictData$Execution==243,], last = "hernandez", first = "adoph")
convictData[convictData$Execution==58, 2] <- getStatement(convictData[convictData$Execution==58,], last = "hererra", first = "leonel")


## The LastStatement variable can now be set to type character
convictData[,2] <- as.character(convictData[,2])


## Save the new tidy dataset to the current working directory
write.csv(convictData, file = "ConvictLastStmtData.csv", row.names=FALSE)
