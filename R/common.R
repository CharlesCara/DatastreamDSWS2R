#' @title .convert_JSON_Date
#' @details converts a JSON Date string (including with Timezone) to a R
#'  Date object
#'
#' @description this is a modification of the function provided by
#'  'phiver' on
#' http://stackoverflow.com/questions/32076957/nas-introduced-when-
#' transforming-json-date
#'
#' @param Input_Strings a JSON Date string (or an array)
#' @return an array of Dates
#' @importFrom stringi stri_locate stri_sub
#'
#'
.convert_JSON_Date <- function(Input_Strings){
  start <- stringi::stri_locate(Input_Strings, regex = "\\(")[,1]
  endPlus <- stringi::stri_locate_first(Input_Strings, regex = "\\+")[,1]
  endBracket <- stringi::stri_locate_first(Input_Strings, regex = "\\)")[,1]
  end <- endPlus
  overwrites <- (endBracket < endPlus) | is.na(endPlus)
  end[which(overwrites)] <- endBracket[which(overwrites)]

  # shift 1 position from the start and end to get the string between the parentheses
  JSON_Date <- stringi::stri_sub(Input_Strings, start+1, end-1)

  # Not interested in time element - return just the date
  JSON_Date <- as.Date(structure(as.numeric(JSON_Date)/1000, class = c("POSIXct", "POSIXt")))

  return(JSON_Date)
}


#-----------------------------------------------------------------------------
#
#' @title .convert_JSON_Datetime
#' @details converts a JSON Date string (including with Timezone) to a
#'  POSIXct/POSIXt object
#'
#' @description this is a modification of the function provided by
#' 'phiver' on
#' http://stackoverflow.com/questions/32076957/nas-introduced-when
#' -transforming-json-date
#'
#' @param Input_Strings a JSON Date string (or an array)
#' @return an array POSIXct/POSIXt object
#' @importFrom stringi stri_locate stri_sub
#'
#'
.convert_JSON_Datetime <- function(Input_Strings){
  start <- stringi::stri_locate(Input_Strings, regex = "\\(")[,1]
  endPlus <- stringi::stri_locate_first(Input_Strings, regex = "\\+")[,1]
  endBracket <- stringi::stri_locate_first(Input_Strings, regex = "\\)")[,1]
  end <- endPlus
  overwrites <- (endBracket < endPlus) | is.na(endPlus)
  end[which(overwrites)] <- endBracket[which(overwrites)]

  # shift 1 position from the start and end to get the string between the parentheses
  JSON_Date <- stringi::stri_sub(Input_Strings, start+1, end-1)

  return(structure(as.numeric(JSON_Date)/1000, class = c("POSIXct", "POSIXt")))
}


#' @title convertJSONString
#' @details converts a JSON string (including with Timezone) into either a
#' numeric, string, or a R Date object
#'
#' @description this is a modification of the function provided by
#' 'phiver' on
#' http://stackoverflow.com/questions/32076957/nas-introduced-when
#' -transforming-json-date
#'
#' @param x the JSON string
#' @return the parsed result: either Date, String or numeric
#' @importFrom stringi stri_locate stri_sub
#'
#'
.convertJSONString <-
  function(x)
  {
    if(is.null(x)) return(NA)
    if(grepl("/?(new )?Date\\(", x)) {
      # date
      start <- stringi::stri_locate(x, fixed = "/Date(")[,1] + 6
      endPlus <- stringi::stri_locate_first(x, fixed = "+")[,1] - 1
      endBracket <- stringi::stri_locate_first(x, fixed = ")")[,1] - 1

      if((endBracket < endPlus) | is.na(endPlus)) {
        end <- endBracket
      } else {
        end <- endPlus
      }
      JSON_Date <- stringi::stri_sub(x, start, end)
      # Not interested in time element - return just the date
      JSON_Date <- as.Date(structure(as.numeric(JSON_Date)/1000, class = c("POSIXct", "POSIXt")))

      return(JSON_Date)
    } else {
      if(!is.na(suppressWarnings(as.numeric(x)))){
        return(as.numeric(x))
      } else{
        return(x)
      }
    }

  }



#-----------------------------------------------------------------------------
#
#' @title .getValueTyped
#' @details extracts and converts a JSON string (including with Timezone)
#'  from the item 'Value'
#' in the list x.  the item is parsed from JSON into either a numeric,
#' string, or a R Date object
#' according to the rules in item 'Type'
#'
#' @description this is a modification of the function provided by
#' 'phiver' on
#' http://stackoverflow.com/questions/32076957/nas-introduced-when
#' -transforming-json-date
#'
#' @param x a list that is expected to have an item 'Value' and item 'Type'
#' @return the parsed result: either Date, String or numeric
#'
.getValueTyped <- function(x, myType){
  thisValue <- .convertJSONString(x$Value)

  if(TRUE %in% grepl("\\$\\$ER:", thisValue)){
    #TODO: write the response in the errorList object
    return(NA)
  } else {
    if(myType == 4){
      if(class(thisValue) != "Date" && thisValue == "NA"){
        return(as.Date(NA))
      } else {
        return(as.Date(thisValue))
      }

    }
    #TODO: do we need to hard type the rest
    return(thisValue)
  }
}




#-----------------------------------------------------------------------------
#
#' @title .getValue
#' @details extracts and converts a JSON string (including with Timezone)
#'  from the item 'Value'
#' in the list x.  the item is parsed from JSON into either a numeric,
#' string, or a R Date object
#'
#' @description this is a modification of the function provided
#'  by 'phiver' on
#' http://stackoverflow.com/questions/32076957/nas-introduced-when
#' -transforming-json-date
#'
#' @param x a list that is expected to have an item 'Value'
#' @return the parsed result: either Date, String or numeric
#'
.getValue <- function(x){
  thisValue <- .convertJSONString(x$Value)
  if(TRUE %in% grepl("\\$\\$ER:", thisValue)){
    #TODO: write the response in the errorList object
    return(NA)
  } else {
    return(thisValue)
  }
}


#-----------------------------------------------------------------------------
#
#' @title .getSymbol
#' @details extracts and converts a JSON string (including with Timezone)
#'  from the item 'Symbol'
#' in the list x.  the item is parsed from JSON into either a numeric,
#'  string, or a R Date object
#'
#' @description this is a modification of the function provided
#' by 'phiver' on
#' http://stackoverflow.com/questions/32076957/nas-introduced-when
#' -transforming-json-date
#'
#' @param x a list that is expected to have an item 'Value'
#' @return the parsed result: either Date, String or numeric
#'
.getSymbol <- function(x){
  thisValue <- .convertJSONString(x$Symbol)
  if(TRUE %in% grepl("\\$\\$ER:", thisValue)){
    #TODO: write the response in the errorList object
    return(NA)
  } else {
    return(thisValue)
  }
}


#-----------------------------------------------------------------------------
#
#' @title .getType
#' @details extracts and converts a JSON string (including with Timezone)
#'  from the item 'Type'
#' in the list x.  the item is parsed from JSON into either a numeric,
#'  string, or a R Date object
#'
#' @description this is a modification of the function provided
#'  by 'phiver' on
#' http://stackoverflow.com/questions/32076957/nas-introduced-when
#' -transforming-json-date
#'
#' @param x a list that is expected to have an item 'Type'
#' @return the parsed result: either Date, String or numeric
#'
.getType <- function(x){
  thisValue <- .convertJSONString(x$Type)
  if(TRUE %in% grepl("\\$\\$ER:", thisValue)){
    #TODO: write the response in the errorList object
    return(NaN)
  } else {
    return(as.integer(thisValue))
  }
}
