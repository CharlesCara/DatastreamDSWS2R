#' @name dotconvert_JSON_Bool
#' @title .convert_JSON_Date
#' @details converts a JSON Boolean string to an R
#'  logical object
#'
#' @description this is a modification of the function provided by
#'  'phiver' on
#' http://stackoverflow.com/questions/32076957/nas-introduced-when-
#' transforming-json-date
#'
#' @param Input_Strings a JSON string (or an array)
#' @return an array of Dates
#'
#' @keywords internal
#'
#'
.convert_JSON_Bool <- function(Input_Strings){
  suppressWarnings({
    ret <- sapply(X = Input_Strings, FUN = as.logical)
  })
  return(ret)
}


#' @name dotconvert_JSON_Integer
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
#'
#' @keywords internal
#'
.convert_JSON_Integer <- function(Input_Strings){
  suppressWarnings({
    ret <- sapply(X = Input_Strings, FUN = as.integer)
  })

  return(ret)
}


#' @name dotconvert_JSON_String
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
#' @return an array of String
#'
#' @keywords internal
#'
.convert_JSON_String <- function(Input_Strings){
  suppressWarnings({
    ret <- sapply(X = Input_Strings, FUN = as.character)
  })

  return(ret)
}

#' @name dotconvert_JSON_Double
#' @title .convert_JSON_Double
#' @details converts a JSON Double object an R
#'  Double object
#'
#' @description this is a modification of the function provided by
#'  'phiver' on
#' http://stackoverflow.com/questions/32076957/nas-introduced-when-
#' transforming-json-date
#'
#' @param Input_Strings a JSON Date string (or an array)
#' @return an array of Dates
#'
#' @keywords internal
#'
.convert_JSON_Double <- function(Input_Strings){
  suppressWarnings({
    ret <- sapply(X = Input_Strings, FUN = as.numeric)
  })
  return(ret)
}




#' @name dotconvert_JSON_Date
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
#' @keywords internal
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
#' @name dotconvert_JSON_Datetime
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
#' @keywords internal
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


#' @name dotconvertJSONString
#' @title .convertJSONString
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
#' @keywords internal
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
#' @name dotgetValueTyped
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
#' @keywords internal
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
#' @name dotgetValue
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
#'
#' @return the parsed result: either Date, String or numeric
#'
#' @importFrom stringr fixed str_detect
#' @keywords internal
#'
.getValue <- function(x){
  if(!("Value" %in% names(x)) | !("Type" %in% names(x))) {
    return(NA)
  } else {
    if(TRUE %in% str_detect(string = x$Value, pattern = fixed("$$\"ER"))){
      #TODO: write the response in the errorList object
      return(x$Value)
    } else {
      return(.getJSONValue(value = x$Value, type = .getType(x)))
    }



  }
}



#-----------------------------------------------------------------------------
#
#' @name dotgetSymbol
#' @title .getSymbol
#' @details extracts and converts a JSON string (including with Timezone)
#'  from the item 'Symbol'
#' in the list x.  the item is parsed from JSON into either a numeric,
#'  string, or a R Date object
#'
#' @description
#'
#' @param x a list that is expected to have an item 'Symbol' and 'Type'
#' @return the parsed result: either Date, String or numeric
#' @keywords internal
#'
.getSymbol <- function(x){

  if(!("Symbol" %in% names(x) )) {
    return("")
  } else {
    thisValue <- x$Symbol
    if(TRUE %in% grepl("\\$\\$ER:", thisValue)){
      #TODO: write the response in the errorList object
      return("")
    } else {
      return(thisValue)
    }
  }
}
#-----------------------------------------------------------------------------
#
#' @name dotgetJSONValue
#' @title .getSymbol
#' @details extracts and converts a JSON string (including with Timezone)
#'  from the item 'Symbol'
#' in the list x.  the item is parsed from JSON into either a numeric,
#'  string, or a R Date object
#'
#' @description
#'
#' @param x a list that is expected to have an item 'Symbol' and 'Type'
#' @return the parsed result: either Date, String or numeric
#' @keywords internal
#'
.getJSONValue <- function(value, type){

  if(length(type) == 0) {
    # Type was missing
    return(NA)
  }

  if(TRUE %in% grepl("\\$\\$ER:", value)){
    return(NA)
  }

  if(type == 0){
    # Value is error
    return(NA)
  }
  if(type == 1){
    # Value is empty
    return(NA)
  }
  if(type == 2){
    # Value is Bool
    return(.convert_JSON_Bool(value))
  }
  if(type == 3){
    # Value is Integer
    return(.convert_JSON_Integer(value))
  }
  if(type == 4){
    # Value is DateTime
    return(.convert_JSON_Date(value))
  }
  if(type == 5){
    # Value is Double
    return(.convert_JSON_Double(value))
  }
  if(type == 6){
    # Value is String
    return(.convert_JSON_String(value))
  }
  if(type == 7){
    # Value is BoolArray
    # TODO: Not handled
    return(NA)
  }
  if(type == 8){
    # Value is IntegerArray
    # TODO: Not handled
    return(NA)
  }
  if(type == 9){
    # Value is DateTimeArray
    # TODO: Not handled
    return(NA)
  }
  if(type == 10){
    # Value is DoubleArray
    # TODO: Not handled
    return(NA)
  }
  if(type == 11){
    # Value is String Array
    # TODO: Not handled
    return(NA)
  }
  if(type == 12){
    # Value is Object Array
    # TODO: Not handled
    return(NA)
  }


}


#-----------------------------------------------------------------------------
#
#' @name dotgetType
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
#' @keywords internal
#'
.getType <- function(x){
  thisValue <- .convertJSONString(x$Type)
  if(TRUE %in% grepl("\\$\\$ER:", thisValue)){
    #TODO: write the response in the errorList object
    return(NaN)
  } else {
    return(suppressWarnings(as.integer(thisValue)))
  }
}
