#' @name cbindRobust
#' @title Function to combine time series that fixes the NA problem
#' @description When combining two xts time series in which one series is an empty NA series and the other is a character series, then
#'  the normal cbind function will return a time series with the correct number of rows and columns but with every cell occupied with NA.
#'  This function overcomes this problem by allowing us to combine an empty series and a character series.
#'
#' @param xts1 First time series to combine
#' @param xts2 Second time series to combine
#'
#' @importFrom xts xts
#' @importFrom zoo cbind.zoo index
#' @export
#'
#'
#'
cbindRobust <- function(xts1, xts2) {

  # If neither of the two series are empty NA series, then we combine them as normal in order to save time.
  if(!is.na(unique(xts1)) && !is.na(unique(xts2))){

    xts3 <- cbind(xts1, xts2)
    return(xts3)

  }

  # If the non-empty time series is not class character, then we combine as normal in order to save time
  if(unique(apply(xts1, MARGIN = 1, FUN = class)) != "character" && unique(apply(xts2, MARGIN = 1, FUN = class)) != "character") {

    xts3 <- cbind(xts1, xts2)
    return(xts3)

  }

  # We now combine the two series using zoo, before converting it into an xts.
  xts3 <- zoo::cbind.zoo(xts1, xts2)
  xts3 <- xts::xts(xts3, order.by = zoo::index(xts3))
  return(xts3)

}

