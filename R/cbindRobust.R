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
  # If either are zero row then we need to be careful
  if (nrow(xts1) == 0 | nrow(xts2) == 0 ) {
    # We need to set the
    if (nrow(xts1) == 0 & nrow(xts2) == 0 ) {
      # We need to return a two combine to give a zero row xts
      xts3 <- xts::xts(matrix(NA, nrow = 1, ncol = ncol(xts1) + ncol(xts2) ), order.by = as.Date("2017-01-01"))["20180101"]
      colnames(xts3) <- c(colnames(xts1), colnames(xts2))

    } else if (nrow(xts1) == 0) {
      # else use zoo to combine
      xts3 <- zoo::cbind.zoo(xts::xts(matrix(NA, nrow = length(index(xts2)), ncol = ncol(xts1)), order.by = index(xts2)), xts2)
      colnames(xts3) <- c(colnames(xts1), colnames(xts2))
      xts3 <- xts::xts(xts3, order.by = zoo::index(xts3))

    } else {
      xts3 <- zoo::cbind.zoo(xts1, xts::xts(matrix(NA, nrow = length(index(xts1)), ncol = ncol(xts2)), order.by = index(xts1)))
      colnames(xts3) <- c(colnames(xts1), colnames(xts2))
      xts3 <- xts::xts(xts3, order.by = zoo::index(xts3))

    }
    return(xts3)
  }


  # If neither of the two series are empty NA series, then we combine them as normal in order to save time.
  if (!is.na(unique(xts1)[1]) && !is.na(unique(xts2)[1])) {

    xts3 <- cbind(xts1, xts2)
    return(xts3)

  }

  # If the non-empty time series is not class character, then we combine as normal in order to save time
  if (unique(apply(xts1, MARGIN = 1, FUN = class))[1] != "character" &&
     unique(apply(xts2, MARGIN = 1, FUN = class))[1] != "character") {

    xts3 <- cbind(xts1, xts2)
    return(xts3)

  }

  # We now combine the two series using zoo, before converting it into an xts.
  xts3 <- zoo::cbind.zoo(xts1, xts2)
  xts3 <- xts::xts(xts3, order.by = zoo::index(xts3))
  return(xts3)

}

