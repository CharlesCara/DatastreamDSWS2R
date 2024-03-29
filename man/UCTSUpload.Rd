% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UCTSUpload.R
\name{UCTSUpload}
\alias{UCTSUpload}
\title{Upload a UCTS timeseries into Datastream}
\usage{
UCTSUpload(
  tsData,
  TSCode = "",
  MGMTGroup = "ABC",
  freq = c("D", "W", "M", "Q", "Y"),
  seriesName,
  Units = "",
  Decimals = 2,
  ActPer = c("N", "Y"),
  freqConversion = c("ACT", "SUM", "AVG", "END"),
  Alignment = c("1ST", "MID", "END"),
  Carry = c("YES", "NO", "PAD"),
  PrimeCurr = "",
  strUsername = ifelse(Sys.getenv("DatastreamUsername") != "",
    Sys.getenv("DatastreamUsername"), options()$Datastream.Username),
  strPassword = ifelse(Sys.getenv("DatastreamPassword") != "",
    Sys.getenv("DatastreamPassword"), options()$Datastream.Password),
  strServerName = "https://product.datastream.com",
  strServerPage = "/UCTS/UCTSMaint.asp"
)
}
\arguments{
\item{tsData}{- an xts (or timeseries object that can be converted to
one) to be uploaded.}

\item{TSCode}{The mnemonic of the target UCTS}

\item{MGMTGroup}{Must have managment group.  Only the first
characters will be used.}

\item{freq}{The frequency of the data to be uploaded}

\item{seriesName}{the name of the series}

\item{Units}{Units of the data - can be no more than 12 characters -
excess will be trimmed to that length}

\item{Decimals}{Number of Decimals in the data - a number between 0 and
9 - if outside that range then trimmed}

\item{ActPer}{Whether the values are percentages ("N") or actual
numbers ("Y")}

\item{freqConversion}{How to do any FX conversions}

\item{Alignment}{Alignment of the data within periods}

\item{Carry}{whether to carry data over missing dates}

\item{PrimeCurr}{the currency of the timeseries}

\item{strUsername}{your Datastream username}

\item{strPassword}{your Datastream Password}

\item{strServerName}{URL of the Datastream server}

\item{strServerPage}{page on the datastream server}
}
\value{
TRUE if the upload has been a success, otherwise an error message
}
\description{
Uploads an xts into a UCTS in the Datastream Database
}
\details{
Note this function does not check to see if there is
 a pre-existing timeseries already in Datastream.  It will just overwrite
  any existing UCTS.
}
