# DatastreamDSWS2R
Functions and a R5 class that allows data to be downloaded and uploaded to the Thomson Reuters Datastream database via the DSWS server.  Thomson Reuters Datastream is a rich database of financial information.  You need to have a Datastream subscription and a username/password to use this package.

The package can be installed using:

    require(devtools)
    install_github(“CharlesCara/DatastreamDSWS2R”)

Your Thomson Reuters Datastream username and password are best stored in environment variables by adding to your .Renviron file the following
lines.  This approach makes DatastreamDSWS2R able to be run securely in Docker containers by passing the credentials into the container at
runtime.

    DatastreamUsername=YOURUSERNAME
    DatastreamPassword=YOURPASSWORD

For backwards compatibility we have retained the ability to put credentials into options().  In this scenario you would add these lines to your .RProfile file:

    options(Datastream.Username = "YOURUSERNAME")
    options(Datastream.Password = "YOURPASSWORD")

Once set up a typical request would be for a snapshot request:

     mydsws <- dsws$new()
     myData <- mydsws$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                   datatype = "P",
                                   requestDate = "0D")

or with expressions (including E codes), where "X" is replaced by "XXXX":

     mydsws <- dsws$new()
     myData <- mydsws$snapshotRequest(instrument = c("ABF","RIO","WPP"),
                                   expression = "PCH#(XXXX,3M)",
                                   requestDate = "0D")

and for a constituent list request (expressions are also supported)

    mydsws <- dsws$new()
    myData <- mydsws$listRequest(instrument = "LFTSE100", datatype = "P", requestDate = "0D")

and for a timeseries request (expressions are also supported)

    mydsws <- dsws$new()
    xtsData <- mydsws$timeSeriesRequest(instrument = "MKS",
                           datatype = "MV",
                           startDate = "-30D",
                           endDate = "-0D",
                           frequency = "D")


The dsws interface will split large requests down into chunks small enough for the DSWS interface to process.  However, the maximum chunk size varies in the DSWS documentation and is either [2000](https://product.datastream.com/DswsClient/Docs/AboutRestSvc.aspx) or [50](https://developers.refinitiv.com/sites/default/files/DSWS%20for%20Desktop%20-%20User%20stats%20and%20limits_0.pdf).  Different users have different limits.  The default chunkLimit is 2000, but it can be to 50:

    mydsws <- dsws$new()
    mydsws$chunkLimit <- 50L

Alternatively this can be set as an option by adding this line to your .RProfile file.

    options(Datastream.ChunkLimit = 50L)

In addition, this package has been built to be largely backwards compatible with the [Datastream2R](https://github.com/CharlesCara/Datastream2R) package.  You just need to replace 
    require(Datastream2R) 
with 
    require(DatastreamDSWS2R)

## CRAN
Thank you to @mbannert for his work making the package ready to be released on CRAN. 

## Update 1.5.1
With this update we have switched from using the RCurl/rjson to using the httr/jsonlite packages for communicating with the Datastream server 
