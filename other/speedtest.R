require(DatastreamDSWS2R)

mydsws <- dsws$new()
mydsws$setLogging(3L)



system.time({
  xtsValues <- mydsws$timeSeriesRequest(instrument = "@AAPL",
                                            datatype = "P",
                                            startDate = as.Date("2010-01-01"),
                                            endDate = as.Date("2016-01-01"),
                                            frequency = "D", format = "ByInstrument")
})


system.time({
  xtsValues <- mydsws$timeSeriesListRequest(instrument = "LFTSE100",
                                            datatype = "P",
                                            startDate = as.Date("2013-01-01"),
                                            endDate = as.Date("2016-01-01"),
                                            frequency = "D", format = "ByInstrument")
})

require(DatastreamDSWS2R)
myDSCodes <- readRDS(file = "~/FT100DSCodes.RDS")
mydsws <- dsws$new()
mydsws$setLogging(3L)
system.time({
xtsValues <- mydsws$timeSeriesRequest(instrument = myDSCodes[50:60],
                                      datatype = "P",
                                      startDate = as.Date("2010-01-01"),
                                      endDate = as.Date("2016-01-01"),
                                      frequency = "D", format = "ByInstrument")

})
print(ncol(xtsValues))

#-------------------------------------------------------------------
require(DatastreamDSWS2R)
step <- 25
myDSCodes <- readRDS(file = "~/FT100DSCodes.RDS")

for(i in 0:(100/step)){
  mydsws <- dsws$new()
  mydsws$setLogging(3L)
  print(paste0("Step ", i , " from ", (i * step + 1), " to ", ((i+1) * step)))
  print("Instruments requested:")
  print(myDSCodes[(i * step + 1):((i+1) * step)])
  startTime <- Sys.time()
  xtsValues <- mydsws$timeSeriesRequest(instrument = myDSCodes[(i * step + 1):((i+1) * step)],
                                        datatype = "P",
                                        startDate = as.Date("2010-01-01"),
                                        endDate = as.Date("2016-01-01"),
                                        frequency = "D", format = "ByInstrument")
  message(paste0("Method took ", Sys.time() - startTime))
  print(ncol(xtsValues))
}