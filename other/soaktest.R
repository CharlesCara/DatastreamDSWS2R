#############################################################
# Soak test

test_that("Soak test - need to check whether memory leak", {


  mydsws <- dsws$new()


  for(i in 1:100){
    xtsData <- mydsws$timeSeriesRequest(instrument =  c("MKS", "D:BASF"),
                                        datatype = "MV",
                                        startDate = as.Date("31/12/2011", "%d/%m/%Y"),
                                        endDate = as.Date("01/08/2015", "%d/%m/%Y"),
                                        frequency = "D")
    Sys.sleep(time = 1)  #pause between hits
  }

})