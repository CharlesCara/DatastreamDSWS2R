##############################################################################################

context("classConstructor.R : test of timeSeriesRequest method - Chunking")

suppressPackageStartupMessages(require(xts))

#testDataFolder <- "tests/testthat/testData/"
testDataFolder <- file.path(testthat::test_path(), "testData")

# Whether to use the server for the requests
useServer <-TRUE


##############################################################################################

test_that("test of a large timeseries request - long list with expression", {
  if(Sys.getenv("DatastreamUsername") == ""){
    skip("Username not available")
  }
  skip_on_cran()



  longListDSCodes <- c(
    "JP3526600006",	"JP3359600008",	"JP3551500006",	"JP3726800000",	"JP3405000005",	"JP3496400007",	"JP3899600005",
    "JP3242800005",	"JP3902400005",	"JP3814000000",	"JP3429800000",	"JP3463000004",	"JP3112000009",	"JP3893600001",
    "JP3249600002",	"JP3493800001",	"JP3258000003",	"JP3830800003",	"JP3605400005",	"JP3260800002",	"JP3705200008",
    "JP3224600001",	"JP3340600000",	NA,	            "JP3895800005",	"JP3404600003",	"JP3246400000",	"JP3326400003",
    "JP3111200006",	"JP3802400006",	"JP3756600007",	"JP3402200004",	"JP3210200006",	"JP3703600001",	"JP3729400006",
    "JP3573000001",	"JP3351200005",	"JP3893200000",	"JP3143600009",	"JP3629000005",	"JP3621000003",	"JP3180400008",
    "JP3266400005",	"JP3289800009",	"JP3407400005",	"JP3955400001",	"JP3420600003",	"JP3304200003",	"JP3522200009",
    "JP3358800005",	"JP3914400001",	"JP3388200002",	"JP3877600001",	"JP3401400001",	"JP3867600003",	"JP3505000004",
    "JP3119600009",	"JP3205800000",	"JP3574200006",	"JP3419400001",	"JP3421800008",	"JP3899800001",	"JP3679700009",
    "JP3626800001",	"JP3870400003",	"JP3942400007",	"JP3973400009",	"JP3443600006",	"JP3174410005",	"JP3721600009",
    "JP3442800003",	"JP3511800009",	"JP3165700000",	"JP3753000003",	"JP3371200001",	"JP3538800008",	"JP3436100006",
    "JP3932800000",	"JP3613400005",	"JP3350800003",	"JP3394200004",	"JP3769000005",	"JP3982800009",	"JP3224200000",
    "JP3903000002",	"JP3190000004",	"JP3197800000",	"JP3108600002",	"JP3571400005",	"JP3134800006",	"JP3397200001",
    "JP3634600005",	"JP3276400003",	"JP3402600005",	"JP3597800006",	"JP3389900006",	"JP3544000007",	"JP3789000001",
    "JP3194000000",	"JP3845400005",	"JP3850200001",	"JP3116000005",	"JP3137200006",	"JP3774200004",	"JP3196000008",
    "JP3596200000",	"JP3160400002",	"JP3868400007",	"JP3197600004",	"JP3657400002",	"JP3833750007",	"JP3351600006",
    "JP3894800006",	"JP3162600005",	"JP3786200000",	"JP3940000007",	"JP3263000006",	"JP3256000005",	"JP3166000004",
    "JP3720800006",	"JP3676000007",	"JP3935600001",	"JP3865600005",	"JP3476600006",	"JP3661000004",	"JP3294400001",
    "JP3674000009",	"JP3901600001",	"JP3102000001",	"JP3837800006",	"JP3493400000",	"JP3802600001",	"JP3117600001",
    "JP3236200006",	"JP3277800003",	"JP3693200002",	"JP3780200006",	"JP3732200005",	"JP3122400009",	"JP3362700001",
    "JP3695200000",	"JP3949600005",	"JP3820000002",	"JP3942600002",	"JP3352000008",	"JP3389500004",	"JP3428600005"
    )

  mydsws <- dsws$new()
  #mydsws$setLogging(3)
  mydsws$chunkLimit <- 50L

  if(!useServer){
    mydsws$jsonResponseLoadFile <- file.path(testDataFolder, "test-dsws-timeSeriesRequestChunking-test01.json")
  }
  xtsData <- mydsws$timeSeriesRequest(instrument = longListDSCodes,
                                      expression = "(XXXX(SAL1FD12)/XXXX(SAL1TR12))-1.00",
                                      startDate = as.Date("1996-01-01"),
                                      endDate = as.Date("2016-01-20"),
                                      frequency = "M")

  expect_is(xtsData, "xts")
  expect_equal(ncol(xtsData), length(longListDSCodes))

  rm(mydsws, xtsData)

})

