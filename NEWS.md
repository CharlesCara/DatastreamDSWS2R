## Update 1.9.8
Change branding from Refinitiv to LSEG

## Update 1.9.5
Handle upload of daily series that begin on a weekend better

## Update 1.9.3
Hold the number of DSWS requests and chunks after each data request

## Update 1.8.1
Change server URLs to https.  Fix issue # 39

## Update 1.7.9
Internal changes to package to make it pass CRAN checks and policies.

## Update 1.7.5
Improvement in handling of Token Callback function.

## Update 1.7.4
Fix for Issues #37 - charToDate error raised when list request had mixture of dates and character "NA""

## Update 1.7.2
Fix for Issues #20 - requesting token after http 403 response

## Update 1.7.1
Fix for Issue #28 - callback function for getting token 


## Update 1.6.6
Fix for Issues #26 and #27

## Update 1.6.3
Handle lower case RIC codes.

## Update 1.6.2
Improve documentation and return Instrument column in snapshot requests.

## Update 1.6.1
Added handling of composite datatypes which return multiple values.

## Update 1.5.1
With this update we have switched from using the RCurl/rjson to using the httr/jsonlite packages for communicating with the Datastream server. 



