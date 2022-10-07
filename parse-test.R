#remotes::install_github("slu-openGIS/postmastr", force=TRUE)
#remotes::install_github("nbarsch/tfwstring")
### Testing Parsing Data by Akila Forde ####
setwd( "H:/data-wrangling" )
library( postmastr )
library( tidyverse )
library( stringi )
library( tfwstring )

## Loading dirty address data
d <- read.csv( '../data-raw/reproductive-health-npos-v2.csv' )
## Already parsed
d2 <- read.csv( '../data-raw/community-development-npos.csv' )

## Filtering out address to test the postmastr functions
d.address <- d %>% select( address )

## Creating address dictionaries for use in the pm_parse functions, both state and directional
x.dir <- pm_dictionary(type = "directional", filter = c("N", "S", "E", "W"), locale = "us")
x.state <- pm_dictionary(type = "state", case = c("title", "upper", "lower"), locale = "us")


### Caveat: to parse city, need census API Key to pull all cities
### Excludes Armed Americas, Armed Americas Europe Middle East and 

tidycensus::census_api_key( "84381427d420f30520867a40efed18b32acc5993" )
x.city <- pm_dictionary( type = "city", filter = state.abb,
                        case = c("title", "upper", "lower"), locale = "us" )

## Introducing code to handle addresses with 4 digit zipcodes
x.last_words <- stri_extract_last_words( d.address$address )

## Loop to add a zero in front of any shortened zipcodes
for ( i in 1:length( x.last_words ) ){
  if ( nchar( x.last_words[i]) == 4 ) {
    d.address$address[i] <- paste( word( d.address$address[i],1,-2 ),
                                   paste0( "0",x.last_words[i]) ) 
    }
}

## Need to ensure the data have a unique identification
d.address <- pm_identify( d.address, var="address" )

## Parse the data for cities, and address
d.parsed <- d.address %>% 
  pm_parse( input='full', address="address", output = "short", keep_parsed = "limited",
            dir_dict=x.dir, state_dict=x.state, city_dict=x.city )

### Notes: Some census cities are not the same as cities inputed in the data, causing an NA to occur
### In the city column, typos. The parsing of City introduces exact matching, need to figure out how
### best to parse the city. Seems a bit backward to do this version

  
## To get the cities for those that aren't on the census list
## Caveat: this may not work if there isn't a zipcode included and if the city is not on census' list
parse_address <- str_match( d.parsed$address,"(.+), (.+), (.+) (.+)" )[, -1]

d.parsed <- subset( d.parsed, select = -address )

for ( i in 1:length( parse_address[,2] ) ){
  if ( is.na( d.parsed$pm.city[i] ) ) {
    d.parsed$pm.city[i] <- parse_address[i,2]
  }
}

d.parsed %>% 
  rename_all( ~stringr::str_replace(.,"^pm.","") )


### Notes: does not handle zipcodes without a zero from the front if US 



