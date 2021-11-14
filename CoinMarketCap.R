library(curl)
library(jsonlite)
#------------------------------------------------------------------------------#
#-----------------------Step 1: Retrieve Data Using Curl-----------------------#
#------------------------------------------------------------------------------#

#Curl provided by CoinMarketCap
#  -H "X-CMC_PRO_API_KEY: Insert Key" 
#  -H "Accept: application/json" -d "start=1&limit=5000&convert=USD" 
#  -G https://pro-api.coinmarketcap.com/v1/cryptocurrency/listings/latest
#  -d "start=1&limit=5000&convert=USD"

#Insert your api key
key <- "Insert Key"

#Headers for request
h <- new_handle(verbose = TRUE)
handle_setheaders(h,
                  "Accept" = "application/json",
                  "X-CMC_PRO_API_KEY" = key
)

#Curl Request
#Gives some error message, but works (Needs Improving)
con <- curl("https://pro-api.coinmarketcap.com/v1/cryptocurrency/listings/latest?start=1&limit=500&convert=USD", handle = h)

#Curl request to JSON
curl_to_JSON <- prettify(readLines(con))
JSON_to_list <- fromJSON(curl_to_JSON)

#JSON to data frame
df <- JSON_to_list$data

#------------------------------------------------------------------------------#
#------------------------------Step 2: Clean Data------------------------------#
#------------------------------------------------------------------------------#
library(tidyr)

#Save raw data to data frame that will be modified
no_max <- df

#Part 1: For this, we want Coins with a max supply 

#Split into multiple parts for better understanding
#Removes coins with max_supply of NA
no_max <- no_max %>% drop_na('max_supply')

#Remove coins with max_supply of 0 (zero)
no_max <- no_max[no_max$max_supply != 0,]

#Remove coins with a circulating supply greater than max_supply
no_max <- no_max[no_max$max_supply >= no_max$circulating_supply,]

#------------------------------------------------------------------------------#
#---------------------------Step 3: Manipulate Data----------------------------#
#------------------------------------------------------------------------------#

#Just want to see if there is any relation between %Change in 24h & Circulating Supply

#Range is too large to see any distinct relationship
#So, we should remove outliers to possibly see any relationship
plot(no_max$circulating_supply,no_max$quote$USD$percent_change_24h)

#setup max and min based on Q1,Q3, and IQR
min_Q1 <- quantile(no_max$circulating_supply,0.25) - 1.5*IQR(no_max$circulating_supply)
max_Q3 <- quantile(no_max$circulating_supply,0.75) + 1.5*IQR(no_max$circulating_supply)

#Remove points <Q1-1.5*IQR and >Q3+1.5*IQR
shrink_range <- no_max[no_max$circulating_supply > min_Q1 & no_max$circulating_supply < max_Q3,]

#Shows that volatility in 24hr percent change increase as circulating supply decreases
#Makes sense because more coins need to be sold or bought to have an effect of price
plot(shrink_range$circulating_supply,shrink_range$quote$USD$percent_change_24h,
     xlab = "Circulating Supply", ylab = "Percent Change 24hr(%)", main = "Percent Change 24hr(%) vs Circulating Supply")
