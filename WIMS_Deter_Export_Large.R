library(jsonlite)
library(httr)
library(tidyverse)

deters <- c("0085",
            "0111",
            "0135",
            "0348",
            "7668")

date1 <- seq(as.Date("2020-01-01"), as.Date("2021-02-01"), by = "1 month")

df <- data.frame() 

for (y in 1:(length(date1) - 1)) {  # Ensure date1[y+1] does not exceed bounds
  
  for (x in deters) {
    
    tryCatch({
      print(paste0("Month ", y))
      
      base_url <- "http://environment.data.gov.uk/water-quality/data/measurement/"
      
      query_params <- list(
        '_limit' = 5,
        'determinand' = x,
        'startDate' = date1[y],
        'endDate' = date1[y+1]  # Ensure valid index range
      )
      
      url <- modify_url(base_url, query = query_params)
      
      response <- GET(url)

      JSON_LD <- fromJSON(rawToChar(response$content), flatten = TRUE)
      dat <- JSON_LD$items
   
      print(dim(df))  # Check dimensions before binding
      df <- bind_rows(df, dat)
      print(dim(df))  # Check dimensions after binding
      
    }, error = function(e) {
      warning(paste("Error encountered in iteration y =", y, ", x =", x, ":", e$message))
    })
    
  }
}

# Transform prior to visualisation
    do <- df %>% mutate(
              Datetime= as.POSIXct(sample.sampleDateTime, format = "%Y-%m-%dT%H:%M:%S", tz="utc"))
