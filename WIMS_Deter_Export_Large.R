library(jsonlite)
library(httr)
library(tidyverse)
library(sf)

# Determinands of interest
deters <- c("0085",
            "0111",
            "0135",
            "0348",
            "7668")

# Due to size of dataset, we compartmentalise data to abide rate limiting 
# Date sequence of interest below
date1 <- seq(as.Date("2020-01-01"), as.Date("2025-02-01"), by = "1 month")

df <- data.frame() 


# Call API with rate limit set to 10 for example.
# Bind rows to eachother to build dataset
for (y in 1:length(date1)) { 
  
  for (x in deters) {
    
    tryCatch({
      print(paste0("Month ", y)) # print as visual QA step
      
      base_url <- "http://environment.data.gov.uk/water-quality/data/measurement/"
      
      query_params <- list(
        '_limit' = 10,
        'determinand' = x,
        'startDate' = date1[y],
        'endDate' = date1[y+1]
      )
      
      url <- modify_url(base_url, query = query_params)
      
      response <- GET(url)

      JSON_LD <- fromJSON(rawToChar(response$content), flatten = TRUE)
      dat <- JSON_LD$items
   
      print(dim(df))  
      df <- bind_rows(df, dat)
      print(dim(df))  # Print as a visual QA to see data added
      
    }, error = function(e) {
      warning(paste("Error encountered in iteration y =", y, ", x =", x, ":", e$message))
    })
    
  }
}

# Transform prior to visualisation
     df %<>% mutate(
              Datetime= as.POSIXct(sample.sampleDateTime, format = "%Y-%m-%dT%H:%M:%S", tz="utc"),
              Threshold = case_when(determinand.notation=="0085"~ 20,
                                    determinand.notation=="0111"~ 14,
                                    determinand.notation=="0135"~ 33,
                                    determinand.notation=="0348"~ 1,
                                    determinand.notation=="7668"~ 0)
                ) |> 
              group_by(determinand.notation) |> 
      
              mutate(
              Threshold_Breach = case_when(result >= Threshold ~ 1)
              ) |> 
      
              select(-c("@id","determinand.@id", "determinand.unit.@id", 
                     "sample.@id", "sample.purpose.@id", "sample.sampledMaterialType.@id",
                     "resultQualifier.@id")) |>  
      
              rename(Deter = determinand.notation)

      
 # Load in shapefile data   
 EA_areas <- read_sf("C:/Users/hg000051/Downloads/AdminBoundEAandNEpublicFaceAreas-SHP/data/Administrative_Boundaries_Environment_Agency_and_Natural_England_Public_Face_Areas.shp")
  

  # Intersect WIMS points with EA areas, so we know which area mon points belong to
  df_sf <-  df %>% st_as_sf(coords = c("sample.samplingPoint.easting", "sample.samplingPoint.northing"), crs=27700) %>% 
                  st_intersection(EA_areas)
 
  # Calculate total breaches in a month per EA area and per site.    
  df <-   df_sf %>% group_by(lubridate::month(sample.sampleDateTime),short_name) %>% 
    mutate(Monthly_breach_tot_area = sum(Threshold_Breach == "1", na.rm = TRUE)) %>%
     ungroup() %>% 
     group_by(lubridate::month(sample.sampleDateTime), sample.samplingPoint.label) %>% 
    mutate(Monthly_breach_tot_location = sum(Threshold_Breach == "1", na.rm = TRUE)) %>%
    st_drop_geometry()
    
  write.csv(df, "C:/Users/hg000051/Downloads/WIMS_SDA_Export.csv")
  
