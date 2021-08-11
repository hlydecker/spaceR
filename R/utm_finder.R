require(tidyverse)
require(purrr)
require(testthat)

find_one_utm_zone <- function(longitude, latitude) {

  # Special zones for Svalbard
  if (latitude >= 72.0 && latitude <= 84.0 ) {
    if (longitude >= 0.0  && longitude <  9.0)
      return("31X");
    if (longitude >= 9.0  && longitude < 21.0)
      return("33X")
    if (longitude >= 21.0 && longitude < 33.0)
      return("35X")
    if (longitude >= 33.0 && longitude < 42.0)
      return("37X")
  }
  # Special zones for Norway
  if (latitude >= 56.0 && latitude < 64.0 ) {
    if (longitude >= 0.0  && longitude <  3.0)
      return("31V");
    if (longitude >= 3.0  && longitude < 12.0)
      return("32V")
  }

  # North + South Poles

  if (latitude > 84.0){
    if ((longitude+180)%%360-180 < 0) {return("Y")}
    if ((longitude+180)%%360-180 > 0) {return("Z")}
  } else if (latitude < -80.0){
    if ((longitude+180)%%360-180 < 0) {return("A")}
    if ((longitude+180)%%360-180 > 0) {return("B")}
  }

  # Everything in the middle

  if ( (latitude>-80.0) && (latitude<=84.0) ){

    mid_zones <- LETTERS[c(3:8,10:14,16:24)] # C to X, skip I and O
    utm_letter <- mid_zones[ min(floor( (latitude + 80) / 8 )+1 , 20) ]
    utm_number <- (floor( (longitude + 180) / 6 ) %% 60) + 1 # modulo in case longitude is 0 to 360 instead of -180 to 180
    utm_zone <- paste0(utm_number, utm_letter)
    return(utm_zone)

  } else {
    stop("lat long not valid (or something else broke)")
  }
}

find_utm_zone <- function(lon, lat){
  purrr::map2_chr(.x = lon, .y = lat, .f = find_one_utm_zone)
}

# Example
locs <-
  tibble(lon = c(-100,30,150, 4, 7, 22, 0, 12, -34, -20),
         lat = c(-45, 85, 12, 57, 81, 83, 5, -81, 85, 83),
         desired_utm_zone = c("14G","Z","56P", "32V" ,"31X","35X","31N", "B","Y","27X"))

locs2 <-
  locs %>%
  mutate(utm_zone = find_utm_zone(lon = lon,lat = lat))

# Test that it worked
testthat::expect_equal(locs2$utm_zone, locs2$desired_utm_zone)

rm(locs, locs2)
