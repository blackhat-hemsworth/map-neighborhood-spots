library(sf)
library(tidyverse)
library(httr2)

get_lat_long <- function(address, g_key) {
  r <-
    request(paste0("https://maps.googleapis.com/maps/api/geocode/json?")) |> 
    req_url_query(address = address) |>
    req_url_query(key = g_key) |> 
    req_perform() |>
    resp_body_json() 
  
  r <- r |> first() |> first()
  r_sf <- as.data.frame(r$geometry$location) |> 
    st_as_sf(coords = c("lng","lat"), crs=4326) 
  return(r_sf$geometry[[1]])
}

google_API_key <- keyring::key_get("google-api")

# could be any geoJSON defining neighborhoods
neighborhoods <- st_read("https://services.arcgis.com/afSMGVsC7QlRK1kZ/arcgis/rest/services/Minneapolis_Neighborhoods/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
bounds <- neighborhoods |> st_bbox()

# any file with addresses
spots <- read.csv("input/address_file.csv")
spots <- 
  spots |> 
  select(name,Address) |> 
  mutate(geometry = st_sfc(st_point(c(0,1))))

for (i in 1:length(spots$Address)) {
  spots$geometry[i] = st_sfc(get_lat_long(spots$Address[i], google_API_key))
}

spots <- spots |> st_as_sf(crs = st_crs(4326)) |> st_set_crs(4326)

neighborhoods |> 
  ggplot() + 
  geom_sf(alpha = .5) + 
  geom_sf_text(aes(label=stringr::str_wrap(BDNAME, width = 12)), size =2) +
  geom_sf(data = spots,color="red",
          aes(label=stringr::str_wrap(name, width = 12)), size =2) +
  coord_sf(xlim = c(bounds["xmin"],bounds["xmax"]),
           ylim = c(bounds["ymin"],bounds["ymax"])) +
  theme_void()

ggsave("neighborhoods.png", bg="white")