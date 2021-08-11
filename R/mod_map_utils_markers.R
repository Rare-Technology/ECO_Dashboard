get_biomass_loc <- function(data_aggreg) {
  aggregate(cbind(lon, lat, biomass_kg_ha) ~ ma_name + location_name, data=data_aggreg, FUN=mean)
}

popupText <- function(data_map) {
  paste0("<strong>MA name: </strong>",
         data_map$ma_name,
         "<br>",
         "<strong>Location: </strong>",
         data_map$location_name,
         "<br><br>",
         "<strong>Fish biomass: </strong>",
         round(data_map$biomass_kg_ha, digits=1)
  )
}