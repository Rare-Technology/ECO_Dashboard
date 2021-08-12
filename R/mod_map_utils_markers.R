aggregate_map_data <- function(data_filtered) {
  aggregate(cbind(biomass_kg_ha, density_ind_ha) ~ country + ma_name +
              location_status + lon + lat + location_name + transect_no,
              data=data_filtered, FUN=sum)
}

get_loc_data <- function(data_aggreg) {
  aggregate(cbind(lon, lat, biomass_kg_ha, density_ind_ha) ~
              ma_name + location_name,data=data_aggreg, FUN=mean)
}

popupText <- function(data_map) {
  paste0("<strong>MA name: </strong>",
            data_map$ma_name,
         "<br>",
         "<strong>Location: </strong>",
            data_map$location_name,
         "<br><br>",
         "<strong>Fish biomass: </strong>",
            round(data_map$biomass_kg_ha, digits=1),
            " kg/ha",
         "<br>",
         "<strong>Fish density: </strong>",
            round(data_map$density_ind_ha, digits=1),
            " fish/ha"
  )
}