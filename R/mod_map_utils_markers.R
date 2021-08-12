# aggregate_map_data <- function(data_filtered) {
#   aggregate(cbind(biomass_kg_ha, density_ind_ha) ~ country + ma_name +
#               location_status + lon + lat + location_name + transect_no,
#               data=data_filtered, FUN=sum) %>% 
#     merge(., aggregate(species ~ country )
# }

get_map_data <- function(data_filtered, coords_filtered) {
  biomass_aggreg <- aggregate_data(data_filtered, 'biomass_kg_ha')
  density_aggreg <- aggregate_data(data_filtered, 'density_ind_ha')
  
  biomass_map <- aggregate(biomass_kg_ha ~ ma_name + location_name,
                           data=biomass_aggreg, FUN=mean)
  density_map <- aggregate(density_ind_ha ~ ma_name + location_name,
                           data=density_aggreg, FUN=mean)
  diversity_map <- aggregate_data(data_filtered, 'species') %>%
    dplyr::select(ma_name, location_name, species)
  
  map_data <- merge(biomass_map, density_map) %>% 
                merge(., diversity_map) %>% 
                merge(., coords_filtered)
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
            " fish/ha",
         "<br>",
         "<strong>Fish diversity: </strong>",
            round(data_map$species),
            " species"
  )
}