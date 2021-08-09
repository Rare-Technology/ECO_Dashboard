popupText <- function(data_filtered) {
  paste0("<strong>MA name: </strong>",
         data_filtered$ma_name,
         "<br>",
         "<strong>Location: </strong>",
         data_filtered$location_name,
         "<br><br>",
         "<strong>Fish biomass: </strong>",
         round(tapply(X=data_filtered$biomass_kg_ha,
                      INDEX=data_filtered$location_name,
                      FUN=mean),
               digits=1
         )
  )
}