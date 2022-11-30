FISH_TABS <- c("Start", "Fish", "Map")
MANGROVE_TABS <- c("Mangrove Forests")
CURRENT_YEAR <- lubridate::year(lubridate::now())
WATERMARK_LABEL <- paste("Fish Forever (", CURRENT_YEAR, "). Rare.", sep="")
PLOT_ERROR <- "Something went wrong. This might be because there is data missing such as species names. If you think this is a mistake, please contact scitech@rare.org"