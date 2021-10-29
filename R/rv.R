initialize_rv <- function(INIT) {
  reactiveValues(
    current_tab = INIT$CURRENT_TAB,
    sel_country = INIT$SEL_COUNTRY$FISH,
    sel_metric = INIT$SEL_METRIC,
    sel_geom = INIT$SEL_GEOM,
    sel_yscale = INIT$SEL_YSCALE,
    sel_family = INIT$SEL_FAMILY,
    basemap = INIT$BASEMAP,
    # there is a fish family that is blank. currently, selecting that will crash
    # the app with error Warning: Error in aggregate.data.frame: no rows to aggregate
    # for now, we will just take out the ~600 samples that have this nameles family
    data_full = list(
      fish = INIT$DATA_FULL$FISH,
      mangroves = INIT$DATA_FULL$MANGROVES
    ),
    data_filtered = list(
      fish = INIT$DATA_FILTERED$FISH,
      mangroves = INIT$DATA_FILTERED$MANGROVES
    ),
    coords = INIT$COORDS)
}