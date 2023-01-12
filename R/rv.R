initialize_rv <- function(INIT) {
  reactiveValues(
    sel_dataset = INIT$DATASET$SELECTED,
    current_tab = INIT$CURRENT_TAB,
    sel_country = INIT$COUNTRY$SELECTED,
    sel_subnational = INIT$SUBNATIONAL$SELECTED,
    sel_local = INIT$LOCAL$SELECTED,
    sel_maa = INIT$MAA$SELECTED,
    sel_family = INIT$FAMILY$SELECTED,
    metrics = INIT$METRICS[["Fish"]],
    sel_metric = INIT$METRICS[["Fish"]][1],
    sel_geom = INIT$SEL_GEOM,
    facet_maa = INIT$FACET_MAA,
    sel_yscale = INIT$SEL_YSCALE,
    basemap = INIT$BASEMAP,
    # there is a fish family that is blank. currently, selecting that will crash
    # the app with error Warning: Error in aggregate.data.frame: no rows to aggregate
    # for now, we will just take out the ~600 samples that have this nameles family
    data_full = INIT$DATA_FULL,
    language = "English")
}