# Data processing functions for DLG dashboard
# Adapted from functions written by Ben Bond-Lamberty for TEMPEST dashboard
# Created 2026-10-07 by Stephanie Pennington | stephanie.pennington@pnnl.gov

# Utility function used throughout the code: filter a dataset to a recent
# window (hours) going from `ddt` (dashboard date time)
# This assumes there's a `Timestamp` column in `x`
filter_recent_timestamps <- function(x, window, ddt) {
  stopifnot(window > 0)
  x |> 
    rename(Timestamp = TIMESTAMP) |> 
    filter(Timestamp > ddt - window * 60 * 60, Timestamp <= ddt)
}

compute_teros <- function(teros, ddt) {
  
  teros |> 
    extract(
      research_name,
      into = c("variable", "depth_cm"),
      regex = "^[^-]+-(.*)-([0-9]+)cm$",
      convert = TRUE, remove = FALSE) -> teros_full
  
  teros_full |> 
    filter_recent_timestamps(FLAG_TIME_WINDOW, ddt) %>%
    left_join(TEROS_RANGE, by = "variable") -> teros_filtered
  
  teros_filtered %>%
    group_by(variable) %>%
    summarise(flag_sensors(value, limits = c(low[1], high[1]))) %>%
    summarise(fraction_in = weighted.mean(fraction_in, n)) %>%
    # average the fraction in values
    mutate(percent_in = if_else(all(is.finite(fraction_in)),
                                paste0(round(fraction_in * 100, 0), "%"),
                                "--"),
           color = badge_color(1 - fraction_in)) ->
    teros_bdg
  
  teros_filtered %>%
    group_by(variable) %>%
    mutate(bad_sensor = which_outside_limits(value,
                                             left_limit = low[1],
                                             right_limit = high[1]),
           .keep = "all") %>%
    filter(bad_sensor) %>%
    ungroup() %>%
    select(Plot, Sensor_ID, variable, depth_cm, Logger, Location) %>%
    distinct(Sensor_ID, Logger, .keep_all = TRUE) ->
    teros_bad_sensors
  
  list(teros = teros_full,
       teros_bdg = teros_bdg,
       teros_bad_sensors = teros_bad_sensors)
  
}