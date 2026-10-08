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

compute_teros12 <- function(teros12, ddt) {
  
  teros12 |> 
    extract(
      research_name,
      into = c("variable", "depth_cm"),
      regex = "^[^-]+-(.*)-([0-9]+)cm$",
      convert = TRUE, remove = FALSE) -> teros12_full

  teros12_full |> 
    filter_recent_timestamps(FLAG_TIME_WINDOW, ddt) %>%
    left_join(TEROS12_RANGE, by = "variable") -> teros_filtered
  
  teros_filtered %>%
    group_by(variable) %>%
    summarise(flag_sensors(value, limits = c(low[1], high[1]))) %>%
    summarise(fraction_in = weighted.mean(fraction_in, n)) %>%
    # average the fraction in values
    mutate(percent_in = if_else(all(is.finite(fraction_in)),
                                paste0(round(fraction_in * 100, 0), "%"),
                                "--"),
           color = badge_color(1 - fraction_in)) ->
    teros12_bdg
  
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
    teros12_bad_sensors
  
  list(teros12 = teros12_full,
       teros12_bdg = teros12_bdg,
       teros12_bad_sensors = teros12_bad_sensors)
  
}

compute_teros21 <- function(teros21, ddt) {
  
  teros21 |> 
    extract(
      research_name,
      into = c("variable", "depth_cm"),
      regex = "^[^-]+-(.*)-([0-9]+)cm$",
      convert = TRUE, remove = FALSE) -> teros21_full
  
  teros21_full |> 
    filter_recent_timestamps(FLAG_TIME_WINDOW, ddt) %>%
    left_join(TEROS21_RANGE, by = "variable") -> teros21_filtered
  
  teros21_filtered %>%
    group_by(variable) %>%
    summarise(flag_sensors(value, limits = c(low[1], high[1]))) %>%
    summarise(fraction_in = weighted.mean(fraction_in, n)) %>%
    # average the fraction in values
    mutate(percent_in = if_else(all(is.finite(fraction_in)),
                                paste0(round(fraction_in * 100, 0), "%"),
                                "--"),
           color = badge_color(1 - fraction_in)) -> teros21_bdg
  
  teros21_filtered %>%
    group_by(variable) %>%
    mutate(bad_sensor = which_outside_limits(value,
                                             left_limit = low[1],
                                             right_limit = high[1]),
           .keep = "all") %>%
    filter(bad_sensor) %>%
    ungroup() %>%
    select(Plot, Sensor_ID, variable, depth_cm, Logger, Location) %>%
    distinct(Sensor_ID, Logger, .keep_all = TRUE) ->
    teros21_bad_sensors
  
  list(teros21 = teros21_full,
       teros21_bdg = teros21_bdg,
       teros21_bad_sensors = teros21_bad_sensors)
}

compute_aquatroll <- function(troll600, ddt) {
  
  troll600 |> 
    extract(
      research_name,
      into = c("variable"),
      regex = "^gw-(.*)$",
      convert = TRUE, remove = FALSE) |> 
    filter(variable %in% c("temperature", "salinity", "density", "rdo-conc")) -> troll600_full

  troll600_full |> 
    filter_recent_timestamps(FLAG_TIME_WINDOW, ddt) %>%
    left_join(AQUATROLL_RANGE, by = "variable") -> troll600_filtered
  
  troll600_filtered |> 
    group_by(variable) %>%
    summarise(flag_sensors(value, limits = c(low[1], high[1]))) %>%
    summarise(fraction_in = weighted.mean(fraction_in, n)) %>%
    # average the fraction in values
    mutate(percent_in = if_else(all(is.finite(fraction_in)),
                                paste0(round(fraction_in * 100, 0), "%"),
                                "--"),
           color = badge_color(1 - fraction_in)) -> troll600_bdg
  
  troll600_filtered %>%
    group_by(variable) %>%
    mutate(bad_sensor = which_outside_limits(value,
                                             left_limit = low[1],
                                             right_limit = high[1]),
           .keep = "all") %>%
    filter(bad_sensor) %>%
    ungroup() %>%
    select(Plot, variable, Logger, Instrument) %>%
    distinct(Plot, Logger, .keep_all = TRUE) ->
    troll600_bad_sensors
  
  list(troll600 = troll600_full,
       troll600_bdg = troll600_bdg,
       troll600_bad_sensors = troll600_bad_sensors)
  
}

