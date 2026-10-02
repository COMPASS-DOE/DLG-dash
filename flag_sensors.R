# flag_sensors.R - utility functions
# BBL June 2023

# These RANGE variables are passed to flag_sensors() by the server, and used
# to identify sensors out of range

# For TEROS, these are all 1%/99% quantiles of test data
# This one is a tibble because TEROS is three variables in a single dataset
TEROS_RANGE <- tribble(~variable, ~low, ~high,
                       "EC",      10,   4500, #225 normally
                       "TSOIL",   5,   25,
                       "VWC",     2000, 4000,
                       "MP", -100000, 0)
SAPFLOW_RANGE <- c(0.2, 0.8) # roughly the 10%/90% quantiles of test data
VOLTAGE_RANGE <- c(12, 14.3) # roughly 0.05%/99.5% quantiles of test data

AQUATROLL_RANGE <- tribble(~variable, ~low,   ~high,
                       "Salinity",      0,    0.2,
                       "Temp",          0,    35, # roughly 1%/99% quantiles of test data
                       "DO_mgl",        0.05, 9.95,
                       "Pressure_psi",  10,   20)

AQUATROLL_TEMP_RANGE <- unlist(
    AQUATROLL_RANGE[AQUATROLL_RANGE$variable=="Temp", c("low", "high")]
    )

DO_RANGE <- c(-5, 120)

REDOX_RANGE <- c(-2500, 2500)

# Badge colors and 'trigger' values
# Currently green-yellow-red; could have more colors if desired
BADGE_COLORS <- c("green" = 0.0,    # green starts at 0% fail (this shouldn't change)
                  "yellow" = 0.05,  # yellow starts at 5% fail
                  "red" = 0.2)      # red starts at 20%

# Compute badge color(s) based on fraction(s) out
badge_color <- function(frac_out, badge_colors = BADGE_COLORS) {
    if(badge_colors[1] != 0.0) {
        stop("The first entry in badge_colors must be zero")
    }

    colors <- cut(frac_out,
                  c(badge_colors, 1.01), # 1.01 so that 1 is included in the highest interval
                  labels = names(badge_colors),
                  right = FALSE)
    x <- as.character(colors)
    x[is.na(x)] <- "black"
    x
}

# Identify which observations are outside of limits
which_outside_limits <- function(values, left_limit, right_limit) {
    is.na(values) | !between(values, left_limit, right_limit)
}

# Compute fraction (0-1) of values outside specified limits
# By default, NA counts as a failure
frac_outside_limits <- function(values, left_limit, right_limit, na.rm = FALSE) {
    if(na.rm) values <- na.omit(values)

    # In this calculation, NAs count as out-of-bounds
    sum(which_outside_limits(values, left_limit, right_limit)) / length(values)
}


bad_sensors <- function(df, values, id, limits) {

    df[!between(values, min(limits), max(limits)), ] %>% select(id, Grid_Square) -> bounds

    df[is.na(values), ] %>% select(id, Grid_Square) -> nas

    unique(bind_rows(nas, bounds))

}


# Return both fraction_out and associated badge color for a vector of
# values and associated limits
flag_sensors <- function(values, limits, na.rm = FALSE) {
    frac_out <- frac_outside_limits(values, min(limits), max(limits), na.rm = na.rm)
    x <- tibble(n = length(values),
                fraction_in = 1- frac_out,
                percent_in = paste0(round(fraction_in * 100, 0), "%"),
                color = badge_color(frac_out))
    # 'NaN' entries usually mean no data
    invalids <- !is.finite(x$fraction_in)
    x$percent_in[invalids] <- "--"
    x
}
