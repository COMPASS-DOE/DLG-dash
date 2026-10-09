# These are global settings for the DELUGE data dashboard
# Adapted from TEMPEST dashboard global.R from June 2023, updated October 2026

library(ggplot2)
theme_set(theme_minimal())
library(dplyr)
library(shiny)
library(DT)
library(readr)
library(lubridate)
library(rdrop2refreshtoken)
library(shinybusy)
library(plotly)
library(janitor)
library(arrow)
library(cowplot)
library(tidyr)

if(!require("compasstools")) {
    stop("Need to devtools::install_github('COMPASS-DOE/compasstools@bypass-dropdir')")
}
library(compasstools)

# The TESTING flag causes the server to load static data in offline-data/
# When writing new code or debugging, it's often useful to set this to TRUE
# so as not to spend time downloading from Dropbox
TESTING <- FALSE

# The DATA_SOURCE flag indicates where sensor data is pulled from. This currently
# has three options: local, cloud, or github
DATA_SOURCE <- "github"

# Flooding event length (hours)
EVENT_LENGTH <- 3

TEXT_MSG_USERS <- tribble(
    ~name,     ~number,       ~carrier,
    "SP",      "3016063322",  "Verizon",
    "BBL",     "6086582217",  "T-Mobile",
    "AMP",     "5203491898",  "Verizon",
    "Julia",   "8644205609",  "Verizon"
)

GRAPH_TIME_WINDOW <- 24   # hours back from the dashboard datetime
GRAPH_TIME_INTERVAL <- "15 minutes"  # used by round_date in graphs
FLAG_TIME_WINDOW <- 120         # hours back from the dashboard datetime

# The 'no data' graph that's shown if no rows are selected, etc.
NO_DATA_GRAPH <- ggplot() +
    annotate("text", x = 1, y = 1, label = "(No selection)", size = 12) +
    theme(axis.title = element_blank(),
          axis.text  = element_blank(),
    )
