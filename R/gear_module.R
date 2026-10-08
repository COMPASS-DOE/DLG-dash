# Gear module
# BBL June 2023

# The module’s UI function
# The first argument to a UI function should always be `id`.
# This is the namespace for the module.
# https://shiny.posit.co/r/articles/improve/modules/
gearUI <- function(id) {
    # `NS(id)` returns a namespace function
    ns <- NS(id)

    dropdownButton(
        icon = icon("gear"),
        circle = TRUE,
        status = "primary",
        dateInput(ns("event_date"),
                  label = "Event Date: yyyy-mm-dd"
        ),
        textInput(ns("event_start"),
                  label = h3("Event Start"),
                  value = "06:00:00",
                  placeholder = "HH:MM:SS"),
        actionButton("prog_button",
                     label = "Update")
    )
}


# Module server function - a fragment of server logic
gearServer <- function(id, session, ddt_reactive) {
    stopifnot(is.reactive(ddt_reactive))

    moduleServer(
        id,
        ## Below is the module function
        function(input, output, session) {
            # If the dashboard datetime has changed, update the default
            # date input for EVENT_START.
            observeEvent(ddt_reactive, {
                updateDateInput(session, "event_date",
                                value = as.Date(ddt_reactive()))
            })

            gear_reactive <- reactive({
                EVENT_START <- as_datetime(paste(input$event_date, input$event_start), tz = "EST")
                # EVENT_LENGTH is defined in global.R
                EVENT_STOP <- EVENT_START + hours(EVENT_LENGTH)
                EVENT_HOURS <- as.numeric(difftime(EVENT_STOP, EVENT_START, units = "hours"))

                list(EVENT_START = EVENT_START,
                     EVENT_STOP = EVENT_STOP,
                     EVENT_HOURS = EVENT_HOURS)
            })

        })
}

