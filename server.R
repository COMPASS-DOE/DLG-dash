# Server code for the TEMPEST data dashboard
# June 2023

source("global.R")

server <- function(input, output, session) {
  
  dataInvalidate  <- reactiveTimer(15 * 60 * 1000) # 15 minutes
  alertInvalidate <- reactiveTimer(60 * 60 * 1000) # 60 minutes
  
  # ------------------ Check whether testing --------------------------
  
  # Note 1. TESTING is defined in the global environment (so "<<-")
  # Note 2. These checks have to be here as shiny.testmode isn't set until server runs
  # Check if we're running in a Shiny testing...
  TESTING <<- TESTING || isTRUE(getOption("shiny.testmode"))
  # ...or continuous integration environment
  TESTING <<- TESTING || Sys.getenv("CI") == "true"
  
  # ------------------ Read in sensor data -----------------------------
  
  # The server normally accesses the SERC Dropbox to download data
  # If we are TESTING, however, skip this and use local test data only
  if(!TESTING & DATA_SOURCE == "dropbox") {
    datadir <- "TEMPEST_PNNL_Data/Current_Data"
    token <- readRDS("droptoken.rds")
    cursor <- rdrop2refreshtoken::drop_dir(datadir, cursor = TRUE, dtoken = token)
  } else if (!TESTING & DATA_SOURCE == "local") {
    datadir <- "~/Dropbox (Smithsonian)/TEMPEST_PNNL_Data/Current_data/"
    token <- NULL
  }
  
  # DASHBOARD_DATETIME is the datetime that the dashboard is showing
  # Normally this is just now (i.e., Sys.time()), but when testing
  # it will be the latest date of the static testing data
  DASHBOARD_DATETIME <- reactive({
    # Invalidate and re-execute this reactive when timer fires
    dataInvalidate()
    
    if(TESTING) {
      # Get the latest time in the test data
      battery <- readRDS("offline-data/battery")
      max(battery$Timestamp)
    } else {
      Sys.time() |> with_tz(tzone = "EST")
    }
  })
  
  # dropbox_data is a list holding all the read-in data along with
  # several secondary products, e.g. the dashboard badge information
  # that's computed from the raw data
  dropbox_data <- reactive({
    # Invalidate and re-execute this reactive when timer fires
    dataInvalidate()
    
    ddt <- isolate({ DASHBOARD_DATETIME() })
    
    if(TESTING) {
      
      #ADD TESTING DATA
      
    } else if(DATA_SOURCE == "github"){
      
      read_parquet("https://github.com/COMPASS-DOE/sensor-data-preprocessor/blob/main/processed_data/DLG_TEROS12.parquet?raw=true") |> 
        compute_teros(ddt) -> teros_list
      
    }
    
    # Do limits testing and compute data needed for badges
    # compute_sapflow() etc. are defined in R/data_processing.R
    c(teros_list)
  })
  
  # ------------------ Gear and progress circle --------------------------
  
  # gearServer is defined in R/gear_module.R
  # We pass it DASHBOARD_DATETIME (a reactive) so it can update
  # its date input field if the datetime changes
  progress <- gearServer("gear", session, DASHBOARD_DATETIME)
  
  time_elapsed <- eventReactive(
    list(input$prog_button, dataInvalidate()),
    {
      start <- progress()$EVENT_START
      end   <- reactive({ DASHBOARD_DATETIME() })()

      mins <- floor(as.numeric(end - start, units = "mins"))
      sprintf("%d hours and %d minutes", mins %/% 60, mins %% 60)
      
      # difftime(reactive({ DASHBOARD_DATETIME() })(),
      #          progress()$EVENT_START,
      #          units = "hours") |> 
      #   seconds_to_period(as.numeric(round(elapsed, digits = 2), units = "secs")) -> elapsed
      
    }, 
    ignoreInit = FALSE)
  
  output$elapsed <- renderText(time_elapsed())
  
  
  # ------------------ Main dashboard bad sensor tables --------------------

  output$teros_bad_sensors_table <- DT::renderDataTable({
      dropbox_data()[["teros_bad_sensors"]] %>%
          datatable(options = list(searching = FALSE, pageLength = 5),
                    class = 'cell-border')
  })
  # 
  # output$troll_bad_sensors <- DT::renderDataTable({
  #     dropbox_data()[["aquatroll_bad_sensors"]] %>%
  #         datatable(options = list(searching = FALSE, pageLength = 5))
  # })
  
  # ------------------ Main dashboard bad sensor graphs  -----------------------
  
  output$bad_teros_plot <- renderPlot({
    
    if(length(input$teros_bad_sensors_table_rows_selected)) {
      
      ddt <- reactive({ DASHBOARD_DATETIME() })()
      
      dropbox_data()[["teros_bad_sensors"]] %>%
        slice(input$teros_bad_sensors_table_rows_selected) ->
        tsensor_selected
      
      dropbox_data()[["teros"]] %>%
        rename(Timestamp = TIMESTAMP) |> 
        semi_join(tsensor_selected, 
                  by = c("Logger", "Plot", "Sensor_ID", "Location", "variable", "depth_cm"))-> selected_data
      
      ggplot(selected_data, aes(Timestamp, value, group = interaction(Sensor_ID, variable, depth_cm))) +
        geom_line() +
        xlab("") -> b #+

              # xlim(c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) -> b
      # Try to assign color intelligently. If different plots are selected,
      # have that be the color; otherwise by depth; otherwise by ID
      if(length(unique(selected_data$Plot)) > 1) {
        b <- b + aes(color = Plot)
      } else if(length(unique(selected_data$variable)) > 1) {
        b <- b + aes(color = variable)
      }else if(length(unique(selected_data$depth_cm)) > 1)  {
        b <- b + aes(color = as.factor(depth_cm))
      } else {
        b <- b + aes(color = Sensor_ID)
      }
      
    } else {
      b <- NO_DATA_GRAPH
    }
    b
  })
  
  # ------------------ TEROS tab ---------------------------
  
  # Define a semi-transparent rectangle to indicate flood start/stop
  # We have to use a geom_rect to accommodate the faceted TEROS plot
  # Each plot passes the ymin and ymax (bc plotly won't do -Inf/Inf) to `...`
  shaded_flood_rect <- function(...)
    reactive({
      geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                
                aes(xmin = progress()$EVENT_START,
                    xmax = Inf, ...))
    })() # remove the reactive before returning
  
  output$teros_plot <- renderPlot({
    # Average TEROS data by plot and 15 minute interval,
    # one facet per sensor (temperature, moisture, conductivity)
    # This graph is shown when users click the "TEROS" tab on the dashboard
    
    ddt <- reactive({ DASHBOARD_DATETIME() })()
    
    dropbox_data()[["teros"]] ->
      teros
    
    if(nrow(teros)) {

      teros %>%
        # Certain versions of plotly seem to have a bug and produce
        # a tidyr::pivot error when there's a 'variable' column; rename
        rename(var = variable, Timestamp = TIMESTAMP) %>%
        #mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
        group_by(Plot, var, Logger, Timestamp) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        left_join(TEROS_RANGE, by = c("var" = "variable")) |> 
        ggplot() +
        facet_wrap(Logger ~ var, scales = "free", ncol = 3) +
        shaded_flood_rect(ymin = -Inf, ymax = Inf) +
        geom_line(aes(Timestamp, value, color = Plot)) +
        xlab("") +
        theme(text = element_text(size = 18))
      
    } else {
      b <- NO_DATA_GRAPH
    }
  })
  
  # ------------------ AquaTROLL tab ---------------------------

  output$aquatroll_plot <- renderPlotly({
    # AquaTroll data plot
    # This graph is shown when users click the "Aquatroll" tab on the dashboard
    
    ddt <- reactive({ DASHBOARD_DATETIME() })()
    bind_rows(dropbox_data()[["aquatroll_200_long"]],
              dropbox_data()[["aquatroll_600_long"]]) ->
      full_trolls_long
    
    if(nrow(full_trolls_long) > 1) {
      full_trolls_long %>%
        mutate(Timestamp_rounded = round_date(Timestamp, GRAPH_TIME_INTERVAL)) %>%
        group_by(Logger_ID, Well_Name, Timestamp_rounded, variable) %>%
        summarise(Well_Name = Well_Name,
                  value = mean(value, na.rm = TRUE), .groups = "drop") %>%
        left_join(AQUATROLL_RANGE, by = "variable") %>%
        # Certain versions of plotly seem to have a bug and produce
        # a tidyr::pivot error when there's a 'variable' column; rename
        rename(var = variable) -> t
      
      t %>%
        filter(var == "Pressure_psi") %>%
        ggplot(aes(Timestamp_rounded, value, color = Well_Name)) +
        coord_cartesian(xlim = c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) +
        shaded_flood_rect(ymin = low, ymax = high) +
        geom_line() +
        geom_hline(aes(yintercept = low), color = "grey", linetype = 2) +
        geom_hline(aes(yintercept = high), color = "grey", linetype = 2) +
        facet_wrap(~var, scales = "free", ncol = 2) +
        xlab("") -> t1
      
      t %>%
        filter(var == "Salinity") %>%
        ggplot(aes(Timestamp_rounded, value, color = Well_Name)) +
        coord_cartesian(xlim = c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) +
        shaded_flood_rect(ymin = low, ymax = high) +
        geom_line() +
        geom_hline(aes(yintercept = low), color = "grey", linetype = 2) +
        geom_hline(aes(yintercept = high), color = "grey", linetype = 2) +
        facet_wrap(~var, scales = "free", ncol = 2) +
        xlab("") -> t2
      
      t %>%
        filter(var == "Temp") %>%
        ggplot(aes(Timestamp_rounded, value, color = Well_Name)) +
        coord_cartesian(xlim = c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) +
        shaded_flood_rect(ymin = low, ymax = high) +
        geom_line() +
        geom_hline(aes(yintercept = low), color = "grey", linetype = 2) +
        geom_hline(aes(yintercept = high), color = "grey", linetype = 2) +
        facet_wrap(~var, scales = "free", ncol = 2) +
        xlab("") -> t3
      
      t %>%
        filter(var == "DO_mgl") %>%
        ggplot(aes(Timestamp_rounded, value, color = Well_Name)) +
        coord_cartesian(xlim = c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) +
        shaded_flood_rect(ymin = low, ymax = high) +
        geom_line() +
        geom_hline(aes(yintercept = low), color = "grey", linetype = 2) +
        geom_hline(aes(yintercept = high), color = "grey", linetype = 2) +
        facet_wrap(~var, scales = "free", ncol = 2) +
        xlab("") -> t4
      
    } else {
      b <- NO_DATA_GRAPH
    }
    
    subplot(ggplotly(t1, tooltip="text", dynamicTicks = TRUE), #%>% add_range(),
            (ggplotly(t2, tooltip="text", dynamicTicks = TRUE)), #%>% add_range()),
            (ggplotly(t3, tooltip="text", dynamicTicks = TRUE)), #%>% add_range()),
            (ggplotly(t4, tooltip="text", dynamicTicks = TRUE)), #%>% add_range()),
            nrows=4, shareX = TRUE, shareY = TRUE)
  })
  
  # ------------------ Dashboard badges -----------------------------
  
  output$teros_bdg <- renderValueBox({
    valueBox(dropbox_data()[["teros_bdg"]]$percent_in[1],
             "TEROS12",
             color = dropbox_data()[["teros_bdg"]]$color[1],
             icon = icon("temperature-high")
    )
  })
  
  output$aquatroll_bdg <- renderValueBox({
    valueBox(dropbox_data()[["aquatroll_bdg"]]$percent_in[1],
             "AquaTroll",
             color = dropbox_data()[["aquatroll_bdg"]]$color[1],
             icon = icon("water")
    )
  })
  
  # ------------------ Text alerts -----------------------------
  #
  #     observeEvent({
  #         # This will calculate values and send out messages to everyone in "new_user" df
  #         # could just have people not choose what they want alerts for?
  #         #initial_alert()
  #         alertInvalidate()
  #     }, {
  #         # send_alerts is defined in R/alerts_module.R
  #         send_alerts(dropbox_data)
  #     })
  #
}
