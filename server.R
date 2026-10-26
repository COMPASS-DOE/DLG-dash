# User interface code for the DELUGE data dashboard
# Stephanie Pennington October 2026

source("global.R")

server <- function(input, output, session) {
  
  dataInvalidate  <- reactiveTimer(15 * 60 * 1000) # 15 minutes
  
  # If pulling from GitHub, we keep track of the latest preprocessor commit
  # and only download when that changes
  last_data_download <- reactiveValues(ldc = "--", ldt = "--")
  
  output$latest_data_download <- 
    renderText(
      paste0("Latest data update: ", 
             format(last_data_download$ldt, format = "%Y-%m-%d %H:%M:%S %Z"),
             " (commit ", last_data_download$ldc, ")") 
    )
 
  # ------------------ Data invalidation --------------------------
  
  check_data  <- reactive({
    
    dataInvalidate()
    
    # Check if the Git commit of the preprocessor has changed
    try(
      commit <- system("git ls-remote https://github.com/COMPASS-DOE/sensor-data-preprocessor.git | head -n 1 | cut -c 1-7",
                       intern = TRUE)
    )
    if(is.character(commit)) {
      invalidate <- commit != last_data_download$ldc
      last_data_download$ldc <- commit
    } else {
      warning("Couldn't contact GitHub")
      invalidate <- FALSE
    }
    
    return(invalidate)
  })
  
  # ------------------ Check whether testing --------------------------
  
  # Note 1. TESTING is defined in the global environment (so "<<-")
  # Note 2. These checks have to be here as shiny.testmode isn't set until server runs
  # Check if we're running in a Shiny testing...
  TESTING <<- TESTING || isTRUE(getOption("shiny.testmode"))
  # ...or continuous integration environment
  TESTING <<- TESTING || Sys.getenv("CI") == "true"
  
  # ------------------ Read in sensor data -----------------------------
  
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
      
      withProgress({
        setProgress(1, message = "Downloading TEROS12")
        compasstools::recent_sensor_data("DLG", "TEROS12") |> 
          compute_teros12(ddt) -> 
          teros12_list
        
        setProgress(2, message = "Downloading TEROS21")
        compasstools::recent_sensor_data("DLG", "TEROS21") |> 
          compute_teros21(ddt) -> 
          teros21_list
        
        setProgress(3, message = "Downloading AQUATROLL600")
        
        compasstools::recent_sensor_data("CRC", "AQUATROLL600") |> 
          filter(Plot == "UP") |> 
          mutate(Site = "DLG", Plot = "F4") -> crc_trolls
        
        compasstools::recent_sensor_data("DLG", "AQUATROLL600") |> 
          compute_aquatroll(crc_trolls, ddt) -> 
          aquatroll_list
        
        setProgress(4, message = "Downloading LEVELTROLL")
        compasstools::recent_sensor_data("DLG", "LEVELTROLL") |> 
          compute_leveltroll(ddt) -> 
          leveltroll_list
      }, min = 0, max = 5)

      
      last_data_download$ldt <- Sys.time()
      
    } else {
      stop("DATA_SOURCE ", DATA_SOURCE, " not supported")
    }

    # Do limits testing and compute data needed for badges
    # compute_sapflow() etc. are defined in R/data_processing.R
    c(teros12_list, teros21_list, aquatroll_list, leveltroll_list)
  })
  
  # ------------------ Gear and progress circle --------------------------
  
  # gearServer is defined in R/gear_module.R
  # We pass it DASHBOARD_DATETIME (a reactive) so it can update
  # its date input field if the datetime changes
  progress <- gearServer("gear", session, DASHBOARD_DATETIME)
  
  time_elapsed <- eventReactive(
    list(input$prog_button, dropbox_data()),
    {
      # start <- progress()$EVENT_START
      start <- START
      end <- reactive({ DASHBOARD_DATETIME() })()
      
      total_mins <- floor(as.numeric(end - start, units = "mins"))
      
      hours <- total_mins %/% 60
      mins <- total_mins %% 60

      paste("DELUGE has been flooding for", hours, "hours and", mins, "minutes!")
      
    }, 
    ignoreInit = FALSE)
  
  current_level <- eventReactive(
    dropbox_data(),
    {
      
      dropbox_data()[["leveltroll"]] |>
        filter(Plot == "F1") |>
        ungroup() |>
        arrange(TIMESTAMP) |> 
        summarise(Timestamp = last(TIMESTAMP), value = round(last(value), digits = 2)) |> 
        mutate(Timestamp = format(Timestamp, "%b %d %H:%M:%S")) -> last_value

      paste("The F1 plot water level is at", last_value$value, "cm on", last_value$Timestamp, "EST")
    }, 
    ignoreInit = FALSE)
  
  output$elapsed <- renderText(time_elapsed())
  output$current_level <- renderText(current_level())
  
  # ------------------ Main dashboard bad sensor tables --------------------
  
  output$teros12_bad_sensors_table <- DT::renderDataTable({
    dropbox_data()[["teros12_bad_sensors"]] %>%
      datatable(extensions = "Buttons",
                options = list(scrollX = TRUE, searching = FALSE, pageLength = 5, dom = "Bt", buttons = list(
                    extend = "copy",
                    text = 'Copy')
      ))
  })
  
  output$teros21_bad_sensors_table <- DT::renderDataTable({
    dropbox_data()[["teros21_bad_sensors"]] %>%
      datatable(extensions = "Buttons",
                options = list(scrollX = TRUE, searching = FALSE, pageLength = 5, dom = "Bt", buttons = list(
                  extend = "copy",
                  text = 'Copy')
                ))
  })
  
  output$troll600_bad_sensors_table <- DT::renderDataTable({
    dropbox_data()[["troll600_bad_sensors"]] %>%
      datatable(extensions = "Buttons",
                options = list(scrollX = TRUE, searching = FALSE, pageLength = 5, dom = "Bt", buttons = list(
                  extend = "copy",
                  text = 'Copy')
                ))
  })
  
  output$leveltroll_bad_sensors_table <- DT::renderDataTable({
    dropbox_data()[["leveltroll_bad_sensors"]] %>%
      datatable(extensions = "Buttons",
                options = list(scrollX = TRUE, searching = FALSE, pageLength = 5, dom = "Bt", buttons = list(
                  extend = "copy",
                  text = 'Copy')
                ))
  })
  
  # ------------------ Main dashboard bad sensor graphs  -----------------------
  
  output$bad_teros12_plot <- renderPlot({
    
    if(length(input$teros12_bad_sensors_table_rows_selected)) {
      
      ddt <- reactive({ DASHBOARD_DATETIME() })()
      
      dropbox_data()[["teros12_bad_sensors"]] %>%
        slice(input$teros12_bad_sensors_table_rows_selected) ->
        tsensor_selected
      
      dropbox_data()[["teros12"]] %>%
        rename(Timestamp = TIMESTAMP) |> 
        semi_join(tsensor_selected, 
                  by = c("Logger", "Plot", "Sensor_ID", "Location", "variable", "depth_cm"))-> selected_data

      p <- try({
        ggplot(selected_data, aes(Timestamp, value, group = interaction(Sensor_ID, variable, depth_cm))) +
          geom_line() +
          xlab("") -> b
        
        # xlim(c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) -> b
        # Try to assign color intelligently. If different plots are selected,
        # have that be the color; otherwise by depth; otherwise by ID
        if(length(unique(selected_data$Plot)) > 1) {
          b <- b + aes(color = Plot)
        } else if(length(unique(selected_data$variable)) > 1) {
          b <- b + aes(color = variable)
        } else if(length(unique(selected_data$depth_cm)) > 1)  {
          b <- b + aes(color = as.factor(depth_cm))
        } else {
          b <- b + aes(color = Sensor_ID)
        }
      })
      if(!is_ggplot(p)) p <- ERROR_GRAPH
    } else {
      p <- NO_DATA_GRAPH
    }
    return(p)
  })
  
  output$bad_teros21_plot <- renderPlot({
    
    if(length(input$teros21_bad_sensors_table_rows_selected)) {
      
      ddt <- reactive({ DASHBOARD_DATETIME() })()
      
      dropbox_data()[["teros21_bad_sensors"]] %>%
        slice(input$teros21_bad_sensors_table_rows_selected) ->
        tsensor_selected
      
      dropbox_data()[["teros21"]] %>%
        rename(Timestamp = TIMESTAMP) |> 
        semi_join(tsensor_selected, 
                  by = c("Logger", "Plot", "Sensor_ID", "Location", "variable", "depth_cm")) -> selected_data

      p <- try({
        ggplot(selected_data, aes(Timestamp, value, group = interaction(Sensor_ID, variable, depth_cm))) +
          geom_line() +
          xlab("") -> b
        
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
      })
      if(!is_ggplot(p)) p <- ERROR_GRAPH
    } else {
      p <- NO_DATA_GRAPH
    }
    return(p)
  })
  
  output$bad_troll600_plot <- renderPlot({
    
    if(length(input$troll600_bad_sensors_table_rows_selected)) {
      
      ddt <- reactive({ DASHBOARD_DATETIME() })()
      
      dropbox_data()[["troll600_bad_sensors"]] %>%
        slice(input$troll600_bad_sensors_table_rows_selected) ->
        tsensor_selected
      
      dropbox_data()[["troll600"]] %>%
        rename(Timestamp = TIMESTAMP) |> 
        semi_join(tsensor_selected, 
                  by = c("Logger", "Plot", "variable", "Instrument"))-> selected_data

      p <- try({
        
        ggplot(selected_data, aes(Timestamp, value, group = variable)) +
          geom_line() +
          xlab("") -> b
        
        # xlim(c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) -> b
        # Try to assign color intelligently. If different plots are selected,
        # have that be the color; otherwise by depth; otherwise by ID
        if(length(unique(selected_data$Plot)) > 1) {
          b <- b + aes(color = Plot)
        } else {
          b <- b + aes(color = variable)
        }
      })
      if(!is_ggplot(p)) p <- ERROR_GRAPH
    } else {
      p <- NO_DATA_GRAPH
    }
    return(p)
  })
  
  output$bad_leveltroll_plot <- renderPlot({
    
    if(length(input$leveltroll_bad_sensors_table_rows_selected)) {

      ddt <- reactive({ DASHBOARD_DATETIME() })()
      
      dropbox_data()[["leveltroll_bad_sensors"]] %>%
        slice(input$leveltroll_bad_sensors_table_rows_selected) ->
        tsensor_selected
      
      dropbox_data()[["leveltroll"]] %>%
        rename(Timestamp = TIMESTAMP) |> 
        semi_join(tsensor_selected, 
                  by = c("Logger", "Plot"))-> selected_data

      p <- try({
        ggplot(selected_data, aes(Timestamp, value, color = variable)) +
          geom_line() +
          xlab("") -> b
        
        # xlim(c(ddt - GRAPH_TIME_WINDOW * 60 * 60, ddt)) -> b
        # Try to assign color intelligently. If different plots are selected,
        # have that be the color; otherwise by depth; otherwise by ID
        if(length(unique(selected_data$Plot)) > 1) {
          b <- b + aes(color = Plot)
        } else {
          b <- b + aes(color = variable)
        }
      })
      if(!is_ggplot(p)) p <- ERROR_GRAPH
    } else {
      p <- NO_DATA_GRAPH
    }
    return(p)
  })
  
  # ------------------ TEROS12 tab ---------------------------
  
  output$teros12_plot <- renderPlot({
    # Average TEROS data by plot and 15 minute interval,
    # one facet per sensor (temperature, moisture, conductivity)
    # This graph is shown when users click the "TEROS12" tab on the dashboard
    
    ddt <- reactive({ DASHBOARD_DATETIME() })()
    
    dropbox_data()[["teros12"]] |> 
      group_by(Plot, variable, depth_cm, TIMESTAMP) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop") -> teros12

    if(nrow(teros12) > 0) {
      p <- try({
        teros12 %>%
          ggplot() +
          facet_wrap(variable ~ depth_cm, scales = "free_y", ncol = 2) +
          geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                    xmin = START, xmax = Inf,
                    ymin = -Inf, ymax = Inf) +
          geom_line(aes(TIMESTAMP, value, color = Plot), na.rm = TRUE) +
          xlab("") +
          theme(text = element_text(size = 16),
                axis.text.x = element_text(angle = 45, hjust = 1),
                strip.background = element_rect(color = "grey90", linewidth = 2)
                )
      })
      if(!is_ggplot(p)) p <- ERROR_GRAPH
    } else {
      p <- NO_DATA_GRAPH
    }
    return(p)
  })
  
  # ------------------ TEROS21 tab ---------------------------
  
  output$teros21_plot <- renderPlot({
    # Average TEROS data by plot and 15 minute interval,
    # one facet per sensor (temperature, moisture, conductivity)
    # This graph is shown when users click the "TEROS21" tab on the dashboard
    
    ddt <- reactive({ DASHBOARD_DATETIME() })()
    
    dropbox_data()[["teros21"]] ->
      teros21

    if(nrow(teros21) > 0) {
      p <- try({
        teros21 %>%
          group_by(Plot, variable, depth_cm, TIMESTAMP) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          ggplot() +
          facet_wrap(variable ~ depth_cm, scales = "free_y", ncol = 2) +
          geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                    xmin = START, xmax = Inf,
                    ymin = -Inf, ymax = Inf) +
          geom_line(aes(TIMESTAMP, value, color = Plot), na.rm = TRUE) +
          xlab("") +
          theme(text = element_text(size = 18),
                axis.text.x = element_text(angle = 45, hjust = 1),
                strip.background = element_rect(color = "grey90", linewidth = 2)
                )
      })
      if(!is_ggplot(p)) p <- ERROR_GRAPH
    } else {
      p <- NO_DATA_GRAPH
    }
    return(p)
  })
  
  # ------------------ AquaTROLL tab ---------------------------
  
  output$aquatroll_plot <- renderPlot({
    # This graph is shown when users click the "AquaTROLL" tab on the dashboard
    
    ddt <- reactive({ DASHBOARD_DATETIME() })()
    
    dropbox_data()[["troll600"]] ->
      troll600
    
    if(nrow(troll600) > 0) {
      p <- try({
        ggplot(troll600) +
          facet_wrap(~variable, scales = "free_y", ncol = 1) +
          geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                    xmin = START, xmax = Inf,
                    ymin = -Inf, ymax = Inf) +
          geom_line(aes(TIMESTAMP, value, color = Plot), na.rm = TRUE) +
          xlab("") +
          theme(text = element_text(size = 18),
                axis.text.x = element_text(angle = 45, hjust = 1),
                strip.background = element_rect(color = "grey90", linewidth = 2)
                )
      })
      if(!is_ggplot(p)) p <- ERROR_GRAPH
    } else {
      p <- NO_DATA_GRAPH
    }
    return(p)
  })
  
  # ------------------ LevelTROLL tab ---------------------------
  
  output$leveltroll_plot <- renderPlot({
    # This graph is shown when users click the "LevelTROLL" tab on the dashboard
    
    ddt <- reactive({ DASHBOARD_DATETIME() })()
    
    dropbox_data()[["leveltroll"]] ->
      leveltroll
browser()
    if(nrow(leveltroll) > 0) {
      p <- try({
        ggplot(leveltroll) +
          facet_wrap(Logger ~ variable, scales = "free_y", ncol = 1) +
          geom_rect(group = 1, color = NA, fill = "#BBE7E6", alpha = 0.7,
                    xmin = START, xmax = Inf,
                    ymin = -Inf, ymax = Inf) +
          geom_line(aes(TIMESTAMP, value, color = Plot), na.rm = TRUE) +
          xlab("") + ylab("Water depth (based on 151 cm sensor height") +
          theme(text = element_text(size = 18),
                axis.text.x = element_text(angle = 45, hjust = 1),
                strip.background = element_rect(color = "grey90", linewidth = 2)
                )
      })
      if(!is_ggplot(p)) p <- ERROR_GRAPH
    } else {
      p <- NO_DATA_GRAPH
    }
    return(p)
  })
  
  # ------------------ Dashboard badges -----------------------------
  
  output$teros12_bdg <- renderValueBox({
    valueBox(dropbox_data()[["teros12_bdg"]]$percent_in[1],
             "TEROS12",
             color = dropbox_data()[["teros12_bdg"]]$color[1],
             icon = icon("temperature-high")
    )
  })
  
  output$teros21_bdg <- renderValueBox({
    valueBox(dropbox_data()[["teros21_bdg"]]$percent_in[1],
             "TEROS21",
             color = dropbox_data()[["teros21_bdg"]]$color[1],
             icon = icon("worm")
    )
  })
  
  output$troll600_bdg <- renderValueBox({
    valueBox(dropbox_data()[["troll600_bdg"]]$percent_in[1],
             "AquaTroll",
             color = dropbox_data()[["troll600_bdg"]]$color[1],
             icon = icon("water")
    )
  })
  
  output$leveltroll_bdg <- renderValueBox({
    valueBox(dropbox_data()[["leveltroll_bdg"]]$percent_in[1],
             "LevelTROLL",
             color = dropbox_data()[["leveltroll_bdg"]]$color[1],
             icon = icon("wifi")
    )
  })
}
