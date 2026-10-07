# User interface code for the TEMPEST data dashboard
# June 2023

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(shinyWidgets)
library(shinybusy)
library(shinyalert)
library(gmailr)

ui <- dashboardPage(
  
  skin = if_else(TESTING, "red-light",
                 "midnight"),
  dashboardHeader(
    title = "DELUGE Dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("compass")),
      menuItem("TEROS12", tabName = "teros12", icon = icon("temperature-high"))
      #menuItem("Alerts", tabName = "alerts", icon = icon("comment-dots"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(".shiny-notification {position: fixed; top: 30% ;left: 50%; width: 300px")),
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          # Clearly display the time of the dashboard
          # If in testing mode, this will be set to the latest timestamp
          # of the offline data
          textOutput("DDT"),
          
          # Front page badges; their attributes are computed by the server
          valueBoxOutput("teros_bdg", width = 2)#,
          #valueBoxOutput("aquatroll_bdg", width = 2),
        ),
        fluidRow(
          # Gear UI is defined in R/gear_module.R
          column(1, gearUI("gear")),
          column(5,
                 progress_circle(value = 0, shiny_id = "circle",
                                 color = "#00B0CA", stroke_width = 15,
                                 trail_color = "#BBE7E6"),
                 tags$h3("Flood Progress", align = "center")
          ),
          column(width = 6,
                 tabBox(width = 12,
                        tabPanel(
                          title = "TEROS12",
                          dataTableOutput("teros_bad_sensors_table")
                        )#,
                        # tabPanel(
                        #   title = "AquaTroll",
                        #   dataTableOutput("troll_bad_sensors")
                        # )
                 )
          )
        ),
        fluidRow(
          tabBox(width = 12,
                 tabPanel(
                   title = "TEROS12",
                   plotOutput("bad_teros_plot", height = "400px")
                 )#,
                 # tabPanel(
                 #   title = "AquaTroll",
                 #   plotlyOutput("aquatroll_plot", height = "400px")
                 # )
          )
        )
      ),
      tabItem(
        tabName = "teros12",
        fluidRow(
          plotOutput("teros_plot", width = "100%", height = "700px")
        )
      )
      # Alerts tab UI is defined in R/alerts_module.R
      # alertsUI("alertsTab")
    )
  )
)
