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
library(bslib)

ui <- dashboardPage(
  
  skin = if_else(TESTING, "red-light",
                 "blue-light"),
  dashboardHeader(
    title = "DELUGE Dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("compass")),
      menuItem("TEROS12", tabName = "teros12", icon = icon("temperature-high")),
      menuItem("TEROS21", tabName = "teros21", icon = icon("worm")),
      menuItem("Aquatroll", tabName = "aquatroll", icon = icon("water")),
      menuItem("LevelTROLL", tabName = "leveltroll", icon = icon("wifi"))
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
          valueBoxOutput("teros12_bdg", width = 3),
          valueBoxOutput("teros21_bdg", width = 3),
          valueBoxOutput("troll600_bdg", width = 3),
          valueBoxOutput("leveltroll_bdg", width = 3)
        ),
        fluidRow(
          # Gear UI is defined in R/gear_module.R
          column(1, 
                 gearUI("gear")),
          column(5,
                 shinydashboardPlus::box(
                   title = "Flood Stats",
                   width = 12,
                   background = "teal",
                   textOutput("elapsed")
                 )
          ),
          column(width = 6,
                 tabBox(width = 12,
                        tabPanel(
                          title = "TEROS12",
                          dataTableOutput("teros12_bad_sensors_table")
                        ),
                        tabPanel(
                          title = "TEROS21",
                          dataTableOutput("teros21_bad_sensors_table")
                        ),
                        tabPanel(
                          title = "AquaTroll",
                          dataTableOutput("troll600_bad_sensors_table")
                        ),
                        tabPanel(
                          title = "LevelTROLL",
                          dataTableOutput("leveltroll_bad_sensors_table")
                        )
                 )
          )
        ),
        fluidRow(
          tabBox(width = 12,
                 tabPanel(
                   title = "TEROS12",
                   plotOutput("bad_teros12_plot", height = "400px")
                 ),
                 tabPanel(
                   title = "TEROS21",
                   plotOutput("bad_teros21_plot", height = "400px")
                 ),
                 tabPanel(
                   title = "AquaTroll",
                   plotOutput("bad_troll600_plot", height = "400px")
                 ),
                 tabPanel(
                   title = "LevelTROLL",
                   plotOutput("bad_leveltroll_plot", height = "400px")
                   
                 )
          )
        )
      ),
      tabItem(
        tabName = "teros12",
        fluidRow(
          plotOutput("teros_plot", width = "100%", height = "700px")
        )
      ),
      tabItem(
        tabName = "teros21",
        fluidRow(
          plotOutput("teros21_plot", width = "100%", height = "700px")
        )
      ),
      tabItem(
        tabName = "aquatroll",
        fluidRow(
          plotOutput("aquatroll_plot", width = "100%", height = "700px")
        )
      ),
      tabItem(
        tabName = "leveltroll",
        fluidRow(
          plotOutput("leveltroll_plot", width = "100%", height = "700px")
        )
      )
      # Alerts tab UI is defined in R/alerts_module.R
      # alertsUI("alertsTab")
    )
  )
)
