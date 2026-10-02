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
            menuItem("Dashboard", tabName = "dashboard", icon = icon("compass"))
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
                    valueBoxOutput("teros_bdg", width = 2),
                    valueBoxOutput("aquatroll_bdg", width = 2),
                ),
                fluidRow(
                    column(width = 6,
                           tabBox(width = 12,
                                  tabPanel(
                                      title = "TEROS",
                                      dataTableOutput("teros_bad_sensors")
                                  ),
                                  tabPanel(
                                      title = "AquaTroll",
                                      dataTableOutput("troll_bad_sensors")
                                  )
                           )

                    )
                ),
                fluidRow(
                    tabBox(width = 12,
                           tabPanel(
                               title = "TEROS",
                               plotlyOutput("teros_plot", height = "400px")
                           ),
                           tabPanel(
                               title = "AquaTroll",
                               plotlyOutput("aquatroll_plot", height = "400px")
                           )
                    )
                )

            )
            # Alerts tab UI is defined in R/alerts_module.R
            # alertsUI("alertsTab")
        )
    )
)
