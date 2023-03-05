library(shiny)
library(ROI.plugin.glpk)
library(ROI)
library(ompr)
library(ompr.roi)
library(dplyr)
library(tidyr)
library(DT)
library(shinyjs)
library(waiter)

fluidPage(
  titlePanel("Labor and Capacity Optimizer"
  ),
  p("This resource-task network optmization model determines the maximum capacity achievable for a network of tasks."),
  p("Future enhancements:"),
  tags$ol(
        tags$li("Either host the excel input template on a website to provide the link in this UI, or move all inputs to the shiny app UI."),
        tags$li("Add a multi-period dimension to the model."), 
        tags$li("Add a two-stage optimization option to maximize capacity, then minimize labor."), 
        tags$li("Add queuing features, allow entities (e.g. packages) to be carried over into another period subject to queue constraints."),
        tags$li("Add an entity attribute dimenion to the model (e.g. package size)"),
        tags$li("Add a multi-scenario run option"),
        tags$li("Add a \"guid me\" features to guide users through a series of questions, ultimately bringing them to the subset of inputs necessary for their use case (like TurboTax)"),
        tags$li("Add an button to push results to a database (demonstrate the ability to store plans)."),
        tags$li("Improve the network visualizer.")
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Instructions:"),
      tags$ol(
        tags$li("Populate the input file template which can be found here."), 
        tags$li("Upload the input file."), 
        tags$li("Upload the csv to this page."),
        tags$li("Click \"Run Optimization.\"")
      ),
      hr(),
      fileInput("file1", "Upload CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      hr(),
      actionButton("runOptimization", "Run Optimization")
    ),
      mainPanel( 
        tabsetPanel(
          tabPanel("Network Visualizer", plotOutput("plot", height = "800")),
          tabPanel("Summary",fluidRow(
            column(4, 
                   DT::dataTableOutput("volume_and_resource_totals")
                   
            ),
            column(4, 
                   tableOutput("resource_totals")
            ),
            column(4, 
                   p()
            )
          )),
          tabPanel("Resource Details",DT::dataTableOutput("resourceDetails")),
          tabPanel("Task Details",DT::dataTableOutput("taskDetails"))
        )
    )
  )
)
