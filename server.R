library(shiny)
library(ROI.plugin.glpk)
library(ROI)
library(ompr)
library(ompr.roi)
library(dplyr)
library(tidyr)
library(DT)
library(shinyjs)
library(httr)
library(jsonlite)
library(waiter)
library(igraph) 
library(network) 
library(sna)
library(ggraph)
library(visNetwork)
library(threejs)
library(networkD3)
library(ndtv)
library(ggplot2)
source("labor_and_capacity_optimizer.R")
source("network_visualizer.R")

waiting_screen <- tagList(
  spin_flower(),
  h4("Generating optimized roster...")
) 
options(scipen = 999)

function(input, output, session) {
  v<-reactiveValues(plot=NULL)
  results<-reactiveValues(resource_task_assignments=NULL, 
                          volume_by_task=NULL,
                          resource_totals=NULL,
                          total_volume_processed=NULL,
                          total_resources=NULL)
  observeEvent(input$runOptimization,{
    input_sheets<-get_input_sheets(input$file1$datapath)
    optimizer_results<-run_optimization(input_sheets)
    v$plot<-plot_network(optimizer_results,input_sheets$task_flow,input_sheets$tsk)
    print(optimizer_results)
    print(v$plot)
    results$resource_task_assignments<-optimizer_results$resource_task_assignments
    results$resource_task_assignments$Count<- prettyNum(round(results$resource_task_assignments$Count,0))
    results$volume_by_task<-optimizer_results$volume_by_task
    results$volume_by_task$Volume<- prettyNum(round(results$volume_by_task$Volume,0))
    results$resource_totals<-optimizer_results$resource_totals
    results$resource_totals$Count <- prettyNum(round(results$resource_totals$Count,0))
    results$total_volume_processed<-optimizer_results$total_volume_processed
    results$total_resources<-optimizer_results$total_resources
  })
  output$resourceDetails<-renderDataTable({
    if (is.null(results$resource_task_assignments)) return()
    datatable(results$resource_task_assignments,options = list(dom = 't'),rownames=NULL)
  })
  output$taskDetails<-renderDataTable({
    if (is.null(results$volume_by_task)) return()
    datatable(results$volume_by_task,options = list(dom = 't'),rownames=NULL)
  })
  output$resource_totals<-renderTable({
    if (is.null(results$resource_totals)) return()
    results$resource_totals
  })
  output$volume_and_resource_totals<-renderDataTable({
    if (is.null(results$resource_totals)) return()
    datatable(data.frame("Name"=c("Total Volume","Total Resources"),"Value"=c(results$total_volume_processed,results$total_resources)),options = list(dom = 't'),colnames=NULL,rownames=NULL)
  })
  output$plot<-renderPlot({
    if (is.null(v$plot)) return()
    v$plot
    })
}