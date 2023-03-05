library(ROI.plugin.glpk)
library(ROI)
library(ompr)
library(ompr.roi)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)


get_input_sheets<-function(file_name){
###read input sheets###
tsk<- read_excel(file_name, sheet = "Tasks")
task_flow<- read_excel(file_name, sheet = "Task Flow")
rsc<- read_excel(file_name, sheet = "Resources")
task_resource<- read_excel(file_name, sheet = "Task-Resource")
params<- read_excel(file_name, sheet = "Parameters", col_names=FALSE)
input_sheet_data<-list("tsk"=tsk,"task_flow"=task_flow,"rsc"=rsc,"task_resource"=task_resource,"params"=params)
return(input_sheet_data)
}

run_optimization<-function(input_sheet_data){
  
tsk<-input_sheet_data$tsk
task_flow<-input_sheet_data$task_flow
rsc<-input_sheet_data$rsc
task_resource<-input_sheet_data$task_resource
params<-input_sheet_data$params

###sets###
resources<-rsc$Resource
tasks<-tsk$Task

###parameters###
rates<-xtabs(`Resource Rate`~Resource+Task,task_resource)
task_soft_min_rsc<-xtabs(`Resource Soft Min`~Resource+Task,task_resource)
task_hard_min_rsc<-xtabs(`Resource Hard Min`~Resource+Task,task_resource)
task_rsc_max<-xtabs(`Resource Max`~Resource+Task,task_resource)
rsc_min<-xtabs(Min~Resource,rsc)
rsc_max<-xtabs(Max~Resource,rsc)
task_max_capacity<-xtabs(`Max Capacity`~Task,tsk)
min_total_rsc<-as.numeric(params[params[,1]=='Min Total Resources',][2])
max_total_rsc<-as.numeric(params[params[,1]=='Max Total Resources',][2])
min_total_volume<-as.numeric(params[params[,1]=='Min Total Volume',][2])
max_total_volume<-as.numeric(params[params[,1]=='Max Total Volume',][2])
  
#create lower bound (lb) and upper bound (ub) tables for flow split percentages
#1 = 100% of the volume from upstream task A can pass to downstream task B
task_flow_lb<-task_flow
task_flow_lb[is.na(task_flow_lb$`Split Percentage`)&task_flow_lb$Task!='Source',]$`Split Percentage`<-1
task_flow_lb[is.na(task_flow_lb)]<-0
task_flow_lb<-xtabs(`Split Percentage`~Task+`Downstream Task`,task_flow_lb)
task_flow_ub<-task_flow
task_flow_ub[is.na(task_flow_ub)]<-1
task_flow_ub<-xtabs(`Split Percentage`~Task+`Downstream Task`,task_flow_ub)

###control parameters###
objective_type<-as.character( params[params[,1]=='Objective',][2])
big_M<-99999999

###model###

model <- MIPModel() %>%
  add_variable(volume[t], t = tasks, type = "continuous", lb=0) %>%
  add_variable(resources_assigned[r,t], r = resources, t = tasks, type = "integer", lb=0) %>%
  set_objective(if(params[params[,1]=="Objective",][2]=="Min Resource"){sum_expr(resources_assigned[r,t],t=tasks,r=resources)}else{volume['Source']}, sense=if(params[params[,1]=="Objective",][2]=="Min Resource"){"min"}else{"max"})%>%
  #volume flow must be within split percentage bounds
  add_constraint(volume[t2] >= sum_expr(volume[t1]*task_flow_lb[t1,t2], t1 = tasks,t1!=t2, t1!='Sink'),t2=tasks,  t2!='Source')%>%
  add_constraint(volume[t2] <= sum_expr(volume[t1]*task_flow_ub[t1,t2], t1 = tasks,t1!=t2, t1!='Sink'),t2=tasks,  t2!='Source')%>%
  add_constraint(volume[t1] >= sum_expr(volume[t2]-sum_expr(volume[t3]*task_flow_ub[t3,t2],t3=tasks, t3!=t2, t3!='Sink',t3!=t1),t2=tasks,t2!='Source',t1!=t2,task_flow_ub[t1,t2]>0), t1 = tasks, t1!='Sink')%>%
  add_constraint(volume[t1] <= sum_expr(volume[t2]/task_flow_ub[t1,t2],t2=tasks,t2!='Source',t1!=t2,task_flow_ub[t1,t2]>0), t1 = tasks, t1!='Sink')%>%
  #stay within task capacity
  add_constraint(volume[t] <= task_max_capacity[t], t = tasks)%>%
  #resource rate must be at least the total volume
  add_constraint(volume[t] <= rates[r,t]*resources_assigned[r,t], t = tasks, r = resources, t %in% colnames(rates), r %in% rownames(rates),rates[r,t]>0 )%>%
  #stay within resource min/max values
  add_constraint(sum_expr(resources_assigned[r,t], t=tasks)>=rsc_min[r],  r = resources )%>%
  add_constraint(sum_expr(resources_assigned[r,t], t=tasks)<=rsc_max[r],  r = resources )%>%
  #soft min resource constraint -- to add later 
  
  #stay within total resource min/max
  add_constraint(sum_expr(resources_assigned[r,t], t=tasks, r = resources)>=min_total_rsc )%>%
  add_constraint(sum_expr(resources_assigned[r,t], t=tasks, r = resources)<=max_total_rsc )%>%
  #stay within total volume
  add_constraint(volume['Sink'] >= min_total_volume)%>%
  add_constraint(volume['Sink'] <= max_total_volume)%>%
  add_constraint(volume['Sink']<=10000)%>% 
  solve_model(with_ROI("glpk", verbose = TRUE))

volume_by_task<-get_solution(model, volume[t]) 
volume_by_task<-volume_by_task[,c('t','value')]
colnames(volume_by_task)<-c('Task','Volume')

resources_by_task<-get_solution(model, resources_assigned[r,t])  %>% 
  dplyr::filter(value >0)
resources_by_task<-resources_by_task[,c('r','t','value')]
colnames(resources_by_task)<-c('Resource','Task','Count')

resource_totals<-aggregate(resources_by_task$Count, list(resources_by_task$Resource), FUN=sum)
colnames(resource_totals)<-c('Resource','Count')
total_volume_processed<-volume_by_task[volume_by_task$Task=='Sink',2]
total_resources<-sum(resource_totals$Count)
results<-list("resource_task_assignments"=resources_by_task, "volume_by_task"=volume_by_task, "resource_totals"=resource_totals,
     "total_volume_processed" =total_volume_processed, "total_resources" =  total_resources)
return(results)
}