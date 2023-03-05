library("igraph") 
library("network") 
library("sna")
library("ggraph")
library("visNetwork")
library("threejs")
library("networkD3")
library("ndtv")

plot_network<-function(results,task_flow,tsk){
tsk$`Max Capacity`<-NULL
tsk$order<-1:nrow(tsk)
tsk<-merge(tsk,results$volume_by_task,by.x="Task",by.y="Task")
tsk<-tsk[order(tsk$order),]
tsk$label<-paste(tsk$Task,"\nVolume:",round(tsk$Volume,0))
net <- graph_from_data_frame(d=task_flow, vertices=tsk, directed=T)
l<-layout_components(net)
cols<-colorRampPalette(c("white", "tomato"))(max(V(net)$Volume))
plot(net, layout=l, edge.color="gray10",vertex.color=cols[sapply(V(net)$Volume, FUN=function(x) max(1,x))], 
     edge.curved=.13, vertex.label.color="gray1", vertex.label=V(net)$label,
     vertex.shape="rectangle",vertex.size=50,vertex.size2=20,edge.arrow.size=0.5,vertex.label.cex=max(11,max(nchar(V(net)$name)))/16*1,); plot1 <- recordPlot()
print("plot done")
return(plot1 )
}

