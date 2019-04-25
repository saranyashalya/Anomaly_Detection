library(ggplot2)

correlationTable = function(graphs) {
  cross = matrix(nrow = length(graphs), ncol = length(graphs))
  for(graph1Id in 1:length(graphs)){
    graph1 = graphs[[graph1Id]]
    print(graph1Id)
    for(graph2Id in 1:length(graphs)) {
      graph2 = graphs[[graph2Id]]
      if(graph1Id == graph2Id){
        break;
      } else {
        correlation = ccf(graph1, graph2, lag.max = 0)
        cross[graph1Id, graph2Id] = correlation$acf[1]
      }
    }
  }
  cross
}

graphs = read.csv("graphs45.csv")
corr = correlationTable(graphs)
View(corr)


## finding highly correlated graphs with graph 4

findCorrelated = function(orig, highCorr){
  match = highCorr[highCorr[,1] == orig | highCorr[,2] == orig,]
  match = as.vector(match)
  match[match != orig]
}

highCorr = which(corr > 0.90 , arr.ind = TRUE)
match = findCorrelated(4, highCorr)
match # print 6 12 23 42 44 45  3

matched_graphs <- graphs[,match]

matched_graphs$Id <- seq(1:nrow(matched_graphs))
library(tidyr)
matched_graphs <- matched_graphs %>% gather(GraphName, Value, 1:7)
graph4 <- data.frame("Id"=seq(1:nrow(graphs)), GraphName="Graph_04","Value"= graphs[[4]])
matched_graphs <- rbind(matched_graphs, graph4)
ggplot(matched_graphs, aes(x=Id, y=Value, color=GraphName))+geom_line(size=1)


## plotting
# bound = function(graphs, orign, match) {
#   graphOrign = graphs[[orign]]
#   graphMatch = graphs[match]
#   allValue = c(graphOrign)
#   for(m in graphMatch){
#     allValue = c(allValue, m)
#   }
#   c(min(allValue), max(allValue))
# }
# 
# plotSimilar = function(graphs, orign, match){
#   lim = bound(graphs, orign, match)
#   
#   graphOrign = graphs[[orign]]
#   plot(ts(graphOrign), ylim=lim, xlim=c(1,length(graphOrign)+25), lwd=3)
#   title(paste("Similar to", orign, "(black bold)"))
#   
#   cols = c()
#   names = c()
#   for(i in 1:length(match)) {
#     m = match[[i]]
#     matchGraph = graphs[[m]]
#     lines(x = 1:length(matchGraph), y=matchGraph, col=i)
#     
#     cols = c(cols, i)
#     names = c(names, paste0(m))
#   }
#   legend("topright", names, col = cols, lty=c(1,1))
# }
# 
# plotSimilar(graphs, 4, match)
# 


##subtracting correlated signals


## subtract the timeseries
multi = sum(graphs$Graph_04/graphs$Graph_06)/length(graphs$Graph_06)
multi #display 1.736529
align_return_df = data.frame("Graph_06"=graphs$Graph_06)
align_return_df$value = align_return_df$Graph_06*multi
graphs$Id <- seq(1:nrow(graphs))
align_return_df$Id <- seq(1:nrow(align_return_df))
combined <- cbind(graphs, align_return_df$value)
ggplot(combined, aes(x=Id,y=Graph_04))+geom_line(color="red") +
  geom_line(aes(x=Id,y=align_return_df$value),color="blue")+
  geom_point(data = combined[,c(4,46,47)][combined$Id==78 ,],  color="red",size=5)

ggplot(combined, aes(x=Id,y=Graph_04))+geom_line(color="red") +
  geom_line(aes(x=Id,y=Graph_06),color="blue")+
  geom_point(data = combined[,c(4,46,6)][combined$Id==78 ,],  color="red",size=5)


# plot_df(graphs[,c("Id","Graph_04")], align_return_df[,c("Id","value")])
# legend("topleft", c("new visitor","returning visitor x1.736"), 
#        col=c("#00c7b0","#ff0816"), lty=c(1,1))
# 

##finding outliers in correlated time series

substract = data.frame("Graph_06"=graphs$Graph_06)
substract$value = graphs$Graph_04 - graphs$Graph_06*multi
substract$Id <- seq(1:nrow(substract))
ggplot(substract, aes(x=Id,y=value))+geom_line()
#plot_df(substract)

hist(substract$value, breaks = 50)


#compute 3 sigma minimal & maximal
min = mean(substract$value, na.rm = T) - 3*sd(substract$value, na.rm = T)
max = mean(substract$value, na.rm = T) + 3*sd(substract$value, na.rm = T)

# plot metric & limit
# plot_df(substract, NULL, c(-120,320))
# abline(h=max, col="#e15f3f", lwd=2)
# abline(h=min, col="#e15f3f", lwd=2)

# plot circle
position = data.frame(id=seq(1, length(substract$value)), value=substract$value)
anomalyH = position[position$value > max, ]
anomalyH = anomalyH[!is.na(anomalyH$value), ]
anomalyL = position[position$value < min, ]
anomalyL = anomalyL[!is.na(anomalyL$value), ]
anomaly = data.frame(id=c(anomalyH$id, anomalyL$id), value=c(anomalyH$value, anomalyL$value))
anomaly = anomaly[!is.na(anomaly$value), ]
points(x = anomaly$date, y=anomaly$value, col="#e15f3f")


ggplot(data = substract, aes(x=Id, y=value)) + geom_line(col="purple") +
  geom_abline(intercept = max, slope=0) +
  geom_abline(intercept = min, slope=0) + geom_point(data = anomaly, aes(x=anomaly$id, y= anomaly$value), color="red")



