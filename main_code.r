# 交通站点的重要性计算
# 数据源是来自于百度地图上爬取得到的交通站点及线路的数据
# 经过测试后发现，直接以原始数据去构造网络，各种指标结果只能突出重要的交通节点，一些市中心的站点排名都会比较靠后
# 后测试了把站点一定范围能可换乘的线路都算作通过了该站点
# 结果是设定站点之间可在800米范围内换乘的中间中心度这个指标会比较理想
# 相对等于绝对中间中心度除以（n^2-3n+2)/2,其中（n^2-3n+2)/2实际等于以n个节点组成的网络里中间中心度的最大可能数值
source("function_.r")
library(data.table)
library(stringr)
library(plyr)
library(igraph)
library(ggplot2)
library(geosphere)
library(ggmap)
library(mapproj)
# Args <- commandArgs()
 # station_type <-  Args[6]
 # area_str     <-  Args[7]  
 # main_region_station_str <-  Args[8]
 # city_str     <-  Args[9]


station_type <- 800
area_str <- "All_City"
main_region_station_str <- "静安"
city_str <- "ShangHai"
out_file <- "output_data/"
map  <- get_map(location = city_str, zoom = 12, maptype = 'roadmap') # 在墙内需要翻墙，不然取不到地图
map1 <- get_map(location = city_str, zoom = 11, maptype = 'roadmap')
# 读取数据
station_data <- fread("input_data/SH_Traffic.csv",header = TRUE,stringsAsFactors = FALSE) 
colnames(station_data)[1:4] <- c("name","lat","lng","combine_lines")
station_data <- station_data[,list(name,lat,lng,combine_lines)]
station_data <- unique(station_data)
tem_station_data <- station_data[,.(line_no = unlist(str_split(combine_lines,";"))),by=list(name,lat,lng)]
tem_station_data$key_str <- paste(tem_station_data$name,tem_station_data$lat,tem_station_data$lng,sep="&")
station_key_str <- sort(unique(tem_station_data$key_str))

## 对于某个公交站点A，计算以A为中心的正方形
## 边长为两公里的范围内所有的公交站的与站点A的距离
tem_data  <- unique(tem_station_data[,list(lng,lat,key_str)])
data_list <- list()

for(j in 1:length(station_key_str)){
  x1 <- unique(tem_data[tem_data$key_str == station_key_str[j]])
  min_lat <- x1$lat - 0.011
  max_lat <- x1$lat + 0.011
  min_lng <- x1$lng - 0.011
  max_lng <- x1$lng + 0.011
  x2 <- tem_data[lat > min_lat & lat < max_lat & lng > min_lng & lng < max_lng]
  tem_dist <- distVincentyEllipsoid(x1[,list(lng,lat)],x2[,list(lng,lat)])
  tem_df   <- data.frame(station_2 = x2$key_str,dist_two = tem_dist,station_1 = station_key_str[j],stringsAsFactors = FALSE)
  data_list[[j]] <- tem_df
}

station_dist <- as.data.table(ldply(data_list,rbind))
print("station_dist is ok")

tem_station_dist <- station_dist[dist_two < station_type]
x1 <- split(tem_station_dist,tem_station_dist$station_1)
data_list <- lapply(x1,group_station_fun)

group_station_data <- as.data.table(ldply(data_list,rbind))
group_station_data$.id <- NULL
group_station_data$station  <- as.character(group_station_data$station)
group_station_data$lines_no <- as.character(group_station_data$lines_no)
print("group_station_data is ok")

colnames(group_station_data) <- c("line_no","key_str") 
tem_bus_lines <- group_station_data
data_lines    <- k_network_data_fun(tem_bus_lines)
colnames(data_lines) <- c("line_2","lines","line_1")
lines_network <- new_undir_network(data_lines[,list(line_1,line_2)])
print("lines_network is ok")

all_lines_info <- network_index_info(lines_network)
colnames(tem_bus_lines) <- c("line_no","key_str")
tem_bus_data <- merge(tem_bus_lines,all_lines_info,by="line_no",all.x = TRUE)
tem_bus_data[is.na(tem_bus_data)] <- 0
station_in <- tem_bus_data[,.(all_degree= sum(degree),
                              all_bet   = sum(betweenness),
                              all_bet_4   = sum(bet_4),
                              all_cen   = sum(centrality),
                              all_clo   = sum(closeness)),by=list(key_str)]
station_in <- lat_and_lng_split(station_in)
write.csv(station_in[,list(key_str,name,lng,lat,all_bet)],file=paste0(out_file,"all_bet_800.csv"),row.names = FALSE,quote = FALSE)


lines_network <- member_for_colour(lines_network)
member.list   <- mark_group_list(lines_network)
name_str <- paste(out_file,area_str,"_lines_network_staion_K_",station_type,"_walktrap_community.png",sep="")
png(name_str,width = 3000,height = 3000)
plot(lines_network,vertex.color = V(lines_network)$color,
     mark.groups  = member.list,
     vertex.size = 0.5,
     vertex.label = V(lines_network)$label,
     vertex.label.cex  = 1,
     edge.color = grey(0.5),
     main=paste(area_str,"_lines_network_staion_K_",station_type,"_walktrap_community",sep=""))
dev.off()


x1 <- station_in[order(-all_bet),list(key_str,all_bet)][1:100]
title_str <- paste(area_str,"_",station_type,"_","1step_top100",sep="")
ggmap(tem_map)+labs(x='Longitude',y='Latitude') +
  geom_point(aes(lng,lat,colour=all_bet,size=all_bet),data=x1)
ggsave(paste(title_str,".png",sep=""),width=200,height = 200,unit="mm", dpi=100)

x1 <- station_in[order(-all_bet_4),list(key_str,all_bet_4)][1:100]
x1 <- lat_and_lng_split(x1)
title_str <- paste(area_str,"_",station_type,"_","3step_top100",sep="")
ggmap(tem_map)+labs(x='Longitude',y='Latitude') +
  geom_point(aes(lng,lat,colour=all_bet_4,size=all_bet_4),data=x1)
ggsave(paste(title_str,".png",sep=""),width=200,height = 200,unit="mm", dpi=100)

print("station_network_betweenness is ok")
