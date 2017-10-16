library(data.table)
library(stringr)
library(plyr)
library(igraph)
library(geosphere)

group_station_fun <- function(x,tem_data = tem_station_data){
  tem_key <- x$station_1[1]
  relate_station <- unique(x$station_2)
  relate_lines   <- unique(tem_data[tem_data$key_str %in% relate_station]$line_no)
  tem_x1 <- data.frame(lines_no = relate_lines,station = tem_key)
  return(tem_x1)
}

# split lng and lat
lat_and_lng_split <- function(x1){
  bus_str <- unlist(str_split(x1$key_str,"&"))
  x1$name <- bus_str[seq(1,length(bus_str),3)]
  x1$lat  <- bus_str[seq(2,length(bus_str),3)]
  x1$lng  <- bus_str[seq(3,length(bus_str),3)]
  x1$lat  <- as.numeric(x1$lat)
  x1$lng  <- as.numeric(x1$lng)
  return(x1)
} 

# creat new undir network
new_undir_network <- function(data_df){
  colnames(data_df) <- c("col_from","col_to")
  labels <- unique(c(data_df$col_from,data_df$col_to))
  ids    <- 1:length(labels)
  names(ids) <- labels
  from  <- data_df$col_from
  to    <- data_df$col_to
  edges <- matrix(c(ids[from],ids[to]),ncol = 2)
  tem_g_undir <- graph.empty(directed = FALSE)
  tem_g_undir <- add.vertices(tem_g_undir,length(labels))
  V(tem_g_undir)$label  <- labels
  V(tem_g_undir)$size   <- 1
  tem_g_undir           <- add.edges(tem_g_undir,t(edges))
  E(tem_g_undir)$weight <- 1
  return(tem_g_undir)
}

# K type relation data
k_network_data_fun <- function(x1){
  colnames(x1) <- c("label_E","line_V")
  label_all <- unique(x1$label_E)
  data_list <- list()
  for(i in 1:(length(label_all)-1)){
    tem_label <- label_all[i]
    else_label <- label_all[(i+1):length(label_all)]
    tem_line <- unique(x1[x1$label_E == tem_label]$line_V)
    tem_data <- x1[x1$line_V %in% tem_line & x1$label_E %in% else_label]
    if(nrow(tem_data) > 0 ){
      tem_data  <- as.data.frame(table(tem_data$label_E))
      colnames(tem_data) <- c("label_2","lines")
      tem_data$label_1 <- tem_label
      data_list[[i]] <- tem_data
    }
  }
  data_lines <- as.data.table(ldply(data_list,rbind))
  data_lines$label_2 <- as.character(data_lines$label_2)
  return(data_lines)
}

# walktrap community
member_for_colour <- function(tem_network){
	member  <- walktrap.community(tem_network)
	V(tem_network)$member <- member$membership
	mem.col <- rainbow(length(unique(member$membership)),alpha=.3)
	V(tem_network)$color  <- mem.col[member$membership] 
	return(tem_network)
}

mark_group_list <- function(tem_network){
	member.num  <- length(table(V(tem_network)$member))
	member.list <- list()
	for(i in 1:member.num){
	member.list <- c(member.list,list(which(V(tem_network)$member == i)))
	}
	return(member.list)
}

# get some network infomation 
network_index_info <- function(tem_network){
	x1 <- degree(tem_network)
	x2 <- estimate_betweenness(tem_network,cutoff = 2)
	x3 <- eigen_centrality(tem_network)
	x4 <- closeness(tem_network)
	x5 <- estimate_betweenness(tem_network,cutoff = 4)

	data_info <- data.frame(line_no = V(tem_network)$label, 
							degree = x1,
							betweenness = x2,
							bet_4 = x5,
							centrality = x3$vector,
							closeness = x4,
							stringsAsFactors = FALSE)

	data_info <- as.data.table(data_info)
	return(data_info)
}