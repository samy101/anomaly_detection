
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(fpc) # for pam, pamk
library(TSdist) # for distance measures

library(ggplot2)
library(reshape2)
library(lattice)
require(RColorBrewer)


data_path = "E:/ibm/kyab/hourly_distance_matrix/"
house_list = list.dirs(data_path, full.names = FALSE)
house_list = house_list[1:length(house_list)]

sensor_list = list()
for (house_name in house_list) {
  house_path = paste(data_path, house_name, sep="")   
  sensor_names = list.files(house_path, pattern="*.csv")  
  sensor_list[[house_name]] = sensor_names  
}

normalize_listdata <- function(listdata, minVal=0, maxVal=1) { 
  
  # http://stats.stackexchange.com/questions/70801/how-to-normalize-data-to-0-1-range  
  listdata_normalized = lapply(listdata, function(data) {  
    mi = min(data$value)
    ma = max(data$value)
    df = ma -mi
    
    #newvalue= (max'-min')/(max-min)*(value-min)+min'.
    #data$value = 2 / df * (data$value - mi) + -1   # between -1 to 1
    data$value = (maxVal-minVal) / df * (data$value - mi) + minVal   # between minVal to maxVal
    
    #normalized = (x-min(x))/(max(x)-min(x))
    #data$value = (data$value - mi) / df
    
    cat(paste(min(data$value), max(data$value), "\n"))  
    data  
  })  
  return (listdata_normalized)
}

aggregate_daily_mean <- function(listdata) {  
  listdata_daily = list(length(listdata))
  for (index in 1:length(listdata)) {    
    data = listdata[[index]]
    
    data_agr = aggregate(. ~ cut(data$timestamp, "1 day"), 
                         data[setdiff(names(data), "timestamp")], mean)  
    names(data_agr) = names(data)
    data_agr$timestamp = as.POSIXct(as.character(data_agr$timestamp), tz="GMT")
    
    listdata_daily[[index]] = data_agr
  }  
  return (listdata_daily)
}  

make_correlation_mat <- function(listdata) {  
  size = length(listdata)  
  co = matrix(0, nrow=size, ncol=size)  
  for( row in 1:size) {
    for( col in 1:size) {
      co[row,col] = cor(listdata[[row]]$value, listdata[[col]]$value)      
      #cat(paste(co[row,col], "\n"), file=stderr())          
    }
  }  
  return (co)
}

load_anomaly_weekend_weekday_all <- function(house_name) {
  
  cat(paste("load_anomaly_weekend_weekday_all ", house_name, "\n"), file=stderr())
  
  house_path = paste(data_path, house_name, sep="/")
  sensor_names = list.files(house_path, pattern="*.csv")  
  listdata = list(length(sensor_names))
  
  index = 1
  for (sensor_name in sensor_names) {    
    # load the weekday_data
    dir = paste(house_path, "weekday_day_data", sep="/")    
    path = paste(dir, paste("anomaly_", sensor_name, sep=""), sep="/")
    cat(paste("  Loading ", path, "\n"), file=stderr())
    d0 = read.csv(path, stringsAsFactors=FALSE)
    
    dir = paste(house_path, "weekday_night_data", sep="/")        
    path = paste(dir, paste("anomaly_", sensor_name, sep=""), sep="/")
    cat(paste("  Loading ", path, "\n"), file=stderr())
    d1 = read.csv(path, stringsAsFactors=FALSE)
    
    # load the weekend_data
    weekend_dir = paste(house_path, "weekend_data", sep="/")    
    weekend_path = paste(weekend_dir, paste("anomaly_", sensor_name, sep=""), sep="/")
    cat(paste("  Loading ", weekend_path, "\n"), file=stderr())
    d2 = read.csv(weekend_path, stringsAsFactors=FALSE)
    
    d3 = rbind(d0, d1, d2)
    d3 = d3[,-(2:5)]  
    d3$timestamp = as.POSIXct(strptime(d3$timestamp, "%Y-%m-%d %H:%M:%S"), tz="GMT")
    
    d4 = d3[order(d3$timestamp), ]
    names(d4) = c("timestamp", "value")
    
    listdata[[index]] = d4
    index = index + 1
  }  
  cat(paste("load_anomaly_weekend_weekday_all ", house_name, "\n\n"), file=stderr())
  
  return (listdata)
}

merge_alldata <- function(listdata, sensor_names) {
  
  namelist = list()  
  alldata = data.frame()  
  
  for (index in 1:length(listdata)) {    
    data = listdata[[index]]    
    name = sensor_names[[index]]    
    
    #m = mean(data$value)
    #s = sd(data$value)    
    #data$value = (data$value - m) / s        
    #data = data[1:50,]
    
    if(index == 1) {
      alldata = data.frame(timestamp=data$timestamp, name=data$value)
    } else {
      alldata = cbind(alldata, name=data$value)      
    }    
    namelist[[index+1]] = name     
  }
  
  namelist[[1]] = "timestamp"
  names(alldata) = namelist
  
  return (alldata)
}

make_alldata_plot <- function(listdata, sensor_names) {  
  
  alldata = merge_alldata(listdata, sensor_names)     
  df_melt = melt(alldata, id.vars = 'timestamp')
  
  plot = ggplot(df_melt, aes(x = timestamp, y = value)) + 
    geom_line() + 
    facet_wrap(~ variable, scales = 'free_y', ncol = 2)
  return (plot)
}  

make_alldata_heatmap <- function(data_all, ncols = 2) { 
  
  days = substr(data_all$timestamp, 1, 10)
  hours = substr(data_all$timestamp, 12, 13)
  
  data_all_new = data.frame(day = days, hour=hours, data_all[,-1])
  data_melt = melt(data_all_new)
  pg <- ggplot(data_melt, aes(day, hour, fill = value)) +
    geom_tile() +
    facet_wrap(~ variable, scales = 'free_y', ncol = ncols) +   
    scale_fill_gradientn("Population", colours = rev(brewer.pal(11, "Spectral")))
  #scale_fill_gradient(trans = "sqrt")
  #  facet_grid(~variable) + geom_tile() 
  print(pg)
}

loadAllSensorData <- function(house_name) { 
  
  cat(paste("loadAllData loading data for ", house_name, "\n"), file=stderr())
  
  #data_path = "C:/samy/kyab/hourly_distance_matrix"
  #house_name = "house_67"
  house_path = paste(data_path, house_name, sep="/")
  sensor_names = list.files(house_path, pattern="*.csv")  
  
  index = 1
  listdata = list(length(sensor_names))
  for (sensor_name in sensor_names) {  
    
    file_path = paste(house_path,sensor_name, sep="/")
    data <- read.csv(file_path, stringsAsFactors=FALSE)
    data$timestamp = as.POSIXct(strptime(data$timestamp, "%Y-%m-%d %H:%M:%S"), tz="GMT")
    data = data[!duplicated(data$timestamp), ] # remove any duplicates based on timestamp  
    
    cat(paste("  ", file_path, nrow(data), data[1,]$timestamp,  
              data[nrow(data),]$timestamp, "\n"), file=stderr())
    
    #data <- data[weekdays(data$timestamp) %in% c('Saturday','Sunday'),]  
    listdata[[index]] = data
    index = index + 1
  } 
  cat(paste("loadAllData loading data for ", house_name, "\n\n"), file=stderr())  
  return (listdata)
}

compute_adjusted_score <- function(anomally_all, cor_mat) { 
  
  anomally_all_day = anomally_all[,1]
  anomally_all = anomally_all[,-1] # remove the timestamp
  anomally.mat = as.matrix(anomally_all)
  
  days = dim(anomally.mat)[1]
  series = dim(anomally.mat)[2]
  
  # copy anomally.mat and set all the values to NA
  anomally.mat.adjusted = apply(anomally.mat, c(1, 2), function(x) NA)
  
  for (ss in 1:series) {  
    for (day in 1:days) {    
      col_data = anomally.mat[day, -ss]  # select all the series data, except the current one
      cor_data = cor_mat[ss, -ss]
      
      weights = cor_data * col_data
      weights_mean = mean(weights)
      
      self_data = anomally.mat[day, ss]
      weights_mean = weights_mean * 0.5 # 50% adjustment
      
      anomally.mat.adjusted[day, ss] = Mod(self_data - weights_mean)
    }
  }
  anomally.mat.adjusted.data = as.data.frame(anomally.mat.adjusted)
  anomally.mat.adjusted.data = cbind(timestamp=anomally_all_day, anomally.mat.adjusted.data)
  return (anomally.mat.adjusted.data)
}

merge_data_and_anomaly <- function(data_self, anomally_self, anomally_self_adjusted) {
  
  data_self = data_self[data_self$timestamp %in% anomally_self$timestamp, ]
  
  data_all = data_self
  data_all = cbind(data_all, anomally_self[,2])
  data_all = cbind(data_all, anomally_self_adjusted[,2])
  
  return (data_all)
}

compute_anomaly_score <- function(house_name, sensor_name, 
                                  listdata_all, listdata_anomally, cor_mat) {
    
  cat ( paste('compute_anomaly_score ', house_name, sensor_name, "\n"), file=stderr())    
  
  house_path = paste(data_path, house_name, sep="/")
  sensor_names = list.files(house_path, pattern="*.csv")  
  
  listdata_all_n = normalize_listdata(listdata_all)
  
  #data_all = merge_alldata(listdata_all, sensor_names)
  data_all_n = merge_alldata(listdata_all_n, sensor_names)  
  anomally_all = merge_alldata(listdata_anomally, sensor_names)
  anomally_all_adjusted =  compute_adjusted_score(anomally_all, cor_mat)
  
  names = c("timestamp", sensor_name)
  
  data_self = data_all_n[names]
  anomally_self = anomally_all[names]
  anomally_self_adjusted = anomally_all_adjusted[names]

  cat ( paste('  compute_anomaly_score ', names(data_self), "\n"), file=stderr())    
  cat ( paste('  compute_anomaly_score ', names(anomally_self), "\n"), file=stderr())    
  cat ( paste('  compute_anomaly_score ', names(anomally_self_adjusted), "\n"), file=stderr())    
  
  names(data_self) = names
  names(anomally_self) = names
  names(anomally_self_adjusted) = names

  cat ( paste('  compute_anomaly_score ', names(data_self), "\n"), file=stderr())    
  cat ( paste('  compute_anomaly_score ', names(anomally_self), "\n"), file=stderr())    
  cat ( paste('  compute_anomaly_score ', names(anomally_self_adjusted), "\n"), file=stderr())    
  
  cat ( paste('  compute_anomaly_score ', nrow(data_self), nrow(anomally_self), nrow(anomally_self_adjusted), "\n"), file=stderr())    
  
  data_merged = merge_data_and_anomaly(data_self, anomally_self, anomally_self_adjusted)
  
  make_alldata_heatmap(data_merged, ncols=1)  
}  

shinyServer(function(input, output, session) {

  observe({
    house = input$house    
    sensors = sensor_list[[house]]    
    cat ( paste('updating house ', house, length(sensors), "\n"), file=stderr())    
    updateSelectInput(session, "sensor", choices = sensors, selected = sensors[[1]])
    
    #compute_anomaly_score_for_a_building(house)
  })  
    
  loadAllData <- reactive({    
    list_alldata = loadAllSensorData(input$house)
    return (list_alldata)
  })

  loadAllAnomalyData <- reactive({   
    cat ( paste('loadAllAnomaData ', input$house, "\n"), file=stderr()) 
    listdata_anomally = load_anomaly_weekend_weekday_all(input$house)
    return (listdata_anomally)
  })
  
  getCorrelationMatix <- reactive({  
    
    cat ( paste('getCorrelationMatix ', input$house, "\n"), file=stderr()) 
    house_path = paste(data_path, input$house, sep="/")
    sensor_names = list.files(house_path, pattern="*.csv")  
    
    list_alldata = loadAllData()    
    #list_alldata = normalize_listdata(list_alldata)
    
    listdata_daily = aggregate_daily_mean(list_alldata)
    co = make_correlation_mat(listdata_daily)
    
    return (co)
  })
  
  getHeight <- reactive({    
    return (800)
  })
  
  output$anomalyPlot <- renderPlot({
    
    house = input$house 
    sensor = input$sensor
    
    if(nchar(sensor) == 0) {
      return (NA)
    } 
    
    cat ( paste('output$anomalyPlot ', house, sensor, "\n"), file=stderr())    
    
    house_path = paste(data_path, house, sep="/")
    sensor_names = list.files(house_path, pattern="*.csv")  
    
    if(!sensor %in% sensor_names) {
      cat(paste("output$anomalyPlot  Invalid statte ", house_path, sensor, "\n"), file=stderr())  
      return (NA)
    }
    
    listdata_all = loadAllData()
    listdata_anomally = loadAllAnomalyData()
    cor_mat = getCorrelationMatix()
    
    cat ( paste('output$anomalyPlot ', house, sensor, " start \n"), file=stderr())    
    compute_anomaly_score(house, sensor, listdata_all, listdata_anomally, cor_mat)
  })
  
  output$allDataPlot <- renderPlot({
    
    house = input$house 
    cat ( paste('output$allDataPlot ', house, input$plotallsensor, "\n"), file=stderr())    
    
    if(input$plotallsensor == F) {
      return (NA)
    }
    
    house_path = paste(data_path, house, sep="/")
    sensor_names = list.files(house_path, pattern="*.csv")  
    
    list_alldata = loadAllData()    
    list_alldata_n = normalize_listdata(list_alldata)    
    data_all = merge_alldata(list_alldata_n,sensor_names)
    
    make_alldata_heatmap(data_all)    
    #plot = make_alldata_plot(list_alldata, sensor_names)
    #print(plot)
  })

  output$correlationPlot <- renderPlot({
    
    house = input$house 
    cat ( paste('output$correlationPlot ', house, input$plotcorrelation, "\n"), file=stderr())    
    
    if(input$plotcorrelation == F) {
      return (NA)
    }
    
    co = getCorrelationMatix()
    
    rgb.palette <- colorRampPalette(c("black","red","white"), space = "Lab")
    plot = levelplot(as.matrix(co),
                     col.regions=rgb.palette(120),
                     xlab = "Loads",
                     ylab = "Loads")
    print(plot)
  })
  
})
