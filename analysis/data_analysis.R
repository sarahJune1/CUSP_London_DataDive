#
## DATA ANALYSIS
#

#CHECK, INSTALL AND LOAD REQUIRED PACKAGES
pkgs <- c("sp","ggplot2","lubridate","rgdal","plyr","dplyr","data.table","proj4","geojsonR","downloader","gganimate","GPfit","forecast",
          "SpatialTools","spdep","rgeos","raster","gridExtra","stringr","rgdal","devtools","spatialEco","transformr","stringi")
for (pkg in pkgs) {
  if(pkg %in% rownames(installed.packages()) == FALSE) {install.packages(pkg)
    lapply(pkgs, require, character.only = TRUE)}
  else {
    lapply(pkgs, require, character.only = TRUE)}
}
rm(pkg,pkgs)
#SET WORKING DIRECTORY TO FILEPATH OF SCRIPT (PREFERRED DIRECTORY WHEN CLONING THE REPOSITORY)
#THIS WILL ONLY WORK WHEN USING R STUDIO, ELSE SET WD MANUALLY
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#DATA PREPARATION
data = fread("data.csv",header=TRUE) #Read response data
dsn = system.file("grid.shp",package="rgdal")
grid = readOGR(dsn=dsn,layer="grid") #Read grid
grid = st_as_sf(grid) #Convert to SF

#CREATE SF OBJECT FROM POINTS
data_sf = data %>% 
  as.data.frame %>% 
  sf::st_as_sf(coords = c("lon_incident","lat_incident"),crs=st_crs(grid))

#ONLY JOIN POINTS WITH GRID (BY GRID CELL)
data_sf = st_join(data_sf, grid, join = st_within)
data_sf$incidentcount = 1
data = as.data.table(data_sf)

#CREATE TRAIN AND TEST SAMPLES FOR INCIDENT TIME SERIES

# NOTE: THIS CODE SHOULD BE EXPANDED TO INCLUDE OTHER TIME-SERIES ON GRID CELL LEVEL:
# AMBULANCE COUNTS, TRAVEL SPEEDS + ANY OTHER NEEDED

full_data= data[,list( #Full data by hour (for plotting)
  count=sum(incidentcount)
),by=list(id,hour)]
train_data= data[data$day <= 20][,list( #Train data by hour-of-month
  count=sum(incidentcount)
),by=list(id,hour,day)]
test_data= data[data$day > 20][,list( #Test data by hour-of-month
  count=sum(incidentcount)
),by=list(id,hour,day)]

#CREATE TARGET TABLE FOR GRID CELL CONTENT (TO AVOID SKIPPING DAYS / HOURS)
temp = as.data.table(rep(unique(day(data$callstart)),each=24*length(grid$id)))
temp$hour = rep(seq(0,23),times=length(unique(day(data$callstart)))*length(grid$id))
temp$id = rep(rep(grid$id,times=31),each=24)
colnames(temp) = c("day","hour","id")
#MERGE WITH GRID
temp2 = as.data.table(rep(grid$id,each=24))
temp2$hour = rep(seq(0,23),times=length(grid$id))
colnames(temp2) = c("id","hour")
full_data = merge(full_data,temp2,by=c("id","hour"),all.y=TRUE)
full_data$count[is.na(full_data$count)] = 0
full_grid = merge(grid,full_data,by="id",all.y=TRUE)

temp2 = temp[temp$day<=20]
train_data = merge(train_data,temp2,by=c("id","hour","day"),all.y=TRUE)
train_data$count[is.na(train_data$count)] = 0
train_grid = merge(grid,train_data,by="id",all.y=TRUE)

temp2 = temp[temp$day>20]
test_data = merge(test_data,temp2,by=c("id","hour","day"),all.y=TRUE)
test_data$count[is.na(test_data$count)] = 0
test_grid = merge(grid,test_data,by="id",all.y=TRUE)

#PLOT ANIMATED GRID CELLS BY HOUR-OF-DAY

traffic_data = fread("traffic_data.csv",header=TRUE) #Load traffic data per hour-of-day
ambulance_data = fread("ambulance_data.csv",header=TRUE) #Load ambulance counts per hour-of-day
full_grid = merge(full_grid,traffic_data,by=c("id","hour"),all.x=TRUE)
full_grid = merge(full_grid,ambulance_data,by=c("id","hour"),all.x=TRUE)
full_grid$Incidents = full_grid$count
#Animated plot: Incidents
p_inc = ggplot() +
  geom_sf(data=full_grid,aes(fill=Incidents)) +
  labs(title = "Hour of Day: {frame_time}") +
  scale_fill_viridis_c(limits=c(0,130),oob=scales::squish) +
  transition_time(hour) +
  theme_bw()
p_inc
#Animated plot: travel speed
p_tra = ggplot() +
  geom_sf(data=full_grid,aes(fill=TravelSpeed)) +
  labs(title = "Hour of Day: {frame_time}") +
  scale_fill_viridis_c(limits=c(1,50),oob=scales::squish,direction=-1) +
  transition_time(hour) +
  theme_bw()
p_tra
#Animated plot: Ambulances
p_amb = ggplot() +
  geom_sf(data=full_grid,aes(fill=Ambulances)) +
  labs(title = "Hour of Day: {frame_time}") +
  scale_fill_viridis_c(limits=c(0,50),oob=scales::squish,direction=-1) +
  transition_time(hour) +
  theme_bw()
p_amb

#TIME SERIES FORECASTING
#CREATE EMPTY DATAFRAMES TO STORE RESULTS
res_t_exp = as.data.frame(matrix(ncol=2,nrow=0))
colnames(res_t_exp) = c("RMSE","MAE")
res_exp = as.data.frame(matrix(ncol=2,nrow=0))
colnames(res_exp) = c("RMSE","MAE")
res_nn = as.data.frame(matrix(ncol=2,nrow=0))
colnames(res_nn) = c("RMSE","MAE")
#LOOP OVER THE TIME SERIES IN EACH GRID CELL
for (i in unique(test_grid$id)) { #Every unique grid ID
  
  test_data = as.data.table(test_grid) #Define test data
  test_data = test_data[test_data$id==i]
  test_data = as.data.table(test_data)
  test_data = test_data[order(id,day,hour)]
  
  train_data = as.data.table(train_grid) #Define train data
  train_data = train_data[train_data$id==i]
  train_data = as.data.table(train_data)
  train_data = train_data[order(id,day,hour)]
  
  ts_train = ts(train_data$count, #Extract time series
                start=c(train_data$day[1],train_data$hour[1]),
                end=c(train_data$day[length(train_data$day)],train_data$hour[length(train_data$day)]),
                frequency=24)
  ts_test = ts(test_data$count, #Extract time series
               start=c(test_data$day[1],test_data$hour[1]),
               end=c(test_data$day[length(test_data$day)],test_data$hour[length(test_data$day)]),
               frequency=24)
  
  #TIME SERIES FORECASTING MODELS:
  # triple exponential - models level, trend, and seasonal components
  fit_t_exp = HoltWinters(ts_train)
  # automatic exponential 
  fit_exp = ets(ts_train)
  # neural network
  fit_nn = nnetar(ts_train,decay=0.5,maxit=300,scale.inputs=TRUE,lambda=0.5)
  
  #Save predicted and true values
  y_pred_t_exp = as.data.frame(forecast(fit_t_exp,nrow(test_data)))$`Point Forecast`
  y_pred_exp = as.data.frame(forecast(fit_exp,nrow(test_data)))$`Point Forecast`
  y_pred_nn = as.data.frame(forecast(fit_nn,nrow(test_data)))$`Point Forecast`
  y_test = as.vector(ts_test)
  #Save performance metrics
  acc_t_exp = as.data.frame(forecast::accuracy(y_pred_t_exp,y_test))
  acc_exp = as.data.frame(forecast::accuracy(y_pred_exp,y_test))
  acc_nn = as.data.frame(forecast::accuracy(y_pred_nn,y_test))
  #Bind metrics to storage dataframes
  res_t_exp = rbind(res_t_exp,acc_t_exp[,c("RMSE","MAE")])
  res_exp = rbind(res_exp,acc_exp[,c("RMSE","MAE")])
  res_nn = rbind(res_nn,acc_nn[,c("RMSE","MAE")])
  #Print running variable (to keep track)
  print(i)
}
#COMPARE ERRORS
mse = c(mean(res_t_exp$RMSE),mean(res_exp$RMSE),mean(res_nn$RMSE))
mae = c(mean(res_t_exp$MAE),mean(res_exp$MAE),mean(res_nn$MAE))


#EXAMPLE PLOTS: TIME SERIES FORECASTING FOR DATA-RICH GRID CELL
test_data = as.data.table(test_grid)
test_data = test_data[test_data$id=="185"]
test_data = as.data.table(test_data)
test_data = test_data[order(id,day,hour)]
train_data = as.data.table(train_grid)
train_data = train_data[train_data$id=="185"]
train_data = as.data.table(train_data)
train_data = train_data[order(id,day,hour)]
ts_train = ts(train_data$count,
              start=c(train_data$day[1],train_data$hour[1]),
              end=c(train_data$day[length(train_data$day)],train_data$hour[length(train_data$day)]),
              frequency=24)
ts_test = ts(test_data$count,
             start=c(test_data$day[1],test_data$hour[1]),
             end=c(test_data$day[length(test_data$day)],test_data$hour[length(test_data$day)]),
             frequency=24)
fit_t_exp = HoltWinters(ts_train)
fit_exp = ets(ts_train)
fit_nn = nnetar(ts_train,decay=0.5,maxit=300,scale.inputs=TRUE,lambda=0.5)
y_pred_t_exp = as.data.frame(forecast(fit_t_exp,nrow(test_data)))$`Point Forecast`
y_pred_exp = as.data.frame(forecast(fit_exp,nrow(test_data)))$`Point Forecast`
y_pred_nn = as.data.frame(forecast(fit_nn,nrow(test_data)))$`Point Forecast`
y_test = as.vector(ts_test)
p1 = ggplot() +
  geom_line(aes(x=1:length(as.vector(ts_train)),y=as.vector(ts_train),color="Train Data")) +
  geom_line(aes(x=481:744,y=as.vector(ts_test),color="Test Data"),linetype="dashed") +
  geom_line(aes(x=481:744,y=y_pred_t_exp,color="Pred: Tri.Exp.")) +
  geom_line(aes(x=481:744,y=y_pred_exp,color="Pred: Exp.")) +
  geom_line(aes(x=481:744,y=y_pred_nn,color="Pred: NN")) +
  scale_color_manual("", 
                     breaks = c("Train Data","Test Data","Pred: Tri.Exp.","Pred: Exp.", "Pred: NN"),
                     values = c("blue","darkgreen","orange","red","black")) +
  xlab("Timesteps") +
  ylab("Incidents") +
  theme_bw()
p2 = ggplot() +
  geom_line(aes(x=1:length(as.vector(ts_train)),y=as.vector(ts_train),color="Train Data")) +
  geom_line(aes(x=481:744,y=as.vector(ts_test),color="Test Data"),linetype="dashed") +
  geom_line(aes(x=481:744,y=y_pred_t_exp,color="Pred: Tri.Exp.")) +
  geom_line(aes(x=481:744,y=y_pred_exp,color="Pred: Exp.")) +
  geom_line(aes(x=481:744,y=y_pred_nn,color="Pred: NN")) +
  scale_color_manual("", 
                     breaks = c("Train Data","Test Data","Pred: Tri.Exp.","Pred: Exp.", "Pred: NN"),
                     values = c("blue","darkgreen","orange","red","black")) +
  xlab("Timesteps") +
  ylab("Incidents") +
  xlim(450,600) +
  theme_bw()
grob1 = arrangeGrob(p1,p2,ncol=2)
ggsave("grob1.png",p2,width=7,height=3)

#EXAMPLE PLOTS: TIME-SERIES FORECASTING FOR DATA-SPARSE AREA
test_data = as.data.table(test_grid)
test_data = test_data[test_data$id=="99"]
test_data = as.data.table(test_data)
test_data = test_data[order(id,day,hour)]
train_data = as.data.table(train_grid)
train_data = train_data[train_data$id=="99"]
train_data = as.data.table(train_data)
train_data = train_data[order(id,day,hour)]
ts_train = ts(train_data$count,
              start=c(train_data$day[1],train_data$hour[1]),
              end=c(train_data$day[length(train_data$day)],train_data$hour[length(train_data$day)]),
              frequency=24)
ts_test = ts(test_data$count,
             start=c(test_data$day[1],test_data$hour[1]),
             end=c(test_data$day[length(test_data$day)],test_data$hour[length(test_data$day)]),
             frequency=24)
fit_t_exp = HoltWinters(ts_train)
fit_exp = ets(ts_train)
fit_nn = nnetar(ts_train,decay=0.5,maxit=300,scale.inputs=TRUE,lambda=0.5)
y_pred_t_exp = as.data.frame(forecast(fit_t_exp,nrow(test_data)))$`Point Forecast`
y_pred_exp = as.data.frame(forecast(fit_exp,nrow(test_data)))$`Point Forecast`
y_pred_nn = as.data.frame(forecast(fit_nn,nrow(test_data)))$`Point Forecast`
y_test = as.vector(ts_test)
p3= ggplot() +
  geom_line(aes(x=1:length(as.vector(ts_train)),y=as.vector(ts_train),color="Train Data")) +
  geom_line(aes(x=481:744,y=as.vector(ts_test),color="Test Data"),linetype="dashed") +
  geom_line(aes(x=481:744,y=y_pred_t_exp,color="Pred: Tri.Exp.")) +
  geom_line(aes(x=481:744,y=y_pred_exp,color="Pred: Exp.")) +
  geom_line(aes(x=481:744,y=y_pred_nn,color="Pred: NN")) +
  scale_color_manual("", 
                     breaks = c("Train Data","Test Data","Pred: Tri.Exp.","Pred: Exp.", "Pred: NN"),
                     values = c("blue","darkgreen","orange","red","black")) +
  xlab("Timesteps") +
  ylab("Incidents") +
  theme_bw()
p4= ggplot() +
  geom_line(aes(x=1:length(as.vector(ts_train)),y=as.vector(ts_train),color="Train Data")) +
  geom_line(aes(x=481:744,y=as.vector(ts_test),color="Test Data"),linetype="dashed") +
  geom_line(aes(x=481:744,y=y_pred_t_exp,color="Pred: Tri.Exp.")) +
  geom_line(aes(x=481:744,y=y_pred_exp,color="Pred: Exp.")) +
  geom_line(aes(x=481:744,y=y_pred_nn,color="Pred: NN")) +
  scale_color_manual("", 
                     breaks = c("Train Data","Test Data","Pred: Tri.Exp.","Pred: Exp.", "Pred: NN"),
                     values = c("blue","darkgreen","orange","red","black")) +
  xlab("Timesteps") +
  ylab("Incidents") +
  xlim(450,600) +
  theme_bw()
grob2 = arrangeGrob(p3,p4,ncol=2)
ggsave("grob2.png",p4,width=7,height=3)