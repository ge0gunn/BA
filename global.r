library(dplyr)
library(proj4)
library(leaflet)
library(RcppCNPy)
library(data.table)
library(Rcpp)
library(Matrix)
library(rgdal)
library(rhandsontable)
library(rgeos)



remove_spaces<-function(mytext){
  return(tolower(gsub(" ","_",mytext)))
  
}


mybuffer<-function(lines,width){
  lines=spTransform(lines,CRS=CRS("+proj=utm +zone=14 +ellps=GRS80 +units=m +no_defs"))
  LS=c()
  for (i in 1:length(lines)){
    coords=lines@lines[[i]]@Lines[[1]]@coords
    L=Line(coords)
    LS=c(LS,L)
  }
  LS=Lines(LS,ID="a")
  SL=SpatialLines(list(LS))
    
  
  wkt=writeWKT(SL)
  lines=readWKT(wkt,p4s=CRS("+proj=utm +zone=14 +ellps=GRS80 +units=m +no_defs"))
  buffer<-gBuffer(lines,width=width)
  buffer<-spTransform(buffer,CRS=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  return(buffer)
}


publish_map<-function(data,rows,columns,data_type,map_name){

  print(paste("mapname",map_name))
  print(sum(rows))
  print(columns)

#   #use sum across columns (variables) if more than one column is selected
  field_ids=data[rows,"field_id"]
  crop_numbers=data[rows,"crop_number"]
  if (length(columns)>1){
    out=apply(data[rows,columns],1,sum)
  }else{
    out=data[rows,columns]
  }

#   #Use average over years if more than one year is selected
  num_years=length(unique(data[rows,"year"]))
  if (num_years>1){
    i=field_ids
    j=rep(1,length(i))
    out=as.matrix(sparseMatrix(i=i,j=j,x=out/num_years))
    field_ids=1:length(out)
  }



  if (data_type=="crop"){
    colormap=crop_colormap(out)
  }
  if (data_type=="numeric"){
    print("YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY")
    print(typeof(out))
    print(length(out))
    print(head(out))
    colormap=percentile_colormap(out)
  }
  if (!(data_type %in% c("crop","numeric"))){
    colormap=random_colormap(out)
  }

  if (num_years>1){
    plot_data=colormap$data
  }else{
    plot_data=as.matrix(sparseMatrix(i=field_ids,j=crop_numbers,x=colormap$data))
  }
  progress$inc(.18)


  data_filename<<-"/mnt/ramdisk/plot_data.npy"
  print("going to write")
  npySave(data_filename,as.matrix(plot_data))
  print("donewriting")
  colormap_filename<<-"/mnt/ramdisk/colormap.csv"
  write.csv(colormap$colormap,colormap_filename,row.names=FALSE)



  if (num_years==1){
    year_string=data[rows,"year"][1]
  }else{
    year_string=""
  }

  command=  paste("/home/diamond/anaconda2/bin/python ./python/publish_map.py",map_name,year_string," & ")#1>>/dev/null 2>>/dev/null")
  print(command)

  system(command)

  prog=read.csv("/mnt/ramdisk/progress.txt",stringsAsFactors=FALSE)
  print("start checking")
  while (!grepl("complete",prog[1,1])){
    Sys.sleep(.5)
    print("checking")
    prog=read.csv("/mnt/ramdisk/progress.txt",stringsAsFactors=FALSE)
    progress$inc(.14)

  }
  prog[1,1]="not_started"
  write.csv(prog,file="/mnt/ramdisk/progress.txt",row.names=FALSE)
  progress$set(1)


 }











run_optimization<-function(){
  print("running optimization")
  command1="/home/diamond/anaconda2/bin/python python/transport/get_transport_costs.py"
  command2="/home/diamond/anaconda2/bin/python python/optimization/setup_optimization.py"
  command3="/home/diamond/anaconda2/bin/python python/optimization/heuristic1.py"
  command4="/home/diamond/anaconda2/bin/python python/optimization/get_soln_instance.py"
  print(command1)
  system(command1)
  print(command2)
  system(command2)
  print(command3)
  system(command3)
  print(command4)
  system(command4)
  demand_cost<<-read.csv("data/optimization/demand_cost_curve.csv")
  print(demand_cost)
  load_optimization_results()
}


run_simulation<-function(){
  command1="/home/diamond/anaconda2/bin/python python/run_simulations.py"
  print(command1)
  system(command1)
  
  data<<-load_simulation_results()
}





crop_colormap<-function(data){
  crop_parameters=read.csv("temp_data/crops/crop_parameters.csv")
  df=data.frame(crop="empty",crop_number=0,red=0,green=0,blue=0)
  df=rbind(df,crop_parameters[,c("crop","crop_number","red","green","blue")])
  df=data.frame(value=df$crop_number,label=df$crop,red=df$red,green=df$green,blue=df$blue)
  return(list(data=data,colormap=df))
}

random_colormap=function(x){
  
  #convert data so there is at most 200 different values by cycling. Too many values causes problems
  #between leaflet and geoserver somehow and gives borders around mapped region. 
  data_vals=unique(x[x>0])
  
  df=data.frame(vals=data_vals,newvals=1+( (1:length(data_vals)) %% 200),row.names=as.character(data_vals))
  x[x>0]=df[as.character(x[x>0]),"newvals"]
  data_vals=unique(x[x>0])
 
  
  num_colors=length(data_vals)
  r=sample(255,num_colors,replace=TRUE)
  g=sample(255,num_colors,replace=TRUE)
  b=sample(255,num_colors,replace=TRUE)
  df=data.frame(value=data_vals,red=r,green=g,blue=b)
  df$label=data_vals
  df=rbind(c(0,0,0,0,0),df)
  return(list(data=x,colormap=df))
  
}


discrete_colormap=function(x){
  
  #convert data so there is at most 200 different values by cycling. Too many values causes problems
  #between leaflet and geoserver somehow and gives borders around mapped region. 
  data_vals=unique(x[x>0])
  
  num_colors=length(data_vals)
  r=sample(255,num_colors,replace=TRUE)
  g=sample(255,num_colors,replace=TRUE)
  b=sample(255,num_colors,replace=TRUE)
  df=data.frame(value=data_vals,red=r,green=g,blue=b)
  df$label=data_vals
  df=rbind(c(0,0,0,0,0),df)
  return(list(data=x,colormap=df))
  
}
weighted_percentiles<-function(x,weight){
  print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  print(typeof(x))
  print(head(x))
  weight=weight/sum(weight)
  xmin=min(x)
  xmax=max(x)
  x=round(4294967296*(x-xmin)/(xmax-xmin))
  df=data.frame(x=x,w=weight,index=1:length(x))
  df=df[order(df$x),]
  df$cum_weight=cumsum(df$w)
  
  
  breaks=c(which(diff(df$x)>0),length(x))
  pbreaks=df$cum_weight[breaks]
  xbreaks=df$x[breaks]
  df1=data.frame(p=pbreaks,row.names=as.character(xbreaks))
  df$p=df1[as.character(df$x),"p"]
  df=df[order(df$index),]
  percentiles=round(100*df$p)
  return(percentiles)
}

percentile_colormap<-function(data,weight=NULL){
  r=c(0, 63, 127, 191, 255, 255, 255, 255, 255, 255)
  g=c(255, 255, 255, 255, 255, 255, 191, 127, 63, 0)
  b=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  #ramp_colors=rgb(r,g,b,maxColorValue=255)
  #mindata=min(data)
  #maxdata=max(data)
  #colors=ramp_colors
  if (is.null(weight)){
    weight=rep(1,length(data))/length(data)
  }
  percentiles=weighted_percentiles(data,weight)
  newdata=sapply(percentiles,function(x) min(10,1+floor(x/10)))
  values_10=spline(percentiles,data,xout=seq(10,100,10))$y
  lower_bound=c(min(data),values_10[1:9])
  upper_bound=values_10
  df=cbind(data.frame(value=1:10,lower_bound=lower_bound,upper_bound=upper_bound),red=r,green=g,blue=b)
  df=rbind(c(0,0,0,0,0,0),df)
  df$label=apply(round(df[,c("lower_bound","upper_bound")],digits=2),1,function(x) paste(x,collapse=" - "))
  #palette=colorBin(colors,c(mindata,maxdata),bins=length(colors))
  return(list(data=newdata,colormap=df))#,palette=palette))  
}


linear_colormap<-function(data){
  r=c(0, 63, 127, 191, 255, 255, 255, 255, 255, 255)
  g=c(255, 255, 255, 255, 255, 255, 191, 127, 63, 0)
  b=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  ramp_colors=rgb(r,g,b,maxColorValue=255)
  mindata=min(data)
  colors=ramp_colors
  maxdata=max(data)
  value=1:length(colors)
  lower_bound=mindata+seq(0,length(colors)-1)/length(colors)*(maxdata-mindata)
  upper_bound=mindata+seq(1,length(colors))/length(colors)*(maxdata-mindata)
  newdata=sapply(data,function(x) min(10,1+floor(10*(x-mindata)/(maxdata-mindata))))
  df=cbind(data.frame(value=value,lower_bound=lower_bound,upper_bound=upper_bound),t(col2rgb(colors)))
  df=rbind(c(0,0,0,0,0,0),df)
  palette=colorBin(colors,c(mindata,maxdata),bins=length(colors))
  return(list(data=newdata,colormap=df,palette=palette))
}







remove_from_geoserver<-function(map_name){
  command=paste("python ./python/remove_from_geoserver.py",map_name,"&")
  print(command)
  system(command)
}

myaccumulate<-function(X){
  i=X[,1]+1
  if (ncol(X)<3){
    j=rep(1,length(i))
  }else{
    j=X[,2]
  }
  x=X[,ncol(X)]
  return(cbind(0:(max(i)-1),as.matrix(sparseMatrix(i=i,j=j,x=x))))
  
}




cppFunction('NumericMatrix accumulate(NumericMatrix x,int len) {
            int nrow = x.nrow(), ncol = x.ncol(),count=0,row=0;
            NumericVector accum(len);
            NumericVector used(len);
            for (int i = 0; i < nrow; i++) {
            accum[x(i,0)] += x(i,1);
            used[x(i,0)]=1;
            }
            for (int i=0;i<len+1;i++){
            count+=used[i];
            }
            NumericMatrix out(count,2);
            for (int i=0;i<len+1;i++){
            if (used[i]==1){
            out[row]=i;
            out[count+row]=accum[i];
            row=row+1;
            }
            }
            return out;
            }')



load_simulation_results<-function(){
  data=as.data.frame(npyLoad("/mnt/ramdisk/simulation_results.npy"))
  colnames(data)=colnames(read.csv("data/simulation_output/simulation_results_headers.csv"))
  data[,"biomass_tonnes_per_hectare"]=data[,"tonnes_biomass_after_baling"]/64.74975
  data$weather_station=data$weather_station+1 #from 0+ python indexing to 1+ R indexing 
  return(data)

}


load_transport_results<-function(){
      result<-npyLoad(filename="data/transport/transport_output.npy")
      colnames(result)=colnames(read.csv("data/transport/transport_output_headers.csv"))
      cols=c("best_storage_node","transport_cost_to_plant","transport_cost_to_storage")
      if (all(cols %in% colnames(data))){
        data[,cols]=result[,cols]
      }else{
        data=cbind(data,result[,cols])
      }
      data$best_storage_node=data$best_storage_node+1 #convert from python 0+ counting to R 1+ counting
      cost_at_storage<<-data$harvest_cost_per_tonne+data$transport_cost_to_storage
      cost_at_plant<<-data$harvest_cost_per_tonne+data$transport_cost_to_plant
      df=data.frame(cost_at_storage=cost_at_storage,cost_at_plant=cost_at_plant)
      cols=c("cost_at_storage","cost_at_plant")
      if (all(cols %in% colnames(data))){
        data[,cols]=df
      }else{
        data=cbind(data,df)
      }
      data<<-data
}


load_optimization_results<-function(){
  filename="data/optimization/storage_assignment.mem"
  con<-file(filename, "rb")
  numints=file.size(filename)/4
  result<- t(matrix(data=readBin(con, what="int", n=numints,size=4),nrow=2,ncol=nrow(data)))
  close(con)
  colnames(result)=colnames(read.csv("data/optimization/storage_assignment_headers.csv"))
  if (all(colnames(result) %in% colnames(data))){
    data[,colnames(result)]=result
  }else{
    data<-cbind(data,result)
  }
  data<<-data
}


load_storage_locations<-function(){
  storage_locations=as.data.frame(npyLoad("temp_data/transport/storage_locations.npy"))
  colnames(storage_locations)=c("X","Y")
  proj4string <- "+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  pj <- proj4::project(storage_locations[,c("X","Y")], proj4string, inverse=TRUE)
  storage_locations$lat=pj$y
  storage_locations$lng=pj$x
  storage_locations$id=1:nrow(storage_locations)
  storage_locations<<-storage_locations
}

#copy file from data to temp_data, where they can be manipulated. Files in data remain unchanged always
files_to_copy=c("weather/locations.csv",
                "weather/weather_data.mem",
                "weather/weather_data_shape.npy",
                "weather/timestamps.mem",
                "crops/crop_parameters.csv",
                "transport/highway_distance.npy",
                "transport/storage_locations.npy",
                "crops/wetland_data.csv",
                "crops/crop_parameters.csv",
                "optimization/credits.csv"
)
for (file in files_to_copy){
  from_file=paste("data/",file,sep="")
  to_file=paste("temp_data/",file,sep="")
  #print(paste("copy",from_file,to_file))
  command=paste("cp",from_file,to_file)
  system(command)
}



layers=c("display_map","storage_locations","indigenous_lands","mb_highways","eco_regions")
years=2009:2015






plot_variables=c(colnames(data),"custom")
plot_variables=plot_variables[order(plot_variables)]




data_cols=read.csv("./data/R/data_columns.csv",stringsAsFactors=FALSE)
rownames(data_cols)=data_cols$variable
plot_variable_types=unique(data_cols$variable_type)
plot_variables=data_cols$variable

weather_stations=read.csv("./temp_data/weather/locations.csv")
weather_stations=weather_stations[order(weather_stations$index),]
rownames(weather_stations)=as.character(weather_stations$index)
weather_stations$station_id=weather_stations$index
filename="temp_data/weather/weather_data.mem"
dimensions=npyLoad("temp_data/weather/weather_data_shape.npy")

#combine year and day of year into one dimension
dimensions=c(dimensions[1],dimensions[2],dimensions[3],dimensions[4]) 
weather_variables=c("mean_temp","relative_humidity","rain","wind_speed","snow")
con<-file(filename, "rb")
numfloats=file.size(filename)/4
weather_data<- array(data=readBin(con, what="numeric", n=numfloats,size=4),dim=dimensions,dimnames=list(NULL,NULL,NULL,weather_variables))
close(con)
default_weather_station=26 #Winnipeg (50,-97)

filename="temp_data/weather/timestamps.mem"
con<-file(filename, "rb")
numfloats=file.size(filename)/4
timestamps<- readBin(con, what="numeric", n=numfloats,size=4)
close(con)
weather_dates<-as.Date(as.POSIXct(timestamps, origin="1970-01-01"))


crop_parameters=read.csv("temp_data/crops/crop_parameters.csv",stringsAsFactors=FALSE)
num_crops=max(crop_parameters$crop_number)
crop2number=crop_parameters[,c("crop","crop_number")]
rownames(crop2number)=crop2number$crop
number2crop=crop_parameters[,c("crop","crop_number")]
rownames(number2crop)=as.character(number2crop$crop_number)
crops=crop_parameters$crop

#get data
data=load_simulation_results()
temp=data$roadside_lat
data$roadside_lat=data$roadside_long
data$roadside_long=temp

load_transport_results()
load_optimization_results()
load_storage_locations()
data$year_index=data$year-min(data$year)
data$crop=number2crop[as.character(data$crop_number),"crop"]

#system("rm /mnt/ramdisk/data_*.tif*")




#*******************************************************************

#data$myvar=data$best_storage_node*(data$best_storage_node %in% unique(data$storage_site))


#get biomass by municipality
muni_id=data.frame(id=muni_data$id,name=tolower(muni_data$MUNI_NAME))
muni_data=read.csv("data/fields/mb_municipalities_wkt_3.csv")
muni_biomass=as.matrix(data.frame(id=floor(data$municipality_id),tonnes=data$tonnes_biomass_after_baling))
biomass_by_muni=as.data.frame(myaccumulate(muni_biomass))
biomass_by_muni[,2]=biomass_by_muni[,2]/7 #average over 5 years 2009,2010,2013,2014,2015
colnames(biomass_by_muni)=c("municipality_id","annual_biomass")
rownames(muni_id)=as.character(muni_id$id)
biomass_by_muni$municipality=muni_id[as.character(biomass_by_muni$municipality_id),"name"]
write.csv(biomass_by_muni, file = "data/simulation_output/biomass_by_muni.csv")

colormap_file="temp_data/crops/crop_parameters.csv"
colormap<<-read.csv(colormap_file)
colormap$colors=rgb(red=colormap$red,green=colormap$green,blue=colormap$blue,maxColorValue=255)
rownames(colormap)=colormap$crop
mycrop_colormap=colormap
mycrop_colormap<<-colormap


#Initialization of main map page
crop_layer<<-"initial"
display_map<<-""
map_displayed<<-FALSE
map_number<<-0
plant_chooser_armed<<-FALSE
map_type<<-"other"
map_name<<-"blank"

storage_locations_temp<<-FALSE
display_layers=c()


# Create the map

#load indiginous lands shapefile
ind_shapes <- readOGR(dsn = "data/indig_lands", layer="AL_TA_MB_2_80_eng")
ind_shapes=ind_shapes[order(ind_shapes@data$NAME1),]

#load mb_highways
highway_shapes <- readOGR(dsn = "data/transport/mb_highways", layer="mb_highways")

#load eco-regions
eco_region_shapes <- readOGR(dsn = "data/eco_regions", layer="eco_regions_5")
eco_region_shapes@data$REGION_NAM=paste(eco_region_shapes@data$REGION_NAM,eco_region_shapes@data$REGION_ID)
eco_region_names<<-eco_region_shapes@data$REGION_NAM
eco_region_shapes@data$centroid_lat<-gCentroid(eco_region_shapes,byid=TRUE)$y
eco_region_shapes@data$centroid_lng<-gCentroid(eco_region_shapes,byid=TRUE)$x

hostname=system("curl -s http://169.254.169.254/latest/meta-data/public-hostname",intern=TRUE)
df=read.csv("data/transport/plant_index.csv")
plant_location_id=df[1,"value"]
print(plant_location_id)
write.csv(df,"temp_data/transport/plant_index.csv",row.names=FALSE)
plant_lng=storage_locations[plant_location_id,"lng"]
plant_lat=storage_locations[plant_location_id,"lat"]

newtable<<-FALSE
display_crops<<-mycrop_colormap$crop
selected_years<<-2015


