library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(rhandsontable)
library(ggplot2)
library(Cairo)
library(gridExtra)
library(grid)
library(parallel)
library(rgdal)
library(spatialEco)
library(rgeos)




shinyServer(function(input, output, session) {
  
  
  
  
 print("got here 23*********************************************************************************") 
 rows=1:nrow(data)
 cols="cost_at_plant"
 rowcol=list(rows=rows,columns=cols)
 rows=rowcol$rows
 columns=rowcol$columns
 

 var=data[rows,columns]
 X=data.frame(crop=data[rows,"crop"],x=var)
 X=X[X[,2]<quantile(X[,2],.99),]
 X=X[X[,2]>0,]
 crops=as.character(unique(X[,1]))
 crops=crops[order(crops)]
 cols=mycrop_colormap[crops,"colors"]
 varname=paste(columns,sep="+")
 colnames(X)=c("crop","x")
 output$plot <- renderPlot({
   ggplot(X,aes(x=crop,y=x,fill=crop))+geom_boxplot()+scale_fill_manual(breaks=crops,values=cols)+ylab(varname)+ggtitle(paste("Mean:",mean(X[,2]),"\nSD:",sd(X[,2])))+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
 })
 print("ok************************************************************************************************")
 
 
 
  
  ## Interactive Map ##########################################
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat=50.5, lng=-97, zoom = 7) %>%
      
      
      addCircleMarkers(data=storage_locations,
                       layerId=~as.character(id),
                       radius=1,
                       color="black",
                       group="storage_locations") %>%
      
      
      removeMarker(plant_location_id) %>%
      addCircleMarkers(lat=plant_lat,lng=plant_lng,
                       group="storage_locations",
                       color="red",
                       radius=3,
                       layerId=plant_location_id) %>%
      hideGroup("storage_locations") %>%
      addPolylines(data=highway_shapes,group="mb_highways",weight=2,color="black",layerId=highway_shapes@data$ID) %>% 
      hideGroup("mb_highways") %>% 
      addPolygons(data=eco_region_shapes, fill=FALSE,group="eco_regions",layerId=eco_region_names) %>%
      hideGroup("eco_regions") 
        })
    
    
    #********************************************************************
    #This observer displays or removes mb highways
  observe({
      if ("mb_highways" %in% input$layers){
        leafletProxy("map") %>% showGroup("mb_highways")
      }else{
        leafletProxy("map") %>% hideGroup("mb_highways")
      }
      
  })
    
 #********************************************************************************   
   
  # #Popup when hovering over eco_regions
  # observeEvent(input$map_shape_mouseover$id, {
  # 
  #     pointId <- input$map_shape_mouseover$id
  #     print(paste("mouseover",pointId))
  #     if ("eco_regions" %in% input$layers){
  #       lat = eco_region_shapes@data[eco_region_shapes@data$REGION_NAM == pointId,"centroid_lat"]
  #       lng = eco_region_shapes@data[eco_region_shapes@data$REGION_NAM == pointId,"centroid_lng"]
  #       print(paste("mouse over",pointId))
  #       leafletProxy("map") %>% addPopups(lat = lat, lng = lng, popup=as.character(pointId), layerId = "hoverPopup")
  #     }
  # })
  
  #This observer displays or removes eco_regions
  observe({
    if ("eco_regions" %in% input$layers){
      leafletProxy("map") %>% showGroup("eco_regions")
    }else{
      leafletProxy("map") %>% hideGroup("eco_regions")
    }
    
  })
    
 
  
  
  #*******************functions ************************************
  popup_message<<-function(message){
    js_string <- 'alert("SOMETHING");'
    js_string <- sub("SOMETHING",message,js_string)
    session$sendCustomMessage(type='jsCode', list(value = js_string))
  }
  
  #******************************************************************************
  update_display_map<<-function(plot_variable,map_name,data_type=NULL){
    
    print(paste("updating with ",map_name))
    hostname=system("curl -s http://169.254.169.254/latest/meta-data/public-hostname",intern=TRUE)
    
    leafletProxy("map") %>% removeTiles("display_map")
    leafletProxy("map") %>% clearControls()
    
    
    if(map_name %in% c("blank","data_0")){
      return()
    }
    
    print("*******************got here**********************")
    print(map_name)
    leafletProxy("map") %>%
      addWMSTiles(
        paste("http://",hostname,":8080/geoserver/iisd1/wms",sep=""),
        layers = paste("iisd1:",map_name,sep=""),
        layerId="display_map",
        options = WMSTileOptions(format = "image/png", transparent = TRUE))
    
    if (grepl("crop",map_name)){
      colormap_file="temp_data/crops/crop_parameters.csv"
    }else{
      colormap_file="/mnt/ramdisk/colormap.csv"
    }
    
    
    
    colormap<<-read.csv(colormap_file)
    mycolors<<-rgb(red=colormap$red,green=colormap$green,blue=colormap$blue,maxColorValue=255)
    cols=read.csv("./data/R/data_columns.csv",stringsAsFactors=FALSE)
    rownames(cols)=cols[,"variable"]
    units=cols[plot_variable,"units"]
    if (length(plot_variable)>1){
      title="custom"
    }else{
      if (!(units %in% c("none","unknown"))){
        title=paste(plot_variable,units)
      }else{
        title=plot_variable
      }
    }
    
    
    if (is.null(data_type)){
      data_columns=read.csv("./data/R/data_columns.csv")
      rownames(data_columns)=data_columns$new_colname
      data_type=data_columns[plot_variable,"colormap_type"][1]
    }
    if (!(data_type %in% c("random"))){
      print("make legend")
      leafletProxy("map") %>%
        addLegend("bottomleft", colors=mycolors[-1], values=colormap$label[-1], title=title,
                  layerId="legend",labels=colormap$label[-1])
      print("done")
    }
    
    
    
  }
  #********************************************************************************************** 
 
  #general functions for updating input data tables
  
  
   modify<- function(key){
    print(paste("modify",key))
    filename<-paste("./temp_data",table_file[[key]],sep="/")
    if (!is.null(input[[key]])){
      D<-hot_to_r(input[[key]])
      D<-D[order(as.matrix(D)[,1]),]
      print(paste("writing to",filename))
      write.csv(as.matrix(D),file=filename,row.names=FALSE)
      reactive_tables[[key]]<-D
      
    }
    print(paste("done",key))
    
  }
  
  reset<-function(key){
    print(paste("reset",key))
    infilename<-paste("./data",table_file[[key]],sep="/")
    outfilename<-paste("./temp_data",table_file[[key]],sep="/")
    D<-read.csv(infilename,stringsAsFactors = FALSE)
    D<-D[order(D[,1]),]
    write.csv(D,file=outfilename,row.names=FALSE)
    reactive_tables[[key]]<-D
    
    
  }
  
  upload<-function(key){
    print(paste("upload",key))
    filename=paste("./temp_data",table_file[[key]],sep="/")
    inFile <- input[[paste(key,'file',sep="_")]]
    if (!is.null(inFile)){
      D<-read.csv(inFile$datapath,stringsAsFactors=FALSE)
      D<-D[order(as.matrix(D)[,1]),]
      reactive_tables[[key]]<-D
      write.csv(D,file=filename,row.names=FALSE)
    }
    if (key=="weather"){
      system("/home/diamond/anaconda2/bin/python python/weather/get_weather_array.py")
    }
    print(paste("done upload",key))
  }
  
  
  
  
  download<-function(key){
    rslt<-downloadHandler(
      filename=paste(key,"csv",sep="."),
      content=function(file){
        print(paste("download",key))
        D<-reactive_tables[[key]]
        print(nrow(D))
        write.csv(D,file=file,row.names=FALSE)
      },
      contentType="text/csv"
    )
    return(rslt)
  }
  
  
  
  
  
  
  #**********************************************************************************************
  #**********************************************************************************************
  #**********************************************************************************************
  #This observer displays or removes indigenous lands
  observe({
    
    output$indigenous_lands_selector <- renderUI({
      selectizeInput("indigenous_lands", "Indigenous Lands Displayed", choices=ind_shapes@data$NAME1,
                     multiple=TRUE,
                     options = list(plugins = list('remove_button', 'drag_drop')))
    })
    
    
    
  })
  
  observe({
    rows=(ind_shapes@data$NAME1 %in% input$indigenous_lands)
    leafletProxy("map") %>% clearGroup("ind_lands")  %>% addPolygons(data=ind_shapes[rows,], fill=FALSE,group="ind_lands")
    
  })
  
  observe({
    if ("indigenous_lands" %in% input$layers){
      leafletProxy("map") %>% showGroup("ind_lands")
    }else{
      leafletProxy("map") %>% hideGroup("ind_lands")
    }
    
  })
  
  #*************************************************************************************
  #This observer displays or removes eco regions
  
  observe({
    if ("eco_regions" %in% input$layers){
      leafletProxy("map") %>% showGroup("eco_regions")
    }else{
      leafletProxy("map") %>% hideGroup("eco_regions")
    }
    
  })
  
  #********************************************************************************************
  #This observer modifies the choices in the plot variable dropdown according to 
  #the plot variable type dropdown
  
  observe({
    x=colnames(data)
    print(x[grepl("cost",x)])
    plot_variable_type=input$plot_variable_type
    choices=intersect(colnames(data),data_cols$variable[data_cols$variable_type==plot_variable_type])
    
    if (length(choices)<2){
      selected=choices[1]
    }else{
      selected=NULL
    }
    if (plot_variable_type=="crop"){
      selected="crop"
    }
    if (plot_variable_type=="biomass"){
      selected="tonnes_biomass_after_baling"
    }
    output$plot_variable_selector <- renderUI({
      selectizeInput("plot_variable", "Plot Variable", choices=choices,
                     selected=selected,multiple=TRUE,
                     options = list(plugins = list('remove_button', 'drag_drop')))
    })
    
    
  })
  #*****************************************************************************************
  #This observer modifies the year  selector to checkboxes for numeric variables 
  #(where we plot average over years) or to a radio button (to plot one year at at time)
    observe({
#   #   print("*****************************************************************")
      plot_variable<-input$plot_variable
      map_type<-input$map_type
      variable_type<-data_cols[plot_variable,"colormap_type"]
      if ( (map_type %in% c("other","custom")) & (length(variable_type)>0) ){
                if (all(grepl("numeric",variable_type)) ) {
                  print("numeric variable")
                  numeric_var<<-TRUE
                  output$year_selector<- renderUI({
                     checkboxGroupInput("year_selection", "Years in Calculation", years, selected = "2015", inline = TRUE, width = NULL)
                             })
                }else{
                  print("non numeric variable")
                  numeric_var<<-FALSE
                  output$year_selector<- renderUI({
                      radioButtons("crop_year", "Year", years, selected = "2015", inline = TRUE, width = NULL)
                  })
                }
      }else{
        print("non numeric variable")
        numeric_var<<-FALSE
        output$year_selector<- renderUI({
           radioButtons("crop_year", "Year", years, selected = "2015", inline = TRUE, width = NULL)
        })
      }
 })
   
 
         
  
  #***********************************************************************
  #This will clear crop checkboxes 
  observeEvent(input$clear_crops,{
    updateCheckboxGroupInput(session=session, inputId="crops", choices=crops, selected=NULL,inline=TRUE)
  })
  
  #**********************************************************************************************
  #This observer is responsible to  display layers that are checked
  observe({
    
    
    print(storage_locations[1,])
    new_display_layers<-input$layers
    
    for (lyr in display_layers){
      if (  !(lyr %in% new_display_layers) ){
        print(paste("remove",lyr))
        if (lyr %in% c("storage_locations","weather_stations")){
          print(paste("hiding",lyr))
          leafletProxy("map") %>% hideGroup(lyr)
        }else{
          leafletProxy("map") %>% removeTiles(lyr)
          leafletProxy("map") %>% clearControls()
        }
        
        display_layers<<-display_layers[!(display_layers==lyr)]
      }
    }
    
    for (lyr in new_display_layers){
      if ( !(lyr %in% display_layers) ){
        print(paste("add",lyr))
        if (lyr =="storage_locations"){
          print(paste("showing",lyr))
          leafletProxy("map") %>% showGroup(lyr)
        }
        
        if (lyr=="typha"){
          leafletProxy("map") %>%
            addWMSTiles(
              paste("http://",hostname,":8080/geoserver/iisd1/wms",sep=""),
              layers = "iisd1:typha",
              layerId=lyr,
              options = WMSTileOptions(format = "image/png", transparent = TRUE))
        }
        if (lyr=="display_map"){
          if (input$map_type=='crop'){
            year=input$crop_year
            lyr=paste("aafc_",year,"_10crop",sep="")
            update_display_map("Crop",lyr)
          }else{
            map_name<-paste("data_",map_number,sep="")
            update_display_map(plot_variable,map_name)
          }
        }
        display_layers<<-new_display_layers
        
        
        
      }}})
  #****************************************************************************************
  #This observer will switch from crop map to other/custom map
  observe({
    print("switching from crop map")
    new_map_type<-input$map_type
    print(new_map_type)
    if (new_map_type=="crop"){
      year<-input$crop_year
      lyr=paste("aafc_",year,"_10crop",sep="")
      print(paste("update with",lyr))
      update_display_map("Crop",lyr)
    }else{
      if (new_map_type=="crop/wetland"){
        year<-input$crop_year
        lyr=paste("crop_map_",year,sep="")
        print(paste("update with",lyr))
        update_display_map("Crop",lyr)
      }else{
        
        map_name<-paste("data_",map_number,sep="")
        print(paste("update with",map_name))
        update_display_map(plot_variable,map_name)
      }
    }
    
  })
  #*******************************************************************************************
  #This will allow user to draw a polygon on the map, which can be used to limit analysis spatially
  
  mypolygon_armed<<-FALSE
  mypolygon_done<<-FALSE
  area <- reactiveValues()
  
  observeEvent(input$start_polygon, {
    mypolygon_armed<<-TRUE
  })
  
  
  observeEvent(input$done_polygon, {
    mypolygon_armed<<-FALSE
    mypolygon_done<<-TRUE
    print("done")
    print(mypolygon)
  })
  
  observeEvent(input$clear_polygon, {
    leafletProxy("map") %>% clearGroup("mymarkers") %>% clearGroup("mypolygon")
    area$mp<<-NULL
    mypolygon_armed<<-FALSE
    mypolygon_done<<-FALSE
    mypolygon<<-NULL
    
  })
  
  
  
  
  observeEvent(input$map_click, {
    
    coords <- input$map_click
    
    
    if (mypolygon_armed){
      if ( !is.null(coords)   ) {
        print("got here")
        cm <- matrix(data = c(coords$lat, coords$lng), nrow = 1, ncol = 2)
        area$mp <- if(!is.null(area$mp)){rbind(area$mp, cm)}else{cm}
        if (nrow(area$mp)<=2){
          leafletProxy("map") %>% addMarkers(lat = area$mp[,1], lng = area$mp[,2],group="mymarkers")
        }else{
          ch=chull(area$mp)
          mypolygon<<-area$mp[ch,]
          leafletProxy("map") %>% clearGroup("mymarkers") %>% clearGroup("mypolygon") %>% addPolygons(lat = mypolygon[ , 1], lng = mypolygon[ , 2],fill=FALSE,group="mypolygon")
        }
      }
      
    } 
    
  })
  
  #***************************************************************************************
  #These observers create charts of chosen variable
  #restricted to data in polygon drawn if there is one
  
  get_plot_data<-function(){
    
    if (input$map_type=="custom"){
      expression=parse(text=input$expression)
      include_crops=input$crops
      if (input$condition %in% c("TRUE","all")){
        condition=parse(text="field_id>=0")
      }else{
        condition=parse(text=input$condition)
      }
      custom_variable_name<<-input$variable_name
      cond1=(data$crop %in% include_crops)
      cond2=eval(condition,data)
      custom_condition<<-cond1 & cond2
      data[,custom_variable_name]=0
      data[custom_condition,custom_variable_name]=eval(expression,data[custom_condition,])
      rows=custom_condition
      columns=custom_variable_name
      return(list(rows=rows,columns=columns))
      
    }else{
      
      plot_variable<<-input$plot_variable
      selected_years<-input$year_selection
      display_crops<<-input$crops
      
      if (is.null(plot_variable)){
        message <- "Please choose a variable or combination of variables to plot in the drop down menu. If you choose         multiple variables, the sum will be plotted."
        js_string <- 'alert("SOMETHING");'
        js_string <- sub("SOMETHING",message,js_string)
        session$sendCustomMessage(type='jsCode', list(value = js_string))
        return()
      }
      
      
      data_type=data_cols[plot_variable,"colormap_type"]
      columns=plot_variable
      
      display_crops=input$crops
      
      if (numeric_var){
        selected_years=input$year_selection
      }else{
        selected_years=c(input$crop_year)
      }
      
      print("*********herehere********************************************************")
      print(display_crops)
      print("***")
      print(selected_years)
      print("***")
      print(numeric_var)
      rows=(data$crop %in% display_crops) & (data$year %in% selected_years)
      
      print(paste("here rowcol",sum(rows),columns,selected_years))
      print(unique(data[rows,"crop"]))
      
      return(list(rows=rows,columns=columns))
    }
    
  }
  
  
  restrict_to_polygons<-function(data,mypolygon,road_buffer){
    proj4string <- "+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
    if (mypolygon_done){
      polygon1_coords <- proj4::project(mypolygon[,c(2,1)], proj4string)
      polygon1<-SpatialPolygons(list(Polygons(list(Polygon(polygon1_coords)),1)),proj4string = CRS(proj4string))
      polygons<-polygon1
    }
    if (road_polygon_done){
      polygon2<-spTransform(road_buffer,CRS=CRS(proj4string))
      if (mypolygon_done){
        polygons<-rbind(polygon1,polygon2)
      }else{
        polygons<-polygon2
      }
    }
    
    
    roadsides=data[,c("roadside_e","roadside_n")]
    roadsides=SpatialPoints(roadsides,proj4string=CRS(proj4string))
    keep=!is.na(over(roadsides,polygons))
    print(paste("keeping",unique(data[keep,"crop"])))
    return(keep)
  }
  
  
  draw_pie_chart<-function(data,mypolygon,mypolygon_done,road_polygon_done){
    rowcol=get_plot_data()
    rows=rowcol$rows
    columns=rowcol$columns
    
    if (mypolygon_done | road_polygon_done){
      in_polygon=restrict_to_polygons(data,mypolygon,road_buffer)
      rows=(rows & in_polygon)
      print(paste("keeping rows",unique(data[rows,"crop"])))
    }
    var=data[rows,columns]
    if (length(columns)>1){
      var=apply(var,1,sum)
    }
    
    X=as.matrix(cbind(data[rows,"crop_number"],var))
    print(paste("X",unique(X[,1])))
    tbl=myaccumulate(X)
    tbl=tbl[tbl[,2]>0,]
    print(tbl)
    lbls=number2crop[as.character(tbl[,1]),"crop"]
    print(lbls)
    print(number2crop)
    print(tbl[,1])
    cols=paste(mycrop_colormap[lbls,"colors"],"FF",sep="")
    slices=tbl[,2]
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, floor(slices)) # add amounts to labels
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels
    varname=paste(columns,sep="+")
    print(lbls)
    output$plot <- renderPlot({
      pie(slices,labels = lbls, col=cols,
          main=paste("Pie Chart of ",varname,"/ntotal=",floor(sum(slices))))
    })
    
  }
  
  
  
  
  draw_histogram<-function(data,mypolygon,mypolygon_done,road_polygon_done){
    rowcol=get_plot_data()
    rows=rowcol$rows
    columns=rowcol$columns
    
    if (mypolygon_done | road_polygon_done){
      in_polygon=restrict_to_polygons(data,mypolygon,road_buffer)
      rows=(rows & in_polygon)
    }
    var=data[rows,columns]
    if (length(columns)>1){
      var=apply(var,1,sum)
    }
    X=data.frame(crop=data[rows,"crop"],x=var)
    X=X[X[,2]<quantile(X[,2],.99),]
    X=X[X[,2]>0,]
    crops=as.character(unique(X[,1]))
    crops=crops[order(crops)]
    cols=mycrop_colormap[crops,"colors"]
    varname=paste(columns,sep="+")
    colnames(X)=c("crop","x")
    output$plot <- renderPlot({
      ggplot(X,aes(x=x,fill=crop))+geom_histogram()+scale_fill_manual(breaks=crops,values=cols)+xlab(varname)
    })
    
  }
  
  
  
  draw_boxplot<-function(data,mypolygon,mypolygon_done,road_polygon_done){
    rowcol=get_plot_data()
    rows=rowcol$rows
    columns=rowcol$columns
    
    if (mypolygon_done | road_polygon_done){
      in_polygon=restrict_to_polygons(data,mypolygon,road_buffer)
      rows=(rows & in_polygon)
    }
    var=data[rows,columns]
    if (length(columns)>1){
      var=apply(var,1,sum)
    }
    X=data.frame(crop=data[rows,"crop"],x=var)
    X=X[X[,2]<quantile(X[,2],.99),]
    X=X[X[,2]>0,]
    crops=as.character(unique(X[,1]))
    crops=crops[order(crops)]
    cols=mycrop_colormap[crops,"colors"]
    varname=paste(columns,sep="+")
    colnames(X)=c("crop","x")
    output$plot <- renderPlot({
      ggplot(X,aes(x=crop,y=x,fill=crop))+geom_boxplot()+scale_fill_manual(breaks=crops,values=cols)+ylab(varname)+ggtitle(paste("Mean:",mean(X[,2]),"\nSD:",sd(X[,2])))+theme(plot.title = element_text(hjust = 0.5))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    
  }
  
  observeEvent(input$draw_pie,{
    draw_pie_chart(data,mypolygon,mypolygon_done,road_polygon_done)
    
  })
  
  observeEvent(input$draw_hist,{
    draw_histogram(data,mypolygon,mypolygon_done,road_polygon_done)
    
  })
  
  observeEvent(input$draw_box,{
    draw_boxplot(data,mypolygon,mypolygon_done,road_polygon_done)
    
  })
  
  output$downloadPlot <- downloadHandler(
    filename = "Shinyplot.png",
    content = function(file) {
      png(file)
      plotInput()
      dev.off()
    })
  
  
  #***************************************************************************************
  #This observer is responsible to  display the currently chosen display map
  observeEvent(input$draw_map,{draw_map()})
               
  draw_map<-function(){
    
    #print("drawing map button")
    progress <<- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Making Map", value = 0)
    
    plot_variable<<-input$plot_variable
    selected_years<-input$year_selection
    display_crops<<-input$crops
    
    if (is.null(plot_variable)){
      message <- "Please choose a variable or combination of variables to plot in the drop down menu. If you choose         multiple variables, the sum will be plotted."
      js_string <- 'alert("SOMETHING");'
      js_string <- sub("SOMETHING",message,js_string)
      session$sendCustomMessage(type='jsCode', list(value = js_string))
      return()
    }
    
    
    
    data_type=data_cols[plot_variable,"colormap_type"]
    print(data_type)
    columns=plot_variable
    t1=Sys.time()
    mydata=get_plot_data()
    
    rows=mydata$rows
 
    print(paste("&*&*&*&*&*&*&*&*&"))
    print(unique(data[rows,"crop"]))
    print(unique(data[rows,"year"]))
    #rows=(data$crop %in% display_crops) & (data$year %in% selected_years)
    if (mypolygon_done | road_polygon_done){
      in_polygon=restrict_to_polygons(data,mypolygon,road_buffer)
      rows=(rows & in_polygon)
    }
    print(paste("rows",sum(data$crop %in% display_crops)))
    
   
    print(paste("t1",Sys.time()-t1))
    t1=Sys.time()
    
    if (map_number>0){
      map_name=paste("data_",map_number,sep="")
      remove_from_geoserver(map_name)
    }
    print(paste("t2",Sys.time()-t1))
    t1=Sys.time()
    
    progress$inc(.12)
    map_number<<-map_number+1
    map_name<-paste("data_",as.character(map_number),sep="")
    map_name<-paste("data_",map_number,sep="")
    print(paste("publish",columns,data_type,map_name,length(rows),sum(rows)))
    
    print("99999999999999999999999999999999999999999999999999999999999999999999999999999999999999")
    print(unique(data[rows,"crop"]))
    publish_map(data,rows,columns,data_type,map_name)
    
    t1=Sys.time()
    update_display_map(plot_variable,map_name,data_type)
    print(paste("t8",Sys.time()-t1))
    t1=Sys.time()
    
    map_displayed<<-TRUE
    display_layers<<-union(display_layers,"display_map")
    
    
    
  }
  
  #*************************************************************************************
  # #This observer and action button are responsible for changing the plant location
  # 
  observeEvent(input$choose_plant_location, {
    #when action button clicked
    
    #Show the storage sites if not already displayed
    print(display_layers)
    if (!("storage_locations" %in% display_layers)){
      storage_locations_temp<<-TRUE
      print("here")
      leafletProxy("map") %>% showGroup("storage_locations")
      
    }
    
    #show a popup instructing to click on new plant location
    message <- "Please click on Plant Location"
    js_string <- 'alert("SOMETHING");'
    js_string <- sub("SOMETHING",message,js_string)
    session$sendCustomMessage(type='jsCode', list(value = js_string))
    
    #arm plant choice so that next click on a storage location will choose it
    plant_chooser_armed<<-TRUE
  })
  
  
  
  observeEvent(input$map_marker_click,{
    #If plant chooser is not armed, do nothing
    
    if (plant_chooser_armed){
      click<-input$map_marker_click
      
      #replace current plant location (red circle) with black circle
      leafletProxy("map") %>%
        removeMarker(layerId=plant_location_id) %>%
        addCircleMarkers(lat=plant_lat,lng=plant_lng,
                         group="storage_locations",
                         color="black",
                         radius=1,
                         layerId=plant_location_id)
      
      #Update plant location id,lat,lng to id of circle marker clicked
      plant_location_id<<-as.character(click$id)
      plant_lat<<-click$lat
      plant_lng<<-click$lng
      
      #save plant location in file
      df=data.frame(key="plant_location_id",value=plant_location_id)
      write.csv(df,"temp_data/transport/plant_index.csv",row.names=FALSE)
      
      #Change plant location marker to larger and red
      leafletProxy("map") %>%
        removeMarker(plant_location_id) %>%
        addCircleMarkers(lat=plant_lat,lng=plant_lng,
                         group="storage_locations",
                         color="red",
                         radius=3,
                         layerId=plant_location_id)
      
      #Show popup with id of plant location chosen
      message <- paste("plant location at ",plant_location_id)
      js_string <- 'alert("SOMETHING");'
      js_string <- sub("SOMETHING",message,js_string)
      session$sendCustomMessage(type='jsCode', list(value = js_string))
      
      #if storage sites displayed temporarily for this procedure
      #hide them again
      if (storage_locations_temp){
        #leafletProxy("map") %>% hideGroup("storage_locations")
        storage_locations_temp<-FALSE
        
      }
      
      #disarm the plant chooser state
      plant_chooser_armed<<-FALSE
      
      
      
      #recalculate transport costs
      print("recalculate transport costs")
      script="./python/transport/get_transport_costs.py"
      command<<-paste("/home/diamond/anaconda2/bin/python",script)
      print(command)
      system(command)
      load_transport_results()
      print(paste("plot_variable",input$plot_variable))
      affected_variables<-c("best_storage_node","transport_cost_to_plant","transport_cost_to_storage","cost_at_storage","cost_at_plant")
      if (input$map_type=="custom"){
        popup_message("The map will be redrawn. The optimization must be rerun to repopulate the solution")
        draw_custom_map()
      }
      if (input$map_type=="other"){
        if (length(intersect(plot_variable,affected_variables))>0){
          popup_message("The map will be redrawn. The optimization must be rerun to repopulate the solution")
          draw_map()
        }
      }
      
    }
  })
  
  #***********************************************************************************
  # #This action button will recalculate transport costs in order to accomodate change
  # #in plant location or transport cost parameters (truck costs etc.)
  observeEvent(input$recalculate_transport_costs, {
    #recalculate transport costs
    print("recalculate transport costs")
    script="./python/transport/get_transport_costs.py"
    command<<-paste("/home/diamond/anaconda2/bin/python",script)
    print(command)
    system(command)
    load_transport_results()
    popup_message("done. Note no maps have been redrawn. You must hit the Draw Map button for that. Also, the optimization must be rerun to repopulate the solution")
    #   
  })
  
  #************************************************************************************
  #  #Transport Cost Parameters
  
  table_file=list()
  table_file[["transport_parameters"]]="transport/transport_parameters.csv"
  table_file[["derived_transport_parameters"]]="transport/derived_transport_parameters.csv"
  
  #initialization of transport cost parameters
  reactive_tables=reactiveValues()
  for (key in c("transport_parameters","derived_transport_parameters")){
    infilename=paste("./data",table_file[[key]],sep="/")
    outfilename=paste("./temp_data",table_file[[key]],sep="/")
    DF=read.csv(infilename,stringsAsFactors=FALSE)
    DF<-DF[order(DF[,1]),]
    write.csv(DF,file=outfilename,row.names=FALSE)
    reactive_tables[[key]]<-DF
    
  }
  
  
  
  
  
  
  
  output$transport_parameters<-renderRHandsontable({
    rhandsontable(as.data.table(reactive_tables[["transport_parameters"]]))
  })
  output$derived_transport_parameters = renderRHandsontable({rhandsontable(as.data.table(reactive_tables[["derived_transport_parameters"]]))})
  
  
  observe({
    modify("transport_parameters")
  })
  observe({
    modify("derived_transport_parameters")
  })
  
  observeEvent(input$transport_parameters_reset,{
    reset("transport_parameters")
    reset("derived_transport_parameters")
    
  })
  
  
  update_derived_transport_parameters<-function(transport_parameters,derived_transport_parameters){
    TP<-transport_parameters
    DTP<-derived_transport_parameters
    
    df=data.frame(t(TP$value))
    colnames(df)=TP$variable
    
    
    
    L1_text=DTP[[which(DTP$variable=="L1_weight"),"expression"]]
    highway_text=DTP[[which(DTP$variable=="highway_weight"),"expression"]]
    L1_weight<-eval(parse(text=L1_text),df)
    highway_weight<-eval(parse(text=highway_text),df)
    
    DTP[DTP$variable=="L1_weight","value"]<-L1_weight
    DTP[DTP$variable=="highway_weight","value"]<-highway_weight
    outfilename<-"./temp_data/transport/derived_transport_parameters.csv"
    write.csv(DTP,file=outfilename,row.names=FALSE)
    
    return(DTP)
    
  }
  
  observe({
    TP<-reactive_tables$transport_parameters
    DTP<-reactive_tables$derived_transport_parameters
    reactive_tables$derived_transport_parameters<-update_derived_transport_parameters(TP,DTP)
  })
  
  modified<-reactiveValues(derived_transport_parameters=FALSE)
  observe({
    DTP<-reactive_tables$derived_transport_parameters
    #count<-isolate(modified$derived_transport_parameters)+1
    #modified$derived_transport_parameters<-count+1
    modified$derived_transport_parameters<-TRUE
  })
  
  #************************************************************************************
  #weather map and data
  
  output$weather_map<-renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat=51.5, lng=-97, zoom = 7) %>%
      
      addCircleMarkers(data=weather_stations,
                       lat=weather_stations$lat,
                       lng=weather_stations$lng,
                       layerId=as.character(weather_stations$station_id),
                       radius=4,
                       color="blue",
                       group="weather_stations") 
    
    
  })
  
  
  ranges_x <- NULL
  ranges_y <- NULL
  
  weather_plot_var<-"mean_temp"
  station_id=default_weather_station
  location=weather_stations[as.character(station_id),c("lat","lng")]
  title=paste("weather station at (",location[1],",",location[2],")",sep="")
  df<-data.frame(date=weather_dates,y=as.vector(t(weather_data[station_id,,,weather_plot_var])))
  print(paste(nrow(df),min(df$y),mean(df$y),max(df$y)))
  
  weather_plot<<-ggplot(df, aes(x=date, y=y)) +geom_line()+ylab(weather_plot_var)+ggtitle(title)
  output$weather_plot <-renderPlot({weather_plot})
  
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges_x <- c(brush$xmin, brush$xmax)
      ranges_x=as.Date(ranges_x,origin="1970-01-01")
      ranges_y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges_x <- NULL
      ranges_y <- NULL
    }
    
    print(ranges_y)
    weather_plot<<-weather_plot+coord_cartesian(xlim = ranges_x, ylim = ranges_y)
    output$weather_plot <-renderPlot({weather_plot})
  })
  
  
  
  observeEvent(input$weather_map_marker_click,{
    click<-input$weather_map_marker_click
    if (click$group=="weather_stations"){
      id=click$id
      print(id)
      station_id=as.numeric(id)
      location=weather_stations[as.character(station_id),c("lat","lng")]
      title=paste("weather station at (",location[1],",",location[2],")",sep="")
      df<-data.frame(date=weather_dates,y=as.vector(t(weather_data[station_id,,,weather_plot_var])))
      weather_plot<<-ggplot(df, aes(x=date, y=y)) +geom_line()+ylab(weather_plot_var)+ggtitle(title)
      output$weather_plot <-renderPlot({weather_plot})
      
      
    }})
  
  observe({
    print("hello hello")
    weather_plot_var<-input$weather_plot_var
    if (is.null(weather_plot_var)){weather_plot_var<-"mean_temp"}
    if ("click" %in% ls()){
      station_id=click$id
    }else{
      station_id=default_weather_station
    }
    location=weather_stations[as.character(station_id),c("lat","lng")]
    title=paste("weather station at (",location[1],",",location[2],")",sep="")
    print("and here")
    print(station_id)
    print(typeof(station_id))
    
    df<-data.frame(date=weather_dates,y=as.vector(t(weather_data[station_id,,,weather_plot_var])))
    print("also here")
    weather_plot<<-ggplot(df, aes(x=date, y=y)) +geom_line()+ylab(weather_plot_var)+ggtitle(title)
    output$weather_plot <-renderPlot({weather_plot})
    
    
  })
  
  
  table_file["weather"]="temp_data/weather/weather_data.csv"
  
  #Initialization of weather data table
  #infilename=paste("./data",table_file[["weather"]],sep="/")
  #reactive_tables[["weather"]]<-weather_data
  
  
  output$weather_download<-download("weather")
  
  observe({upload("weather")})
  observeEvent(input$weather_reset,{reset("weather")})
  
  
  
  #************************************************************************
  
  #Harvest Schedule Data
  
  table_file[["harvest_schedule"]]="time_series/HarvestSchedule.csv"
  
  #Initialization of weather data table
  infilename=paste("./data",table_file[["harvest_schedule"]],sep="/")
  D=read.csv(infilename,header=TRUE,stringsAsFactors=FALSE)
  reactive_tables[["harvest_schedule"]]<-D
  
  
  
  output$harvest_schedule<-renderRHandsontable({
    rhandsontable(as.data.table(reactive_tables[["harvest_schedule"]]))
  })
  
  output$harvest_schedule_download<-download("harvest_schedule")
  
  observe({upload("harvest_schedule")})
  observe({modify("harvest_schedule")})
  observeEvent(input$harvest_schedule_reset,{reset("harvest_schedule")})
  
  #**********************************************************************************
  #wetland Data
  
  table_file[["wetland_data"]]="crops/wetland_data.csv"
  
  #Initialization of weather data table
  infilename=paste("./data",table_file[["wetland_data"]],sep="/")
  D=read.csv(infilename,header=TRUE,stringsAsFactors=FALSE)
  reactive_tables[["wetland_data"]]<-D
  
  
  
  output$wetland_data<-renderRHandsontable({
    rhandsontable(as.data.table(reactive_tables[["wetland_data"]]))
  })
  
  output$wetland_data_download<-download("wetland_data")
  
  observe({upload("wetland_data")})
  observe({modify("wetland_data")})
  observeEvent(input$wetland_data_reset,{reset("wetland_data")})
  
  #*****************************************************************************************
  #Crop Data
  
  table_file[["crop_data"]]="crops/crop_parameters.csv"
  
  #Initialization of crop data table
  infilename=paste("./data",table_file[["crop_data"]],sep="/")
  D=read.csv(infilename,header=TRUE,stringsAsFactors=FALSE)
 
   
  reactive_tables[["crop_data"]]<-D
  
  
  
  output$crop_data<-renderRHandsontable({
    rhandsontable(as.data.table(reactive_tables[["crop_data"]]))
  })
  
  output$crop_data_download<-download("crop_data")
  
  observe({upload("crop_data")})
  observe({modify("crop_data")})
  observeEvent(input$crop_data_reset,{reset("crop_data")})
  
  #**********************************************************************************************
  
  #Credit Data
  
  # table_file[["credit_data"]]="optimization/credits.csv"
  # 
  # #Initialization of credit data table
  # infilename=paste("./data",table_file[["credit_data"]],sep="/")
  # D=read.csv(infilename,header=TRUE,stringsAsFactors=FALSE)
  # print(D)
  # 
  # reactive_tables[["credit_data"]]<-D
  # 
  # 
  # 
  # output$credit_data<-renderRHandsontable({
  #   rhandsontable(as.data.table(reactive_tables[["credit_data"]]))
  # })
  # 
  # 
  # observe({modify("credit_data")})
  # observeEvent(input$credit_data_reset,{reset("credit_data")})
  
  
  #*****************************************************************************************
  
  
  #***********************************************
  
  #Draw plot with plant effeciency curve
  plant_efficiency_curve<-function(D,my_plant_type,my_decay_parameter,max_demand,scale_type){
    
    print(paste("redrawing plant curves",my_decay_parameter))
    D$pred=0
    decay_parameter=list()
    for (pt in unique(D$plant_type)){
      d=D[D$plant_type==pt,]
      model=lm(data=d,formula= log(cost_per_tonne) ~ log(kt_per_year))
      decay_parameter[pt]=model$coefficients[2]
      D[D$plant_type==pt,"pred"]=exp(predict(model))
    }
    decay_parameter["mymodel"]=my_decay_parameter
    g<-ggplot()+geom_point(data=D,aes(x=kt_per_year,y=cost_per_tonne,color=plant_type))
    
    gj_per_tonne=D[min(which(D$plant_type==my_plant_type)),"gj_per_tonne"]
    mymodel=data.frame(plant_type="mymodel",kt_per_year=(1:8)/8*max_demand,gj_per_tonne=gj_per_tonne)
    starting=which.min(D[D$plant_type==my_plant_type,"kt_per_year"])
    starting_point=D[D$plant_type==my_plant_type,c("kt_per_year","cost_per_tonne")][starting,]
    mymodel$cost_per_tonne=starting_point[[2]]*( (mymodel$kt_per_year/starting_point[[1]])**my_decay_parameter)
    mymodel$pred=mymodel$cost_per_tonne
    D=rbind(D,mymodel)
    
    
    DF=data.frame()
    for (plant_type in unique(D$plant_type)){
      model=lm(data=D[D$plant_type==plant_type,],formula= log(cost_per_tonne) ~ log(kt_per_year))
      x=.01*(1:100)*max_demand
      y=exp(predict(model,newdata=data.frame(kt_per_year=x)))
      df=data.frame(plant_type=plant_type,kt_per_year=x,cost_per_tonne=y)
      DF=rbind(DF,df)
    }
    
    g=g+geom_line(data=DF,aes(x=kt_per_year,y=cost_per_tonne,color=plant_type))
    if (scale_type=="loglog"){
      g<-g+ scale_x_log10() + scale_y_log10() 
    }
    D1=data.frame()
    for (plant_type in unique(D$plant_type)){
      df=DF[DF$plant_type==plant_type,]
      row=which.max(DF$kt_per_year)
      x=.8*df$kt_per_year[row]
      y=df$cost_per_tonne[row]
      label = paste("decay:",round(decay_parameter[[plant_type]],digits=3),sep=" ")
      df=data.frame(x=x,y=y,label=label,plant_type=plant_type)
      D1=rbind(D1,df)
      
    }
    g=g+geom_text(aes(x=D1$x,y=D1$y,label=D1$label, color = D1$plant_type))
   
    
    mymodel$plant_type=my_plant_type
    mymodel=mymodel[,setdiff(colnames(mymodel),"pred")]
    
    return(list(plot=g,data=mymodel,decay_parameters=decay_parameter))
    
  }
  
  
  run_heuristic<-function(max_demand,pcredit){
    command=paste("/home/diamond/anaconda2/bin/python python/optimization/run_heuristic.py",
                  max_demand*1000,pcredit)
    print(command)
    system(command)
    D=read.csv("data/optimization/cost.csv")
    D$kt_per_year=D$kt_per_year/1000
    return(D)
  }
  
  
  net_cost<-function(ecurve,heuristic_data,plant_type,carbon_credit,displacing,emmissions,energy_value){
    
    
    
    DF=ecurve$data
    print("0000000000000000000000000000000000000")
    print(DF)
    print(heuristic_data)
    DF$raw_material_cost_per_tonne=heuristic_data$raw_material_cost_per_tonne
    DF$phosphorous_credit_per_tonne=heuristic_data$phosphorous_credit_per_tonne
    DF=DF[,c("plant_type","gj_per_tonne","kt_per_year","raw_material_cost_per_tonne","phosphorous_credit_per_tonne","cost_per_tonne")]
    colnames(DF)[6]="processing_cost_per_tonne"
    DF$carbon_credit_per_tonne=carbon_credit*emmissions[[displacing]]/1000*DF$gj_per_tonne
    print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
    print(paste("carbon",carbon_credit,emmissions[[displacing]]/1000,DF$gj_per_tonne[1]))
    DF$energy_credit_per_tonne=energy_value[[displacing]]*DF$gj_per_tonne
    DF$net_cost_per_tonne=DF$raw_material_cost_per_tonne-DF$phosphorous_credit_per_tonne+DF$processing_cost_per_tonne-DF$carbon_credit_per_tonne-DF$energy_credit_per_tonne
    g=ggplot()+geom_point(data=DF,aes(x=kt_per_year,y=net_cost_per_tonne,color=plant_type))
    g=g+geom_line(data=DF,aes(x=kt_per_year,y=net_cost_per_tonne,color=plant_type))
    return(list(data=DF,net_cost_plot=g))
  } 

#*************************************************
  
  #defaults
  emmissions=list(electricity=0,natural_gas=50.5) #kg CO2 per GJ
  energy_value=list(electricity=27.777,natural_gas=2.56) #cad per GJ
  decay_parameter=-.75
  max_demand=350
  scale_type="loglog"
  p_credit=50
  carbon_credit=50.5
  plant_type="biogas"
  displacing="natural_gas"
  plant_data=read.csv("data/optimization/plant_data.csv")
  
  ecurve<<-plant_efficiency_curve(plant_data,plant_type,decay_parameter,max_demand,scale_type)
  heuristic_data<<-run_heuristic(max_demand,p_credit)
  net_cost_data<<-net_cost(ecurve,heuristic_data,plant_type,carbon_credit,displacing,emmissions,energy_value)
  
  
  
  
  
  #plant data
  table_file[["plant_data"]]="optimization/plant_data.csv"
  
  #Initialization of crop data table
  infilename=paste("./data",table_file[["plant_data"]],sep="/")
  D=read.csv(infilename)
  reactive_tables[["plant_data"]]<-D
  output$plant_data<-renderRHandsontable({
    rhandsontable(as.data.table(reactive_tables[["plant_data"]]))
  })
  
  reactive_tables[["net_cost_data"]]<-net_cost_data$data
  output$net_cost_data<-renderRHandsontable({
    rhandsontable(as.data.table(reactive_tables[["net_cost_data"]]))
  })
 
  
  
  
  reactive_tables[["my_plant_model_data"]]=ecurve$data
  output$my_plant_model_data<-renderRHandsontable({
    rhandsontable(as.data.table(reactive_tables[["my_plant_model_data"]]))
  })
  
  output$plant_plot <- renderPlot({
       ecurve$plot
    })
  
  output$net_cost_plot <- renderPlot({
    net_cost_data$net_cost_plot
  })
  
  
  
  
  
 
  
   observe({
     plant_type=input$plant_type
     decay_parameter=input$decay_parameter
     max_demand=input$max_demand
     scale_type=input$scale_type
     D=read.csv("data/optimization/plant_data.csv")
     ecurve<-plant_efficiency_curve(D,plant_type,decay_parameter,max_demand,scale_type)
     print(ecurve$data)
     reactive_tables[["my_plant_model_data"]]<-ecurve$data
     output$plant_plot <- renderPlot({
       ecurve$plot
     })
     output$my_plant_model_data<-renderRHandsontable({
       rhandsontable(as.data.table(reactive_tables[["my_plant_model_data"]]))
     })
   })
     
     
     observe({
       p_credit=input$p_credit
       max_demand=input$max_demand
       heuristic_data<<-run_heuristic(max_demand,p_credit)
       net_cost_data<<-net_cost(ecurve,heuristic_data,plant_type,carbon_credit,displacing,emmissions,energy_value)
       output$net_cost_plot <- renderPlot({
           net_cost_data$net_cost_plot
       })
       reactive_tables[["net_cost_data"]]<-net_cost_data$data
       output$net_cost_data<-renderRHandsontable({
           rhandsontable(as.data.table(reactive_tables[["net_cost_data"]]))
       })
       
     })
     
     observe({
       decay_parameter=input$decay_parameter
       max_demand=input$max_demand
       scale_type=input$scale_type
       p_credit=input$p_credit
       carbon_credit=input$carbon_credit
       plant_type=input$plant_type
       displacing=input$displacing
       ecurve<<-plant_efficiency_curve(plant_data,plant_type,decay_parameter,max_demand,scale_type)
       net_cost_data<<-net_cost(ecurve,heuristic_data,plant_type,carbon_credit,displacing,emmissions,energy_value)
       output$net_cost_plot <- renderPlot({
         net_cost_data$net_cost_plot
       })
       reactive_tables[["net_cost_data"]]<-net_cost_data$data
       output$net_cost_data<-renderRHandsontable({
         rhandsontable(as.data.table(reactive_tables[["net_cost_data"]]))
       })
       
     })
    
    

  
  
  output$plant_data_download<-download("plant_data")
  
  observe({upload("plant_data")})
  observe({modify("plant_data")})
  observeEvent(input$plant_data_reset,{reset("plant_data")})
  
  
  #*****************************************************************************************
  # #Equipment Parameters
  #
  # #assign file names and titles to equipment list
  equipment_titles=c("Baler",
                     "Baling Tractor",
                     "Loader with Bale Grabber",
                     "Shredding Tractor",
                     "Stinger For Bales",
                     "Windrower")
  equipment_list=remove_spaces(equipment_titles)
  mylist=list()
  for (i in 1:length(equipment_list)){
    mylist[equipment_list[[i]]]=equipment_titles[[i]]
  }
  equipment_titles=mylist
  equipment_file_list=c("baler.csv",
                        "tractor120.csv",
                        "LoaderwithBaleGrabber.csv",
                        "tractor160.csv",
                        "StingerforBales.csv",
                        "windrower.csv")
  
  for (i in 1:length(equipment_list)){
    table_file[equipment_list[[i]]]=paste("equipment",equipment_file_list[[i]],sep="/")
  }
  
  
  
  #read in initial equipment data
  for (machine in equipment_list){
    DF=read.csv(paste("./data",table_file[[machine]],sep="/"),header=TRUE)
    DF<-DF[order(DF[,1]),]
    reactive_tables[[machine]]<-DF
  }
  
  
  
  
  
  
  
  output$baler_download<-download("baler")
  output$baling_tractor_download<-download("baling_tractor")
  output$loader_with_bale_grabber_download<-download("loader_with_bale_grabber")
  output$shredding_tractor_download<-download("shredding_tractor")
  output$stinger_for_bales_download<-download("stinger_for_bales")
  output$windrower_download<-download("windrower")
  
  
  observe({upload("baler")})
  observe({upload("baling_tractor")})
  observe({upload("loader_with_bale_grabber")})
  observe({upload("shredding_tractor")})
  observe({upload("stinger_for_bales")})
  observe({upload("windrower")})
  
  observe({modify("baler")})
  observe({modify("baling_tractor")})
  observe({modify("loader_with_bale_grabber")})
  observe({modify("shredding_tractor")})
  observe({modify("stinger_for_bales")})
  observe({modify("windrower")})
  
  
  
  
  observeEvent(input$baler_reset,{
    reset("baler") })
  observeEvent(input$baling_tractor_reset,{
    reset("baling_tractor")})
  observeEvent(input$loader_with_bale_grabber_reset,{
    reset("loader_with_bale_grabber")})
  observeEvent(input$shredding_tractor_reset,{
    reset("shredding_tractor")})
  observeEvent(input$stinger_for_bales_reset,{
    reset("stinger_for_bales")})
  observeEvent(input$windrower_reset,{
    reset("windrower")})
  
  
  
  
  
  #create interactive tables for equipment
  output$baler<-renderRHandsontable({
    rhandsontable(reactive_tables[["baler"]])
  })
  output$baling_tractor<-renderRHandsontable({
    rhandsontable((reactive_tables[["baling_tractor"]]))
  })
  output$loader_with_bale_grabber<-renderRHandsontable({
    rhandsontable((reactive_tables[["loader_with_bale_grabber"]]))
  })
  output$shredding_tractor<-renderRHandsontable({
    rhandsontable((reactive_tables[["shredding_tractor"]]))
  })
  output$stinger_for_bales<-renderRHandsontable({
    rhandsontable((reactive_tables[["stinger_for_bales"]]))
  })
  output$windrower<-renderRHandsontable({
    rhandsontable((reactive_tables[["windrower"]]))
  })
  
  
  #********************************************************************************************
  #This allows you to add a storage location by clicking on the map
  add_storage_location_armed<<-FALSE
  
  observeEvent(input$add_storage_location, {
    add_storage_location_armed<<-TRUE
    if (!("storage_locations" %in% display_layers)){
      storage_locations_temp<<-TRUE
      print("here")
      leafletProxy("map") %>% showGroup("storage_locations")
      
    }
  })
  
  observeEvent(input$map_click, {
    
    coords <- input$map_click
    proj4string <- "+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
    pj <- proj4::project(c(coords$lng,coords$lat), proj4string)
    print(pj)
    print(paste("armed",add_storage_location_armed))
    
    
    if (add_storage_location_armed){
      if ( !is.null(coords)   ) {
        proj4string <- "+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
        pj <- proj4::project(c(coords$lng,coords$lat), proj4string)
        print(paste("EPSG26914:",pj))
        lat=coords$lat
        lng=coords$lng
        command=paste("python python/add_storage_point.py",lat,lng)
        print(command)
        system(command)
        load_storage_locations()
        leafletProxy("map") %>% clearGroup("storage_locations") %>%
          addCircleMarkers(data=storage_locations,
                           layerId=~as.character(id),
                           radius=1,
                           color="black",
                           group="storage_locations") %>%
          removeMarker(plant_location_id) %>%
          addCircleMarkers(lat=plant_lat,lng=plant_lng,
                           group="storage_locations",
                           color="red",
                           radius=3,
                           layerId=plant_location_id)
        #if storage sites displayed temporarily for this procedure
        #hide them again
        if (storage_locations_temp){
          #leafletProxy("map") %>% hideGroup("storage_locations")
          storage_locations_temp<-FALSE
          
        }
        add_storage_location_armed<<-FALSE
        
        
        
      }
      
    }
  })
  
  #****************************************************************************************
 #For running simulations
  observeEvent(input$run_simulation, {
    
    withProgress(message = "Running Simulations", value = 0,{
  
      command="/home/diamond/anaconda2/bin/python python/run_simulations.py &"
      print(command)
      d=data.frame(time=0)
      print(d)
      write.csv(d,file="progress.csv",row.names=FALSE)
      prog=read.csv("progress.csv",stringsAsFactors=FALSE)
      print(prog)
      system(command)
      prog=read.csv("progress.csv",stringsAsFactors=FALSE)
      print("start checking")
      while (prog$time[nrow(prog)]<1){
        Sys.sleep(.5)
        print("checking")
        prog=read.csv("progress.csv",stringsAsFactors=FALSE)
        setProgress(prog$time[nrow(prog)])
        
      }
    })
    data<<-load_simulation_results()
    temp=data$roadside_lat
    data$roadside_lat=data$roadside_long
    data$roadside_long=temp
    
    load_transport_results()
    load_optimization_results()
    print("done")
    
    
  })
  
  #************************************************************************************************
  #This will create a polygon around a highway
  
  #This will allow user to draw a polygon on the map, which can be used to limit analysis spatially
  
  road_polygon_armed<<-FALSE
  road_polygon_done<<-FALSE
  area <- reactiveValues()
  polygon_roads<<-c()

  observeEvent(input$add_road_buffer, {
    road_polygon_armed<<-TRUE
    
  })


  # observeEvent(input$done_road_polygon, {
  #   road_polygon_armed<<-FALSE
  #   road_polygon_done<<-TRUE
  #   print("done")
  #   print(polygon_roads)
  #   sections=highway_shapes[highway_shapes@data$ID %in% polygon_roads,]
  #   road_buffer<<-mybuffer(sections,width=input$road_buffer*1000)
  #   leafletProxy("map") %>% addPolygons(data=road_buffer,group="road_buffer") 
  #   
  # })
  # 
  #observer for change in road buffer width
  observe({
    buffer_width=input$road_buffer
    print("change buffer width")
    if (road_polygon_done){
      print("inside")
      leafletProxy("map") %>% clearGroup("road_buffer")
      sections=highway_shapes[highway_shapes@data$ID %in% polygon_roads,]
      road_buffer<<-mybuffer(sections,width=buffer_width*1000)
      leafletProxy("map") %>% addPolygons(data=road_buffer,group="road_buffer")
    }
 })

  observeEvent(input$clear_road_buffers, {
    leafletProxy("map")  %>% clearGroup("road_buffer")
    leafletProxy("map")  %>% clearGroup("chosen_highways")
    road_polygon_armed<<-FALSE
    road_polygon_done<<-FALSE
    polygon_roads<<-c()

  })
   
   observeEvent(input$map_shape_click$id, {
     if (road_polygon_armed){
       pointId <- input$map_shape_click$id
       if (pointId %in% highway_shapes@data$ID){
         print(paste("clicked on ",pointId))
         polygon_roads<<-c(polygon_roads,pointId)
         print(polygon_roads)
         row=which(highway_shapes@data$ID==pointId)
         leafletProxy("map") %>% addPolylines(data=highway_shapes[row,],group="chosen_highways",weight=3,color="red")
         road_polygon_armed<<-FALSE
         road_polygon_done<<-TRUE
         print("done")
         print(polygon_roads)
         sections=highway_shapes[highway_shapes@data$ID %in% polygon_roads,]
         road_buffer<<-mybuffer(sections,width=input$road_buffer*1000)
         leafletProxy("map") %>% clearGroup("road_buffer") %>% addPolygons(data=road_buffer,group="road_buffer") 
       }
     }
   })
  
  #********************************************
   observeEvent(input$map_click, {
     
     coords <- input$map_click
     proj4string <- "+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
     pj <- proj4::project(c(coords$lng,coords$lat), proj4string)
     print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
     print(pj)
   })
  #*********************************************************************************
  #*********************************************************************************************
})
