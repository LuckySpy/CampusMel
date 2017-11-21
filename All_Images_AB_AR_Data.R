
library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)


visualizeAB<-function(directory.name, file.name.SP, file.name.AR, plot.name, image.directory){
  
  
  # We load the csv data to see how we are going to process them.
  
  if(!dir.exists(directory.name)){
    
    return("No Results") }
  
  else{
   
    ################### THE TABLES TO BE COMBINED FROM SP DATA AND ARDUINO #############################################
    
    
    #  We load the csv data from SP and arduino
    experiment    <- fread(file.name.SP)
    exper.arduino <- fread(file.name.AR)
    plot.name     <-plot.name
    
    
    # we take only the rows that have actual data
    experiment$Bezeichnung <- as.character(experiment$Bezeichnung)
    experiment             <- subset(experiment, nchar(experiment$Bezeichnung)> 0)
    
    
    
    
    # #We have to try to visualize the time for each one of the different parts of the data from the smart pen
    # #what we visualize is the start and the ending point
    temp1           <- strptime(experiment$Anfang_0, "%H:%M:%S")
    temp2           <- strptime(experiment$Ende_0, "%H:%M:%S")
    task.time.diff  <- as.data.frame(as.numeric(difftime(temp2, temp1, units = "sec")))
    
    
    
    
    # We calculate the time in sec using hms(hours minutes second the seconds of the beginning and ending)
    
    task.time.strt <- as.data.frame(as.numeric(as.period(hms(experiment$Anfang_0), unit = "sec")))
    task.time.end  <- as.data.frame(as.numeric(as.period(hms(experiment$Ende_0),   unit = "sec")))
    
    
    colnames(task.time.strt)[1] <- ("start_sec")
    colnames(task.time.end )[1] <- ("end_sec")
    colnames(task.time.diff)[1] <- ("duration")
    
    
    
    temp               <- cbind(experiment$Bezeichnung, task.time.strt, task.time.end, task.time.diff)
    colnames(temp)[1]  <- ("Bezeichnung")
    
    
    #################### Process the table so we can have data we need for each individual stage of the experiment##########################
    
    
    # Put together in a dataframe the columns we are interested in
    
    exper.steps.table.all <- data.frame(  temp,`Abstand [cm]`  = experiment$`Abstand [cm]`, `Absorber-dicke [cm]` = experiment$`Absorber-dicke [cm]`, 
                                          `Zeitein-stellung [s]` = experiment$`Zeitein-stellung [s]`, `Benutzt in Ausw.` = experiment$`Benutzt in Ausw.`)
    
    exper.steps.table.all <- exper.steps.table.all[!with(exper.steps.table.all,is.na(start_sec) & is.na(duration) & is.na(end_sec) ),]
    
    
    
    #We drop some columns which are not useful for the individual stages, so we can create tables for each stage of the experiment
    
    exper.steps.table.ind <-exper.steps.table.all[!complete.cases(exper.steps.table.all[ , 2]),]
    exper.steps.table.ind$start_sec <-NULL
    exper.steps.table.ind$duration  <-NULL
    
    
    
    #Isolation of each different stage of the experiment (Abstand, Copper, Aluminium)
    
    exper.abstand   <-exper.steps.table.ind[grepl(".*_Ab",   exper.steps.table.ind$Bezeichnung),]
    exper.aluminium <-exper.steps.table.ind[grepl(".*_Al",   exper.steps.table.ind$Bezeichnung),]
    exper.kupfer    <-exper.steps.table.ind[grepl(".*_Cu",   exper.steps.table.ind$Bezeichnung),]
    
    
    
    
    # The first and last recordings of the stages(Ab, Al, Cu) based on the smart pen data
    
    first.record.sm.ab <- select(exper.abstand,Bezeichnung,end_sec) %>% filter( Bezeichnung == 'Messung_Ab') %>% select(end_sec) %>% head(n=1) 
    last.record.sm.ab  <- select(exper.abstand,Bezeichnung,end_sec) %>% filter( Bezeichnung == 'Messung_Ab') %>% select(end_sec) %>% tail(n=1)
    
    first.record.sm.al <- select(exper.aluminium,Bezeichnung,end_sec) %>% filter( Bezeichnung == 'Messung_Al') %>% select(end_sec) %>% head(n=1)
    last.record.sm.al  <- select(exper.aluminium,Bezeichnung,end_sec) %>% filter( Bezeichnung == 'Messung_Al') %>% select(end_sec) %>% tail(n=1)
    
    first.record.sm.cu <- select(exper.kupfer,Bezeichnung,end_sec) %>% filter( Bezeichnung == 'Messung_Cu') %>% select(end_sec) %>% head(n=1)
    last.record.sm.cu  <- select(exper.kupfer,Bezeichnung,end_sec) %>% filter( Bezeichnung == 'Messung_Cu') %>% select(end_sec) %>% tail(n=1)
    
    
    
    
    
    #################### ARDUINO DATA ######################
    colnames(exper.arduino) <- c("matt_1","matt_2","matt_3","matt_4","matt_5","distance_mm","measur_status")
    
    
    # We add the column real.sec that contains the real second that pass since the arduino started working
    exper.arduino$real_sec <- 1:nrow(exper.arduino) 
    
    
    # Select only the rows of the individual stages(abstand 0, aluminium 2, Kupfer 1) and put them in separate tables table
    
    
    exper.ard.abstand   <- filter(exper.arduino, real_sec > (first.record.sm.ab[1,1] - 120)  &  (real_sec < last.record.sm.ab[1,1] + 120) )
    
    exper.ard.aluminium <- filter(exper.arduino, real_sec > (first.record.sm.al[1,1] - 120)  &  (real_sec < last.record.sm.al[1,1] + 120) )
    
    exper.ard.kupfer    <- filter(exper.arduino, real_sec > (first.record.sm.cu[1,1] - 120)  &  (real_sec < last.record.sm.cu[1,1] + 120) )
    
    
    
    #Transform the mm into cm and add a column to the exper.ard.abstand
    
    exper.ard.abstand <- exper.ard.abstand %>% transmute(distance_cm = distance_mm /10) %>% cbind(exper.ard.abstand)
  
   
    
    ###################COMBINATION OF ARDUINO AND SMARTPEN DATA AND VISUALIZATION####################
    
    
    
    
    # Extract the unique "real" distances during the active periods of the arduino
    
    exp.distances <-  filter( exper.ard.abstand, measur_status==1) %>% unique()
    exp.distances <- unique(exp.distances$distance_cm) %>% as.data.frame()
    colnames(exp.distances)[1]<-"distances"
    
    
    
    # Extract only the columns we need from thee SP data for the Abstand
    
    visual.abstand <- filter(exper.abstand, Bezeichnung == 'Messung_Ab') 
    visual.abstand <- cbind(end_sec= visual.abstand$end_sec, Abstand..cm. = visual.abstand$Abstand..cm., 
                            Benutzt.in.Ausw. = visual.abstand$Benutzt.in.Ausw.) %>% as.data.frame()
    
    
    
    #Divide the tables produced in the 4 different categories we are going to visualize
    
    visual.abstand.1    <- filter(visual.abstand, visual.abstand$Benutzt.in.Ausw. == 1)
    visual.abstand.0    <- filter(visual.abstand, visual.abstand$Benutzt.in.Ausw. == 0)
    exper.ard.abstand.1 <- filter(exper.ard.abstand, exper.ard.abstand$measur_status == 1)
    exper.ard.abstand.0 <- filter(exper.ard.abstand, exper.ard.abstand$measur_status == 0)
    
    
    
    # Correct the fluctating values
    
    # temp1<-unique(exper.ard.abstand.1$distance_cm) %>% as.data.frame()
    
    
    
    
    # Visualization of the desired 4 categories that mentioned above
    
    p <- ggplot() +
      ggtitle(plot.name) +    
      # inactive periods
      geom_point(data = exper.ard.abstand.0, aes(x= real_sec, y= distance_cm),   colour = 'blue', shape = 15, size = 0.5) + 
      # active periods
      geom_point(data = exper.ard.abstand.1, aes(x= real_sec, y= distance_cm),   colour = 'coral', shape = 15, size = 0.5 ) +  
      # measurements not considered
      geom_point(data = visual.abstand.0,    aes(x= end_sec, y= Abstand..cm.),   colour = 'red', shape = 10, size = 10 ) + 
      # measurements considered
      geom_point(data = visual.abstand.1,    aes(x = end_sec, y = Abstand..cm.), colour = 'green', shape = 10, size = 10 )+
      
      scale_y_continuous(name ="Abstand in cm",expand=c(0,0),limits = c(0,40)) +        # The limits (strart and finish) of y axes which are discrete values
      scale_x_continuous(name ="Versuchszeit in Sekunden")
    
    
    


    #saving the plot in pdf file
    plot.name=paste(plot.name, ".jpg")
    ggsave(plot.name, plot = last_plot(), path = image.directory)


}}
  
  