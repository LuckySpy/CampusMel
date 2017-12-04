


# # We load the csv data to see how we are going to handle them.
# 
# experiment<-read.csv("1N16_SP.csv", sep=";")
# 
# 
# # we take only the rows that have actual data
# experiment$Bezeichnung <- as.character(experiment$Bezeichnung)
# experiment             <- subset(experiment, nchar(experiment$Bezeichnung)> 0)
# 
# 
# 
# 
# #We have to try to visualize the time for each one of the different parts of the data from the smart pen
# #what we visualize is the start and the ending point
# temp1           <- strptime(experiment$Anfang_0, "%H:%M:%S")
# temp2           <- strptime(experiment$Ende_0, "%H:%M:%S")
# task.time.diff  <- as.data.frame(as.numeric(difftime(temp2, temp1, units = "sec")))
# 
# 
# 
# 
# # From library lubridate we calculate using hms(hours minutes second the seconds of the beginning and ending)
# library(lubridate)
# 
# task.time.strt <- as.data.frame(as.numeric(as.period(hms(experiment$Anfang_0), unit = "sec")))
# task.time.end  <- as.data.frame(as.numeric(as.period(hms(experiment$Ende_0),   unit = "sec")))
# 
# 
# colnames(task.time.strt)[1] <- ("start_sec")
# colnames(task.time.end )[1] <- ("end_sec")
# colnames(task.time.diff)[1] <- ("duration")
# 
# 
# 
# temp<- cbind(experiment$Bezeichnung, task.time.strt, task.time.end, task.time.diff)
# colnames(temp)[1]  <- ("Bezeichnung")
# 
# 
# #We filter the tables so we can keep only the information that we are going to use
# 
# exper.steps.table.all <- data.frame(temp,experiment$Abstand..cm.,experiment$Absorber.dicke..cm.,experiment$Zeitein.stellung..s.)
# exper.steps.table.all <- exper.steps.table.all[!with(exper.steps.table.all,is.na(start_sec) & is.na(duration) & is.na(end_sec) ),]
# 
# exper.steps.table.ind <-exper.steps.table.all[!complete.cases(exper.steps.table.all[ , 2]),]  
# exper.steps.table.ind$start_sec <-NULL
# exper.steps.table.ind$duration  <-NULL
# 
# 
# colnames(exper.steps.table.ind)[2] <- ("time_marked")
# colnames(exper.steps.table.ind)[3] <- ("Abstand..cm.")
# colnames(exper.steps.table.ind)[4] <- ("Absorber.dicke..cm.")
# colnames(exper.steps.table.ind)[5] <- ("Zeitein.stellung..s.")






#Isolation of each different stage of the experiment (Abstand, Copper, Aluminium)

# 
# exper.abstand   <-exper.steps.table.ind[grepl(".*_Ab",   exper.steps.table.ind$Bezeichnung),]
# exper.aluminium <-exper.steps.table.ind[grepl(".*_Al",   exper.steps.table.ind$Bezeichnung),]
# exper.kupfer    <-exper.steps.table.ind[grepl(".*_Cu",   exper.steps.table.ind$Bezeichnung),]






#------> we may need the settings of the distances to make more clear the visualization. (350, 290, 248, 190....)
#either we could discard them. or we can assign them to the closest ones



#######load the arduino data#######
library(tidyverse)

#  We load the csv data from SP and arduino
experiment    <- fread('./voll/1N16/1N16_SP.csv')
# exper.arduino <- fread('./voll/1N16/1N16_AR.csv')
exper.arduino       <-read.csv("./voll/1N16/1N16_AR.csv", sep=";")

plot.name  <-"1N16_AR"
colnames(exper.arduino) <- c("matt_1","matt_2","matt_3","matt_4","matt_5","distance_mm","measur_status")



# We add the column real.sec that contains the real second that pass since the arduino started working
exper.arduino$real_sec <- 1:nrow(exper.arduino) 




# Select only the rows of the individual stages(abstand 0, aluminium 2, Kupfer 1) and put them in separate tables table
exper.ard.abstand   <- exper.arduino[with(exper.arduino, matt_1 == 0  &  matt_2 == 0 & matt_3 == 0 & matt_4 == 0 & matt_5 == 0),]
exper.ard.aluminium <- exper.arduino[with(exper.arduino, matt_1 == 2  |  matt_2 == 2 | matt_3 == 2 | matt_4 == 2 | matt_5 == 2),]
exper.ard.kupfer    <- exper.arduino[with(exper.arduino, matt_1 == 1  |  matt_2 == 1 | matt_3 == 1 | matt_4 == 1 | matt_5 == 1),]



exper.ard.abstand$actual_order   <- 1
exper.ard.aluminium$actual_order <- 1
exper.ard.kupfer$actual_order    <- 1

for (i in 2:length(exper.ard.abstand$real_sec)){
  
  exper.ard.abstand$actual_order[i] <- exper.ard.abstand$real_sec[i] - exper.ard.abstand$real_sec[i-1]
}



for (i in 2:length(exper.ard.aluminium$real_sec)){
  
  exper.ard.aluminium$actual_order[i] <- exper.ard.aluminium$real_sec[i] - exper.ard.aluminium$real_sec[i-1]
  
}

for (i in 2:length(exper.ard.kupfer$real_sec)){
  
  exper.ard.kupfer$actual_order[i] <- exper.ard.kupfer$real_sec[i] - exper.ard.kupfer$real_sec[i-1]
  
}




##############Noisy recordings######################





# Find the frequency of each distance so that we can see which are the faulty ones

  # limit.dist           <- as.data.frame(table(exper.ard.abstand$distance_mm)) %>% filter(Freq > 10) %>%  select(diff_dist= Var1) 
  # 
  # limit.dist$diff_dist <- as.matrix(as.numeric(as.character(limit.dist$diff_dist)))
  # this doesnt work because we have some numbers very close to each other

  
limit.dist<-matrix(c(60,110, 154, 179, 199, 248, 260, 290, 350))
colnames(limit.dist) <- c("distance_mm")

  
  
  
  
  
# The noisy disntaces that have been recorded during the experiment
  
  correct.dist <- as.data.frame( as.numeric(exper.ard.abstand$distance_mm))
  colnames(correct.dist) <- c("distance_mm")
  
 
   
# Replace the noisy distance because of vibrant movements into the standard ones predefined(350, 190, 55, ....)

for (i in 1:nrow(correct.dist)){

  minim <- 100

  for(j in 1:nrow(limit.dist)){

    if (minim > abs(correct.dist[i,1] - limit.dist[j,1]))
    {
      minim <- abs(correct.dist[i,1] - limit.dist[j,1])
      index <- j
    }
  }

  correct.dist[i,1] <- limit.dist[index,1]
}



exper.ard.abstand$distance_mm <- correct.dist$distance_mm

exper.ard.abstand$distance_cm <-  exper.ard.abstand %>% transmute(distance_mm /10)


###################VISUALIZATION##################################

library(plotly)

p <- ggplot(exper.ard.abstand, aes( real_sec, distance_mm))
p + geom_point(shape = 15,size = 0.5, aes(colour = factor(measur_status))) 
# +   xlim(2200, 3750)











#####################OTHER################################

#using the above results we can separate the stages and the ones that they are within the time of other stages for the visualization

# # Add the relative second column so we can discriminate between actual stages of the experiment
# exper.ard.abstand$relative.sec   <- 1:nrow(exper.ard.abstand) 
# exper.ard.aluminium$relative.sec <- 1:nrow(exper.ard.aluminium) 
# exper.ard.kupfer$relative.sec    <- 1:nrow(exper.ard.kupfer) 
# 
# 
# exper.ard.abstand$actual_sec   <- exper.ard.abstand$real.sec   - (exper.ard.abstand$real.sec[1]   - 1)
# exper.ard.aluminium$actual_sec <- exper.ard.aluminium$real.sec - (exper.ard.aluminium$real.sec[1] - 1)
# exper.ard.kupfer$actual_sec    <- exper.ard.kupfer$real.sec    - (exper.ard.kupfer$real.sec[1]    - 1)






# time.arduino.abstand   <- data.frame(exper.ard.abstand$real.sec,   exper.ard.abstand$relative.sec)
# time.arduino.aluminium <- data.frame(exper.ard.aluminium$real.sec, exper.ard.aluminium$relative.sec)
# time.arduino.kupfer    <- data.frame(exper.ard.kupfer$real.sec,    exper.ard.kupfer$relative.sec)
# 
# 
# 
# colnames(time.arduino.abstand)   <- c("real.sec","relative.sec")
# colnames(time.arduino.aluminium) <- c("real.sec","relative.sec")
# colnames(time.arduino.kupfer)    <- c("real.sec","relative.sec")
# 
# 
# time.arduino.abstand$actual_sec   <- time.arduino.abstand$real.sec   - (time.arduino.abstand$real.sec[1]   - 1)
# time.arduino.aluminium$actual_sec <- time.arduino.aluminium$real.sec - (time.arduino.aluminium$real.sec[1] - 1)
# time.arduino.kupfer$actual_sec    <- time.arduino.kupfer$real.sec    - (time.arduino.kupfer$real.sec[1]    - 1)









######################This is to compute the total thikness of the absorbers that were placed in one of the five slots###############
thik_table <-  exper.arduino  

#matching the material with the coresponding thikness of the 5 tracks

thiknify <- function (matt,track)
{
  if (matt == 1){
    switch(track,
           0.1,
           0.2,
           0.25,
           0.3,
           0.5
    )
  }
  else {
    switch(track,
           0.05,
           0.2,
           0.25,
           0.4,
           0.5
    )
  }
}



#match each track to its corresponding thiknes

for (i in 1:length(exper.arduino$matt_1)){
  for (j in 1:5){
    
    if (exper.arduino[i,j]== 1){
      thik_table[i,j]<-  thiknify(i,j)
    }  
    
    if (exper.arduino[i,j]== 2){
      thik_table[i,j]<-  thiknify(i,j)
    }
    else {
      thik_table[i,j] <- 0
      
    }
  }
}



#we calculate the thikness of all materials that they are present in the tracks during the expiriment
thik_table$thik_sum <- rowSums(thik_table[, c(1, 2, 3, 4, 5)])
  
  