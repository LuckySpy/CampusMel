library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)

################### THE TABLES TO BE COMBINED FROM SP DATA AND ARDUINO #############################################


#Functions to be used
thiknify <- function (matt, track, exp.num)
{
  if (matt == 1 & (exp.num == 1 |exp.num == 3 | exp.num == 7 | exp.num == 8)){
    switch(track,
           0.003,
           0.005,
           0.01,
           0.05,
           0.08
    )
  }
  
  else if(matt == 1 & (exp.num == 2 |exp.num == 4 | exp.num == 5)) {
    switch(track,
           0.0025,
           0.005,
           0.01,
           0.03,
           0.06
    )
  }
  
  else if(matt == 1 & (exp.num == 6)) {
    switch(track,
           0.03,
           0.01,
           0.005,
           0.06,
           0.0025
    )
  }
  
  else{ 
    switch(track,
           0.01,
           0.05,
           0.1,
           0.2,
           0.3
    )
  }
}



# Function to check weather there is a measurement that hasn't been recorded
warnUnrecorded <- function(df){
  check<- any((df$matt_1 == 0 & df$matt_2 == 0 & df$matt_3 == 0 & df$matt_4 == 0 & df$matt_5 == 0) & df$measur_status == 1)
  
  check<- any((df$matt_1 == 2 | df$matt_2 == 2 | df$matt_3 == 2 | df$matt_4 == 2 | df$matt_5 == 2) & df$measur_status == 1)
  
  check<- any((df$matt_1 == 1 | df$matt_2 == 1 | df$matt_3 == 1 | df$matt_4 == 1 | df$matt_5 == 1) & df$measur_status == 1)
  
  return(check)
  
}


#  We load the csv data from SP and arduino
experiment    <- fread('./Daten/voll/3J24/3J24_SP.csv')
exper.arduino <- read.csv('./Daten/voll/3J24/3J24_AR.csv', sep = ";")
plot.name  <-"3J24"

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
exper.aluminium <-exper.steps.table.ind[grepl(".*_Al",   exper.steps.table.ind$Bezeichnung),]

# The first and last recordings of the stages(Ab, Al, Cu) based on the smart pen data
first.record.sm.al <- min(exper.aluminium$end_sec)
last.record.sm.al  <- max(exper.aluminium$end_sec)


#################### ARDUINO DATA ######################
colnames(exper.arduino) <- c("matt_1","matt_2","matt_3","matt_4","matt_5","distance_mm","measur_status")

# We add the column real.sec that contains the real second that pass since the arduino started working
exper.arduino$real_sec <- 1:nrow(exper.arduino) 

#Create the table thik_table that consists the thikness of the material in each track (1-5)
thik_table<-cbind(matt_1 = exper.arduino$matt_1, matt_2 = exper.arduino$matt_2, matt_3 = exper.arduino$matt_3, matt_4= exper.arduino$matt_4,
                  matt_5 = exper.arduino$matt_5  ) %>% as.data.frame()

# Retrieve the number of the experiment from the string name
experiment.num <- substr(plot.name, 1, 1) %>% as.numeric()

# Correspond the material and the thikness for each track and fill the df with the proper thikness
for (i in 1:nrow(exper.arduino)){
  for (j in 1:5){
    
    if (exper.arduino[i,j] == 1){
      thik_table[i,j] <- 0
    }  
    
    else if (exper.arduino[i,j] == 2){
      thik_table[i,j]<-  thiknify(2,j,experiment.num)
    }
    else {
      thik_table[i,j] <- 0
      
    }
  }
}

# Add an extra column to exper.arduino that contains the sum of the thikness of all the tracks
exper.arduino$dicke_ges <-  rowSums(thik_table[, c(1, 2, 3, 4, 5)])

# Select only the rows of the individual stages(abstand 0, aluminium 2, Kupfer 1) and put them in separate tables table
exper.ard.aluminium <- filter(exper.arduino, real_sec > (first.record.sm.al - 150)  &  (real_sec < last.record.sm.al + 150) )

############################### WARNING ABOUT UNRECORDED ACTIVE STAGES FROM THE SP DATA ##################################

# Set a warning that we had an arduino measurement that was not recorded with the SP data in between the different stages
# idle.time.exper.ard.abstand.before <- filter(exper.arduino, (real_sec <= (first.record.sm.ab - 120)))
# idle.time.exper.ard.abstand.after  <- filter(exper.arduino, (real_sec >= (last.record.sm.ab + 120)) &  (real_sec <= (first.record.sm.al - 120)))
# idle.time.exper.ard.abstand        <- rbind(idle.time.exper.ard.abstand.before,idle.time.exper.ard.abstand.after)
# 
# unrecorded.exper.ard.abstand <- warnUnrecorded(idle.time.exper.ard.abstand)
# 
# idle.time.exper.ard.aluminium.before <- filter(exper.arduino, (real_sec >= (last.record.sm.ab + 120)) & (real_sec <= (first.record.sm.al - 120)))
# idle.time.exper.ard.aluminium.after  <- filter(exper.arduino, (real_sec >= (last.record.sm.al + 120)) & (real_sec <= (first.record.sm.cu - 120)))
# idle.time.exper.ard.aluminium        <- rbind(idle.time.exper.ard.aluminium.before,idle.time.exper.ard.aluminium.after)
# 
# unrecorded.exper.ard.aluminium <- warnUnrecorded(idle.time.exper.ard.aluminium)
# 
# idle.time.exper.ard.kupfer.before <- filter(exper.arduino, (real_sec >= (last.record.sm.al + 120)) &  (real_sec <= (first.record.sm.cu - 120)))
# idle.time.exper.ard.kupfer.after  <- filter(exper.arduino, (real_sec > (last.record.sm.cu + 120)))
# idle.time.exper.ard.kupfer        <- rbind(idle.time.exper.ard.kupfer.before,idle.time.exper.ard.kupfer.after)
# 
# unrecorded.exper.ard.kupfer <- warnUnrecorded(idle.time.exper.ard.kupfer)


###################COMBINATION OF ARDUINO AND SMARTPEN DATA AND VISUALIZATION####################

# Extract only the columns we need from the SP data for the Abstand
visual.aluminium<- filter(exper.aluminium, Bezeichnung == 'Messung_Al') 
visual.aluminium <- cbind(end_sec= visual.aluminium$end_sec, Absorber.dicke..cm. = visual.aluminium$Absorber.dicke..cm., 
                          Benutzt.in.Ausw. = visual.aluminium$Benutzt.in.Ausw.) %>% as.data.frame()

#Divide the tables produced in the 4 different categories we are going to visualize
visual.aluminium.1    <- filter(visual.aluminium, visual.aluminium$Benutzt.in.Ausw. == 1)
visual.aluminium.0    <- filter(visual.aluminium, visual.aluminium$Benutzt.in.Ausw. == 0)
exper.ard.aluminium.1 <- filter(exper.ard.aluminium, exper.ard.aluminium$measur_status == 1)
exper.ard.aluminium.0 <- filter(exper.ard.aluminium, exper.ard.aluminium$measur_status == 0)


#Illustrate the results via a plot
p <- ggplot() +
  # name of the plot
  ggtitle(plot.name) +
  # inactive periods
  geom_point(data = exper.ard.aluminium.0, aes(x= real_sec, y= dicke_ges),   colour = '#3F3F3F', shape = 15, size = 0.5) + 
  # active periods
  geom_point(data = exper.ard.aluminium.1, aes(x= real_sec, y= dicke_ges),   colour = '#6FDC6F', shape = 15, size = 0.5 ) +  
  # measurements not considered
  geom_point(data = visual.aluminium.0,    aes(x= end_sec, y= Absorber.dicke..cm.),   colour = '#CC071E', shape = 10, size = 10 ) + 
  # measurements considered
  geom_point(data = visual.aluminium.1,    aes(x = end_sec, y = Absorber.dicke..cm.), colour = '#57AB27', shape = 10, size = 10 )+
  # The limits (strart and finish) of y axes which are discrete values
  scale_y_continuous(name ="Dicke in mm",expand=c(0,0),limits = c(-0.02,max(exper.ard.aluminium$dicke_ges)+0.06)) +        
  scale_x_continuous(name ="Versuchszeit in Sekunden")



#saving the plot in pdf file
plot.name=paste(plot.name,'_AL',".jpg")
#ggsave(plot.name, plot = last_plot(), width = 7, height = 3,path = './Bilder/MessverlaufAL')

ggsave(plot.name, plot = last_plot(), width = 7, height = 3)



