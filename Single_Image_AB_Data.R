library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)



################### THE TABLES TO BE COMBINED FROM SP DATA AND ARDUINO #############################################


# Function to check weather there is a measurement that hasn't been recorded
warnUnrecorded <- function(df){
  
  check<- any((df$matt_1 == 0 & df$matt_2 == 0 & df$matt_3 == 0 & df$matt_4 == 0 & df$matt_5 == 0) & df$measur_status == 1)
  
  check<- any((df$matt_1 == 2 | df$matt_2 == 2 | df$matt_3 == 2 | df$matt_4 == 2 | df$matt_5 == 2) & df$measur_status == 1)
  
  check<- any((df$matt_1 == 1 | df$matt_2 == 1 | df$matt_3 == 1 | df$matt_4 == 1 | df$matt_5 == 1) & df$measur_status == 1)
  
  return(check)
  
}





#  We load the csv data from SP and arduino
experiment    <- read.csv('./Daten/voll/1D19/1D19_SP.csv',sep = ";")
exper.arduino <- fread('./Daten/voll/1D19/1D19_AR.csv')
plot.name  <-"1D19"


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
first.record.sm.ab <- min(exper.abstand$end_sec)
last.record.sm.ab  <- max(exper.abstand$end_sec)

first.record.sm.al <- min(exper.aluminium$end_sec)
last.record.sm.al  <- max(exper.aluminium$end_sec)

first.record.sm.cu <- min(exper.kupfer$end_sec)
last.record.sm.cu  <- max(exper.kupfer$end_sec)



#################### ARDUINO DATA ######################
colnames(exper.arduino) <- c("matt_1","matt_2","matt_3","matt_4","matt_5","distance_mm","measur_status")


# We add the column real.sec that contains the real second that pass since the arduino started working
exper.arduino$real_sec <- 1:nrow(exper.arduino) 


# Select only the rows of the individual stages(abstand 0, aluminium 2, Kupfer 1) and put them in separate tables table
exper.ard.abstand   <- filter(exper.arduino, real_sec > (first.record.sm.ab - 150)  &  (real_sec < last.record.sm.ab + 150) )

exper.ard.aluminium <- filter(exper.arduino, real_sec > (first.record.sm.al - 150)  &  (real_sec < last.record.sm.al + 150) )

exper.ard.kupfer    <- filter(exper.arduino, real_sec > (first.record.sm.cu - 150)  &  (real_sec < last.record.sm.cu + 150) )



#Transform the mm into cm and add a column to the exper.ard.abstand
if(grepl("[1-8]O[1-31]",plot.name)){
  
  colnames(exper.ard.abstand)[colnames(exper.ard.abstand)=="distance_mm"] <- "distance_cm"
}else{
  exper.ard.abstand <- exper.ard.abstand %>% transmute(distance_cm = distance_mm /10) %>% cbind(exper.ard.abstand)}





############################### WARNING ABOUT UNRECORDED ACTIVE STAGES FROM THE SP DATA ##################################

# Set a warning that we had an arduino measurement that was not recorded with the SP data in between the different stages
idle.time.exper.ard.abstand.before <- filter(exper.arduino, (real_sec <= (first.record.sm.ab - 120)))
idle.time.exper.ard.abstand.after  <- filter(exper.arduino, (real_sec >= (last.record.sm.ab + 120)) &  (real_sec <= (first.record.sm.al - 120)))
idle.time.exper.ard.abstand        <- rbind(idle.time.exper.ard.abstand.before,idle.time.exper.ard.abstand.after)

unrecorded.exper.ard.abstand <- warnUnrecorded(idle.time.exper.ard.abstand)

idle.time.exper.ard.aluminium.before <- filter(exper.arduino, (real_sec >= (last.record.sm.ab + 120)) & (real_sec <= (first.record.sm.al - 120)))
idle.time.exper.ard.aluminium.after  <- filter(exper.arduino, (real_sec >= (last.record.sm.al + 120)) & (real_sec <= (first.record.sm.cu - 120)))
idle.time.exper.ard.aluminium        <- rbind(idle.time.exper.ard.aluminium.before,idle.time.exper.ard.aluminium.after)

unrecorded.exper.ard.aluminium <- warnUnrecorded(idle.time.exper.ard.aluminium)

idle.time.exper.ard.kupfer.before <- filter(exper.arduino, (real_sec >= (last.record.sm.al + 120)) &  (real_sec <= (first.record.sm.cu - 120)))
idle.time.exper.ard.kupfer.after  <- filter(exper.arduino, (real_sec > (last.record.sm.cu + 120)))
idle.time.exper.ard.kupfer        <- rbind(idle.time.exper.ard.kupfer.before,idle.time.exper.ard.kupfer.after)

unrecorded.exper.ard.kupfer <- warnUnrecorded(idle.time.exper.ard.kupfer)

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

p <- ggplot() +
  
  ggtitle(plot.name) +
  # inactive periods
  geom_point(data = exper.ard.abstand.0, aes(x= real_sec, y= distance_cm),   colour = '#00549F', shape = 15, size = 0.5) + 
  # active periods
  geom_point(data = exper.ard.abstand.1, aes(x= real_sec, y= distance_cm),   colour = '#6FDC6F', shape = 15, size = 0.5 ) +  
  # measurements not considered
  geom_point(data = visual.abstand.0,    aes(x= end_sec, y= Abstand..cm.),   colour = '#CC071E', shape = 10, size = 10 ) + 
  # measurements considered
  geom_point(data = visual.abstand.1,    aes(x = end_sec, y = Abstand..cm.), colour = '#57AB27', shape = 10, size = 10 )+
  # The limits (strart and finish) of y axes which are discrete values
  scale_y_continuous(name ="Abstand in cm",expand=c(0,0),limits = c(0,40)) +        
  scale_x_continuous(name ="Versuchszeit in Sekunden")
  



#saving the plot in pdf file
plot.name=paste(plot.name,'_AB',".jpg")
#ggsave(plot.name, plot = last_plot(), width = 7, height = 3,path = './Bilder/MessverlaufAB')

ggsave(plot.name, plot = last_plot(), width = 7, height = 3)

  
  