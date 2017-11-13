



# We load the csv data to see how we are going to process them.

experiment<-read.csv("1N16_SP.csv", sep=";")


#We have to try to visualize the time for each one of the different parts of the data from the smart pen
#what we visualize is the start and the ending point
temp1           <- strptime(experiment$Anfang_0, "%H:%M:%S")
temp2           <- strptime(experiment$Ende_0, "%H:%M:%S")
task.time.diff  <- as.data.frame(as.numeric(difftime(temp2, temp1, units = "sec")))

  


# From library lubridate we calculate using hms(hourss minutes second the seconds of the beginning and ending)
library(lubridate)

task.time.strt <- as.data.frame(as.numeric(as.period(hms(experiment$Anfang_0), unit = "sec")))
task.time.end  <- as.data.frame(as.numeric(as.period(hms(experiment$Ende_0),   unit = "sec")))


colnames(task.time.strt)[1] <- ("start_sec")
colnames(task.time.end )[1] <- ("end_sec")
colnames(task.time.diff)[1] <- ("duration")



temp<- cbind(experiment$Bezeichnung, task.time.strt, task.time.end, task.time.diff)
colnames(temp)[1]  <- ("Bezeichnung")



#Conditional choice of specific rows that do not have NA values
exper.stages.table <-temp[!(is.na(temp$duration) ), ]



#Remove the rows of the data table we do not need
exper.stages.table <-exper.stages.table[!grepl(".*_Achse",   exper.stages.table$Bezeichnung),]
exper.stages.table <-exper.stages.table[!grepl(".*x_Gr.*e",  exper.stages.table$Bezeichnung),]
exper.stages.table <-exper.stages.table[!grepl(".*y_Gr.*e",  exper.stages.table$Bezeichnung),]
exper.stages.table <-exper.stages.table[!grepl(".*_Einheit", exper.stages.table$Bezeichnung),]
exper.stages.table <-exper.stages.table[!grepl(".*_Skala",   exper.stages.table$Bezeichnung),]
exper.stages.table <-exper.stages.table[!grepl(".*_Zahlen",  exper.stages.table$Bezeichnung),]
exper.stages.table <-exper.stages.table[!grepl("Zeichnung_.*_Gerade",  exper.stages.table$Bezeichnung),]





#At which cells are the starting time and finishing time of each stage of the experimentiment
start.Ab  <- which(exper.stages.table == "Messung_Ab_ges",    arr.ind = TRUE)
finish.Ab <- which(exper.stages.table == "Auswertung_Ab_ges", arr.ind = TRUE)
start.Al  <- which(exper.stages.table == "Messung_Al_ges",    arr.ind = TRUE)
finish.Al <- which(exper.stages.table == "Auswertung_Al_ges", arr.ind = TRUE)
start.Cu  <- which(exper.stages.table == "Messung_Cu_ges",    arr.ind = TRUE)
finish.Cu <- which(exper.stages.table == "Auswertung_Cu_ges", arr.ind = TRUE)




#Initialization of an empty dataframe 

total.time.stages <- data.frame(matrix(0,ncol = 4, nrow = 3))

colnames(total.time.stages)[1] <-('Bezeichnung')
colnames(total.time.stages)[2] <-('start_sec')
colnames(total.time.stages)[3] <-('end_sec' )
colnames(total.time.stages)[4] <-('duration')


 
total.time.stages[1,1] <-'Gesamt_Ab_'
total.time.stages[2,1] <-'Gesamt_Al_'
total.time.stages[3,1] <-'Gesamt_Cu_'

total.time.stages[1,2] <- exper.stages.table[start.Ab[1,1],2]
total.time.stages[1,3] <- exper.stages.table[finish.Ab[nrow(finish.Ab),1],3]

total.time.stages[2,2] <- exper.stages.table[start.Al[1,1],2]
total.time.stages[2,3] <- exper.stages.table[finish.Al[nrow(finish.Al),1],3]

total.time.stages[3,2] <- exper.stages.table[start.Cu[1,1],2]
total.time.stages[3,3] <- exper.stages.table[finish.Cu[nrow(finish.Cu),1],3]




# Total time for each stage to be completed in seconds
total.time.stages <- transform(total.time.stages, duration = total.time.stages$end_sec - total.time.stages$start_sec )




#Create categories so that we can distinguish between the results on the plots


exper.stages.table$stage <- 0
exper.stages.table$stage <- exper.stages.table$Bezeichnung

exper.stages.table$stage <- gsub(".*_Ab_.*"                 , "Abstand"      ,exper.stages.table$stage)
exper.stages.table$stage <- gsub(".*_Al_.*"                 , "Aluminium"     ,exper.stages.table$stage)
exper.stages.table$stage <- gsub(".*_Cu_.*"                 , "Kupfer"        ,exper.stages.table$stage)
exper.stages.table$stage <- gsub(".*Vorbesprechung*"        , "Vorbesprechung",exper.stages.table$stage)
exper.stages.table$stage <- gsub("Vergleich_Proportional.*" , "VergleichP"    ,exper.stages.table$stage)



#Create column substages so that we can use the labels for the visualization

exper.stages.table$sub.stage <- 0
exper.stages.table$sub.stage <- exper.stages.table$Bezeichnung

exper.stages.table$sub.stage <- gsub("Messung_.*"               , "Messung"   ,exper.stages.table$sub.stage)
exper.stages.table$sub.stage <- gsub("Auswertung_.*"            , "Auswertung",exper.stages.table$sub.stage)
exper.stages.table$sub.stage <- gsub("Zeichnung_.*"             , "Zeichnung" ,exper.stages.table$sub.stage)
exper.stages.table$sub.stage <- gsub("Berechnung_.*"            , "Berechnung",exper.stages.table$sub.stage)
exper.stages.table$sub.stage <- gsub("Vergleich_Proportional.*" , "VergleichP",exper.stages.table$sub.stage)




# This must be changed so we can have only the rows we need
exper.stages.table <-exper.stages.table[!grepl("Zeichnung_.*_Gerade",   exper.stages.table$Bezeichnung),]
exper.stages.table <-exper.stages.table[!grepl("Erkl.*rung",   exper.stages.table$Bezeichnung),]



# Convert stages and substages to factor so we can customize their levels. It will be needed for the visualization
exper.stages.table$stage = factor(exper.stages.table$stage,levels         = c('Vorbesprechung','Abstand','Aluminium','Kupfer','VergleichP'))
exper.stages.table$sub.stage = factor(exper.stages.table$sub.stage,levels = c('Vorbesprechung', 'Auswertung', 'Zeichnung', 'Berechnung', 'Messung', 'VergleichP'))





################## Customize the size of each facet####################
 
 #PLOT





 library(ggplot2)

# We set the colors to be the same as the differnet categories we are going to present
# we have to set the same number of levels. We do that by scale_colour_manual

myColors <- c('#1972A4', '#59DB4E', '#CFDB4E', '#F2B85C', '#C64A4A',"#65ECEF")
names(myColors) <- levels(exper.stages.table$sub.stage)
colScale <- scale_colour_manual(name = "sub.stage",values = myColors)


 
 pl<-ggplot(exper.stages.table) +
   scale_y_discrete(name ="Arbeitet Schritte")+
   scale_x_continuous(name ="Versuchszeit (sec)",expand = c(0, 0), limits = c(0,exper.stages.table$end_sec[nrow(exper.stages.table)]+100))+
   theme(legend.position = "None") +
   geom_segment(aes(x = start_sec, y = sub.stage, xend = end_sec, yend = sub.stage,
                    color = sub.stage, size = 4)) +
   facet_wrap(stage ~., ncol = 1, nrow = 5) +
   facet_grid(stage ~.,scales = "free",space = "free") +
   theme(strip.text.y = element_text(angle = 0))+
   guides(size = 'none')+
   colScale 
 
 
 #gesamtdaves
