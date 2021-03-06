
library(tidyverse)
library(data.table)
library(stringr)
library(lubridate)

source("FirstVisualization_2.R")

experiment.number <- as.character(c(1:8))
experiment.month  <-c("O","N","D","J","F")
experiment.day    <-c('01','02','03','04','05','06','07','08','09') %>% append (as.character(c(10:31)))
experiment.data   <- c("_SP.csv","_AR.csv")

test           <-as.vector(outer(experiment.number, experiment.month, paste, sep=""))
directory.name <-as.vector(outer(test, experiment.day, paste, sep=""))
file.name      <-as.vector(outer(directory.name, experiment.day, paste, sep=""))
counter        <-0
image.dir      <- "./Bilder/MessverlaufAB"


for(i in 1:length(directory.name)){
  
  
  dfnSP        <- paste("./Daten/voll/",directory.name[i],"/",directory.name[i],"_SP.csv", sep ="")
  dfnAR        <- paste("./Daten/voll/",directory.name[i],"/",directory.name[i],"_AR.csv", sep ="")
  dn           <- paste("./Daten/voll/",directory.name[i],sep = "")
  plot.name.AB <- paste(directory.name[i],"_AB",sep = "")
  
  
  if(i!=737){
    
    visualizeAB(dn, dfnSP, dfnAR , plot.name.AB, image.dir)
    }
  
  counter[i+1]<- counter[i] +1
  
}