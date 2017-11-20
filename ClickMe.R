

source("testfunction.R")

experiment.number <- as.character(c(1:8))
experiment.month  <-c("O","N","D","J","F")
experiment.day    <-c('01','02','03','04','05','06','07','08','09') %>% append (as.character(c(10:31)))
experiment.data   <- c("_SP.csv","_AR.csv")

test<-as.vector(outer(experiment.number, experiment.month, paste, sep=""))
directory.name<-as.vector(outer(test, experiment.day, paste, sep=""))
file.name<-as.vector(outer(directory.name, experiment.day, paste, sep=""))

for(i in 1:length(directory.name)){
  
  dnSP<- paste("./voll/",directory.name[i],"/",directory.name[i],"_SP.csv", sep ="")
  dnAR<- paste("./voll/",directory.name[i],"/",directory.name[i],"_AR.csv", sep ="")
  
  visualizeIT(directory.name[i],dnSP,directory.name[i])
  
}