#
## DATA CLEANING
#

#CHECK, INSTALL AND LOAD REQUIRED PACKAGES
pkgs <- c("data.table")
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

#READ DATA
r1 = fread("responsesP1(Jan-Feb).csv",header=TRUE)
r2 = fread("responsesP2(Mar-Apr).csv",header=TRUE)
r3 = fread("responsesP3(May-Jun).csv",header=TRUE)
r4 = fread("responsesP4(Jul-Aug).csv",header=TRUE)
r5 = fread("responsesP5(Sep-Oct).csv",header=TRUE)
r6 = fread("responsesP6(Nov-Dec).csv",header=TRUE)
data = as.data.table(rbind.data.frame(r1,r2,r3,r4,r5,r6))
rm(r1,r2,r3,r4,r5,r6)

#SUBSET (Only Westminster and Bexley)
data = data[data$borough == "Westminster" | data$borough == "Bexley"]

#DATA CLEANING
data$distance_to_scene = as.numeric(data$distance_to_scene) #Convert distance to scene to numeric
data = data[!is.na(data$distance_to_scene)] #Delete NA entries
data = data[data$activationid>=0] #Only valid activation IDs
data = data[data$activationid!="NULL"]
data = data[data$running>=0] #Only valid running times
data = data[data$running!="NULL"]
data = data[data$distance_to_scene>0] #Only valid distance to scene
data = data[data$distance_to_scene!="NULL"] 
data = data[data$arrivedatscene!="NULL"] #Only valid arrival time
data = data[data$dispatch!="NULL"] #Only valid dispatch time
data = data[data$lat_activation!="NULL"] #Only valid dispatch geography
data = data[data$lon_activation!="NULL"]
data = data[data$lat_incident!="NULL"] #Only valid incident geography
data = data[data$lon_incident!="NULL"]
data = data[data$ht!=1] #Only valid incidents
data = data[data$callstart!="NULL"] #Only valid call IDs
#fwrite(data,"data.csv",row.names=FALSE,sep=",") #Data for traffic speed checks
data = data[data$dohcategory == "C1 " | data$dohcategory == "C2 "] #Only chose severe incidents
#fwrite(data,"data.csv",row.names=FALSE,sep=",") #Data for incidents