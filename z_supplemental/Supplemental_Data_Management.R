#* Write data files to the supplemental directory
data(jagsDists, package="HydeNet")
data(jagsFunctions, package="HydeNet")
data(PE, package="HydeNet")
write.csv(jagsDists, "C:/Users/nutterb/Documents/GitHub/HydeNet/z_supplemental/jagsDists.csv",
          na="", row.names=FALSE)
write.csv(jagsFunctions, "C:/Users/nutterb/Documents/GitHub/HydeNet/z_supplemental/jagsFunctions.csv",
          na="", row.names=FALSE)
write.csv(PE, "C:/Users/nutterb/Documents/GitHub/HydeNet/z_supplemental/PE.csv",
          na="", row.names=FALSE)

#* Read in edited data files
jagsDists <- read.csv("/home/benjamin/GitHub/HydeNet/z_supplemental/jagsDists.csv",
         stringsAsFactors=FALSE)
jagsFunctions <- read.csv("/home/benjamin/GitHub/HydeNet/z_supplemental/jagsFunctions.csv",
          stringsAsFactors=FALSE)
PE <- read.csv("/home/benjamin/GitHub/HydeNet/z_supplemental/PE.csv",
          stringsAsFactors=FALSE)

#* Save the data files as package resources
save(jagsDists, file="/home/benjamin/GitHub/HydeNet/data/jagsDists.RData")
save(jagsFunctions, file="/home/benjamin/GitHub/HydeNet/data/jagsFunctions.RData")
save(jagsDists, jagsFunctions, file="/home/benjamin/GitHub/HydeNet/R/sysdata.rda")
