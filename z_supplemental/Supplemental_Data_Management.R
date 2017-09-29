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
jagsDists <- read.csv("C:/Users/Nutter/Documents/GitHub/HydeNet/z_supplemental/jagsDists.csv",
         stringsAsFactors=FALSE)
jagsFunctions <- read.csv("C:/Users/Nutter/Documents/GitHub/HydeNet/z_supplemental/jagsFunctions.csv",
          stringsAsFactors=FALSE)
PE <- read.csv("C:/Users/Nutter/Documents/GitHub/HydeNet/z_supplemental/PE.csv",
          stringsAsFactors=FALSE)

#* Save the data files as package resources
save(jagsDists, file="C:/Users/Nutter/Documents/GitHub/HydeNet/data/jagsDists.Rdata")
save(jagsFunctions, file="C:/Users/Nutter/Documents/GitHub/HydeNet/data/jagsFunctions.Rdata")
save(jagsDists, jagsFunctions, file="C:/Users/Nutter/Documents/GitHub/HydeNet/R/sysdata.Rda")
