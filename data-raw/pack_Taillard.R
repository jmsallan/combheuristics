setwd("~/Dropbox (UPC)/00-curso1920/MH1920q2/AssignmentsCodeExamples")
load("TaillardFS.RData")
Taillard_FS <- list(tai.20.5=tai20.5, tai20.10=tai20.10, tai20.20=tai20.20, tai50.5=tai50.5, tai50.10=tai50.10, tai50.20=tai50.20, tai100.5=tai100.5, tai100.10=tai100.10, tai100.20=tai100.20, tai200.10=tai200.10, tai200.10=tai200.20, tai500.20=tai500.20)

save(Taillard_FS, file="Taillard_FS.RData")
