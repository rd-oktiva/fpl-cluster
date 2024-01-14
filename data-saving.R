######################### RECOMMENDATIONS ###############################
fin.gk$rec <- ifelse(fin.gk$cluster == 1, "Recommended",
                     ifelse(fin.gk$cluster == 2, "Can be alternative",
                            ifelse(fin.gk$cluster == 5, "Can be alternative",
                            "Not recommended")))

fin.def$rec <- ifelse(fin.def$cluster == 7, "Recommended",
                      ifelse(fin.def$cluster == 5, "Can be alternative",
                             "Not recommended"))

fin.mid$rec <- ifelse(fin.mid$cluster == 6, "Recommended",
                      ifelse(fin.mid$cluster == 4, "Can be alternative",
                             "Not recommended"))

fin.fwd$rec <- ifelse(fin.fwd$cluster == 6, "Recommended",
                      ifelse(fin.fwd$cluster == 3, "Can be alternative",
                             ifelse(fin.fwd$cluster == 2, "Can be alternative",
                             "Not recommended")))

##### GATHER #####
### GATHER
FPL <- rbind(fin.gk, fin.def, fin.mid, fin.fwd)

##### EXPORT DATA #####
### CSV
write.csv(FPL, file="FPLClust_240113_0020.csv", row.names=FALSE)
write.csv(agg.gk, file="AGG_GK_240113_0020.csv", row.names=FALSE)
write.csv(agg.def, file="AGG_DEF_240113_0020.csv", row.names=FALSE)
write.csv(agg.mid, file="AGG_MID_240113_0020.csv", row.names=FALSE)
write.csv(agg.fwd, file="AGG_FWD_240113_0020.csv", row.names=FALSE)
