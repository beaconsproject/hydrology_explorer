library(sf)
library(beaconstools)
## cathcment

catchm <- st_read("./www/catchments_att.shp")
catchm$objectid <- catchm$CATCHNUM

up_catch <- get_upstream_catchments(catchm, "objectid", catchm)

unique(catchm$FDA_M)
catch10aa <- subset(catchm, catchm$FDA_M == "10AA")
up_catch <- get_upstream_catchments(catch10aa, "objectid", catch10aa)

catch10ab <- subset(catchm, catchm$FDA_M == "10AB")
up_catch <- get_upstream_catchments(catch10ab, "objectid", catch10ab)
save(up_catch, file = "upstream_catchments.RData")


#loop
catch_list <- unique(catchm$CATCHNUM)
catch_df <- c()
n<- 0
for (c in catch_list){
  catch <- subset(catchm, catchm$CATCHNUM == c)
  up_catch <- get_upstream_catchments(catch, "objectid", catchm)
  n<- n+1
  print(n)
  #catch_df <- cbind(catch_df, up_catch)
  
}
  
  
test_catch <- catchs[catchs$CATCHNUM == 97383,]
# upstream
upstream_list <- get_upstream_catchments(test_catch, "CATCHNUM", catchs)




catchm <- st_read("./www/catchments_yt_3578_att.shp")
catchm$objectid <- catchm$CATCHNUM
up_catch <- get_upstream_catchments(catchm, "objectid", catchm)
save(up_catch, file = "upstream_catchments.RData")
#--- NOT RUNNING
catchm <- st_read("./www/catchments_yt_3578_att.shp")
catchm$objectid <- catchm$CATCHNUM
catch_list <- unique(catchm$CATCHNUM)
catch_df <- c()
#n<- 0
for (c in catch_list){
  catch <- subset(catchm, catchm$CATCHNUM == c)
  up_catch <- get_upstream_catchments(catch, "objectid", catchm)
  print(catch$CATCHNUM)
  #catch_df <- cbind(catch_df, up_catch)
  
}

catchm <- st_read("./www/catchments_yt_att.shp")
nrow(catchm)
length(unique(catchm$CATCHNUM))
