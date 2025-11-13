dwdServer  <- function(input, output, session, project, map, rv){
  
  ################################################################################################
  # Save features to a geopackage
  ################################################################################################
  output$downloadData <- downloadHandler(
    filename = function() { paste("Hydro_explorer_output-", Sys.Date(), ".gpkg", sep="") },
    content = function(file) {
    
      catchment_updated <- rv$layers_rv$catchment_pr %>%
        dplyr::left_join(st_drop_geometry(rv$layers_rv$catch_up) %>% dplyr::select(CATCHNUM, up), by = "CATCHNUM") %>%
        dplyr::left_join(st_drop_geometry(rv$layers_rv$catch_down) %>% dplyr::select(CATCHNUM, down), by = "CATCHNUM") %>%
        dplyr::left_join(st_drop_geometry(rv$layers_rv$catch_stem) %>% dplyr::select(CATCHNUM, stem), by = "CATCHNUM")
      
      catchment_updated <- catchment_updated[,c("CATCHNUM", "Area_Land", "Area_Water", "Area_Total", "intact", "down", "stem", "up")]
      
      x <- data.frame(AOI_area = rv$outtab1()[2,2],
                      AOI_intact  = rv$outtab1()[3,3],
                      PAs_AOI_per = rv$outtab1()[4,3],
                      Upstream_area = rv$outtab1()[5,2],
                      Upstream_mean_AWI= rv$outtab1()[8,3],
                      Downstream_area = rv$outtab1()[7,2],
                      Downstream_mean_AWI = rv$outtab1()[10,3],
                      Downstream_stem_area = rv$outtab1()[6,2],
                      Downstream_stem_mean_AWI = rv$outtab1()[9,3],
                      Fire_within_study_area = rv$outfiretab()[1,2],
                      Fire_within_aoi = rv$outfiretab()[2,2],
                      Fire_within_upstream_area = rv$outfiretab()[4,2],
                      Fire_within_downstream_stem_area = rv$outfiretab()[4,2],
                      Fire_within_downstream_area = rv$outfiretab()[5,2])
      colnames(x) <-c("area_km2","AOI_intact_per","PAs_AOI_per","Upstream_area", "Upstream_mean_AWI","Downstream_area","Downstream_mean_AWI",
                      "Downstream_stem_area","Downstream_stem_mean_AWI","Fire_areastudy_area","Fire_area_aoi",  "Fire_area_upstream",
                      "Fire_area_downstream_stem", "Fire_area_downstream")
      y <- rv$layers_rv$analysis_aoi %>% st_union() 
      analysis_aoi <- st_as_sf(data.frame(
        geometry = y
      ))
      
      aoi <- cbind(analysis_aoi, x)
      st_write(rv$layers_rv$stream_sf, dsn=file, layer='streams', append=TRUE)
      st_write(rv$layers_rv$planreg_sf, dsn=file, layer='studyarea', append=TRUE)
      st_write(rv$layers_rv$analysis_aoi, dsn=file, layer='aoi', append=TRUE) 
      st_write(catchment_updated, dsn=file, layer='catchments', append=TRUE)
      st_write(rv$layers_rv$catch_up[,c("CATCHNUM", "Area_Total", "intact", "up")], dsn=file, layer='upstream', append=TRUE)
      st_write(rv$layers_rv$catch_down[,c("CATCHNUM", "Area_Total", "intact", "down")], dsn=file, layer='downstream', append=TRUE)
      st_write(rv$layers_rv$catch_stem[,c("CATCHNUM", "Area_Total", "intact", "stem")], dsn=file, layer='downstream_stem', append=TRUE)
    }
  )
  
  ##############################################################################
  # Save Stats
  ##############################################################################
  output$downloadStats <- downloadHandler(
    filename = function() { paste("hydrology_explorer_stats-", Sys.Date(), ".csv", sep="") },
    content = function(file) {
      write.csv(rv$outputsumStats(), file, row.names = FALSE)
    }
  )
}