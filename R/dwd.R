dwdServer  <- function(input, output, session, project, map, rv){
  
  ################################################################################################
  # Save features to a geopackage
  ################################################################################################
  output$downloadData <- downloadHandler(
    filename = function() { paste("Hydro_explorer_output-", Sys.Date(), ".gpkg", sep="") },
    content = function(file) {
      catchment_updated <- rv$layers_rv$catchment_pr
      
      if (!is.null(rv$layers_rv$catch_up)) {
        catchment_updated <- catchment_updated %>%
          left_join(
            st_drop_geometry(rv$layers_rv$catch_up) %>% dplyr::select(CATCHNUM, up),
            by = "CATCHNUM"
          )
      }else{
        catchment_updated$up <- 0
      }
      
      if (!is.null(rv$layers_rv$catch_down)) {
        catchment_updated <- catchment_updated %>%
          left_join(
            st_drop_geometry(rv$layers_rv$catch_down) %>% dplyr::select(CATCHNUM, down),
            by = "CATCHNUM"
          )
      }else{
        catchment_updated$down <- 0
      }
      
      if (!is.null(rv$layers_rv$catch_stem)) {
        catchment_updated <- catchment_updated %>%
          left_join(
            st_drop_geometry(rv$layers_rv$catch_stem) %>% dplyr::select(CATCHNUM, stem),
            by = "CATCHNUM"
          )
      }else{
        catchment_updated$stem <- 0
      }
      
      catchment_updated <- catchment_updated[,c("CATCHNUM", "Area_land", "Area_water", "Area_total", "intact", "down", "stem", "up")]
      sa_stats <- data.frame(area_km2 = rv$outtab1()[1,2],
                              area_intact_km2  = rv$outtab1()[2,2],
                              area_intact_per  = rv$outtab1()[2,3],
                              Feature_km2 = rv$outfeaturetab()[1,2],
                              Feature_per = rv$outfeaturetab()[1,3]
                              )
      colnames(sa_stats) <-c("area_km2", "intact_km2","intact_per","Feature_km2", "Feature_per")
      
      x <- rv$layers_rv$planreg_sf %>% st_union() 
      sa_sf <- st_as_sf(data.frame(
        geometry = x
      ))
      studyarea <- cbind(sa_sf, sa_stats)
      
      aoi_stats <- data.frame(AOI_area = rv$outtab1()[3,2],
                      AOI_intact  = rv$outtab1()[4,3],
                      Upstream_area = rv$outtab1()[5,2],
                      Upstream_mean_AWI= rv$outtab1()[8,3],
                      Downstream_area = rv$outtab1()[7,2],
                      Downstream_mean_AWI = rv$outtab1()[10,3],
                      Downstream_stem_area = rv$outtab1()[6,2],
                      Downstream_stem_mean_AWI = rv$outtab1()[9,3],
                      Feat_aoi_km2 = rv$outfeaturetab()[2,2],
                      Feat_aoi_per = rv$outfeaturetab()[2,3],
                      Feat_within_upstream_area = rv$outfeaturetab()[4,2],
                      Feat_within_downstream_stem_area = rv$outfeaturetab()[4,2],
                      Feat_within_downstream_area = rv$outfeaturetab()[5,2])
      colnames(aoi_stats) <-c("area_km2", "AOI_intact_per","Upstream_area", "Upstream_mean_AWI","Downstream_area","Downstream_mean_AWI",
                      "Downstream_stem_area","Downstream_stem_mean_AWI","Feature_aoi_km2", "Feature_aoi_per", "Feature_area_upstream",
                      "Feature_area_downstream_stem", "Feature_area_downstream")
      y <- rv$layers_rv$analysis_aoi %>% st_union() 
      analysis_aoi <- st_as_sf(data.frame(
        geometry = y
      ))
      aoi <- cbind(analysis_aoi, aoi_stats)
      
      st_write(rv$layers_rv$streams_sf, dsn=file, layer='streams', append=TRUE)
      st_write(studyarea, dsn=file, layer='studyarea', append=TRUE)
      st_write(aoi, dsn=file, layer='aoi', append=TRUE) 
      st_write(catchment_updated, dsn=file, layer='catchments', append=TRUE)
      if (!is.null(rv$layers_rv$trackFeat)) st_write(rv$layers_rv$trackFeat, dsn=file, layer=input$featLayer, append=TRUE)
      if (!is.null(rv$layers_rv$undisturbed)) st_write(rv$layers_rv$undisturbed, dsn=file, layer='intactness', append=TRUE)
      if (!is.null(rv$layers_rv$catch_up)) st_write(rv$layers_rv$catch_up[,c("CATCHNUM", "Area_total", "intact", "up")], dsn=file, layer='upstream', append=TRUE)
      if (!is.null(rv$layers_rv$catch_down)) st_write(rv$layers_rv$catch_down[,c("CATCHNUM", "Area_total", "intact", "down")], dsn=file, layer='downstream', append=TRUE)
      if (!is.null(rv$layers_rv$catch_stem)) st_write(rv$layers_rv$catch_stem[,c("CATCHNUM", "Area_total", "intact", "stem")], dsn=file, layer='downstream_stem', append=TRUE)
    }
  )
  
  ##############################################################################
  # Save Stats
  ##############################################################################
  output$downloadStats <- downloadHandler(
    filename = function() { paste("Hydrology_explorer_stats-", Sys.Date(), ".csv", sep="") },
    content = function(file) {
      write.csv(rv$outputsumStats(), file, row.names = FALSE)
    }
  )
}