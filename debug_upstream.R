catch_path <- "E:/MelinaStuff/BEACONs/git/beaconshydro_explorer/www/cathcments.shp"
catch <- st_read(catch_path)
catch$objectid <- catch$CATCHNUM
catch_up <- get_upstream_catchments(catch, "objectid", catch)

#catch_tbl <- catch %>%
#  st_drop_geometry() %>%
#  write.csv("E:/MelinaStuff/BEACONs/temp/catch.csv")

#seed <- seeds(catch)
#write.csv(seed, "E:/MelinaStuff/BEACONs/temp/seed.csv")
#neighbour <- neighbours(catch)
#write.csv(neighbour, "E:/MelinaStuff/BEACONs/temp/neighbour.CSV")

#---------------------------------
utf32_greaterthan_vectorized <- function(x, y){
    sapply(x, function(xx){
      utf32_greaterthan(xx, y)
    })
}

utf32_greaterthan <- function(x, y){
  # is x greater than y?
  # used to identify upstream catchments based on comparison of ORDER1 and ORDER1_3 strings.
  
  # get vectors of code points to compare
  xx <- utf8ToInt(x)
  yy <- utf8ToInt(y)
  
  # what is the length of the shortest vector?
  ii <- min(c(length(xx), length(yy)))
  
  # for each index...
  for(i in 1:ii){
    
    # if the value is not the same...
    if(xx[i] != yy[i]){
      
      # test if x > y
      if(xx[i] > yy[i]){
        return(TRUE)
      } else{
        return(FALSE)
      }
    }
    # otherwise move on to next index
  }
  
  # If they are the same up to length ii, the upstream catchment will have more characters, so if x is longer than y, return true.
  if(length(xx) > length(yy)){
    return(TRUE)
  } else{
    return(FALSE)
  }
}
#-------------------------------------------
getAggregationUpstreamCatchments_R <- function(catchment_tab, agg_catchments){
  catchment_tab$ORDER2 <- as.numeric(as.character(catchment_tab$ORDER2))
  # get list of BASINs for the agg_catchments
  basins <- catchment_tab %>%
    dplyr::filter(CATCHNUM %in% agg_catchments) %>%
    dplyr::pull(BASIN) %>%
    unique()
  
  # filter catchments to only include matching BASINs, and to remove agg_catchments
  search_tab <- catchment_tab %>%
    dplyr::filter(!CATCHNUM %in% agg_catchments,
                  BASIN %in% basins)
  
  # make second table to hold just the agg catchments (hopefully faster to query 2 smaller tables than one big)
  agg_tab <- catchment_tab %>%
    dplyr::filter(CATCHNUM %in% agg_catchments) %>%
    dplyr::arrange(nchar(ORDER1), ORDER2) # run the most downstream catchments first to remove as many agg_catchments as possible. This minimizes queries. Rough approximation of downstream is shorter ORDER1 and lower ORDER2 values.
  
  catchList <- c()
  agg_up_list <- c()
  if(nrow(agg_tab)>1){
    for(current_catchment in agg_tab$CATCHNUM){
    
      #print(paste0(length(catchList), "  -  ", length(agg_up_list), "  -  ", current_catchment))
    
      if(!current_catchment %in% agg_up_list){ # skip if the current agg_catchment is upstream of one that has already been tested. They'll have the same result.
      
        current_BASIN <- getBasin(agg_tab, current_catchment)
        current_ORDER1 <- getOrder1(agg_tab, current_catchment)
        current_ORDER2 <- getOrder2(agg_tab, current_catchment)
        current_ORDER3 <- getOrder3(agg_tab, current_catchment)
      
        # Get all CATCHNUMS matching first test: basin = BASIN and order1 = ORDER1 and order2 >= ORDER2
        test1 <- search_tab %>%
          dplyr::filter(BASIN == current_BASIN &
                          ORDER1 == current_ORDER1 &
                          ORDER2 >= current_ORDER2) %>%
          dplyr::pull(CATCHNUM)
      
        # Get all CATCHNUMS matching second test: basin = BASIN and order1 contains ORDER1 and order1 > ORDER1.ORDER3
        test2 <- search_tab %>%
          dplyr::filter(BASIN == current_BASIN &
                        grepl(current_ORDER1, ORDER1) &
                        utf32_greaterthan_vectorized(ORDER1, paste0(current_ORDER1, current_ORDER3))) %>%
          dplyr::pull(CATCHNUM)
      
        # add new upstream catchments to out list
        catchList <- c(catchList, test1, test2)
      
        # remove test1 and test2 catchments from the search table so they can't be added again
        search_tab <- search_tab %>%
          dplyr::filter(!CATCHNUM %in% c(test1, test2))
      
        ###################
      
        # add agg_catchments upstream of current_catchment to a list.
        # these do not need to be tested because they'll have the same result as the current catchment
        test1_agg <- agg_tab %>%
          dplyr::filter(BASIN == current_BASIN &
                        ORDER1 == current_ORDER1 &
                        ORDER2 >= current_ORDER2) %>%
          dplyr::pull(CATCHNUM)
      
        test2_agg <- agg_tab %>%
          dplyr::filter(BASIN == current_BASIN &
                        grepl(current_ORDER1, ORDER1) &
                        utf32_greaterthan_vectorized(ORDER1, paste0(current_ORDER1, current_ORDER3))) %>%
          dplyr::pull(CATCHNUM)
      
        agg_up_list <- c(agg_up_list, test1_agg, test2_agg)
      } 
    }
  }
  
  # Should already be unique
  outVals <- unique(catchList)
  return(outVals)
}
#------------------------------------
get_upstream_catchments <- function(cons_sf, cons_id, catchments_sf){
  if(!all(c("ORDER1", "ORDER2", "ORDER3", "BASIN", "CATCHNUM") %in% colnames(catchments_sf))){
    stop("catchments_sf must have attributes: ORDER1, ORDER2, ORDER3, BASIN, CATCHNUM")
  }
  # make sure catchnums are integer. Out tables in BUILDER wide format should use integer class for CATCHNUM
  catchments_sf <- make_catchnum_integer(catchments_sf)
  
  # get list of catchnums in each PA
  cons_catchnums_tab <- catchnums_in_polygon(cons_sf, cons_id, catchments_sf)
  
  
  up_agg_list <- list()
  for(col_id in colnames(cons_catchnums_tab)){
      print(col_id)
      agg_catchments <- get_catch_list(col_id, cons_catchnums_tab)
      up_agg <- getAggregationUpstreamCatchments_R(catchments_sf, agg_catchments)
      # add to up_agg_list
      up_agg_list[[col_id]] <- up_agg
    
  }
  browser()
  out_df <- dplyr::as_tibble(list_to_wide(up_agg_list))
  
  return(out_df)
}