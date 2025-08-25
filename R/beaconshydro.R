getOrder1 <- function(catchment_df, current_catchment){
  # get ORDER1 value from the provided CATCHNUM in a data.table
  as.character(catchment_df$ORDER1[catchment_df$CATCHNUM==current_catchment])
}

getOrder2 <- function(catchment_df, current_catchment){
  # get ORDER2 value from the provided CATCHNUM in a data.table
  as.numeric(as.character(catchment_df$ORDER2[catchment_df$CATCHNUM==current_catchment]))
}

getOrder3 <- function(catchment_df, current_catchment){
  # get ORDER3 value from the provided CATCHNUM in a data.table
  as.character(catchment_df$ORDER3[catchment_df$CATCHNUM==current_catchment])
}

getBasin <- function(catchment_df, current_catchment){
  # get BASIN value from the provided CATCHNUM in a data.table
  as.numeric(as.character(catchment_df$BASIN[catchment_df$CATCHNUM==current_catchment]))
}

order3ToOrder2 <- function(order3){
  # replicating builder function to get order 2 from 3
  base64Codes <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "=", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "_", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
  idx0 <- match(substr(order3, 1, 1), base64Codes) - 1 # C# indexes start at zero so minus 1
  idx1 <- match(substr(order3, 2, 2), base64Codes) - 1
  return(idx0 * length(base64Codes) + idx1)
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

utf32_greaterthan_vectorized <- function(x, y){
  sapply(x, function(xx){
    utf32_greaterthan(xx, y)
  })
}

isUpstream <- function(catchment_df, current_catchment, other_catchment){
  
  # Test if other_catchment is upstream of current_catchment
  
  # First test: basin = BASIN and order1 = ORDER1 and order2 >= ORDER2
  if(getBasin(catchment_df, other_catchment) == getBasin(catchment_df, current_catchment)){
    if(getOrder1(catchment_df, other_catchment) == getOrder1(catchment_df, current_catchment)){
      if(getOrder2(catchment_df, other_catchment) >= getOrder2(catchment_df, current_catchment)){
        return(TRUE)
      }
    }
  }
  # Second test: basin = BASIN and order1 contains ORDER1 and order1 > ORDER1.ORDER3
  if(getBasin(catchment_df, other_catchment) == getBasin(catchment_df, current_catchment)){
    if(grepl(getOrder1(catchment_df, current_catchment), getOrder1(catchment_df, other_catchment))){
      if(
        utf32_greaterthan(
          getOrder1(catchment_df, other_catchment),
          paste0(getOrder1(catchment_df, current_catchment), getOrder3(catchment_df, current_catchment))
        )
      ){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

getAggregationUpstreamCatchments_BUILDER_method <- function(catchment_df, agg_catchments, neighbours_tib){
  
  lst_inspected <- c()
  
  # create list to hold upstream catchments
  lst_upstream <- c()
  
  for(aggCatchmntIdx in agg_catchments){ # for each catchment in the aggregation
    
    # create a pending queue
    pendingQueue <- c()
    
    # add aggCatchmntIdx to the queue
    pendingQueue <- c(pendingQueue, aggCatchmntIdx)
    
    while(length(pendingQueue) > 0){
      
      currentCatchment <- pendingQueue[1]
      
      # remove current catchment from pending queue
      pendingQueue <- pendingQueue[-1]
      
      # get current catchments neighbours
      neighbourCatchments <- neighbours_tib %>%
        dplyr::filter(CATCHNUM == currentCatchment) %>%
        dplyr::pull(neighbours)
      
      for(nbr in neighbourCatchments){
        # if not already inspected
        if(!nbr %in% lst_inspected){
          
          # if not in the aggregation
          if(!nbr %in% agg_catchments){
            
            # test if neighbour is upstream of current agg catchment
            if(isUpstream(catchment_df = catchment_df, current_catchment = aggCatchmntIdx, other_catchment = nbr)){
              
              # if yes, add to upstream list
              lst_upstream <- c(lst_upstream, nbr)
              
              # put this catchment in the queue so its neighbours can be tested
              pendingQueue <- c(pendingQueue, nbr)
              
              # add this catchment to the inspected list
              lst_inspected <- c(lst_inspected, nbr)
            } else{
              # not upstream, do nothing
            }
          } else{
            # if it's in the aggregation, put it in the pending queue so its neighbours can be tested
            #pendingQueue <- c(pendingQueue, nbr)
            #lst_inspected <- c(lst_inspected, nbr)
            
            ### Dropping these lines because if its in the aggregation it'll get tested as an aggCatchmntIdx
            ### The aggCatchmntIdx will still run even if it's already been tested here, so these lines are
            ### unnecessarily repeating the search for neighbors.
          }
        }
      }
    }
  }
  return(lst_upstream)
}

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
  
  # Should already be unique
  outVals <- unique(catchList)
  return(outVals)
}

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
    # get list of catchments
    agg_catchments <- get_catch_list(col_id, cons_catchnums_tab)
    up_agg <- getAggregationUpstreamCatchments_R(catchments_sf, agg_catchments)
    
    # add to up_agg_list
    up_agg_list[[col_id]] <- up_agg
  }
  out_df <- dplyr::as_tibble(list_to_wide(up_agg_list))
  
  return(out_df)
}


getAggregationDownstreamCatchments_R <- function(catchment_tab, agg_catchments){
  
  catchment_tab$ORDER2 <- as.numeric(as.character(catchment_tab$ORDER2))
  
  # filter catchments to remove agg_catchments
  search_tab <- catchment_tab %>%
    dplyr::filter(!CATCHNUM %in% agg_catchments)
  
  # make second table to hold just the agg catchments (hopefully faster to query 2 smaller tables than one big)
  agg_tab <- catchment_tab %>%
    dplyr::filter(CATCHNUM %in% agg_catchments) %>%
    dplyr::arrange(dplyr::desc(nchar(ORDER1)), dplyr::desc(ORDER2)) # run the most upstream catchments first to remove as many agg_catchments as possible. This minimizes queries. Rough approximation of downstream is longest ORDER1 and highest ORDER2 values.
  
  catchList <- c()
  for(current_catchment in agg_tab$CATCHNUM){
    
    if(!current_catchment %in% catchList){ # skip if the current agg_catchment is downstream of one that has already been tested. They'll have the same result.
      
      # Get all downstream on current catchments ORDER1
      current_ORDER1 <- getOrder1(agg_tab, current_catchment)
      current_ORDER2 <- getOrder2(agg_tab, current_catchment)
      current_BASIN <- getBasin(agg_tab, current_catchment)
      
      current_catchList <- search_tab %>%
        dplyr::filter(BASIN == current_BASIN &
                        ORDER1 == current_ORDER1 &
                        ORDER2 < current_ORDER2) %>%
        dplyr::pull(CATCHNUM)
      
      # Get ORDER1 and ORDER2 for each 'next downstream' catchments
      # Use them to grab all lower ORDER2 catchments on the stream and add to catchList
      count <- (nchar(current_ORDER1)/3) - 1
      for(i in 1:count){
        next_ORDER1 <- substr(current_ORDER1, 1, nchar(current_ORDER1) - i*3)
        next_ORDER3 <- substr(current_ORDER1, (nchar(current_ORDER1) - i*3)+1, (nchar(current_ORDER1) - i*3) + 2)
        next_ORDER2 <- order3ToOrder2(next_ORDER3)
        
        down_i <- search_tab %>%
          dplyr::filter(BASIN == current_BASIN &
                          ORDER1 == next_ORDER1 &
                          ORDER2 <= next_ORDER2) %>%
          dplyr::pull(CATCHNUM)
        
        current_catchList <- c(current_catchList, down_i)
      }
      
      # add new downstream catchments to out list
      catchList <- c(catchList, current_catchList)
      
      # remove new downstream catchments from the search table so they can't be added again
      if(length(current_catchList) > 0){
        search_tab <- search_tab %>%
          dplyr::filter(!CATCHNUM %in% current_catchList)
      }
    }
  }
  
  # Should already be unique
  outVals <- unique(catchList)
  return(outVals)
}

get_downstream_catchments <- function(cons_sf, cons_id, catchments_sf){
  
  if(!all(c("ORDER1", "ORDER2", "ORDER3", "BASIN", "CATCHNUM") %in% colnames(catchments_sf))){
    stop("catchments_sf must have attributes: ORDER1, ORDER2, ORDER3, BASIN, CATCHNUM")
  }
  # make sure catchnums are integer. Out tables in BUILDER wide format should use integer class for CATCHNUM
  catchments_sf <- make_catchnum_integer(catchments_sf)
  
  # get list of catchnums in each PA
  cons_catchnums_tab <- catchnums_in_polygon(cons_sf, cons_id, catchments_sf)
  
  down_agg_list <- list()
  for(col_id in colnames(cons_catchnums_tab)){
    
    # get list of catchments
    agg_catchments <- get_catch_list(col_id, cons_catchnums_tab)
    down_agg <- getAggregationDownstreamCatchments_R(catchments_sf, agg_catchments)
    
    # add to up_agg_list
    down_agg_list[[col_id]] <- down_agg
  }
  
  out_df <- dplyr::as_tibble(list_to_wide(down_agg_list))
  
  return(out_df)
}

sep_network_names <- function(network_names){
  
  if(length(network_names) == 1){
    out_val <- strsplit(network_names, "__")[[1]]
  }
  
  if(length(network_names) > 1){
    out_val <- lapply(network_names, function(x){
      strsplit(x, "__")[[1]]
    })
    names(out_val) <- network_names
  }
  return(out_val)
}

catchnums_in_polygon <- function(cons_sf, cons_id, catchments_sf){
  if(cons_id == "CATCHNUM"){
    cons_id <- "CONS_ID"
    names(cons_sf)[names(cons_sf) == "CATCHNUM"] <- "CONS_ID"
  }
  
  sf::st_agr(catchments_sf) = "constant"
  
  catch_within <- catchments_sf %>%
    sf::st_point_on_surface() %>% # get catchment centroids.
    sf::st_within(cons_sf) %>% # test within for all centroids in all PAs, returns a row for each match. row.id is a catchnum index, col.id is a PA index.
    as.data.frame()
  
  catchments_sf$key <- 1:nrow(catchments_sf) # add a key column to sf table. Must be an index to match st_within output
  sf_catch_key <- sf::st_drop_geometry(catchments_sf[c("key","CATCHNUM")])
  cons_sf$key <- 1:nrow(cons_sf) # add key to cons_sf
  cons_key <- sf::st_drop_geometry(cons_sf[c("key",cons_id)])
  
  # convert indexes from st_within to catchnums using the keys to join
  tbl_long <- catch_within %>%
    dplyr::left_join(sf_catch_key, by = c("row.id" = "key")) %>%
    dplyr::left_join(cons_key, by = c("col.id" = "key")) %>%
    dplyr::select(CATCHNUM, .data[[cons_id]]) %>%
    dplyr::arrange(.data[[cons_id]])
  
  # convert long table to wide table with missing values as NA
  out_tab <- long_to_wide(tbl_long, cons_id, "CATCHNUM")
  return(out_tab)
}

dissolve_catchments_from_table <- function(catchments_sf, input_table, out_feature_id, calc_area = FALSE, intactness_id = NULL, dissolve_list = c(), drop_table = NULL){
  check_catchnum(catchments_sf) # check for CATCHNUM
  check_for_geometry(catchments_sf)
  check_catchnum_class(catchments_sf, input_table) # Check catchments match, warning if not
  input_table <- remove_oid(input_table) #drop oid column is it exists
  #** MH_fix: commented because we need to handle empty dataframe differently
  #check_for_rows(input_table)
  
  #Change geom column for geometry
  if("geom" %in% names(catchments_sf)){
    catchments_sf <- catchments_sf %>%
      dplyr::rename(geometry = geom)
  }
  
  # get colnames to process
  if(length(dissolve_list > 0)){
    feature_list <- dissolve_list
  } else{
    feature_list <- colnames(input_table)
  }
  
  saveCount <- 1
  for(col_id in feature_list){
    
    # separate names if combination
    col_ids <- sep_network_names(col_id)
    
    # get list of catchments
    catchments_list <- get_catch_list(col_ids, input_table)
    
    # only proceed if there are catchments in the list. if no catchments, the network will not be included in the output table
    if(length(catchments_list) > 0){
      
      # drop catchments if requested
      if(!is.null(drop_table)){
        drop_list <- get_catch_list(col_ids, drop_table)
        
        catchments_list <- catchments_list[!catchments_list %in% drop_list]
      }
      
      # dissolve based on parameters
      if(calc_area){
        dslv <- catchments_sf %>%
          dplyr::filter(CATCHNUM %in% catchments_list) %>%
          dplyr::summarise(geometry = sf::st_union(geometry)) %>%
          dplyr::mutate(id = col_id,
                        area_km2 = round(as.numeric(sf::st_area(geometry) / 1000000), 2))
      } else{
        dslv <- catchments_sf %>%
          dplyr::filter(CATCHNUM %in% catchments_list) %>%
          dplyr::summarise(geometry = sf::st_union(geometry)) %>%
          dplyr::mutate(id = col_id)
      }
      
      # join AWI if requested
      if(!is.null(intactness_id)){
        if(intactness_id %in% colnames(catchments_sf)){
          awi <- catchments_sf %>%
            dplyr::filter(CATCHNUM %in% catchments_list) %>%
            dplyr::mutate(area = as.numeric(sf::st_area(geometry))) %>%
            sf::st_drop_geometry() %>%
            dplyr::summarise(AWI = sum(.data[[intactness_id]] * area) / sum(area))
          
          dslv$AWI <- round(awi$AWI, 4)
        } else{
          warning(paste0("Area-weighted intactness cannot be calculated, ", '"', intactness_id, '"', " not in catchments_sf"))
        }
      }
      
      # set out name
      names(dslv)[names(dslv) == "id"] <- out_feature_id
      
      # reorder columns - move geometry to last
      dslv <- dslv  %>%
        dplyr::relocate(geometry, .after = dplyr::last_col())
      
      # append to df
      if(saveCount == 1){
        out_sf <- dslv
        saveCount <- saveCount + 1
      } else{
        out_sf <- rbind(out_sf, dslv)
      }
    }
  }
  return(out_sf)
}

extract_catchments_from_table <- function(catchments_sf, input_table, extract_feature_id, out_feature_id){
  
  check_catchnum(catchments_sf) # check for CATCHNUM
  check_catchnum_class(catchments_sf, input_table) # Check catchments match, warning if not
  check_for_geometry(catchments_sf)
  
  # get list of catchments
  catchments_list <- get_catch_list(extract_feature_id, input_table)
  
  # extract
  ex <- catchments_sf %>%
    dplyr::filter(CATCHNUM %in% catchments_list) %>%
    dplyr::mutate(id = paste0(extract_feature_id,collapse="__")) %>%
    dplyr::select(CATCHNUM,id)
  
  # set out name
  names(ex)[names(ex) == "id"] <- out_feature_id
  
  return(ex)
}

check_catchnum <- function(catchments_sf){
  
  # check CATCHNUM exists
  if(!"CATCHNUM" %in% names(catchments_sf)){
    stop("Catchments must contain column 'CATCHNUM'")
  }
}

check_catchnum_class <- function(catchments_sf, builder_table){
  col_classes <- sapply(colnames(builder_table), function(x) class(builder_table[[x]]))
  if(!all(col_classes == class(catchments_sf$CATCHNUM))){
    warning(paste0("Table column classes do not match class(catchments_sf$CATCHNUM) for columns: ", paste0(colnames(builder_table)[col_classes != class(catchments_sf$CATCHNUM)], collapse=", ")))
  }
}

make_catchnum_integer <- function(catchments_sf){
  
  # check CATCHNUM exists
  if(!"CATCHNUM" %in% names(catchments_sf)){
    stop("Catchments must contain column 'CATCHNUM'")
  }
  
  # make sure it's an integer then convert to character
  catchments_sf$CATCHNUM <- as.integer(catchments_sf$CATCHNUM)
  
  return(catchments_sf)
}

get_catch_list <- function(cons_ids, input_table){
  
  # check nets are in input_table
  if(!all(cons_ids %in% colnames(input_table))){
    stop(paste0("names do not appear as colnames in the input table: ", cons_ids[!cons_ids %in% colnames(input_table)]))
  }
  
  # get unique catchments vector from input_table using vector of colnames
  net_catchments <- unique(unlist(lapply(cons_ids, function(x){
    input_table[[x]]
  })))
  
  net_catchments[!is.na(net_catchments)]
}

check_for_geometry <- function(in_sf){
  if(!"geometry" %in% names(in_sf) & !"geom" %in% names(in_sf)){
    stop("Object must contain column: geometry")
  }
}

long_to_wide <- function(long_df, col_names, values_col){
  
  # get out table nrow (i.e. longest list of values)
  tbl_rows <- long_df %>%
    dplyr::group_by(.data[[col_names]]) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::summarise(m = max(n)) %>%
    dplyr::pull(m)
  
  values_list <- lapply(unique(long_df[[col_names]]), function(x){
    vals <- long_df[[values_col]][long_df[[col_names]]==x]
    c(vals, rep(NA, tbl_rows - length(vals)))
  })
  names(values_list) <- unique(long_df[[col_names]])
  out_tab <- as.data.frame(do.call(cbind, values_list))
  out_tab <- dplyr::as_tibble(do.call(cbind, values_list))
  
  return(out_tab)
}

# remove OID from tables comgin out of BUILDER
remove_oid <- function(in_table){
  if("OID" %in% colnames(in_table)){
    out_table <- in_table %>%
      dplyr::select(-OID)
  } else{
    out_table <- in_table
  }
  return(out_table)
}

# Check for rows
check_for_rows <- function(in_table){
  if(nrow(in_table) == 0){
    stop("input_table has no data")
  }
}

# convert list of vectors into df with list element names as colnames and missing values as NAs (i.e. BUILDER style table)
list_to_wide <- function(values_list){
  
  # get out table nrow (i.e. longest list of values)
  tbl_rows <- max(unlist(lapply(values_list, function(x){
    length(x)
  })))
  
  values_list_nas <- lapply(values_list, function(x){
    c(x, rep(NA, tbl_rows - length(x)))
  })
  
  out_tab <- as.data.frame(do.call(cbind, values_list_nas))
  
  return(out_tab)
}

calc_dci <- function(CAs_sf, stream_sf, CAs_id = "network", buffer_width = 0.1){
  
  stopifnot(sf::st_crs(CAs_sf) == sf::st_crs(stream_sf))
  
  # set stream and conservation area attributes to be constant throughout each polygon to avoid warnings in st_intersection
  sf::st_agr(stream_sf) = "constant"
  sf::st_agr(CAs_sf) = "constant"
  
  # clip streams to full area of all conservation areas
  reserve_dci <- CAs_sf %>%
    sf::st_intersection(stream_sf, sf::st_union()) %>% # get streams just for the required region
    dplyr::filter(BASIN != -1) %>% # remove isolated stream segments
    sf::st_buffer(dist = buffer_width, endCapStyle = "ROUND") %>% # buffer to make sure streams are connected
    dplyr::summarise(geometry = sf::st_union(geometry)) %>% # merge into single feature
    sf::st_intersection(CAs_sf) %>% # intersect with reserves to get buffered stream for each reserve
    dplyr::select(dplyr::all_of(CAs_id)) %>%
    sf::st_cast("MULTIPOLYGON", warn = FALSE) %>% # this is needed to avoid geometries being lost in the POLYGON cast
    sf::st_cast("POLYGON", warn = FALSE) %>% # explode into individual stream segments
    dplyr::mutate(stream_length = as.numeric(sf::st_area(geometry)) / buffer_width) %>% # divide area by buffer to get length of each stream segment
    sf::st_drop_geometry() %>% # drop the geometry for speed
    dplyr::group_by(!!sym(CAs_id)) %>% # for each network...
    dplyr::summarise(L = sum(stream_length), dci = sum((stream_length*stream_length) / (L*L)))  # calculate L2 then use to calculate dci
  
  # reserves that do not intersect the stream network get dropped during st_intersection.
  # join dci back to original reserves and set missing reserves to have dci of 0
  dci <- dplyr::left_join(sf::st_drop_geometry(CAs_sf), reserve_dci, by = CAs_id) %>%
    tidyr::replace_na(list(dci=0)) %>%
    dplyr::pull(dci) %>%
    round(3)
  
  return(dci)
}

# This should be written in RCPP
ks_stat <- function(refVal, netVal) {
  # calculate KS statistic (representation index)
  ri <- suppressWarnings(round(ks.test(refVal, netVal)[[1]][[1]], 3))
  return(ri)
}

# This should be written in RCPP
bc_stat <- function(refVal, netVal) {
  
  x1 <- dplyr::as_tibble(refVal) %>%
    dplyr::count(.data$value)
  names(x1) <- c("cat","strata")
  
  x2 <- dplyr::as_tibble(netVal) %>%
    dplyr::count(.data$value)
  names(x2) <- c("cat","reserve")
  
  x <- merge(x1,x2,by="cat",all=T)
  #x$cat <- as.character(x$cat)
  x$strata <- as.numeric(x$strata)
  x$reserve <- as.numeric(x$reserve)
  x$reserve[is.na(x$reserve)] <- 0
  x$reserve[is.na(x$strata)] <- 0 # this is needed in case there is one reserve pixel and no strata pixel
  x$strata[is.na(x$strata)] <- 0
  x$strata <- x$strata/sum(x$strata)
  x$reserve <- x$reserve/sum(x$reserve)
  
  # calculate Bray-Curtis dissimilariy
  ri <- round(sum(abs(x$strata-x$reserve))/(sum(x$strata)+sum(x$reserve)), 3)
  return(ri)
}

ks_plot <- function(refVal, netVal, plotTitle="") {
  
  regLab <- "Reference area"
  netLab <- "Network"
  
  z1 <- c(refVal, netVal)
  z2 <- c(rep(regLab,length(refVal)), rep(netLab,length(netVal)))
  zz <- data.frame(cbind(z1,z2),stringsAsFactors=FALSE)
  names(zz) <- c("values","criteria")
  zz$values <- round(as.numeric(zz$values),3)
  
  # create and save density plot
  p <- ggplot2::ggplot(zz, ggplot2::aes(x=.data$values)) + ggplot2::geom_density(ggplot2::aes(group=.data$criteria, color=.data$criteria)) +
    ggplot2::ggtitle(plotTitle) +
    ggplot2::labs(x="Indicator value", y="Density")
  
  return(p)
}

bc_plot <- function(refVal, netVal, plotTitle="", labels=data.frame()) {
  
  x1 <- dplyr::as_tibble(refVal) %>%
    dplyr::count(.data$value)
  names(x1) <- c("cat","strata")
  
  x2 <- dplyr::as_tibble(netVal) %>%
    dplyr::count(.data$value)
  names(x2) <- c("cat","reserve")
  
  x <- merge(x1,x2,by="cat",all=T)
  x <- x[order(as.integer(as.character(x$cat))),]
  x$strata <- as.numeric(x$strata)
  x$reserve <- as.numeric(x$reserve)
  x$reserve[is.na(x$reserve)] <- 0
  x$reserve[is.na(x$strata)] <- 0 # this is needed in case there is one reserve pixel and no strata pixel
  x$strata[is.na(x$strata)] <- 0
  x$strata <- x$strata/sum(x$strata) #as.integer(x$strata)
  x$reserve <- x$reserve/sum(x$reserve) #as.integer(x$reserve)
  
  # prep labels if present
  if(nrow(labels) > 0 & "values" %in% names(labels)){
    for(i in labels$values){
      if(i %in% x$cat){
        x$cat[x$cat == i] <- labels$label[labels$values == i]
      }
    }
  }
  x$cat <- factor(x$cat, levels = x$cat)
  
  p <- ggplot2::ggplot(x, ggplot2::aes(x=.data$cat, y=.data$reserve)) + ggplot2::geom_bar(stat="identity", fill="white", colour="black") + ggplot2::coord_flip()
  p <- p + ggplot2::geom_point(data=x, ggplot2::aes(x=.data$cat, y=.data$strata), colour="black", size=3) + ggplot2::theme(legend.position = "none")
  p <- p + ggplot2::labs(x="", y="Proportional area (dots indicate regional proportions)")
  p <- p + ggplot2::ggtitle(plotTitle)
  
  return(p)
}


### calc_dissimilarity ###
#
#' Calculate dissimilarity values between a set of polygons and a reference area.
#'
#' For a list of features (e.g. conservation areas or networks), calculate the dissimilarity value between each feature and a reference area for the 
#' provided raster layer. Continuous rasters use the KS-statistic to compare distributions, categorical rasters use the Bray-Curtis
#' statistic. Graphs comparing distributions can optionally be created and saved to a user provided file path.
#' 
#' NA values are always removed. For categorical rasters, values can optionally be subset for the calculation and graphs using 
#' \code{categorical_class_values}.
#'
#' @param reserves_sf sf object with unique id column named \code{network}
#' @param reference_sf sf object of the reference area to compare against.
#' @param raster_layer Raster object that will be clipped to the reference and reserve areas, with crs matching reserves_sf
#' @param raster_type 'categorical' will use Bray-Curtis, 'continuous' will use KS-statistic.
#' @param categorical_class_values Vector of raster values in \code{raster_layer} of type 'categorical' to include in the calculation. 
#' Allows unwanted values to be dropped. Defaults to include all non-NA values.
#' @param plot_out_dir Path to folder in which to save plots. Default is not to create plots. 
#' Only creates plots if valid file path is provided. Dir will be created if it doesn't exist.
#' @param categorical_class_labels Optional data.frame object with columns \code{values} and \code{labels} indicating the label to use in Bray-Curtis graphs
#' for each raster value. Defaults to using the raster values. Labels can be provided for all or a subset of values. See examples.
#'
#' @return A vector of dissimilarity values matching the order of the input \code{reserves_sf}. Optionally a dissimilarity plot saved in the 
#' \code{plot_out_dir} for each computed value.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats ks.test
#' @export
#'
#' @examples
#' reserves <- dissolve_catchments_from_table(
#'   catchments_sample, 
#'   builder_table_sample, 
#'   "network",
#'   dissolve_list = c("PB_0001", "PB_0002", "PB_0003"))
#' calc_dissimilarity(reserves, ref_poly, led_sample, 'categorical')
#' calc_dissimilarity(reserves, ref_poly, led_sample, 'categorical', c(1,2,3,4,5))
#' calc_dissimilarity(reserves, ref_poly, led_sample, 'categorical', c(1,2,3,4,5), 
#'   "C:/temp/plots", data.frame(values=c(1,2,3,4,5), labels=c("one","two","three","four","five")))
#' calc_dissimilarity(reserves, ref_poly, led_sample, 'continuous', plot_out_dir="C:/temp/plots")
calc_dissimilarity <- function(reserves_sf, reference_sf, raster_layer, raster_type, categorical_class_values=c(), plot_out_dir=NULL, categorical_class_labels=data.frame()){
  
  # geometries should match
  stopifnot(sf::st_crs(reserves_sf) == sf::st_crs(reference_sf))
  stopifnot(sf::st_crs(reserves_sf) == sf::st_crs(raster_layer))
  
  # set up output vector
  result_vector <- c()
  
  # check raster_layer is valid
  if(!raster_type %in% c('categorical', 'continuous')){
    stop("raster_layer must be on of: 'categorical', 'continuous'")
  }
  
  # check for network column
  check_network(reserves_sf)
  
  # check geometry column is present in sf objects
  check_for_geometry(reference_sf)
  check_for_geometry(reserves_sf)
  
  # should plots be made? Attempt to create directory if it doesn't already exist
  if(!is.null(plot_out_dir)){
    make_plots <- TRUE
    dir.create(plot_out_dir, recursive = TRUE, showWarnings = FALSE)
  } else{
    make_plots <- FALSE
  }
  
  # get the reference area and make sure it is a single geometry
  ref_sf <- reference_sf %>%
    dplyr::summarise(geometry = sf::st_union(.data$geometry))
  
  # extract values in ref area
  ref_ext <- exactextractr::exact_extract(raster_layer, reference_sf, progress = FALSE)[[1]]
  
  # filter by class_vals if provided, remove NAs, only keep cells with majority in reference_sf
  if(raster_type == 'categorical' & length(categorical_class_values) > 0){
    reference_vals <- ref_ext %>%
      dplyr::filter(!is.na(.data$value)) %>%
      dplyr::filter(.data$value %in% categorical_class_values) %>%
      dplyr::filter(.data$coverage_fraction > 0.5) %>% # only keep values from cells with at least half their area in the polygon
      dplyr::pull(.data$value)
  } else{
    reference_vals <- ref_ext %>%
      dplyr::filter(!is.na(.data$value)) %>%
      dplyr::filter(.data$coverage_fraction > 0.5) %>%
      dplyr::pull(.data$value)
  }
  
  # split networks into blocks of 10 for processing
  net_list <- as.character(reserves_sf$network)
  net_list_grouped <- split(net_list, ceiling(seq_along(net_list)/10))
  
  # run in blocks of 10 - seems optimal for maintaining a fast extract
  counter <- 1
  for(net_list_g in net_list_grouped){
    
    message(paste0("processing ", counter*10-9, " of ", length(net_list)))
    counter <- counter + 1
    
    reserves_sf_g <- reserves_sf[reserves_sf$network %in% net_list_g,] # subset dissolved networks by the block of networks
    x <- exactextractr::exact_extract(raster_layer, reserves_sf_g, progress = FALSE) # extract
    
    names(x) <- net_list_g # name the list elements by their associated netname
    
    for(net in net_list_g){
      
      # for each network in the block, extract the values...
      if(raster_type == 'categorical' & length(categorical_class_values) > 0){
        target_vals <- x[[net]] %>%
          dplyr::filter(!is.na(.data$value)) %>%
          dplyr::filter(.data$value %in% categorical_class_values) %>%
          dplyr::filter(.data$coverage_fraction > 0.5) %>% # only keep values from cells with at least half their area in the polygon
          dplyr::pull(.data$value)
      } else{
        target_vals <- x[[net]] %>%
          dplyr::filter(!is.na(.data$value)) %>%
          dplyr::filter(.data$coverage_fraction > 0.5) %>%
          dplyr::pull(.data$value)
      }
      
      # run dissimilarity
      if(raster_type == "categorical"){
        result <- bc_stat(reference_vals, target_vals)
      } else{
        result <- ks_stat(reference_vals, target_vals)
      }
      
      # add result t return vector
      result_vector <- c(result_vector, result)
      
      # generate plot if requested
      if(make_plots){
        plot_out_path <- file.path(plot_out_dir, paste0(net, ".png"))
        
        if(raster_type == 'categorical'){
          plt <- bc_plot(reference_vals, target_vals, plotTitle = paste0(net, " (BC=", result, ")"), labels=categorical_class_labels)
        } else{
          plt <- ks_plot(reference_vals, target_vals, plotTitle = paste0(net, " (KS=", result, ")"))
        }
        ggplot2::ggsave(plt, file=plot_out_path)
      }
    }
  }
  return(result_vector)
}

check_network <- function(reserves_sf){
  
  if(!"network" %in% names(reserves_sf)){
    stop("Object must contain column: network")
  }
  
  # make sure it's a character
  if(!is.character(reserves_sf$network)){
    reserves_sf$network <- as.character(reserves_sf$network)
  }
  
  return(reserves_sf)
}
