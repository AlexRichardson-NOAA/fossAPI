#' API Wrapper for NOAA FOSS Database
#'
#' The query_foss() function is designed to help the R-using public use simple notation to query the APIs in the FOSS database.
#' There are currently seven FOSS APIs that can be accessed through the fossAPI package: landings, trade_data, source_species, gears, itis_history, source_gears, and tsn_species.
#' Each API has a number of variables that one can use to limit the results of a query. To access these variables, specify one of the APIs above with the "series" argument and
#' set the "list_variables" argument to TRUE.
#'
#' There are three types of variable - numeric, character, and name.
#'
#' Numeric variables are coded in the data as numeric, but should still be input as strings in order to avoid over-taxing the FOSS system. For example,
#' the range between 100 and 200 should be specified as "100:200" - not 100:200. Multiple ranges can be input together as a list: c("100:200", "300:400")
#' will return all results between 100 and 200 and between 300 and 400, but will exclude any results below 100, between 200 and 300, and above 400. (Note
#' that some data fields seem like they should be numeric but were specified in the data as characters.)
#'
#' Character variables must be entered as precise, non-case-sensitive string matches. For example, the arguments state_name = "california" or state_name
#' = "CALIFORNIA" will return all results from the state of California, but state_name = "cali" will return no matches.
#'
#' Name variables may be specified either as precise, non-case-sensitive string matches or as partial matches using the "in_" modified version of their
#' variable name. For example, the argument ts_afs_name = "shrimp" will return no matches, but ts_afs_name = "shrimp, peppermint" does. However, the
#' argument in_ts_afs_name = "shrimp" will return all arguments for which the field ts_afs_name includes the string "shrimp".
#'
#' Example:
#'
#' landings_data = query_foss(series = "landings", in_ts_afs_name = c("shrimp", "fish"), pounds = c("0:1000", "5000:10000"), collection = "commercial", list_variables = F)
#'
#' @series Specify the API you want to query. Defaults to NA
#' @allow_duplicates Specify whether duplicated data should be allowed into the final results. Defaults to FALSE
#' @list_variables Set to TRUE to list acceptable variables for a series (instead of querying the API). Defaults to FALSE
#' @tsn API argument. Defaults to NA
#' @ts_afs_name API argument. Defaults to NA
#' @in_ts_afs_name API argument. Defaults to NA
#' @ts_scientific_name API argument. Defaults to NA
#' @in_ts_scientific_name API argument. Defaults to NA
#' @region_name API argument. Defaults to NA
#' @state_name API argument. Defaults to NA
#' @year API argument. Defaults to NA
#' @month API argument. Defaults to NA
#' @pounds API argument. Defaults to NA
#' @dollars API argument. Defaults to NA
#' @tot_count API argument. Defaults to NA
#' @\source API argument. Defaults to NA
#' @collection API argument. Defaults to NA
#' @hts_number API argument. Defaults to NA
#' @\name API argument. Defaults to NA
#' @in_name API argument. Defaults to NA
#' @cntry_code API argument. Defaults to NA
#' @fao API argument. Defaults to NA
#' @cntry_name API argument. Defaults to NA
#' @in_cntry_name API argument. Defaults to NA
#' @district_code API argument. Defaults to NA
#' @district_name API argument. Defaults to NA
#' @in_district_name API argument. Defaults to NA
#' @edible_code API argument. Defaults to NA
#' @kilos API argument. Defaults to NA
#' @val API argument. Defaults to NA
#' @association API argument. Defaults to NA
#' @rfmo API argument. Defaults to NA
#' @nmfs_region_code API argument. Defaults to NA
#' @\source_species_code API argument. Defaults to NA
#' @ss_name API argument. Defaults to NA
#' @in_ss_name API argument. Defaults to NA
#' @nmfs_species_code API argument. Defaults to NA
#' @\source_id API argument. Defaults to NA
#' @tsn_species_id API argument. Defaults to NA
#' @gear_id API argument. Defaults to NA
#' @isscfg API argument. Defaults to NA
#' @g_name_short API argument. Defaults to NA
#' @g_name_long API argument. Defaults to NA
#' @in_g_name_short API argument. Defaults to NA
#' @in_g_name_long API argument. Defaults to NA
#' @g_desc API argument. Defaults to NA
#' @g_level_ind API argument. Defaults to NA
#' @g_parent_id API argument. Defaults to NA
#' @old_itis API argument. Defaults to NA
#' @new_itis API argument. Defaults to NA
#' @\source_gear_code API argument. Defaults to NA
#' @sg_name API argument. Defaults to NA
#' @in_sg_name API argument. Defaults to NA
#' @ts_complex API argument. Defaults to NA
#' @ts_confidential_name API argument. Defaults to NA
#' @in_ts_confidential_name API argument. Defaults to NA
#' @ts_finfish_ind API argument. Defaults to NA
#' @ts_habitat_ind API argument. Defaults to NA
#' @protected API argument. Defaults to NA
#' @reported API argument. Defaults to NA
#' @\family_itis API argument. Defaults to NA
#' @\family_name API argument. Defaults to NA
#' @in_family_name API argument. Defaults to NA
#' @superclass_itis API argument. Defaults to NA
#' @superclass_name API argument. Defaults to NA
#' @in_superclass_name API argument. Defaults to NA
#' @ts_kingdom API argument. Defaults to NA
#' @in_ts_kingdom API argument. Defaults to NA
#' @genus_itis API argument. Defaults to NA
#' @genus_name API argument. Defaults to NA
#' @in_genus_name API argument. Defaults to NA
#' @\family_sc_name API argument. Defaults to NA
#' @in_family_sc_name API argument. Defaults to NA
#' @genus_sc_name API argument. Defaults to NA
#' @in_genus_sc_name API argument. Defaults to NA
#' @export

query_foss <- function(series = NA,
                    allow_duplicates = FALSE,
                    list_variables = FALSE,
                    tsn = NA,
                    ts_afs_name = NA,
                    in_ts_afs_name = NA,
                    ts_scientific_name = NA,
                    in_ts_scientific_name = NA,
                    region_name = NA,
                    state_name = NA,
                    year = NA,
                    month = NA,
                    pounds = NA,
                    dollars = NA,
                    tot_count = NA,
                    source = NA,
                    collection = NA,
                    hts_number = NA,
                    name = NA,
                    in_name = NA,
                    cntry_code = NA,
                    fao = NA,
                    cntry_name = NA,
                    in_cntry_name = NA,
                    district_code = NA,
                    district_name = NA,
                    in_district_name = NA,
                    edible_code = NA,
                    kilos = NA,
                    val = NA,
                    association = NA,
                    rfmo = NA,
                    nmfs_region_code = NA,
                    source_species_code = NA,
                    ss_name = NA,
                    in_ss_name = NA,
                    nmfs_species_code = NA,
                    source_id = NA,
                    tsn_species_id = NA,
                    gear_id = NA,
                    isscfg = NA,
                    g_name_short = NA,
                    g_name_long = NA,
                    in_g_name_short = NA,
                    in_g_name_long = NA,
                    g_desc = NA,
                    g_level_ind = NA,
                    g_parent_id = NA,
                    old_itis = NA,
                    new_itis = NA,
                    source_gear_code = NA,
                    sg_name = NA,
                    in_sg_name = NA,
                    ts_complex = NA,
                    ts_confidential_name = NA,
                    in_ts_confidential_name = NA,
                    ts_finfish_ind = NA,
                    ts_habitat_ind = NA,
                    protected = NA,
                    reported = NA,
                    family_itis = NA,
                    family_name = NA,
                    in_family_name = NA,
                    superclass_itis = NA,
                    superclass_name = NA,
                    in_superclass_name = NA,
                    ts_kingdom = NA,
                    in_ts_kingdom = NA,
                    genus_itis = NA,
                    genus_name = NA,
                    in_genus_name = NA,
                    family_sc_name = NA,
                    in_family_sc_name = NA,
                    genus_sc_name = NA,
                    in_genus_sc_name = NA){

  if(!tolower(series) %in% c("landings", "trade_data", "source_species", "gears", "itis_history", "source_gears", "tsn_species")){
    print("Please enter a data series from the list: landings, trade_data, source_species, gears, itis_history, source_gears, tsn_species")
    return(NULL)
  }

  if(list_variables == TRUE){

    if(tolower(series) == "landings"){

      print("Numeric Variables:")
      print("tsn, year, pounds, dollars, tot_count")
      print("Character Variables:")
      print("region_name, state_name, source, collection")
      print("Name Variables:")
      print("ts_afs_name/in_ts_afs_name, ts_scientific_name/in_ts_scientific_name")

    }

    if(tolower(series) == "trade_data"){

      print("Numeric Variables:")
      print("year, hts_number, cntry_code, fao, district_code, kilos, val")
      print("Character Variables:")
      print("month, cntry_name, edible_code, source, association, rfmo, nmfs_region_code")
      print("Name Variables:")
      print("name/in_name, cntry_name/in_cntry_name, district_name/in_district_name")

    }

    if(tolower(series) == "source_species"){

      print("Numeric Variables:")
      print("source_species_code, nmfs_species_code, source_id, tsn, tsn_species_id")
      print("Name Variables:")
      print("ss_name/in_ss_name")


    }

    if(tolower(series) == "gears"){

      print("Numeric Variables:")
      print("gear_id, g_level_ind, g_parent_id")
      print("Character Variables:")
      print("isscfg, g_desc (search in variable)")
      print("Name Variables:")
      print("g_name_short/in_g_name_short, g_name_long/in_g_name_long")

    }

    if(tolower(series) == "itis_history"){

      print("Numeric Variables:")
      print("old_itis, new_itis")

    }

    if(tolower(series) == "source_gears"){

      print("Numeric Variables:")
      print("gear_id, source_id")
      print("Character Variables:")
      print("source_gear_code")
      print("Name Variables:")
      print("sg_name/in_sg_name")

    }

    if(tolower(series) == "tsn_species"){

      print("Numeric Variables:")
      print("tsn_species_id, tsn, family_itis, superclass_itis, genus_itis")
      print("Character Variables:")
      print("ts_complex, ts_finfish_ind, ts_habitat_ind, protected, reported")
      print("Name Variables:")
      print("ts_afs_name/in_ts_afs_name, ts_scientific_name/in_ts_scientific_name, ts_confidential_name/in_ts_confidential_name, family_name/in_family_name, superclass_name/in_superclass_name, ts_kingdom/in_ts_kingdom, genus_name/in_genus_name, family_sc_name/in_family_sc_name, genus_sc_name/in_genus_sc_name")

    }

  } else {

    # Define a utility function

    toUpperFirst <- function(string){
      split.string <- strsplit(trimws(tolower(string), which = "both"), split = "")[[1]]
      return.string <- gsub(", ","",toString(append(toupper(split.string[1]), split.string[2:length(split.string)])))
      return(return.string)
    }

    ranger <- function(x) if (length(x) == 1) x else paste(x[1], x[length(x)], sep = ":")


    # Define the translation functions

    numeric_range = function(variable, m, json_df){
      variable <- as.data.frame(tapply(variable, cumsum(c(TRUE, diff(variable) != 1)), ranger))[,1]
      for(n in 1:length(variable)){
        if(stringr::str_detect(variable[n], ":")){
          temp = stringr::str_split(variable[n], ":")[[1]]
          temp[temp==""]<-"NULL"
          json_df[n,m] <- paste0("\"", deparse(substitute(variable)), "\":{\"$between\":[", temp[1], ",", temp[2], "]}")
        } else {
          json_df[n,m] <- paste0("\"", deparse(substitute(variable)), "\":\"", variable[n], "\"")
        }
      }
      return(json_df)
    }

    character_input = function(variable, m, json_df, type = "Upper"){
      for(n in 1:length(variable)){
        if(type == "Upper"){
          json_df[n,m] <- paste0("\"", deparse(substitute(variable)), "\":\"", toupper(variable[n]), "\"")
        }
        if(type == "Title"){
          json_df[n,m] <- paste0("\"", deparse(substitute(variable)), "\":\"", toUpperFirst(variable[n]), "\"")
        }
        if(type == "Lower"){
          json_df[n,m] <- paste0("\"", deparse(substitute(variable)), "\":\"", tolower(variable[n]), "\"")
        }
        if(type == "Exact"){
          json_df[n,m] <- paste0("\"", deparse(substitute(variable)), "\":\"", variable[n], "\"")
        }
      }
      return(json_df)
    }

    character_input_title = function(variable, m, json_df, option){
      for(n in 1:length(variable)){
        json_df[n,m] <- paste0("\"", deparse(substitute(variable)), "\":\"", toUpperFirst(tolower(variable[n])), "\"")
      }
      return(json_df)
    }

    character_like = function(variable, m, json_df, type = "Upper"){
      for(n in 1:length(variable)){
        if(type == "Upper"){
          json_df[n,m] <- paste0("\"", stringr::str_remove(deparse(substitute(variable)), "in_"), "\":{\"$like\":\"%", toupper(variable[n]), "%\"}")
        }
        if(type == "Title"){
          json_df[n,m] <- paste0("\"", stringr::str_remove(deparse(substitute(variable)), "in_"), "\":{\"$like\":\"%", tolower(variable[n]), "%\"}")
          json_df[n,m] <- paste0("\"", stringr::str_remove(deparse(substitute(variable)), "in_"), "\":{\"$like\":\"%", toUpperFirst(variable[n]), "%\"}")
        }
      }
      return(json_df)
    }


    json_df = data.frame() # Initialize the code dataframe

    m = 1 # Set a column counter

    if(tolower(series) == "landings"){

      # Each of these checks to see if there is an input and then transforms it into a URL snippet, checking to see if it's a range, and increments M.

      if(!is.na(tsn[1])){
        json_df <- numeric_range(tsn, m, json_df)
        m = m+1
      }

      if(!is.na(ts_afs_name[1]) & is.na(in_ts_afs_name[1])){
        json_df <- character_input(ts_afs_name, m, json_df)
        m = m+1
      }

      if(!is.na(in_ts_afs_name[1])){
        json_df <- character_like(in_ts_afs_name, m, json_df)
        m = m+1
      }

      if(!is.na(ts_scientific_name[1]) & is.na(in_ts_scientific_name[1])){
        json_df <- character_input(ts_scientific_name, m, json_df, type = "Title")
        m = m+1
      }

      if(!is.na(in_ts_scientific_name[1])){
        json_df <- character_like(in_ts_scientific_name, m, json_df, type = "Title")
        m = m+1
      }

      if(!is.na(region_name[1])){
        json_df <- character_input_title(region_name, m, json_df, type = "Exact")
        m = m+1
      }

      if(!is.na(state_name[1])){
        json_df <- character_input(state_name, m, json_df)
        m = m+1
      }

      if(!is.na(year[1])){
        json_df <- numeric_range(year, m, json_df)
        m = m+1
      }

      if(!is.na(pounds[1])){
        json_df <- numeric_range(pounds, m, json_df)
        m = m+1
      }

      if(!is.na(dollars[1])){
        json_df <- numeric_range(dollars, m, json_df)
        m = m+1
      }

      if(!is.na(tot_count[1])){
        json_df <- numeric_range(tot_count, m, json_df)
        m = m+1
      }

      if(!is.na(source[1])){
        json_df <- character_input(source, m, json_df)
        m = m+1
      }

      if(!is.na(collection[1])){
        json_df <- character_input_title(collection, m, json_df, type = "Title")
        m = m+1
      }

    }

    if(tolower(series) == "trade_data"){

      # Each of these checks to see if there is an input and then transforms it into a URL snippet, checking to see if it's a range, and increments M.

      if(!is.na(year[1])){
        json_df <- numeric_range(year, m, json_df)
        m = m+1
      }

      if(!is.na(month[1])){
        json_df <- character_input(month, m, json_df, type = "Exact")
        m = m+1
      }

      if(!is.na(hts_number[1])){
        json_df <- numeric_range(hts_number, m, json_df)
        m = m+1
      }

      if(!is.na(name[1]) & is.na(in_name[1])){
        json_df <- character_input(name, m, json_df)
        m = m+1
      }

      if(!is.na(in_name[1])){
        json_df <- character_like(in_name, m, json_df)
        m = m+1
      }

      if(!is.na(cntry_code[1])){
        json_df <- numeric_range(cntry_code, m, json_df)
        m = m+1
      }

      if(!is.na(fao[1])){
        json_df <- numeric_range(fao, m, json_df)
        m = m+1
      }

      if(!is.na(cntry_name[1]) & is.na(in_cntry_name[1])){
        json_df <- character_input(cntry_name, m, json_df)
        m = m+1
      }

      if(!is.na(in_cntry_name[1])){
        json_df <- character_like(in_cntry_name, m, json_df)
        m = m+1
      }

      if(!is.na(district_code[1])){
        json_df <- numeric_range(district_code, m, json_df)
        m = m+1
      }

      if(!is.na(district_name[1]) & is.na(in_district_name[1])){
        json_df <- character_input(district_name, m, json_df)
        m = m+1
      }

      if(!is.na(in_district_name[1])){
        json_df <- character_like(in_district_name, m, json_df)
        m = m+1
      }

      if(!is.na(edible_code[1])){
        json_df <- character_like(edible_code, m, json_df)
        m = m+1
      }

      if(!is.na(kilos[1])){
        json_df <- numeric_range(kilos, m, json_df)
        m = m+1
      }

      if(!is.na(val[1])){
        json_df <- numeric_range(val, m, json_df)
        m = m+1
      }

      if(!is.na(source[1])){
        json_df <- character_input(source, m, json_df)
        m = m+1
      }

      if(!is.na(association[1])){
        json_df <- character_like(association, m, json_df)
        m = m+1
      }

      if(!is.na(rfmo[1])){
        json_df <- character_input(rfmo, m, json_df)
        m = m+1
      }

      if(!is.na(nmfs_region_code[1])){
        json_df <- character_input(nmfs_region_code, m, json_df)
        m = m+1
      }
    }

    if(tolower(series) == "source_species"){

      # Each of these checks to see if there is an input and then transforms it into a URL snippet, checking to see if it's a range, and increments M.

      if(!is.na(source_species_code[1])){
        json_df <- numeric_range(source_species_code, m, json_df)
        m = m+1
      }

      if(!is.na(ss_name[1]) & is.na(in_ss_name[1])){
        json_df <- character_input(ss_name, m, json_df)
        m = m+1
      }

      if(!is.na(in_ss_name[1])){
        json_df <- character_like(in_ss_name, m, json_df)
        m = m+1
      }

      if(!is.na(nmfs_species_code[1])){
        json_df <- numeric_range(nmfs_species_code, m, json_df)
        m = m+1
      }


      if(!is.na(source_id[1])){
        json_df <- numeric_range(source_id, m, json_df)
        m = m+1
      }

      if(!is.na(tsn[1])){
        json_df <- numeric_range(tsn, m, json_df)
        m = m+1
      }

      if(!is.na(tsn_species_id[1])){
        json_df <- numeric_range(tsn_species_id, m, json_df)
        m = m+1
      }
    }

    if(tolower(series) == "gears"){

      # Each of these checks to see if there is an input and then transforms it into a URL snippet, checking to see if it's a range, and increments M.

      if(!is.na(gear_id[1])){
        json_df <- numeric_range(gear_id, m, json_df)
        m = m+1
      }

      if(!is.na(isscfg[1])){
        json_df <- character_input(isscfg, m, json_df)
        m = m+1
      }

      if(!is.na(g_name_short[1]) & is.na(in_g_name_short[1])){
        json_df <- character_input(g_name_short, m, json_df)
        m = m+1
      }

      if(!is.na(in_g_name_short[1])){
        json_df <- character_like(in_g_name_short, m, json_df)
        m = m+1
      }

      if(!is.na(g_name_long[1]) & is.na(in_g_name_long[1])){
        json_df <- character_input(g_name_long, m, json_df)
        m = m+1
      }

      if(!is.na(in_g_name_long[1])){
        json_df <- character_like(in_g_name_long, m, json_df)
        m = m+1
      }

      if(!is.na(g_desc[1])){
        json_df <- character_like(g_desc, m, json_df, type = "Title")
        m = m+1
      }


      if(!is.na(g_level_ind[1])){
        json_df <- numeric_range(g_level_ind, m, json_df)
        m = m+1
      }

      if(!is.na(g_parent_id[1])){
        json_df <- numeric_range(g_parent_id, m, json_df)
        m = m+1
      }
    }

    if(tolower(series) == "itis_history"){

      # Each of these checks to see if there is an input and then transforms it into a URL snippet, checking to see if it's a range, and increments M.

      if(!is.na(old_itis[1])){
        json_df <- numeric_range(old_itis, m, json_df)
        m = m+1
      }

      if(!is.na(new_itis[1])){
        json_df <- numeric_range(new_itis, m, json_df)
        m = m+1
      }
    }

    if(tolower(series) == "source_gears"){

      # Each of these checks to see if there is an input and then transforms it into a URL snippet, checking to see if it's a range, and increments M.

      if(!is.na(source_gear_code[1])){
        json_df <- character_input(source_gear_code, m, json_df)
        m = m+1
      }


      if(!is.na(sg_name[1]) & is.na(in_sg_name[1])){
        json_df <- character_input(sg_name, m, json_df)
        m = m+1
      }

      if(!is.na(in_sg_name[1])){
        json_df <- character_like(in_sg_name, m, json_df, type = "Title")
        m = m+1
      }


      if(!is.na(gear_id[1])){
        json_df <- numeric_range(gear_id, m, json_df)
        m = m+1
      }

      if(!is.na(source_id[1])){
        json_df <- numeric_range(source_id, m, json_df)
        m = m+1
      }
    }

    if(tolower(series) == "tsn_species"){

      # Each of these checks to see if there is an input and then transforms it into a URL snippet, checking to see if it's a range, and increments M.

      if(!is.na(tsn_species_id[1])){
        json_df <- numeric_range(tsn_species_id, m, json_df)
        m = m+1
      }

      if(!is.na(tsn[1])){
        json_df <- numeric_range(tsn, m, json_df)
        m = m+1
      }


      if(!is.na(ts_afs_name[1]) & is.na(in_ts_afs_name[1])){
        json_df <- character_input(ts_afs_name, m, json_df)
        m = m+1
      }

      if(!is.na(in_ts_afs_name[1])){
        json_df <- character_like(in_ts_afs_name, m, json_df)
        m = m+1
      }

      if(!is.na(ts_scientific_name[1]) & is.na(in_ts_scientific_name[1])){
        json_df <- character_input(ts_scientific_name, m, json_df, type = "Title")
        m = m+1
      }

      if(!is.na(in_ts_scientific_name[1])){
        json_df <- character_like(in_ts_scientific_name, m, json_df, type = "Title")
        m = m+1
      }

      if(!is.na(ts_confidential_name[1]) & is.na(in_ts_confidential_name[1])){
        json_df <- character_input(ts_confidential_name, m, json_df)
        m = m+1
      }

      if(!is.na(in_ts_confidential_name[1])){
        json_df <- character_like(in_ts_confidential_name, m, json_df)
        m = m+1
      }


      if(!is.na(ts_complex[1])){
        json_df <- character_input(ts_complex, m, json_df, type = "Title")
        m = m+1
      }

      if(!is.na(ts_finfish_ind[1])){
        json_df <- character_input(ts_finfish_ind, m, json_df)
        m = m+1
      }

      if(!is.na(ts_habitat_ind[1])){
        json_df <- character_input(ts_habitat_ind, m, json_df)
        m = m+1
      }

      if(!is.na(protected[1])){
        json_df <- character_input(protected, m, json_df)
        m = m+1
      }

      if(!is.na(reported[1])){
        json_df <- character_input(reported, m, json_df)
        m = m+1
      }

      if(!is.na(family_itis[1])){
        json_df <- numeric_range(family_itis, m, json_df)
        m = m+1
      }

      if(!is.na(family_name[1]) & is.na(in_family_name[1])){
        json_df <- character_input(family_name, m, json_df)
        m = m+1
      }

      if(!is.na(in_family_name[1])){
        json_df <- character_like(in_family_name, m, json_df)
        m = m+1
      }

      if(!is.na(superclass_itis[1])){
        json_df <- numeric_range(superclass_itis, m, json_df)
        m = m+1
      }

      if(!is.na(superclass_name[1]) & is.na(in_superclass_name[1])){
        json_df <- character_input(superclass_name, m, json_df)
        m = m+1
      }

      if(!is.na(in_superclass_name[1])){
        json_df <- character_like(in_superclass_name, m, json_df)
        m = m+1
      }

      if(!is.na(ts_kingdom[1]) & is.na(in_ts_kingdom[1])){
        json_df <- character_input(ts_kingdom, m, json_df, type = "Title")
        m = m+1
      }

      if(!is.na(in_ts_kingdom[1])){
        json_df <- character_like(in_ts_kingdom, m, json_df, type = "Title")
        m = m+1
      }

      if(!is.na(genus_itis[1])){
        json_df <- numeric_range(genus_itis, m, json_df)
        m = m+1
      }

      if(!is.na(genus_name[1]) & is.na(in_genus_name[1])){
        json_df <- character_input(genus_name, m, json_df)
        m = m+1
      }

      if(!is.na(in_genus_name[1])){
        json_df <- character_like(in_genus_name, m, json_df)
        m = m+1
      }

      if(!is.na(family_sc_name[1]) & is.na(in_family_sc_name[1])){
        json_df <- character_input(family_sc_name, m, json_df)
        m = m+1
      }

      if(!is.na(in_family_sc_name[1])){
        json_df <- character_like(in_family_sc_name, m, json_df)
        m = m+1
      }

      if(!is.na(genus_sc_name[1]) & is.na(in_genus_sc_name[1])){
        json_df <- character_input(genus_sc_name, m, json_df)
        m = m+1
      }

      if(!is.na(in_genus_sc_name[1])){
        json_df <- character_like(in_genus_sc_name, m, json_df)
        m = m+1
      }
    }

    # Create an expand grid of the code snippets and drops NAs. This pairs each input with each other input (but several lists will make it run long).
    json_grid = expand.grid(json_df)
    json_grid = tidyr::drop_na(json_grid)

    json_objects = c() # Initialize a holder for the final json objects

    # Iterates through the expand grid, forms the json objects, and places them in the holder.
    for(n in 1:length(json_grid$V1)){
      temp = "{"
      for(m in 1:length(json_grid)){
        temp = paste0(temp, json_grid[n,m])
        if(m < length(json_grid)){
          temp = paste0(temp, ", ")
        } else {
          temp = paste0(temp, "}")
        }
      }
      json_objects = append(json_objects, temp)
    }

    if(length(json_objects)>25){
      print("There are a lot of json_objects here... Did you use numerics to specify a range instead of a string? Check ?query_foss for more information.")
    }
    if(length(json_objects)>500){
      print("There are more than 500 json_objects here. It is likely you used numerics to specify a range instead of a string. Check ?query_foss for more information. Aborting query.")
      return(NULL)
    }

    api_output = data.frame() # Initializes a holder for the API output

    # Iterates through the json objects and calls the API
    for(n in json_objects){

      url = paste0("https://apps-st.fisheries.noaa.gov/ods/foss/", tolower(series), "/")
      temp = httr::GET(url, query = list(q = eval(n), limit = 10000))
      print(temp)
      if(temp$status_code==503){
        print("API Service Temporarily Unavailable")
        return(NULL)
      }
      tomp = jsonlite::fromJSON(rawToChar(temp$content))
      api_output = dplyr::bind_rows(api_output, tomp$items)
      counter = 0

      # Checks to see if there is more data on the next page and grabs it if so
      while(tomp$hasMore==TRUE){
        link = tomp$links$href[tomp$links$rel=="next"]
        temp = httr::GET(link)
        tomp = jsonlite::fromJSON(rawToChar(temp$content))
        api_output = dplyr::bind_rows(api_output, tomp$items)
        counter = counter +1
        if(counter>10){
          print("This is taking a suspiciously long time... Is your computer connected to the internet?")
        }
      }

    }

    if(length(api_output)==0){
      print("Empty Dataset")
      return(NULL)
    }

    # Each entry has a unique link and there is risk of duplication with this wrapper method, so this filters duplicates out (unless otherwise specified).
    if(allow_duplicates==FALSE){
      api_output = dplyr::filter(api_output, !duplicated(api_output$links))
    }

    # Returns the output
    return(api_output)
  }
}
