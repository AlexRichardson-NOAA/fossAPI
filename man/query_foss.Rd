% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fossAPI_function.R
\name{query_foss}
\alias{query_foss}
\title{API Wrapper for NOAA FOSS Database}
\usage{
query_foss(
  series = NA,
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
  in_genus_sc_name = NA
)
}
\description{
The query_foss() function is designed to help the R-using public use simple notation to query the APIs in the FOSS database.
There are currently seven FOSS APIs that can be accessed through the fossAPI package: landings, trade_data, source_species, gears, itis_history, source_gears, and tsn_species.
Each API has a number of variables that one can use to limit the results of a query. To access these variables, specify one of the APIs above with the "series" argument and
set the "list_variables" argument to TRUE.
}
\details{
There are three types of variable - numeric, character, and name.

Numeric variables are coded in the data as numeric, but should still be input as strings in order to avoid over-taxing the FOSS system. For example,
the range between 100 and 200 should be specified as "100:200" - not 100:200. Multiple ranges can be input together as a list: c("100:200", "300:400")
will return all results between 100 and 200 and between 300 and 400, but will exclude any results below 100, between 200 and 300, and above 400. (Note
that some data fields seem like they should be numeric but were specified in the data as characters.)

Character variables must be entered as precise, non-case-sensitive string matches. For example, the arguments state_name = "california" or state_name
= "CALIFORNIA" will return all results from the state of California, but state_name = "cali" will return no matches.

Name variables may be specified either as precise, non-case-sensitive string matches or as partial matches using the "in_" modified version of their
variable name. For example, the argument ts_afs_name = "shrimp" will return no matches, but ts_afs_name = "shrimp, peppermint" does. However, the
argument in_ts_afs_name = "shrimp" will return all arguments for which the field ts_afs_name includes the string "shrimp".

Example:

landings_data = query_foss(series = "landings", in_ts_afs_name = c("shrimp", "fish"), pounds = c("0:1000", "5000:10000"), collection = "commercial", list_variables = F)
}
