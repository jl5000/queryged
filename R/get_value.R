
#' Extract a particular value from a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param record_xref The xref of the record in which the value may exist.
#' @param tag The tag associated with the value.
#' @param level The level number of the value.
#' @param after_tag Whether the tag should be subordinate to this parent tag. 
#'
#' @return The particular value fitting the criteria of the input arguments. If no value is found,
#' an empty string is returned.
#' @export
#' @tests
#' expect_equal(gedcom_value(tidyged::sample555, "HD", "FORM", 2), "LINEAGE-LINKED")
#' expect_equal(gedcom_value(tidyged::sample555, "HD", "TEST", 1), "")
#' expect_equal(gedcom_value(tidyged::sample555, "HD", "VERS", 2), "5.5.5")
#' expect_equal(gedcom_value(tidyged::sample555, "HD", "VERS", 3), "5.5.5")
#' expect_equal(gedcom_value(tidyged::sample555, "@I1@", "VERS", 3), "")
gedcom_value <- function(gedcom, record_xref, tag, level, after_tag = NULL) {
  
  gedcom_filtered <- dplyr::filter(gedcom, record %in% record_xref)
  if(nrow(gedcom_filtered) == 0) return("")
  
  active <- is.null(after_tag)
  for(i in seq_len(nrow(gedcom_filtered))) {
    if(is.null(after_tag)) {
      active <- TRUE
    } else if(gedcom_filtered$tag[i] == after_tag && gedcom_filtered$level[i] < level) {
      active <- TRUE
    } else if(active && gedcom_filtered$level[i] < level){
      active <- FALSE
    }
    
    if(active) {
      if(gedcom_filtered$tag[i] == tag & gedcom_filtered$level[i] == level)
        return(gedcom_filtered$value[i])
    }
    
    if(i == nrow(gedcom_filtered)) return("")
  }
  
}


#' Get an individual's name
#'
#' @param gedcom A tidyged object.
#' @param xref An xref of an Individual record.
#'
#' @return The name of the individual. If one is not defined, the xref is returned.
#' @export
indi_name <- function(gedcom, xref){
  
  xref <- get_valid_xref(gedcom, xref, .pkgenv$record_string_indi, tidyged::is_indi)
  
  name <- gedcom_value(gedcom, xref, "NAME", 1, "INDI") %>% 
    stringr::str_remove_all("/")
  
  if(name == ""){
    xref
  } else {
    name
  }
  
}