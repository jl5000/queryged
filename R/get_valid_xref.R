
#' Derive a valid cross-reference identifier
#' 
#' Validate an xref provided explicitly or implicitly (through the active record).
#' 
#' @details This helper function is designed to derive and run validation checks on an xref
#' provided explicitly or implicitly. An xref is provided implicitly through the active
#' record of the tidyged object.
#' 
#' Once found, the xref is checked to ensure it is of the appropriate type.
#'
#' @param gedcom A tidyged object.
#' @param xref A record xref.
#' @param record_type A character string describing the record type. Generally one of
#' the global record_string_* values.
#' @param record_type_fn A function to check the record type. Generally one of the is_*
#' functions.
#'
#' @return A valid xref identifier.
get_valid_xref <- function(gedcom, xref, record_type, record_type_fn) {
  
  if (length(xref) == 0 || xref == "") {
    # xref not given explicitly, get it from active record
    xref <- tidyged::active_record(gedcom)
  } 
  
  if(is.null(xref))
    stop("No xref is provided and no ", record_type, " record is activated.")
  
  if(!grepl(tidyged.internals::reg_xref(TRUE), xref))
    stop("The provided xref is not valid")
  
  if(!record_type_fn(gedcom, xref))
    stop("The provided or active record is not a ", record_type, " record")
  
  xref
}

