

#' Identify the rows of subrecords in a tidyged object
#'
#' @param gedcom A tidyged object.
#' @param containing_level The level of the first line of the subrecord.
#' @param containing_tags The accepted tags of the first line of the subrecord.
#' @param containing_values The accepted values of the first line of the subrecord.
#' @param xrefs The xrefs of records containing the subrecord (default is all records).
#' @param first_only Whether to return only the first match found or to return all matches. 
#'
#' @return A vector of rows in the tidyged object of the subrecord(s).
#' @export
#' @tests
#' expect_equal(identify_section(tidyged::sample555, 0, "HEAD", ""), 1:18)
#' expect_equal(identify_section(tidyged::sample555, 1, "GEDC", "", first_only = TRUE), 2:5)
#' expect_equal(identify_section(tidyged::sample555, 2, "FORM", "LINEAGE-LINKED"), 4:5)
#' expect_equal(identify_section(tidyged::sample555, 3, "VERS", "5.5.5"), 5)
identify_section <- function(gedcom,
                             containing_level,
                             containing_tags,
                             containing_values = character(),
                             xrefs = character(),
                             first_only = FALSE) {
  
  no_xrefs_defined <- length(xrefs) == 0
  no_values_defined <- length(containing_values) == 0
  rows_to_return <- integer()
  
  active <- FALSE
  for(i in seq_len(nrow(gedcom))) {
    
    if(active) {
      if(gedcom$level[i] <= containing_level) {
        active <- FALSE
        if(first_only) break
      } else {
        rows_to_return <- c(rows_to_return, i)
      }
      
    }
    
    if(no_xrefs_defined || gedcom$record[i] %in% xrefs) {
      if(no_values_defined || gedcom$value[i] %in% containing_values) {
        if(gedcom$level[i] == containing_level & gedcom$tag[i] %in% containing_tags) {
          
          active <- TRUE
          rows_to_return <- c(rows_to_return, i) 
        } 
      }
    }
  }
  rows_to_return
  
}


#' Find a particular row position in a tidyged object.
#' 
#' This is for inserting rows at the end of a record or subrecord.
#'
#' @param gedcom A tidyged object.
#' @param xref The xref of the record where the insertion point will be.
#' @param parent_level The level of the row where the insertion point will be.
#' @param parent_tag The tag of the row where the insertion point will be.
#' @param parent_value The value of the row where the insertion point will be.
#'
#' @return The row after the insertion point in the tidyged object.
#' @export
#' @tests
#' expect_equal(find_insertion_point(tidyged::sample555, "HD", 2, "VERS"), 4)
#' expect_equal(find_insertion_point(tidyged::sample555, "HD", 3, "VERS"), 6)
#' expect_equal(find_insertion_point(tidyged::sample555, "HD", 1, "CHAR"), 7)
find_insertion_point <- function(gedcom,
                                 xref,
                                 parent_level,
                                 parent_tag,
                                 parent_value = NULL) {
  
  active <- FALSE
  for(i in seq_len(nrow(gedcom))) {
    
    if(active && gedcom$level[i] <= parent_level) break
    
    if(gedcom$record[i] == xref && gedcom$level[i] == parent_level && gedcom$tag[i] == parent_tag) {
      if(is.null(parent_value) || gedcom$value[i] == parent_value) {
        active <- TRUE  
      }
    } 
    
  }
  i
}

