

#' Identify all spouses for an individual
#'
#' @param gedcom A tidyged object.
#' @param individual The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param return_name Whether to return the spouse's name(s) instead of the xref(s).
#'
#' @return A character vector of spouse xrefs or names.
#' @export
#' @examples
#' get_spouses(tidyged::sample555, "@I1@")
#' get_spouses(tidyged::sample555, "@I1@", return_name = TRUE)
#' get_spouses(tidyged::sample555, "@I3@")
#' @tests
#' expect_equal(get_spouses(tidyged::sample555, "@I1@"), "@I2@")
#' expect_equal(get_spouses(tidyged::sample555, "@I2@", TRUE), "Robert Eugene Williams")
#' expect_equal(get_spouses(tidyged::sample555, "@I3@"), character(0))
get_spouses <- function(gedcom,
                        individual = character(),
                        return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, tidyged::is_indi)
  
  fams_xref <- get_families_as_spouse(gedcom, xref)
  
  spou_xref <- dplyr::filter(gedcom, level == 1, record %in% fams_xref, tag %in% c("HUSB","WIFE"),
                              value != xref)$value
    
  if (return_name) {
    purrr::map_chr(spou_xref, indi_name, gedcom=gedcom)
  } else {
    spou_xref
  }
}


#' Identify all children for an individual
#'
#' @param gedcom A tidyged object.
#' @param individual The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param return_name Whether to return the childrens name(s) instead of the xref(s).
#'
#' @return A character vector of children xrefs or names.
#' @export
#' @examples 
#' get_children(tidyged::sample555, "@I2@")
#' get_children(tidyged::sample555, "@I2@", return_name = TRUE)
#' @tests
#' expect_error(get_children(tidyged::sample555, "@I4@"))
#' expect_equal(get_children(tidyged::sample555, "@I1@"), "@I3@")
#' expect_equal(get_children(tidyged::sample555, "@I2@", TRUE), "Joe Williams")
get_children <- function(gedcom,
                         individual = character(),
                         return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, tidyged::is_indi)
  
  fams_xref <- get_families_as_spouse(gedcom, xref)
  
  chil_xref <- unique(dplyr::filter(gedcom, level == 1, record %in% fams_xref, tag == "CHIL")$value)
  
  if (return_name) {
    purrr::map_chr(chil_xref, indi_name, gedcom=gedcom)
  } else {
    chil_xref
  }
  
}

#' Identify all parents for an individual
#'
#' @param gedcom A tidyged object.
#' @param individual The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param return_name Whether to return the parents name(s) instead of the xref(s).
#'
#' @return A character vector of parent xrefs or names.
#' @export
#' @examples
#' get_parents(tidyged::sample555, "@I2@")
#' get_parents(tidyged::sample555, "@I3@")
#' get_parents(tidyged::sample555, "@I3@", return_name = TRUE)
#' @tests
#' expect_equal(get_parents(tidyged::sample555, "@I3@"), c("@I1@", "@I2@"))
#' expect_equal(get_parents(tidyged::sample555, "@I3@", TRUE), c("Robert Eugene Williams", "Mary Ann Wilson"))
get_parents <- function(gedcom,
                        individual = character(),
                        return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, tidyged::is_indi)
  
  famc_xref <- get_families_as_child(gedcom, xref)
  
  par_xref <- unique(dplyr::filter(gedcom, level == 1, record %in% famc_xref, tag %in% c("HUSB","WIFE"))$value)
  
  if (return_name) {
    purrr::map_chr(par_xref, indi_name, gedcom=gedcom)
  } else {
    par_xref
  }
  
}


#' Identify all siblings for an individual
#'
#' @param gedcom A tidyged object.
#' @param individual The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param include_half_siblings Whether to include siblings that only share one parent.
#' @param return_name Whether to return the parents name(s) instead of the xref(s).
#'
#' @return A character vector of sibling xrefs or names.
#' @export
get_siblings <- function(gedcom,
                        individual = character(),
                        include_half_siblings = FALSE,
                        return_name = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, tidyged::is_indi)
  
  if (include_half_siblings) {
    par_xref <- get_parents(gedcom, xref)
    
    sib_xref <- purrr::map(par_xref, get_children, gedcom = gedcom) %>% 
      unlist() %>%
      purrr::discard(. == xref) %>% 
      unique()
  } else {
    famc_xref <- get_families_as_child(gedcom, xref)
    
    sib_xref <- unique(dplyr::filter(gedcom, level == 1, record %in% famc_xref, tag == "CHIL", value != xref)$value)
  }
  
  if (return_name) {
    purrr::map_chr(sib_xref, indi_name, gedcom=gedcom)
  } else {
    sib_xref
  }
  
}

#' Identify all families for an individual where they are a spouse
#'
#' @param gedcom A tidyged object.
#' @param individual The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#'
#' @return A character vector of family xrefs.
#' @export
#' @examples 
#' get_families_as_spouse(tidyged::sample555, "@I2@")
#' @tests
#' expect_equal(get_families_as_spouse(tidyged::sample555, "@I1@"), c("@F1@", "@F2@"))
#' expect_equal(get_families_as_spouse(tidyged::sample555, "@I2@"), "@F1@")
get_families_as_spouse <- function(gedcom, individual = character()) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, tidyged::is_indi)
  
  unique(dplyr::filter(gedcom, record == xref, level == 1, tag == "FAMS")$value) 
  
}

#' Identify all families for an individual where they are a child
#'
#' @param gedcom A tidyged object.
#' @param individual The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param birth_only Whether to only return the family containing the biological parents.
#'
#' @return A character vector of family xrefs.
#' @examples 
#' get_families_as_child(tidyged::sample555, "@I3@")
#' @export
#' @tests
#' expect_equal(get_families_as_child(tidyged::sample555, "@I3@"), c("@F1@", "@F2@"))
get_families_as_child <- function(gedcom,
                                  individual = character(),
                                  birth_only = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, tidyged::is_indi)
  
  # return all family links
  famc <- unique(dplyr::filter(gedcom, record == xref, tag == "FAMC")$value)
  
  if(length(famc) == 0) return(NULL)
  if(!birth_only) return(famc)
  
  # Look in birth events first
  famc_evt <- gedcom_value(gedcom, xref, "FAMC", 2, "BIRT")
  if(famc_evt != "") return(famc_evt)
  
  # Look at pedigrees
  if(length(famc) == 1) {
    if(tolower(gedcom_value(gedcom, xref, "PEDI", 2, "FAMC")) == "birth") 
      return(famc)
  } else {
    famcs <- dplyr::filter(gedcom, record == xref, tag %in% c("FAMC","PEDI"))
    
    row <- which(famcs$value == "birth")
    if(length(row) == 1) return(famcs$value[row - 1])
  }
    
  # assume first family is birth family
  gedcom_value(gedcom, xref, "FAMC", 1, "INDI")
  
}


#' Identify all supporting records for a set of records
#' 
#' This function gets all supporting records (and onwards dependencies) for a set of records. Supporting records
#' are note, multimedia, source, and repository records, i.e. those providing supporting evidence and comments.
#'
#' @param gedcom A tidyged object.
#' @param xrefs The xrefs of records to get supporting records for.
#' @param include_note Whether to include Note records.
#' @param include_media Whether to include Multimedia records.
#' @param include_sour Whether to include Source records.
#' @param include_repo Whether to include Repository records.
#'
#' @return A character vector of supporting record xrefs.
#' @export
#' @examples 
#' get_supporting_records(tidyged::sample555, "@I1@")
#' get_supporting_records(tidyged::sample555, "@F1@")
#' @tests
#' expect_equal(get_supporting_records(tidyged::sample555, "@I1@"), c("@S1@", "@R1@"))
get_supporting_records <- function(gedcom,
                                   xrefs,
                                   include_note = TRUE,
                                   include_media = TRUE,
                                   include_sour = TRUE,
                                   include_repo = TRUE) {
  
  if (length(xrefs) == 0) return(NULL)
  
  tags <- NULL
  if (include_note) tags <- c(tags, "NOTE")
  if (include_media) tags <- c(tags, "OBJE")
  if (include_sour) tags <- c(tags, "SOUR")
  if (include_repo) tags <- c(tags, "REPO")
  
  links <- unique(dplyr::filter(gedcom, 
                                record %in% xrefs, 
                                tag %in% tags, 
                                stringr::str_detect(value, tidyged.internals::reg_xref(TRUE)))$value)
  
  unique(
    c(links,
    get_supporting_records(gedcom, links, include_note, include_media, include_sour, include_repo))
  )
  
}


#' Identify all descendants for an individual
#' 
#' This function identifies records in an entire branch of the family tree below a certain individual.
#' 
#' @param gedcom A tidyged object.
#' @param individual The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param include_individual Whether to also include the individual themselves.
#' @param include_spouses Whether to also include all spouses of this individual (and their descendants and
#' descendants' spouses).
#' @param include_families Whether to also include all Family Group records where this individual is a spouse 
#' (and all descendants' Family Group records).
#' @param include_supp_records Whether to also include all supporting records (Note, Source, Repository, Multimedia).
#'
#' @return A vector of xrefs of descendants.
#' @export
#' @tests
#' expect_equal(get_descendants(tidyged::sample555, "@I1@"), "@I3@")
#' expect_equal(get_descendants(tidyged::sample555, "@I1@", TRUE), c("@I1@","@I3@"))
#' expect_equal(get_descendants(tidyged::sample555, "@I1@", TRUE, TRUE), c("@I2@","@I1@","@I3@"))
#' expect_equal(get_descendants(tidyged::sample555, "@I1@", TRUE, TRUE, TRUE), c("@F1@","@F2@","@I2@","@I1@","@I3@"))
get_descendants <- function(gedcom,
                            individual = character(),
                            include_individual = FALSE,
                            include_spouses = FALSE,
                            include_families = FALSE,
                            include_supp_records = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, tidyged::is_indi)
  
  return_xrefs <- NULL
  
  spou_xref <- get_spouses(gedcom, xref)
  chil_xref <- get_children(gedcom, xref)
  fams_xref <- get_families_as_spouse(gedcom, xref)
  
  # if spouse is to be included, add their children to be included
  if (include_spouses) {
    spou_chil <- unlist(purrr::map(spou_xref, get_children, gedcom=gedcom))

    chil_xref <- unique(c(chil_xref, spou_chil))
  }
  
  #deal with family groups first (while the individuals are still in them)
  if (include_families) return_xrefs <- c(return_xrefs, fams_xref)
  if (include_spouses) return_xrefs <- c(return_xrefs, spou_xref)
  if (include_individual) return_xrefs <- c(return_xrefs, xref)
  
  # identify children
  for(i in seq_along(chil_xref)) {
    return_xrefs <- c(return_xrefs,
                      get_descendants(gedcom, chil_xref[i], TRUE, include_spouses, include_families, FALSE))
  }
  
  # only get supporting records if this is the top level call
  if (include_supp_records && length(as.character(sys.call())) == 7 && 
      any(as.character(sys.call()) != c("get_descendants","gedcom","chil_xref[i]",
                                        "TRUE","include_spouses","include_families","FALSE"))){
    
    c(return_xrefs,
      get_supporting_records(gedcom, return_xrefs))
  } else {
    return_xrefs
  }
  
}


#' Identify all ancestors for an individual
#' 
#' This function identifies records in an entire branch of the family tree above a certain individual.
#' 
#' @param gedcom A tidyged object.
#' @param individual The xref of an Individual record to act on if one 
#' is not activated (will override active record).
#' @param include_individual Whether to also include the individual themselves.
#' @param include_siblings Whether to also include all siblings of ancestors (siblings of this individual will only be
#' included if the individual is included).
#' @param include_families Whether to also include all Family Group records where this individual is a child 
#' (and all ancestors' Family Group records).
#' @param include_supp_records Whether to also include all supporting records (Note, Source, Repository, Multimedia).
#'
#' @return A vector of xrefs of ancestors.
#' @examples 
#' get_ancestors(tidyged::sample555, "@I3@")
#' get_ancestors(tidyged::sample555, "@I3@", include_individual = TRUE)
#' get_ancestors(tidyged::sample555, "@I3@", include_individual = TRUE, include_families = TRUE)
#' @export
get_ancestors <- function(gedcom,
                          individual = character(),
                          include_individual = FALSE,
                          include_siblings = FALSE,
                          include_families = FALSE,
                          include_supp_records = FALSE) {
  
  xref <- get_valid_xref(gedcom, individual, .pkgenv$record_string_indi, tidyged::is_indi)
  
  return_xrefs <- NULL

  sib_xref <- get_siblings(gedcom, xref)
  par_xref <- get_parents(gedcom, xref)
  famc_xref <- get_families_as_child(gedcom, xref)
  
  if (include_individual & include_siblings) {
    sib_par <- unlist(purrr::map(sib_xref, get_parents, gedcom=gedcom))
    
    par_xref <- unique(c(par_xref, sib_par))
  }
  
  if (include_families) return_xrefs <- c(return_xrefs, famc_xref)
  if (include_individual & include_siblings) return_xrefs <- c(return_xrefs, sib_xref)
  if (include_individual) return_xrefs <- c(return_xrefs, xref)
  
  for(i in seq_along(par_xref)) {
    return_xrefs <- c(return_xrefs,
                      get_ancestors(gedcom, par_xref[i], TRUE, include_siblings, include_families, FALSE))
  }
  
  # only get supporting records if this is the top level call
  if (include_supp_records && length(as.character(sys.call())) == 7 && 
      any(as.character(sys.call()) != c("get_ancestors","gedcom","par_xref[i]",
                                        "TRUE","include_siblings","include_families","FALSE"))) {
    
    c(return_xrefs,
      get_supporting_records(gedcom, return_xrefs))
  } else {
    return_xrefs
  }
}


