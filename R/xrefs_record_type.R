

xrefs_record_type <- function(gedcom, record_tag) {
  dplyr::filter(gedcom, level == 0 & tag == record_tag)$record
}

#' Get the xrefs of particular record types
#'
#' These functions return the xrefs of all records of a particular type in a tidyged object.
#'
#' @param gedcom A tidyged object.
#'
#' @return A vector of xrefs of records of the relevant type.
#' @export
#' @examples 
#' xrefs_indi(tidyged::sample555)
#' xrefs_famg(tidyged::sample555)
#' xrefs_note(tidyged::sample555)
#' xrefs_repo(tidyged::sample555)
#' xrefs_sour(tidyged::sample555)
#' @tests
#' expect_equal(xrefs_indi(tidyged::sample555), paste0("@I", 1:3, "@"))
xrefs_indi <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_indi) }

#' @export
#' @rdname xrefs_indi
#' @tests
#' expect_equal(xrefs_famg(tidyged::sample555), paste0("@F", 1:2, "@"))
xrefs_famg <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_famg) }

#' @export
#' @rdname xrefs_indi
#' @tests
#' expect_equal(xrefs_subm(tidyged::sample555), paste0("@U", 1, "@"))
xrefs_subm <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_subm) }

#' @export
#' @rdname xrefs_indi
#' @tests
#' expect_equal(xrefs_sour(tidyged::sample555), paste0("@S", 1, "@"))
xrefs_sour <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_sour) }

#' @export
#' @rdname xrefs_indi
xrefs_repo <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_repo) }

#' @export
#' @rdname xrefs_indi
xrefs_note <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_note) }

#' @export
#' @rdname xrefs_indi
xrefs_media <- function(gedcom) {  xrefs_record_type(gedcom, .pkgenv$record_tag_obje) }



