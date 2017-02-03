#' Frogs sighting locations.
#'
#' Dataset containing sighting locations of L. raniformis in New Zealand.
#' The columns are as follows:
#'
#' \itemize{
#'   \item year: year of the sighting
#'   \item y: latitude in the New Zealand Transverse Mercatore coordinate system (length unit of measure: meter).
#'   \item x: longitude in the New Zealand Transverse Mercatore coordinate system (length unit of measure: meter).
#'   \item species: the name of the species.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name frogs
#' @usage data(frogs)
#' @format A data frame with 194 rows and 4 columns
NULL


#' Frogs sighting locations and the output of the EM() function.
#'
#' Dataset containing sighting locations of L. raniformis in New Zealand as returned by function EM().
#' The columns are as follows:
#'
#' \itemize{
#'   \item year: year of the sighting
#'   \item y: latitude in the New Zealand Transverse Mercatore coordinate system (length unit of measure: meter).
#'   \item x: longitude in the New Zealand Transverse Mercatore coordinate system (length unit of measure: meter).
#'   \item species: the name of the species.
#'   \item Pnat: probability of being of natural origin [0;1].
#'   \item Dist: distance from nearest point of natural origin or form anchor point.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name frogsEM
#' @usage data(frogsEM)
#' @format A data frame with 194 rows and 6 variables
NULL


#' Four Jackknife re-samplings done on dataset 'frogs'.
#'
#' List of four Jackknife re-samplings of data frame 'frogs' (as returned by function jackKnife()(see examples in ?jackKnife())).
#' The columns of all four data sets are as follows:
#'
#' \itemize{
#'   \item year: year of the sighting
#'   \item y: latitude in the New Zealand Transverse Mercatore coordinate system (length unit of measure: meter).
#'   \item x: longitude in the New Zealand Transverse Mercatore coordinate system (length unit of measure: meter).
#'   \item species: the name of the species.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name frogsJK
#' @usage data(frogsJK)
#' @format A list of four data frames with 164 rows and 6 columns each.
NULL


#' Simulated datasets for comparison with 'frogs' generated with function simulacro() (see examples in ?simulacro()).
#'
#' List of eight lists (one per different Alpha value used) each containing ten data frames of simulated sigthing locations built to emulate dataset 'frogs' (data(frogs)).
#' The columns of all 80 data sets are as follows:
#'
#' \itemize{
#'   \item year: year of the sighting
#'   \item y: latitude in the New Zealand Transverse Mercatore coordinate system (length unit of measure: meter).
#'   \item x: longitude in the New Zealand Transverse Mercatore coordinate system (length unit of measure: meter).
#'   \item species: the name of the species.
#'   \item Pnat: probability of being of natural origin [0;1].
#'   \item Dist: distance from nearest point of natural origin or form anchor point.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name frogsLacro
#' @usage data(frogsLacro)
#' @format list of 8 lists of 10 data frames each with 164 rows and 6 columns each.
NULL


#' Summary of the dissimilarity values of each of the data frames in 'frogsLacro' with data frame 'frogs'.
#'
#' Data frame with dissimilarity values and respective Alpha value as obtained by function modSel() (see examples in ?modSel()).
#' The columns are as follows:
#'
#' \itemize{
#'   \item dissimilarity: dissimilarity value
#'   \item compAlpha: alpha value of the comparison dataset.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name frogsSum
#' @usage data(frogsSum)
#' @format list of 8 lists of 10 data frames each with 164 rows and 6 columns each.
NULL


#' Landmasses of the two main islands of New Zealand.
#'
#' Object of class sp::SpatialPolygons of the North and South Islands of New Zealand in the New Zealand Transverse Mercatore projection.
#' Source of data: mapdata::worldHires().
#'
#' @docType data
#' @keywords datasets
#' @name nzp
#' @usage data(nzp)
#' @format object of class sp::SpatialPolygons.
NULL


#' Landmasses of the two main islands of New Zealand.
#'
#' Object of class spatstat::owin of the North and South Islands of New Zealand in the New Zealand Transverse Mercatore projection.
#'
#' @docType data
#' @keywords datasets
#' @name nzw
#' @usage data(nzw)
#' @format object of class spatstat::owin.
NULL
