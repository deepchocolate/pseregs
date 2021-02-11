#' Convert alphabetical grade to numerical
#'
#' This function takes vectors of year and raw grades in a subject
#' and converts it into a number between 0 and 20 according to the
#' official scoring table by Skolverket.
#'
#' @param grade A vector of grades (e.g., A, E, VG, F)
#' @param year A vector of graduation years for the corrsponding grade.
#' @export
#'
scoreGrade <- function (grade, year) {
  bef2013 <-  year <= 2012
  scores <- list(M=20, V=15, G=10, '1'=0, A=NA, F=NA, '2'=NA)
  for (g in names(scores)) {
    if (length(grade[bef2013 & grade==g]) > 0) grade[bef2013 & grade==g] <- scores[[g]];
  }
  aft2012 <- year >= 2013
  scores <- list(A=20, B=17.5, C=15, D=12.5, E=10, F=0, '9'=NA, X=NA, '3'=NA, '2'=NA, Y=NA, M=20, V=15, G=10, '1'=0)
  for (g in names(scores)) {
    if (length(grade[aft2012 & grade==g])) grade[aft2012 & grade==g] <-scores[[g]];
  }
  # Set remaining to missing.
  if (length(grade[!is.na(grade) & !grade %in% scores]) > 0) grade[!is.na(grade) & !grade %in% scores] <- NA
  return(as.numeric(grade))
}
