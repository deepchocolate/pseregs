#' A class to represent the Swedish national school register (NSR).
#' @slot subjects The list of subjects available in the NSR.
#' @include GPA.R
#' @export
checkState <- function (object) {
  state <- T
  if (length(object@subjects) == 0) state <- 'Bla'
  return(state)
}
NSR = setClass("NSR",
         representation(subjects = "list"),
         validity=checkState)
setMethod("GPA", signature("NSR"),
          function (object) {
            GPA(object@subjects)
          })
setGeneric("setSubjects", function (subjects, object) standardGeneric("setSubjects"))
setMethod("setSubjects", signature(subjects="list", object="NSR"),
          function (subjects, object) {
            object@subjects <- subjects
            object
          })
