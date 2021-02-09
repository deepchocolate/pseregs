#' A class to represent the Swedish national school register (NSR).
#' @slot subjects The list of subjects available in the NSR.
#' @include GPA.R
#' @export
checkState <- function (object) {
  state <- T
  #if (length(object@subjects) == 0) state <- 'Bla'
  return(state)
}

NSR = setClass("NSR",
         representation(data="character", SUBJECTNAMES='vector', DESCRIPTIONS='list'),
         prototype = list(
           SUBJECTNAMES=names(SUBJECTNAMES),
           DESCRIPTIONS=SUBJECTNAMES
           ),
         validity=checkState)
setMethod("initialize", "NSR", function (.Object, data) {
  callNextMethod(.Object, data=deparse(substitute(data)))
})
setGeneric("GPA", function (object, data, subjects=F) standardGeneric("GPA"))
setMethod("GPA", signature("NSR"), function (object, data, subjects=F) {
  if (!is.vector(subjects)) subjects <- object@SUBJECTNAMES
  subjects <- base::intersect(object@SUBJECTNAMES, subjects)
  rowMeans(data[,subjects])
})
setGeneric("setSubjects", function (subjects, object) standardGeneric("setSubjects"))
setMethod("setSubjects", signature(subjects="list", object="NSR"),
          function (subjects, object) {
            object@subjects <- subjects
            object
          })
