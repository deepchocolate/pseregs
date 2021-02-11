checkState <- function (object) {
  state <- T
  #if (length(object@subjects) == 0) state <- 'Bla'
  return(state)
}
#' Create an NSR object
#' @param data A data frame.
#' @param subjects A list of subjects.
#' @export
NSR <- function (data, subjects=F) {
  dn = deparse(substitute(data))
  return(new('NSR', data=dn, subjects=subjects))
}

#' A class to represent the Swedish national school register (NSR).
#' @slot subjects The list of subjects available in the NSR.
#' @include GPA.R
setClass("NSR",
         prototype = list(
           ),
         slots=list(
           dataName="character",
           subjectNames='list',
           dataColumns='vector',
           SUBJECTNAMES='vector',
           DESCRIPTIONS='list'
         ),
         validity=checkState)
# Constructor
setMethod("initialize", "NSR", function (.Object, data, subjects=F) {
  .Object@SUBJECTNAMES = GRADES
  .Object@DESCRIPTIONS = DESCRIPTIONS
  dta = eval(parse(text=data), envir=globalenv())
  .Object = setColumns(.Object, colnames(dta))
  if (!is.list(subjects)) {
    subjects = as.list(.Object@SUBJECTNAMES)
    names(subjects) = .Object@SUBJECTNAMES
  }
  .Object = setSubjectNames(.Object, subjects)
  callNextMethod(.Object, dataName=data)
})
# Match grade columns (for internal use only)
# Returns a vector
setGeneric("matchGradeNames", function (object) standardGeneric("matchGradeNames"))
setMethod("matchGradeNames", signature("NSR"),
          function (object) {
            base::intersect(object@SUBJECTNAMES, object@subjectNames)
          })
# Get available grade columns names in data
setGeneric("getGradeColumns", function (object) standardGeneric("getGradeColumns"))
setMethod("getGradeColumns", signature("NSR"),
          function (object) {
            base::intersect(object@dataColumns, as.vector(unlist(object@subjectNames)))
          })
# Set data columns
setGeneric("setColumns", function (object, cols) standardGeneric("setColumns"))
setMethod("setColumns", signature("NSR", "vector"),
          function (object, cols) {
            object@dataColumns <- cols
            object
          })

#' Set subject names
#' @param subjectNames A named list of subjects.
#' @export
setGeneric("setSubjectNames", function (object, subjectNames) standardGeneric("setSubjectNames"))
setMethod("setSubjectNames", signature("NSR", "list"),
          function (object, subjectNames) {
            # Get subjects not in subjectNames
            keys = names(subjectNames)
            # Unassigned
            unass = base::setdiff(object@SUBJECTNAMES, keys)
            ass = base::intersect(object@SUBJECTNAMES, keys)
            newKeys = c(unass, ass)
            newSubjects = c(unass, as.vector(unlist(subjectNames)))
            temp <- as.list(newSubjects)
            names(temp) <- newKeys
            object@subjectNames <- temp
            object
          })

#' Convert alphabetical grades to numeric
#'
#' @param object An object of class NSR
#' @param year A vector of graduation years for the corresponding grades
#' @param ... Grades to score.
#' @rdname  scoreGrade
#' @export
setGeneric("scoreSubjects", function (object, year, ...) standardGeneric("scoreSubjects"))
#' @rdname scoreGrade
setMethod("scoreSubjects", signature("NSR", "vector"),
          function (object, year, ...){
            subjects <- c(...)
            dta = eval(parse(text=object@dataName), envir=globalenv())
            dta <- dta[, subjects]
            for (subject in subjects) {
              dta[,subject] <- as.character(dta[,subject])
              dta[,subject] <- pseregs::scoreGrade(dta[,subject], year)
            }
            dta
          })

#' Calculate grade-point average (GPA)
#'
#' @param object An object of class NSR.
#' @param subjects A list of subjects to include in the GPA
#' @export
#' @rdname GPA
setGeneric("GPA", function (object, subjects=F) standardGeneric("GPA"))
#' @rdname GPA
setMethod("GPA", signature("NSR"),
          function (object, subjects=F) {
            if (!is.vector(subjects)) subjects <- object@subjectNames
            subjects <- getGradeColumns(object)
            dta = eval(parse(text=object@dataName), envir=globalenv())[,subjects]
            rowMeans(dta)
          })

# To convert object to a data frame
setMethod("as.vector", signature("NSR"),
          function (x, mode) {
            as.vector(GPA(x), mode)
          })
# To assign something of the data to a data.frame
setMethod("rep", signature("NSR"),
          function (x, length.out) {
            GPA(x)
          })
"
data is Math, HI
a <- NSR(data) or
a <- NSR(data, list(MA='Math'))
a@subjectNames returns list(MA='Math', HI='HI')
devtools::document()
a <- NSR(students, subjects=list(EN='Eng'))
GPA(a)
"
