#' A class to represent the Swedish national school register (NSR).
#' @slot subjects The list of subjects available in the NSR.
#' @include GPA.R
#' @export
checkState <- function (object) {
  state <- T
  #if (length(object@subjects) == 0) state <- 'Bla'
  return(state)
}
a <- GRADES
b <- DESCRIPTIONS
NSR = setClass("NSR",
         prototype = list(
           SUBJECTNAMES=a,
           DESCRIPTIONS=b
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
  dn = deparse(substitute(data))
  #print(colnames(eval(parse(text=dn), envir = globalenv())))
  #.Object = setSubjects(.Object, colnames(data))
  .Object = setColumns(.Object, colnames(data))
  if (!is.list(subjects)) {
    subjects = as.list(.Object@SUBJECTNAMES)
    names(subjects) = .Object@SUBJECTNAMES
  }
  .Object = setSubjectNames(.Object, subjects)
  callNextMethod(.Object, dataName=dn)
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
# Set subjects
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
setGeneric("scoreSubjects", function (object, year, ...) standardGeneric("scoreSubjects"))
#' @rdname scoreGrade
setMethod("scoreSubjects", signature("NSR", "vector"),
          function (object, year, ...){
            subjects <- c(...)
            dta = eval(parse(text=object@dataName), envir=globalenv())
            for (x in subjects) {
              dta[,x] <- as.character(dta[,x])
              dta[,x] <- pseregs::scoreGrade(dta[,x], year)
            }
            dta
          })

#' Calculate grade-point average (GPA)
#'
#' @param object An object of class NSR.
#' @param subjects A list of subjects to include in the GPA
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
