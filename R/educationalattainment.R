#' Calculate educational attainment level.
#' @description Calculate educational attainment level according to Statistics Swedens classification of education.
#' @details This function takes educational attainment codes according to Statistics Swedens classification
#' (mostly named SUN2000NIVA in datasets) and recodes
#' them into three levels where compulsory is equivalient of at most 9 years of education, secondary 9 to 12 years (one exception), and
#' tertiary greater than 12 years. Warnings are generated if data contains codes that are unknown according to the offical
#' documentation from statistics Sweden.
#'
#' Some post secondary educations are included in secondary and these are shoften shorter than 2 years and occur outside university or
#' job oriented educations shorter than 2 years at a university. See codes 410-417.
#' @examples # 2 with compulsory education, 1 with secondary, 1 with tertiary, 2 missing
#' sunCodes <- c('001','200','312', '415', '525','999', NA)
#' newCodes <- educationalAttainment(sunCodes, list(compulsory=1, secondary=2, tertiary=3))
#' codes
#' # [1] "1", "1", "2", "2", "3", NA, NA
#' @param object A vector of SUN2000NIVA codes
#' @param attainments A named list of educational attainments to code, eg list(compulsory='mycode1', secondary='mycode2', tertiary='mycode3')
#' @param missing The character code for a missing observation, defaults to '999' and are treated as other missing values in data.
#' @export
setGeneric('educationalAttainment', function (object, attainments, missing) standardGeneric('educationalAttainment'))
setMethod('educationalAttainment', signature('vector', 'list', 'character'),
          function (object, attainments, missing) EA(object, attainments, codeLevel=1, missing))
setMethod('educationalAttainment', signature('vector', 'list', 'missing'),
          function (object, attainments) EA(object, attainments, codeLevel=1))

#' @keywords internal
EA <- function (object, attainments, codeLevel, missingCode='999') {
  attainmentNames <- names(attainments)
  if (!sum(attainmentNames %in% c('compulsory', 'secondary', 'tertiary')) > 0) stop('Valid attainments are: compulsory, secondary and tertiary')
  if (sum(!is.character(object)) > 0) stop('Data contains numeric values. Only character values are allowed')
  missing <- substr(missingCode, 1, codeLevel)
  validCodes <- c(names(SUN2000[[codeLevel]]), missing, NA)
  codes <- substr(object, 1, codeLevel)
  uniqueCodes <- unique(codes)
  contained <- uniqueCodes %in% validCodes
  if (sum(!contained) > 0) warning(paste0('Data contains unknown codes: ', paste(uniqueCodes[!contained])))
  outputs <- object
  outputs[!is.na(outputs) & codes == '9'] <- NA
  outputs[!is.na(outputs) & codes <= '2'] <- attainments[['compulsory']]
  outputs[!is.na(outputs) & codes >= '3' & codes <= '4'] <- attainments[['secondary']]
  outputs[!is.na(outputs) & codes >= '5'] <- attainments[['tertiary']]
  nComp <- sum(outputs == attainments[['compulsory']], na.rm=T)
  nSec <- sum(outputs == attainments[['secondary']], na.rm=T)
  nTer <- sum(outputs == attainments[['tertiary']], na.rm=T)
  nMiss <- sum(is.na(outputs), na.rm=T)
  n <- length(outputs)
  cat('Summmary:\n')
  cat('Compulsory <= 9 years: ', nComp, ' (',round(100*nComp/n,2),'%)\n', sep='')
  cat('Secondary 9 to 12 years: ', nSec, ' (',round(100*nSec/n,2),'%)\n', sep='')
  cat('Tertiary > 12 years: ', nTer, ' (',round(100*nTer/n,2),'%)\n', sep='')
  cat('Missing: ', nMiss, ' (',round(100*nMiss/n, 2), '%)\n', sep='')
  outputs
}
