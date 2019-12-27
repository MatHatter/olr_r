#' olr: Optimal Linear Regression
#'
#' The main olr() runs all of the possible linear regression equation combinations, which are all of the
#' combinations of dependent variables respect to the independent variable. In essence, the olr()
#' returns the best fit linear regression model. The user can prompt the olr() to either return the
#' best fit statistical summary of either the greatest adjusted R-squared or R-squared term.
#'
#'
#' Complimentary functions below follow the format: function(dataset, responseName, predictorNames) \cr \cr
#' olrmodels: returns the list of models accompanied by the coefficients. After typing in \code{olrmodels(dataset, responseName, predictorNames)} type the desired summary number to the right of the comma in the brackets: \code{[,x]} where x equals the desired summary number. For example, \code{olrmodels(dataset, responseName, predictorNames)[,8]} \cr \cr
#' olrformulas: returns the list of olr() formulas \cr \cr
#' olrformulasorder: returns the formulas with the predictors (dependent variables) in ascending order \cr \cr
#' adjr2list: list of the adjusted R-squared terms \cr \cr
#' r2list: list of the R-squared terms \cr \cr
#'
#' \emph{A 'Python' version is available at} <https://pypi.org/project/olr>.
#'
#' @param dataset is defined by the user and points to the name of the dataset that is being used.
#' @param responseName the response variable name defined as a string. For example, it represents a header in the data table.
#' @param predictorNames the predictor variable or variables that are the terms that are to be regressed against the \code{responseName}. Place desired headers from the \code{dataset} in here as a character vector.
#' @param adjr2 \code{adjr2 = TRUE} means you want to return the regression summary for the maximum adjusted R-squared term. \code{adjr2 = FALSE} means you want to return the regression summary for the maximum R-squared term.
#' @keywords olr
#' @return The regression summary for the adjusted R-squared or the R-squared, specified with \code{TRUE} or \code{FALSE} in the olr().
#' @examples
#' file <- system.file("extdata", "oildata.csv", package = "olr", mustWork = TRUE)
#' oildata <- read.csv(file, header = TRUE)
#'
#' dataset <- oildata
#' responseName <- 'OilPrices'
#' predictorNames <- c('SP500', 'RigCount', 'API', 'Field_Production', 'OperableCapacity', 'Imports')
#'
#' olr(dataset, responseName, predictorNames, adjr2 = TRUE)
#'
#' @import plyr
#' @import stats
#' @importFrom utils combn
#' @export
olr <- function(dataset, responseName=NULL, predictorNames=NULL, adjr2 = TRUE){

  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (colnames(dataset[-1])), combine, colnames(dataset[-1])))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(colnames(dataset[1])),'~', paste(v,collapse = '')))
  olrformulasorder <- olrformulas[order(unlist(olrformulas))]
  olrmodels <- lapply(olrformulas, function(x, data) eval(bquote(lm(.(x),data=dataset))), data=dataset)
  summarylist <- sapply(olrmodels, summary)

  if (adjr2 == "TRUE" || adjr2 == "True" || adjr2 == "true" || adjr2 == TRUE || adjr2 == T){

    #adj. r^2

    maxadjr2 <- max(sapply(summarylist[9,], max))
    adjr2max_match <- match(maxadjr2, summarylist[9,])

    olrsummary_adj_r <- summarylist[,adjr2max_match]
    print(olrsummary_adj_r) # returns the summary for the max adj. r sqd

  } else if (adjr2 == "FALSE" || adjr2 == "False" || adjr2 == "false" || adjr2 == FALSE || adjr2 == F) {

    #r^2

    maxr2 <- max(sapply(summarylist[8,], max))
    r2max_match <- match(maxr2, summarylist[8,])

    olrsummary_r <- summarylist[,r2max_match]
    print(olrsummary_r) # returns the summary for the max r sqd

  }


  else if ((is.null(responseName) == FALSE) & (is.null(predictorNames) == FALSE)) {


  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (predictorNames), combine, predictorNames))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(responseName),'~', paste(v,collapse = '')))
  olrformulasorder <- olrformulas[order(unlist(olrformulas))]
  olrmodels <- lapply(olrformulas, function(x, data) eval(bquote(lm(.(x),data=dataset))), data=dataset)
  summarylist <- sapply(olrmodels, summary)

  if (adjr2 == "TRUE" || adjr2 == "True" || adjr2 == "true" || adjr2 == TRUE || adjr2 == T){

    #adj. r^2

    maxadjr2 <- max(sapply(summarylist[9,], max))
    adjr2max_match <- match(maxadjr2, summarylist[9,])

    olrsummary_adj_r <- summarylist[,adjr2max_match]
    print(olrsummary_adj_r) # returns the summary for the max adj. r sqd

  } else if (adjr2 == "FALSE" || adjr2 == "False" || adjr2 == "false" || adjr2 == FALSE || adjr2 == F) {

    #r^2

    maxr2 <- max(sapply(summarylist[8,], max))
    r2max_match <- match(maxr2, summarylist[8,])

    olrsummary_r <- summarylist[,r2max_match]
    print(olrsummary_r) # returns the summary for the max r sqd

  }

}
}


#' @rdname olr
#' @import plyr
#' @import stats
#' @importFrom utils combn
#' @export
olrmodels <- function(dataset, responseName = NULL, predictorNames = NULL){

  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (colnames(dataset[-1])), combine, colnames(dataset[-1])))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(colnames(dataset[1])),'~', paste(v,collapse = '')))
  olrformulaorder <- olrformulas[order(unlist(olrformulas))]
  olrmodels <- lapply(olrformulas, function(x, data) eval(bquote(lm(.(x),data=dataset))), data=dataset)
  summarylist <- sapply(olrmodels, summary)
  print("After typing in olrmodels(dataset, responseName, predictorNames) or olrmodels(dataset) type the desired summary number to the right of the comma in the brackets: [,x] where x equals the desired summary number. For example, olrmodels(dataset, responseName, predictorNames)[,8] or olrmodels(dataset)[,8]. There seems to be a limit for the number of predictor variables for this function, thus too many variables may produce a NULL response where there 'should' be a model printed after the word $call. The model is shown in the example olrmodels(dataset, responseName, predictorNames)[,8], but not in olrmodels(dataset)[,8] because the latter has a more column names (not every column name was listed in predictorNames).")
  print(summarylist)

  if ((is.null(responseName) == FALSE) & (is.null(predictorNames) == FALSE)) {

    combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
    combination_mat <- unlist (lapply (1:length (predictorNames), combine, predictorNames))
    combination_mat  <- as.matrix(combination_mat)
    olrformulas <- lapply(combination_mat, function(v) paste(paste(responseName),'~', paste(v,collapse = '')))
    olrformulaorder <- olrformulas[order(unlist(olrformulas))]
    olrmodels <- lapply(olrformulas, function(x, data) eval(bquote(lm(.(x),data=dataset))), data=dataset)
    summarylist <- sapply(olrmodels, summary)
    print("After typing in olrmodels(dataset, responseName, predictorNames) or olrmodels(dataset) type the desired summary number to the right of the comma in the brackets: [,x] where x equals the desired summary number. For example, olrmodels(dataset, responseName, predictorNames)[,8] or olrmodels(dataset)[,8]. There seems to be a limit for the number of predictor variables for this function, thus too many variables may produce a NULL response where there 'should' be a model printed after the word $call. The model is shown in the example olrmodels(dataset, responseName, predictorNames)[,8], but not in olrmodels(dataset)[,8] because the latter has a more column names (not every column name was listed in predictorNames).")
    print(summarylist)

}
}
#' @rdname olr
#' @import plyr
#' @import stats
#' @importFrom utils combn
#' @export
olrformulas <- function(dataset, responseName = NULL, predictorNames = NULL){

  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (colnames(dataset[-1])), combine, colnames(dataset[-1])))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(colnames(dataset[1]),'~', paste(v,collapse = ''))))
  print(olrformulas)

  if ((is.null(responseName) == FALSE) & (is.null(predictorNames) == FALSE)) {

    combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
    combination_mat <- unlist (lapply (1:length (predictorNames), combine, predictorNames))
    combination_mat  <- as.matrix(combination_mat)
    olrformulas <- lapply(combination_mat, function(v) paste(paste(responseName),'~', paste(v,collapse = '')))
    print(olrformulas)
  }
}

#' @rdname olr
#' @import plyr
#' @import stats
#' @importFrom utils combn
#' @export
olrformulaorder <- function(dataset, responseName = NULL, predictorNames = NULL){

  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (colnames(dataset[-1])), combine, colnames(dataset[-1])))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(colnames(dataset[1])),'~', paste(v,collapse = '')))
  olrformulaorder <- olrformulas[order(unlist(olrformulas))]
  print(olrformulaorder)

  if ((is.null(responseName) == FALSE) & (is.null(predictorNames) == FALSE)) {

    combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
    combination_mat <- unlist (lapply (1:length (predictorNames), combine, predictorNames))
    combination_mat  <- as.matrix(combination_mat)
    olrformulas <- lapply(combination_mat, function(v) paste(paste(responseName),'~', paste(v,collapse = '')))
    olrformulaorder <- olrformulas[order(unlist(olrformulas))]
    print(olrformulaorder)

}
}
#' @rdname olr
#' @import plyr
#' @import stats
#' @importFrom utils combn
#' @export
adjr2list <- function(dataset, responseName = NULL, predictorNames = NULL){

  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (colnames(dataset[-1])), combine, colnames(dataset[-1])))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(colnames(dataset[1])),'~', paste(v,collapse = '')))
  olrformulaorder <- olrformulas[order(unlist(olrformulas))]
  olrmodels <- lapply(olrformulas, function(x, data) eval(bquote(lm(.(x),data=dataset))), data=dataset)
  summarylist <- sapply(olrmodels, summary)

  #adj. r^2
  adjr2list <- sapply(summarylist[9,], max)
  print(adjr2list)

  if ((is.null(responseName) == FALSE) & (is.null(predictorNames) == FALSE)) {

    combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
    combination_mat <- unlist (lapply (1:length (predictorNames), combine, predictorNames))
    combination_mat  <- as.matrix(combination_mat)
    olrformulas <- lapply(combination_mat, function(v) paste(paste(responseName),'~', paste(v,collapse = '')))
    olrformulaorder <- olrformulas[order(unlist(olrformulas))]
    olrmodels <- lapply(olrformulas, function(x, data) eval(bquote(lm(.(x),data=dataset))), data=dataset)
    summarylist <- sapply(olrmodels, summary)

    #adj. r^2
    adjr2list <- sapply(summarylist[9,], max)
    print(adjr2list)


}
}
#' @rdname olr
#' @import plyr
#' @import stats
#' @importFrom utils combn
#' @export
r2list <- function(dataset, responseName = NULL, predictorNames = NULL){

  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (colnames(dataset[-1])), combine, colnames(dataset[-1])))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(colnames(dataset[1])),'~', paste(v,collapse = '')))
  olrformulaorder <- olrformulas[order(unlist(olrformulas))]
  olrmodels <- lapply(olrformulas, function(x, data) eval(bquote(lm(.(x),data=dataset))), data=dataset)
  summarylist <- sapply(olrmodels, summary)

  #r^2
  r2list <- sapply(summarylist[8,], max)
  print(r2list)

  if ((is.null(responseName) == FALSE) & (is.null(predictorNames) == FALSE)) {

  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (predictorNames), combine, predictorNames))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(responseName),'~', paste(v,collapse = '')))
  olrformulaorder <- olrformulas[order(unlist(olrformulas))]
  olrmodels <- lapply(olrformulas, function(x, data) eval(bquote(lm(.(x),data=dataset))), data=dataset)
  summarylist <- sapply(olrmodels, summary)

  #r^2
  r2list <- sapply(summarylist[8,], max)
  print(r2list)

}
}
