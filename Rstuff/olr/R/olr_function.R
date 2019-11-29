#' olr: Optimal Linear Regression
#'
#' The olr function runs all of the possible linear regression equation combinations, which is all of the
#' combinations of dependent variables in respect to the independent variable. Basically, the olr function
#' returns the best fit linear regression model. The user can prompt the olr function to either return the
#' best fit statistical summary of either the greatest adjusted R-squared or R-squared term.
#'
#'
#' Complimentary functions below follow this format: function(datasetname, resvarname, expvarnames) \cr \cr
#' olrmodels: returns the list of models accompanied by the coefficients. After typing in olrmodels(datasetname, resvarname, expvarnames) put the number you want to see in the brackets on the right of the comma [,]. For example, olrmodels(datasetname, resvarname, expvarnames)[,1] \cr \cr
#' olrformulas: returns the list of olr formulas \cr \cr
#' olrformulasorder: returns the formulas with the predictors (dependant variables) in ascending order \cr \cr
#' adjr2list: list of the adjusted R-squared terms \cr \cr
#' r2list: list of the R-squared terms \cr \cr
#'
#' A Python version is available at https://pypi.org/project/olr/.
#'
#' @param datasetname is defined by the user and points to the name of the dataset that is being used.
#' @param resvarname the response variable name defined as a string. For example. It represents a header in the data table.
#' @param expvarnames is a variable that you must define as a list before your function. Place desired headers from the data table in here as a list.
#' @param adjr2 adjr2 = TRUE means you want to return the regression summary for the maximum adjusted R-squared term. adjr2 = FALSE means you want to return the regression summary for the maximum R-squared term.
#' @keywords olr
#' @return The regression summary for the adjusted R-squared or the R-squared, specified with TRUE or FALSE in the olr function.
#' @examples
#' \dontrun{
#' For this example, 'Import Dataset' from this location: ../olr/insta/extdata
#'
#' Or use the function to find the location:
#' system.file("extdata", "oildata.csv", package = "olr", mustWork = TRUE)
#'
#' Go to "Environment" and go to "Import Dataset". Then choose "From Text (base)...".
#' Afterwards, find the file in the directory. After clicking the file, choose "Yes" for "Headers"
#' Then, click on "Import"
#'
#' oildata should now be listed as one of the global variables.
#'
#' datasetname <- oildata
#' resvarname <- 'OilPrices'
#' expvarnames <- c('SP500', 'RigCount', 'API', 'Field_Production', 'RefinerNetInput',
#' 'OperableCapacity', 'Imports', 'StocksExcludingSPR')
#' olr(datasetname, resvarname, expvarnames, adjr2 = TRUE)
#' }
#' @importFrom utils combn
#' @export
olr <- function(datasetname, resvarname, expvarnames, adjr2 = TRUE){

  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (expvarnames), combine, expvarnames))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(resvarname),'~', paste(v,collapse = '')))
  olrformulasorder <- olrformulas[order(unlist(olrformulas))]
  olrmodels <- lapply(olrformulas, function(x, data) eval(bquote(lm(.(x),data=datasetname))), data=datasetname)
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

#' @rdname olr
#' @importFrom utils combn
#' @export
olrmodels <- function(datasetname, resvarname, expvarnames){

  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (expvarnames), combine, expvarnames))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(resvarname),'~', paste(v,collapse = '')))
  olrformulaorder <- olrformulas[order(unlist(olrformulas))]
  olrmodels <- lapply(olrformulas, function(x, data) eval(bquote(lm(.(x),data=datasetname))), data=datasetname)
  summarylist <- sapply(olrmodels, summary)
  print("After typing in olrmodels(datasetname, resvarname, expvarnames) put the number you want to see in the brackets on the right of the comma [,]. For example, olrmodels(datasetname, resvarname, expvarnames)[,1]")
  print(summarylist)
}

#' @rdname olr
#' @importFrom utils combn
#' @export
olrformulas <- function(datasetname, resvarname, expvarnames){

  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (expvarnames), combine, expvarnames))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(resvarname),'~', paste(v,collapse = '')))
  print(olrformulas)

}

#' @rdname olr
#' @importFrom utils combn
#' @export
olrformulaorder <- function(datasetname, resvarname, expvarnames){

  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (expvarnames), combine, expvarnames))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(resvarname),'~', paste(v,collapse = '')))
  olrformulaorder <- olrformulas[order(unlist(olrformulas))]
  print(olrformulaorder)

}

#' @rdname olr
#' @importFrom utils combn
#' @export
adjr2list <- function(datasetname, resvarname, expvarnames){

  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (expvarnames), combine, expvarnames))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(resvarname),'~', paste(v,collapse = '')))
  olrformulaorder <- olrformulas[order(unlist(olrformulas))]
  olrmodels <- lapply(olrformulas, function(x, data) eval(bquote(lm(.(x),data=datasetname))), data=datasetname)
  summarylist <- sapply(olrmodels, summary)

  #adj. r^2
  adjr2list <- sapply(summarylist[9,], max)
  print(adjr2list)

}

#' @rdname olr
#' @importFrom utils combn
#' @export
r2list <- function(datasetname, resvarname, expvarnames){

  combine <- function (x, y) {combn (y, x, paste, collapse = '+')}
  combination_mat <- unlist (lapply (1:length (expvarnames), combine, expvarnames))
  combination_mat  <- as.matrix(combination_mat)
  olrformulas <- lapply(combination_mat, function(v) paste(paste(resvarname),'~', paste(v,collapse = '')))
  olrformulaorder <- olrformulas[order(unlist(olrformulas))]
  olrmodels <- lapply(olrformulas, function(x, data) eval(bquote(lm(.(x),data=datasetname))), data=datasetname)
  summarylist <- sapply(olrmodels, summary)

  #r^2
  r2list <- sapply(summarylist[8,], max)
  print(r2list)

}
