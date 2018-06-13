##############################################################################################
#' @title Check if specified columns exists in data frame

#' @author 
#' AMM

#' @description 
#' Check and set quality flag

#' @param Data.F Data frame
#' @param ColNames.V.s  Character Vector of variables to be checked

#' @references None

#' @return Nothing. Function stops on error


#' @keywords Currently None.

#' @examples Currently None

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2018-06-08)
#     original creation after fCheckColNames function in DataFunctions.R
##############################################################################################

fCheckColNames <- function(
  ##description<<
  ## Check if specified columns exists in data frame
  Data.F                ##<< Data frame
  ,ColNames.V.s         ##<< Vector of variables to be checked
)
  ##author<<
  ## AMM
  # TEST: Data.F <- Date.F.x; ColNames.V.s <- c('Year.n', 'none', 'Month.n', 'test'); 
{
  #Exclude dummy 'none'
  NoneCols.b <- ColNames.V.s %in% 'none'
  #Check if specified columns exist in data frame
  NameCols.b <- ColNames.V.s[!NoneCols.b] %in% colnames(Data.F)
  if( !all(NameCols.b) ){
    ColNames.s <- paste( ColNames.V.s[!NoneCols.b][!NameCols.b], collapse=', ', sep='' )
    stop('fCheckColNames::: Missing specified columns in dataset: ', ColNames.s, '!')
  }
  
  ##value<< 
  ## Function stops on errors.
}
