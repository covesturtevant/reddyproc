##############################################################################################
#' @title Check if variable is a non-empty character string

#' @author 
#' AMM

#' @description 
#' Check if variable is a non-empty character string

#' @param Value.s Value to be checked if string

#' @references None

#' @return Logical value indicating variable is string (TRUE) or not-string (FALSE)


#' @keywords Currently None.

#' @examples Currently None

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2018-06-12)
#     original creation after fCheckValString function in DataFunctions.R
##############################################################################################
fCheckValString <- function(
  ##description<<
  ## Check if variable is a non-empty character string
  Value.s               ##<< Value to be checked if string
)
  ##author<<
  ## AMM
  ##details<<
  ## See test_CheckValue.R for more details.
{
  if(  length(Value.s) == 0 ) {
    FALSE
  } else if( !is.na(Value.s) && (!is.character(Value.s) || !nzchar(Value.s)) ) {
    FALSE
  } else {
    TRUE
  }
  ##value<< 
  ## Boolean value if true of false.
}