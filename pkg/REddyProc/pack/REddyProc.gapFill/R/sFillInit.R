##############################################################################################
#' @title Initialize gap-filling output

#' @author 
#' AMM

#' @description 
#' initialize Gap-filling output for gap-filling method after Reichstein et al. (2005)

#' @param sDATA Data frame
#' @param Var.s Variable to be filled
#' @param QFVar.s Quality flag of variable to be filled
#' @param QFValue.n Value of quality flag for _good_ (original) data, other data is set to missing
#' @param FillAll.b Fill all values to estimate uncertainties?


#' @references 
#' M. Reichstein, E. Falge, D. Baldocchi, D. Papale, M. Aubinet, P. Berbigier, C. Bernhofer, N. Buchmann, T. Gilmanov, A. Granier, T. Grunwald, K. Havrankova, H. Ilvesniemi, D. Janous, A. Knohl, T. Laurila, A. Lohila, D. Loustau, G. Matteucci, T. Meyers, F. Miglietta, J.M. Ourcival, J. Pumpanen, S. Rambal, E. Rotenberg, M. Sanz, J. Tenhunen, G. Seufert, F. Vaccari, T. Vesala, D. Yakir, R. Valentini: On the separation of net ecosystem exchange into assimilation and ecosystem respiration: review and improved algorithm, Global Change Biology 11(9) 1424–1439 (2005)

#' @return A list of:\cr
#' Data frame sTEMP with:
#' VAR\emph{_orig} - Original values used for gap filling \cr
#' VAR\emph{_f   } - Original values and gaps filled with mean of selected datapoints (condition depending on gap filling method) \cr
#' VAR\emph{_fqc} - Quality flag assigned depending on gap filling method and window length (0 = original data, 1 = most reliable, 2 = medium, 3 = least reliable) \cr
#' VAR\emph{_fall} - All values considered as gaps (for uncertainty estimates) \cr
#' VAR\emph{_fall_qc} - Quality flag assigned depending on gap filling method and window length (1 = most reliable, 2 = medium, 3 = least reliable) \cr
#' VAR\emph{_fnum} - Number of datapoints used for gap-filling \cr
#' VAR\emph{_fsd} - Standard deviation of datapoints used for gap filling (uncertainty) \cr
#' VAR\emph{_fmeth} - Method used for gap filling (1 = similar meteo condition (sFillLUT with Rg, VPD, Tair), 2 = similar meteo (sFillLUT with Rg only), 3 = mean diurnal course (sFillMDC)) \cr
#' VAR\emph{_fwin} - Full window length used for gap filling \cr
#' \cr
#' List sINFO

#' @keywords Currently None.

#' @examples Currently None

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2018-06-05)
#     original creation after sFillInit function in EddyGapFilling.R
##############################################################################################
sFillInit = function(
  ##title<<
  ## sEddyProc$sFillInit - Initialize gap filling
  ##description<<
  ## Initializes data frame sTEMP for newly generated gap filled data and qualifiers.
  sDATA
  ,Var.s                 ##<< Variable to be filled
  ,QFVar.s='none'       ##<< Quality flag of variable to be filled
  ,QFValue.n=NA_real_   ##<< Value of quality flag for _good_ (original) data, other data is set to missing
  ,FillAll.b=TRUE       ##<< Fill all values to estimate uncertainties
  #! ,QF.V.b = TRUE        ##<< boolean vector of length nRow(sData), to allow specifying bad data directly (those entries that are set to FALSE)
)
  ##author<<
  ## AMM
{
  'Initializes data frame sTEMP for newly generated gap filled data and qualifiers.'
    
  fCheckColNames(sDATA,c(Var.s, QFVar.s,'sDateTime'))
  
  sTEMP <- sDATA['sDateTime']
  sINFO <- list()
  sINFO$DIMS <- nrow(sDATA)
  sINFO$DTS <- median(diff(sDATA$sDateTime))
  units(sINFO$DTS) <- "mins"
  sINFO$DTS <- 60*24/as.numeric(sINFO$DTS) # how many measurements in a day
  sINFO$Y.START <- data.table::year(sDATA$sDateTime[1])
  sINFO$Y.END <- data.table::year(sDATA$sDateTime[sINFO$DIMS])
  sINFO$Y.NUMS <- sINFO$Y.END-sINFO$Y.START+1
  sINFO$Y.NAME <- paste0(sINFO$Y.START-2000,"-",sINFO$Y.END-2000)
  
  
  
  Var.V.n <- fSetQF(cbind(sDATA,sTEMP), Var.s, QFVar.s, QFValue.n)
  #! Var.V.n[QF.V.b == FALSE]   <-   NA_real_
  
  # Abort if variable to be filled contains no data
  if( sum(!is.na(Var.V.n)) == 0 ) {
    warning('sFillInit::: Variable to be filled (', Var.s, ') contains no data at all!')
    return(-111)
  }
  ##details<<
  ## Description of newly generated variables with gap filled data and qualifiers: \cr
  ##details<<
  ## VAR\emph{_orig} - Original values used for gap filling \cr
  ## VAR\emph{_f   } - Original values and gaps filled with mean of selected datapoints (condition depending on gap filling method) \cr
  ## VAR\emph{_fqc} - Quality flag assigned depending on gap filling method and window length (0 = original data, 1 = most reliable, 2 = medium, 3 = least reliable) \cr
  ## VAR\emph{_fall} - All values considered as gaps (for uncertainty estimates) \cr
  ## VAR\emph{_fall_qc} - Quality flag assigned depending on gap filling method and window length (1 = most reliable, 2 = medium, 3 = least reliable) \cr
  ## VAR\emph{_fnum} - Number of datapoints used for gap-filling \cr
  ## VAR\emph{_fsd} - Standard deviation of datapoints used for gap filling (uncertainty) \cr
  ## VAR\emph{_fmeth} - Method used for gap filling (1 = similar meteo condition (sFillLUT with Rg, VPD, Tair), 2 = similar meteo (sFillLUT with Rg only), 3 = mean diurnal course (sFillMDC)) \cr
  ## VAR\emph{_fwin} - Full window length used for gap filling \cr
  
  lTEMP <- data.frame(
    VAR_orig=Var.V.n               # Original values of variable VAR used for gap filling
    ,VAR_f=NA_real_                # Original values and filled gaps
    ,VAR_fqc=NA_real_              # Quality flag assigned depending on gap filling method and window length
    ,VAR_fall=NA_real_             # All values considered as gaps (for uncertainty estimates)
    ,VAR_fall_qc=NA_real_          # Quality flag assigned depending on gap filling method and window length
    ,VAR_fnum=NA_real_             # Number of datapoints used for gap-filling
    ,VAR_fsd=NA_real_              # Standard deviation of data points used for filling
    ,VAR_fmeth=NA_real_            # Method used for gap filling
    ,VAR_fwin=NA_real_             # Full window length used for gap filling
  )
  
  # Set fqc to zero for original values
  lTEMP$VAR_f <- lTEMP$VAR_orig
  lTEMP$VAR_fqc <- ifelse(!is.na(lTEMP$VAR_orig), 0, NA_real_)
  
  # Set filling of only gaps
  if( FillAll.b==FALSE) lTEMP$VAR_fall <- lTEMP$VAR_orig #"prefill" with original data
  
  # Add units
  attr(lTEMP$VAR_f, 'units') <- attr(Var.V.n, 'units')
  attr(lTEMP$VAR_fall, 'units') <- attr(Var.V.n, 'units')
  attr(lTEMP$VAR_fsd, 'units') <- attr(Var.V.n, 'units')
  attr(lTEMP$VAR_fwin, 'units') <- 'days'
  
  ##details<<
  ## Long gaps (larger than 60 days) are not filled.
  #! Not congruent with PV-Wave, there the code is performed on single years only with long gaps of 60 days in the beginning or end skipped.
  GapLength.V.n <- fCalcLengthOfGaps(lTEMP$VAR_orig)
  kMaxGap.n <- sINFO$DTS * 60 #Halfhours in 60 days
  while ( max(GapLength.V.n) > kMaxGap.n ) {
    #Flag long gap with -9999.0
    End.i <- which(GapLength.V.n == max(GapLength.V.n))
    Start.i <- End.i - max(GapLength.V.n) + 1
    lTEMP$VAR_fall[Start.i:End.i] <- -9999.0 #Set to -9999.0 as a flag for long gaps
    GapLength.V.n[Start.i:End.i] <- -1 #Set to -1 since accounted for
    warning('sMDSGapFill::: The long gap between position ', Start.i, ' and ', End.i, ' will not be filled!')
  }
  
  if( FillAll.b==T ) {
    message('Initialized variable \'', Var.s, '\' with ', sum(is.na(lTEMP$VAR_orig)), 
            ' real gaps for gap filling of all ', sum(is.na(lTEMP$VAR_fall)) ,' values (to estimate uncertainies).')
  } else {
    message('Initialized variable \'', Var.s, '\' with ', sum(is.na(lTEMP$VAR_orig)),
            ' real gaps for gap filling.')
  }
  
  sTEMP <- data.frame(c(sTEMP, lTEMP))	# twutz: error prone if sTEMP already contains columns of lTEMP
  return(list(sTEMP,sINFO))
}
