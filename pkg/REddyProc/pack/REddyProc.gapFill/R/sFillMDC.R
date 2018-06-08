##############################################################################################
#' @title Gap-fill with Mean Diurnal Course method

#' @author 
#' AMM

#' @description 
#' Gap-fill with Mean Diurnal Course (MDC) algorithm based on average values within +/- one hour of adjacent days, after 
#' procedure in Reichstein et al. (2005)

#' @param WinDays.i Window size for filling in days
#' @param Verbose.b Logical. Print status information to screen?

#' @references 
#' M. Reichstein, E. Falge, D. Baldocchi, D. Papale, M. Aubinet, P. Berbigier, C. Bernhofer, N. Buchmann, T. Gilmanov, A. Granier, T. Grunwald, K. Havrankova, H. Ilvesniemi, D. Janous, A. Knohl, T. Laurila, A. Lohila, D. Loustau, G. Matteucci, T. Meyers, F. Miglietta, J.M. Ourcival, J. Pumpanen, S. Rambal, E. Rotenberg, M. Sanz, J. Tenhunen, G. Seufert, F. Vaccari, T. Vesala, D. Yakir, R. Valentini: On the separation of net ecosystem exchange into assimilation and ecosystem respiration: review and improved algorithm, Global Change Biology 11(9) 1424–1439 (2005)

#' @return A data frame of:\cr



#' @keywords Currently None.

#' @examples Currently None

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2018-06-08)
#     original creation after sFillMDC function in EddyGapFilling.R
##############################################################################################


sFillMDC = function(
  ##title<<
  ## sEddyProc$sFillMDC - Gap filling with Mean Diurnal Course (MDC)
  ##description<<
  ## Mean Diurnal Course (MDC) algorithm based on average values within +/- one hour of adjacent days
  WinDays.i           ##<< Window size for filling in days
  ,Verbose.b=TRUE     ##<< Print status information to screen
)
  ##author<<
  ## AMM 
  # TEST: WinDays.i <- 2; Pos.i <- 100
{
  'Mean Diurnal Course (MDC) algorithm based on average values within +/- one hour of adjacent days'
  
  #! Attention: For performance reasons, gap filled values and properties are first written to single 
  #! variables and local matrix lGF.M
  #! (rather than changing single values in sTEMP which copies the data frame each time!)
  #! Improved algorithm speed by more than a factor of 10 (maybe even 100...)
  lGF.M <- matrix(NA_real_, nrow=0, ncol=7, dimnames=list(NULL,c('index','mean','fnum','fsd','fmeth','fwin','fqc')))
  
  # Determine gap positions
  ToBeFilled.V.i <- which(is.na(sTEMP$VAR_fall))
  if( length(ToBeFilled.V.i) > 0 ) {
    for(Pos.i in 1:length(ToBeFilled.V.i)){
      # Message on progress if wanted
      if( Verbose.b && Pos.i == 1 ) message('Mean diurnal course with window size of ', WinDays.i, ' days: .', sep='')
      
      # Set index within window size
      Gap.i   <- ToBeFilled.V.i[Pos.i]
      Index.V.i <- numeric(0)
      for (Day.i in (0:WinDays.i))
        if( Day.i == 0 ) { 
          Index.V.i <- c(Index.V.i, Gap.i+(-2:2))
        } else {
          Index.V.i <- c(Index.V.i, Gap.i+c(-Day.i*sINFO$DTS +(-2:2)), Gap.i+c(Day.i*sINFO$DTS +(-2:2)))
        }
      Index.V.i <- Index.V.i[Index.V.i>0 & Index.V.i<=nrow(sTEMP)]
      
      # If enough available data, fill gap
      #iRecsSimilar <- Index.V.i[ !is.na(sTEMP$VAR_orig[Index.V.i]) ]
      lMDC.V.n <- subset(sTEMP$VAR_orig[Index.V.i], !is.na(sTEMP$VAR_orig[Index.V.i]))
      
      if( length(lMDC.V.n) > 1 ){
        #if( Gap.i == 15167  ) recover()		
        
        lVAR_index.i <- Gap.i
        lVAR_mean.n <- mean(lMDC.V.n)
        lVAR_fnum.n  <- length(lMDC.V.n)
        lVAR_fsd.n  <- sd(lMDC.V.n)
        lVAR_fmeth.n  <- 3
        
        #Set window size and quality flag
        if (T==T) {
          #! Non-congruent with MR PV-Wave
          lVAR_fwin.n <- 2*WinDays.i + 1                 #! Full window length, not congruent with MR PV-Wave (see below), in paper single window sizes stated
        } else { 
          #! Code if required to be congruent with MR PV-Wave --> window calculation changes depending on size
          lVAR_fwin.n <- if( WinDays.i < 7 ) { 
            2*WinDays.i + 1
          } else { 
            WinDays.i + 1
          }
        }
        
        if( lVAR_fwin.n <= 1 ) lVAR_fqc.n <- 1
        if( lVAR_fwin.n >  1 & lVAR_fwin.n <= 5 ) lVAR_fqc.n <- 2  
        if( lVAR_fwin.n >  5 ) lVAR_fqc.n <- 3
        
        lGF.M <- rbind(lGF.M, c(lVAR_index.i, lVAR_mean.n, lVAR_fnum.n, lVAR_fsd.n, lVAR_fmeth.n, lVAR_fwin.n, lVAR_fqc.n))
      }
      if( Verbose.b && Pos.i%%100 == 0 )  message('.', appendLF=FALSE)
      if( Verbose.b && Pos.i%%6000 == 0 ) message('\n.', appendLF=FALSE)
    }
    if( Verbose.b ) message('', nrow(lGF.M))
  }
  # Copy gap filled values and properties to sTEMP
  if( nrow(lGF.M) > 0 ) {
    # Fill all rows in VAR_fall and co
    sTEMP[lGF.M[,'index'],c('VAR_fall','VAR_fnum','VAR_fsd','VAR_fmeth','VAR_fwin','VAR_fall_qc')] <<- 
      lGF.M[,c('mean','fnum','fsd','fmeth','fwin','fqc')]
    # Only fill gaps in VAR_f and VAR_fqc
    Gaps.b <- is.na(sTEMP[lGF.M[,'index'],'VAR_f'])
    # twutz: inserted drop=FALSE, otherwise one-row matrix was not converted to data.frame correctly
    sTEMP[lGF.M[,'index'],c('VAR_f','VAR_fqc')][Gaps.b,] <<- as.data.frame(lGF.M[,c('mean','fqc') ,drop=FALSE])[Gaps.b,] 
  }
  
  return(invisible(sTEMP[,c('VAR_orig','VAR_f','VAR_fall','VAR_fnum','VAR_fsd','VAR_fwin')])) #Other columns are specific for full MR MDS algorithm
  ##value<< 
  ## MDC filling results in sTEMP data frame.
}
