#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ R script with functions for data input and output
#+++ Dependencies: DataFunctions.R
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Load ascii format data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fLoadTXTIntoDataframe <- function(
  ##title<<
  ## Load text file with one header and one unit row into data frame
  ##description<<
  ## If gaps with the flag -9999.0 exist, these are set to NA.
  FileName.s            ##<< File name
  ,Dir.s=''             ##<< Directory
) 
  ##author<<
  ## AMM
  # TEST: FileName.s <- 'Example_DETha98.txt'; Dir.s <- 'inst/examples'
{
	##details<<
	## Alternatively \code{\link{fLoadFluxNCIntoDataframe}} loads data from NetCDF-Files.
	## In addition, \code{\link{fLoadEuroFlux16}} loads data from several annual files in format corresponding to europe-fluxdata 2016.
	
  InputFile.s <- fSetFile(FileName.s, Dir.s, T, 'fLoadTXTIntoDataframe')  
  
  # Read in header
  Header.V.s <- as.character(read.csv(InputFile.s, header=F, sep='', dec='.', nrows=1, stringsAsFactors=F))
  Units.V.s <- as.character(read.csv(InputFile.s, header=F, sep='', dec='.', skip=1, nrows=1, stringsAsFactors=F))
  if( length(Header.V.s) != length(Units.V.s) )
    stop('fLoadTXTIntoDataframe::: Entries in header row and unit row are not the same length: \n', 
         length(Header.V.s), ': ', paste(Header.V.s, collapse=' '), '\n', 
         length(Units.V.s), ': ', paste(Units.V.s, collapse=' '))
  # Skip unit row and read in data
  Data.F <- read.csv(InputFile.s, header=F, skip=2, sep='', dec='.')
  # Rename columns with header information
  names(Data.F) <- Header.V.s
  # Add units (and also varnames) as attribute
  for (Var.i in 1:length(Header.V.s)) {
    attr(Data.F[,Header.V.s[Var.i]], 'varnames') <- Header.V.s[Var.i]
    attr(Data.F[,Header.V.s[Var.i]], 'units') <- Units.V.s[Var.i]
  } 
  message('Loaded file ', FileName.s, ' with the following variables (units):')
  message('*** ', paste(colnames(Data.F), '(', as.character(lapply(Data.F, attr, which='units')), ')', collapse=' ', sep=''))
  # Convert gap flags to NA
  Data.F <- fConvertGapsToNA(Data.F)
  
  Data.F 
  ##value<<
  ## Data frame with data from text file.
}

attr(fLoadTXTIntoDataframe, 'ex') <- function() {
  if( FALSE ) { #Do not always execute example code (e.g. on package installation)
    Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
    EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt',Dir.s)
  }
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Load NetCDF data
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fLoadFluxNCIntoDataframe <- function(
  ##title<<
  ## Load NetCDF file
  ##description<<
  ## Load specified variables and time stamp information from NetCDF file in Fluxnet BGI format.
  ## The time stamp information needs to be provided as variables 'year', 'month', 'day', 'hour'.
  VarList.V.s           ##<< Vector of variables to be read in 
  ,FileName.s           ##<< File name             
  ,Dir.s=''             ##<< Directory
  ,NcPackage.s='ncdf4'  ##<< Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')
  ,fReadTime=fReadTimeSeveralCols	##<< function that reads time columns, must append columns year (from 0AD), month, day, and hour (fractional)
  ,...					##<< further arguments to var.get.nc or ncvar_get, such as start and count
) 
  ##author<<
  ## AMM, KS
  # TEST: FileName.s <- 'Example_DE-Tha.1996.1998.hourly.nc'; Dir.s <- 'inst/examples';
  # TEST: VarList.V.s <- c('NEE', 'Rg', 'rH', 'Tair', 'NEE_f'); NcPackage.s <- 'ncdf4'
  # TEST: str(tmp <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg'), 'Example_DE-Tha.1996.1998.hourly.nc', 'inst/examples', count=c(1L,1L,4000L) ))
{
  # Check for R NetCDF packages
  if( !(( NcPackage.s=='ncdf4' && suppressWarnings(require(ncdf4)) )
        || ( NcPackage.s=='RNetCDF' && suppressWarnings(require(RNetCDF)) )) )
    stop('fLoadFluxNCIntoDataframe::: Required package \'', NcPackage.s, '\' could not be loaded!')
  
  # Read in time variables
  Data.F <- fReadTime(NULL, FileName.s, Dir.s, NcPackage.s, 'fLoadFluxNCIntoDataframe', ...)
  # Convert time format to POSIX
  # !Attention: Use YMDH time format because julday and hour time stamps inconsistent at end of year
  Data.F <- fConvertTimeToPosix(Data.F, 'YMDH', Year.s = 'year', Month.s='month', Day.s = 'day', Hour.s = 'hour')
  
  # Read in variables from a given list of needed variables
  Data.F <- 
		  tmp <- fAddNCFVar(Data.F, setdiff(VarList.V.s, colnames(Data.F)), FileName.s, Dir.s, NcPackage.s, 'fLoadFluxNCIntoDataframe', ...)
  message('Loaded BGI Fluxnet NC file: ', FileName.s, ' with the following headers:')
  message('*** ', paste(colnames(Data.F), '(', as.character(lapply(Data.F, attr, which='units')), ')', collapse=' ', sep=''))
  
  Data.F 
  ##value<<
  ## Data frame with data from nc file.
}
attr(fLoadFluxNCIntoDataframe, 'ex') <- function() {
  if( FALSE ) { #Do not always execute example code (e.g. on package installation)
    Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
    EddyNCData.F <- fLoadFluxNCIntoDataframe(c('NEE', 'Rg', 'NEE_f'), 'Example_DE-Tha.1996.1998.hourly.nc', Dir.s)
  }
}

fReadTimeSeveralCols <- function(
		### Reading 
		Data.F                ##<< Data frame
		,FileName.s           ##<< NetCDF file name
		,Dir.s                ##<< Directory
		,NcPackage.s          ##<< Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')
		,CallFunction.s=''    ##<< Name of function called from
		, colYear='year'	  ##<< Name of variable holding the year
		, colMonth='month'	  ##<< Name of variable holding the month
		, colDay='day'		  ##<< Name of variable holding the day
		, colHour='hour'	  ##<< Name of variable holding the hour
		, defaultHour=0	  	  ##<< default used when colHour=NA, when only days are specified
		,...				  ##<< further arguments to var.get.nc or ncvar_get, such as start and count
){
	if( !length(colHour) ){
		Data.F <- fAddNCFVar(Data.F, c(colYear, colMonth, colDay), FileName.s, Dir.s, NcPackage.s, CallFunction.s
				, VarNew.s=c("year","month","day"), ...)
		Data.F$hour <- defaultHour
	} else {
		Data.F <- fAddNCFVar(Data.F, c(colYear, colMonth, colDay, colHour), FileName.s, Dir.s, NcPackage.s, CallFunction.s
						, VarNew.s=c("year","month","day","hour"), ...)
	}
	Data.F
}

fReadTimeBerkeley <- function(
		### Reads time columns (year, month, day, hour) from column in ISODate integer format
		Data.F                ##<< Data frame
		,FileName.s           ##<< NetCDF file name
		,Dir.s                ##<< Directory
		,NcPackage.s          ##<< Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')
		,CallFunction.s=''    ##<< Name of function called from
		,colTime='TIMESTAMP_END'	##<< the column name holding time with format described in details
		,...					##<< further arguments to var.get.nc or ncvar_get, such as start and count
){
	##details<< 
	## In the Berkeley-Release of the fluxnet data, the time is stored as an integer
	## with base10-digits representing YYYYMMddhhmm 
	Data.F <- fAddNCFVar(Data.F, colTime, FileName.s, Dir.s, NcPackage.s, CallFunction.s, ...)
	timeStampChar <- as.character(Data.F[[colTime]])
recover()	
	Data.F <- cbind(Data.F, data.frame(
					year = as.integer(substr(timeStampChar,1,4))
					,month = as.integer(substr(timeStampChar,5,6))
					,day = as.integer(substr(timeStampChar,7,8))
					,hour = as.integer(substr(timeStampChar,9,10)) + 
						as.integer(substr(timeStampChar,11,12))/60
	))
	#str(Data.F)
	Data.F
}

.tmp.f <- function(){
	# testing if baseDate is stored in nc-file
	# http://stackoverflow.com/questions/18819112/use-r-to-extract-time-series-from-netcdf-data
	InputNCF.s <- fSetFile(FileName.s, Dir.s, T, 'fAddNCFVar')
	NCFile.C <- open.nc(InputNCF.s)
	baseDate <- att.get.nc(NCFile.C, "NC_GLOBAL", "base_date")	
}






#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fAddNCFVar <- function(
  ##description<<
  ## Add variable from NetCDF file to data frame
  Data.F                ##<< Data frame
  ,Var.s                ##<< Variable name or names
  ,FileName.s           ##<< NetCDF file name
  ,Dir.s                ##<< Directory
  ,NcPackage.s          ##<< Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')
  ,CallFunction.s=''    ##<< Name of function called from
  ,VarNew.s=Var.s		##<< Name of the variable in Data.F, offer renaming
  ,...					##<< further arguments to var.get.nc or ncvar_get, such as start and count
)
  ##author<<
  ## AMM, KS, TW
  #TEST: Data.F <- NULL; Var.s <- 'NEE'; FileName.s <- 'Example_DE-Tha.1996.1998.hourly.nc'; Dir.s <- 'inst/examples'
  #TEST: NcPackage.s <- 'ncdf4'
{
  if( length(VarNew.s) != length(Var.s) ) stop("VarNew.s must have the same length as Var.s")
  InputNCF.s <- fSetFile(FileName.s, Dir.s, T, 'fAddNCFVar')

  if( NcPackage.s=='RNetCDF' ) {
	  fOpen <- open.nc  
	  fReadVar <- var.get.nc
	  fClose <- close.nc
	  #fInqVar <- var.inq.nc
	  fGetAtt <- att.get.nc
  } else if( NcPackage.s=='ncdf4' ) {
	  fOpen <- nc_open  
	  fReadVar <- ncvar_get
	  fClose <- nc_close
	  fGetAtt <- function(...){ ncatt_get(...)$value }
  } else {
	stop(CallFunction.s, ':::fAddNCFVar::: NC file ', InputNCF.s, ' could not be opened!')
  }
  #
	NCFile.C <- fOpen(InputNCF.s)
	tmpFilename <- tempfile(); tmpFile <- file(tmpFilename, open = "wt")
	tryCatch({
				newCols <- lapply( seq_along(Var.s), function(i){
							newCol <- try( as.vector(fReadVar(NCFile.C, Var.s[i], ...)) )
							if( length(newCol) && !inherits(newCol,"try-error")){
								attr(newCol, 'varnames') <- VarNew.s[i]
								sink( tmpFile, type = "message" ) # to prevent error message, that appears even with try(,silent=TRUE) on non-existing attribute
								try( attr(newCol, 'units') <- fGetAtt(NCFile.C, Var.s[i], 'units'), silent=TRUE)
								sink(NULL, type="message")
								newCol
							} else {
								warning("could not read variable ",Var.s[i]," from netCdf-File ",FileName.s)
								return(NULL)
							}
							#attr(Data.F[[1]], 'units')
						})	
				names(newCols) <- VarNew.s
				newCols.F <- as.data.frame(newCols[sapply(newCols,length) != 0L])
				# Use c() instead of cbind() to be able to bind dataframe Data.F even if empty
				# twutz160121: c gives a list instead of error when nRows differ between Data.F and newCol.F (e.g. with different count argument to fReadVar), therefore better use cbind
				if( nrow(newCols.F) )
					Data.F <- if(length(Data.F)) cbind(Data.F, newCols.F) else newCols.F
			}, 
			finally = {
				fClose(NCFile.C)
				close(tmpFile)
				unlink(tmpFilename)
			}
	)
  Data.F
  ##value<<
  ## Data frame with new nc variable added.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fLoadFluxNCInfo <- function(
  ##title<<
  ## Get site information from BGI NetCDF files
  ##description<<
  ## Load site information attributes such as latitude, longitude and others from BGI NetCDF files
  FileName.s            ##<< NetCDF file name
  ,Dir.s                ##<< Directory
  ,NcPackage.s='ncdf4'  ##<< Name of R NetCDF package (implemented for 'RNetCDF' and 'ncdf4')
  ,CallFunction.s=''    ##<< Name of function called from
)
  ##author<<
  ## AMM
  #TEST: FileName.s <- 'Example_DE-Tha.1996.1998.hourly.nc'; Dir.s <- 'inst/examples'
  #TEST: NcPackage.s <- 'ncdf4'
  #TEST: fLoadFluxNCInfo('Example_DE-Tha.1996.1998.hourly.nc','inst/examples','ncdf4')
{
  # Check for R NetCDF packages
  if( !(( NcPackage.s=='ncdf4' && suppressWarnings(require(ncdf4)) )
        || ( NcPackage.s=='RNetCDF' && suppressWarnings(require(RNetCDF)) )) )
    stop('fLoadFluxNCIntoDataframe::: Required package \'', NcPackage.s, '\' could not be loaded!')
  
  InputNCF.s <- fSetFile(FileName.s, Dir.s, T, 'fAddNCFVar')
  
  if( NcPackage.s=='RNetCDF' ) {
    NCFile.C <- open.nc(InputNCF.s)
    tryCatch({
      ##details<<
      ## Description of attribute list:
      ##describe<<
      SiteInfo.L <- list( 
        ID    = att.get.nc(NCFile.C,'NC_GLOBAL','Site_ID')                 ##<< SiteID
        ,DIMS = dim.inq.nc(NCFile.C,'time')$length                        ##<< Number of data rows
        ,LON  = as.numeric(att.get.nc(NCFile.C,'NC_GLOBAL','Longitude'))  ##<< Longitude
        ,LAT  = as.numeric(att.get.nc(NCFile.C,'NC_GLOBAL','Latitude'))   ##<< Latitude
        ,TZ   = as.numeric(att.get.nc(NCFile.C,'NC_GLOBAL','TimeZone'))   ##<< Time zone
        ,ELEV = as.numeric(att.get.nc(NCFile.C,'NC_GLOBAL','Elevation')) ##<< Elevation
        ,IGBP = att.get.nc(NCFile.C,'NC_GLOBAL','IGBP_class')            ##<< IGBP class
      )
    }, 
             finally = close.nc(NCFile.C)
    )
  } else if( NcPackage.s=='ncdf4' ) {
    NCFile.C <- nc_open(InputNCF.s, write=FALSE, readunlim=TRUE, verbose=FALSE)
    tryCatch({
      SiteInfo.L <- list( 
        ID    = ncatt_get(NCFile.C,0,'Site_ID')$value
        ,DIMS = NCFile.C$dim$time$len
        ,LON  = as.numeric(ncatt_get(NCFile.C,0,'Longitude')$value)
        ,LAT  = as.numeric(ncatt_get(NCFile.C,0,'Latitude')$value)
        ,TZ   = as.numeric(ncatt_get(NCFile.C,0,'TimeZone')$value)
        ,ELEV = as.numeric(ncatt_get(NCFile.C,0,'Elevation')$value)
        ,IGBP = ncatt_get(NCFile.C,0,'IGBP_class')$value
      )
    }, 
             finally = nc_close(NCFile.C)
    )
  } else {
    stop(CallFunction.s, ':::fLoadFluxNCInfo::: NC file ', InputNCF.s, ' could not be opened!')
  }
  
  SiteInfo.L
  ##value<<
  ## Attibute list
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Write data to file
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fWriteDataframeToFile <- function(
  ##description<<
  ## Write data frame to ASCII tab-separated text file
  Data.F                ##<< Data frame     
  ,FileName.s           ##<< File base name
  ,Dir.s=''             ##<< Directory
  ,Digits.n=5			      ##<< number of digits, i.e. precision, for numeric values  
)
  ##author<<
  ## AMM, KS
  ##details<<
  ## Missing values are flagged as -9999.0
  # TEST: Data.F <- EddyData.F; FileName.s='none'; Dir.s <- 'inst/examples'; 
{
  # Set file name
  OutputFile.s <- fSetFile(FileName.s, Dir.s, F, 'fWriteDataframeToFile')
  
  # Convert NAs to gap flag
  Data.F <- fConvertNAsToGap(Data.F)
  
  # Write tab delimited file
  Lines.V.s <- vector(mode='character', length = 2)
  Lines.V.s[1] <- paste(colnames(Data.F), collapse='\t')
  Lines.V.s[1] <- gsub('DateTime', 'Date Time', Lines.V.s[1]) #If POSIX column replace name
  Lines.V.s[2] <- paste(as.character(lapply(Data.F, attr, which='units')), collapse='\t')
  Lines.V.s[2] <- gsub('NULL', '-', Lines.V.s[2])
  Lines.V.s[2] <- gsub('DateTime', 'Date Time', Lines.V.s[2])  #if POSIX column replace unit
  write(Lines.V.s, file=OutputFile.s, append=F)
  write.table(format(Data.F, digits=Digits.n, drop0trailing=T, trim=T), file=OutputFile.s, col.names=F, row.names=F, sep='\t', quote=F, append=T)
  message('Wrote tab separated textfile: ', OutputFile.s)
  
  ##value<<
  ## Output of data frame written to file of specified type.
}

attr(fWriteDataframeToFile, 'ex') <- function() {
  if( FALSE ) { #Do not always execute example code (e.g. on package installation)
    Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
    EddyData.F <- fLoadTXTIntoDataframe('Example_DETha98.txt', Dir.s)
    fWriteDataframeToFile(EddyData.F, 'OutputTest.txt', 'out')
  }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ File handling
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fInitFilesDir <- function(
  ##description<<
  ## Get all available files with specific file extension in directory
  Dir.s           ##<< Directory to be searched for files
  ,lFileExt.s     ##<< File extension specification
  ,fixed=TRUE	  ##<< set to FALSE, if using regular expressions
)
  ##author<<
  ## AMM
{
  # List files in path and grep files with specified file extension as character string
  list.files(path=Dir.s)[grep(lFileExt.s, list.files(path=Dir.s), fixed=fixed)] 
  ##value<< 
  ## Character vector with names of all available site files.
}

attr(fInitFilesDir, 'ex') <- function()   {
  if( FALSE ) { #Do not always execute example code (e.g. on package installation)
    Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
    fInitFilesDir(Dir.s, 'txt')
  }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fStripFileExtension <- function(
  ##description<<
  ## Strip file extension 
  lFiles.V.s     ##<< Vector with names of all available site files
)
  ##author<<
  ## AMM
{
  # RegExp: Search for first dot and replace rest of string with nothing
  sub('[.].*','',lFiles.V.s)
  ##value<< 
  ## Character vector containing the first part of file names (before first dot in file name).
}

attr(fStripFileExtension, 'ex') <- function()  {
  if( FALSE ) { #Do not always execute example code (e.g. on package installation)
    Dir.s <- paste(system.file(package='REddyProc'), 'examples', sep='/')
    fStripFileExtension(fInitFilesDir(Dir.s, 'txt'))
  }
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fSetFile <- function(
  ##description<<
  ## Set file name with path and check if directory and/or file exists
  FileName.s            ##<< File name
  ,Dir.s                ##<< Directory
  ,IO.b                 ##<< Input/output flag, TRUE for input, FALSE for output
  ,CallFunction.s=''    ##<< Name of function called from
)
  ##author<<
  ## AMM
  # TEST: Dir.s <- 'inst/examples'; FileName.s <- 'Example_DETha98.txt'; IO.b <- T; CallFunction.s <- 'test'
{
  # Check if string for directory provided
  Dir.b <- fCheckValString(Dir.s)
  
  # Check if directory exists
  if ( IO.b && Dir.b && (file.access(Dir.s, mode=4) != 0))
    stop(CallFunction.s, ':::fSetFile::: Directory does not exist: ', Dir.s)
  
  # Make directory if mode is output
  if(  !IO.b && Dir.b && (file.access(Dir.s, mode=0) != 0) ) {
    dir.create(Dir.s)
    message(CallFunction.s, ':::fSetFile::: Directory created: ', Dir.s)
    if( file.access(Dir.s, mode=2) != 0 )
      stop(CallFunction.s, ':::fSetFile::: Directory could not be created: ', Dir.s)
  }
  
  # Set file name accordingly
  File.s <- if( Dir.b ) {  paste(Dir.s, '/', FileName.s, sep='')
  } else { FileName.s }
  
  # If input file, check if file exists
  if ( IO.b && (file.access(File.s, mode=4) != 0) )
    stop(CallFunction.s, ':::fSetFile::: File does not exist or has no read permission: ', File.s)
  
  File.s
  ##value<< 
  ## Returns name of file with complete path.
}
