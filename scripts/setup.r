#
# DESCRIPTION:
#     setup.r is the first file that should be loaded into the R interpreter.
#     Other routines and analysis depend on project specific variables defined 
#     and libraries loaded by this file.
#
# REQUIREMENTS:
#     Minimum R version 2.4.0
#     readxl is a light-weight read-only library for excel workbooks found in later version of R.
#     Our usage of this library requires R 2.4.0 or later.
#

# readxl is a light-weight read-only library for excel workbooks
library( readxl )

#
# xlsx is an alternate library for reading and writing excel workbooks, but it requires
# Java and may be too memory intensive.
#


#
# Output files should be dated...don't overwrite older analysis data
#
today <- Sys.Date()

# server_share <- "\\\\ameda7aisr0107\\ISR_CANCIOLAB_4"
server_share <- "\\\\aisra7svr1132\\ISR_CANCIOLAB_4"
protocol_dir <- paste(server_share,  "\\00-PROTOCOLS\\A-16-037 PH REBOA", sep="") 

chart_dir <- paste( protocol_dir, "\\A-16-037 Vitals Charting\\Data\\", sep="")
idea_dir <- paste( protocol_dir, "\\A-16-037 IDEA Data\\", sep="")
groups <- list.files( chart_dir )

# include today’s date in the analysis output directory name
analysis_dir <- paste( protocol_dir, "\\WLB-Analysis\\", today, sep="")

#'
#'  Read a single column from an Excel spreadsheet.
#'
#' @param f            The Excel file to read
#' @param sheet_name   The Excel sheet name to read
#' @param col_idx      The index(es) of the column(s) to read
#'
#' @return A dataset consisting of a single column, formatted as a string.
#'
#' @export

read_column <- function( f, sheet_name, col_idx ) {
	
	col <- read_xlsx( f, 	  # if using the xlsx library, these are the arguments: f
		sheet=sheet_name,     # sheetName=sheet_name,
		col_names=FALSE,      # header=FALSE,
		skip=4,               # startRow=5,
							  # colIndex=col_idx,
							  # stringsAsFactors=FALSE,
		col_types="text"      # colClasses=c( "character")
		)
	#
	# reduce from a dataframe to an array
	#
	col <- as.character(unlist(col[,col_idx]))            #	col <- col[,1]
	
	#
	# find the last value that is not NA, and truncate array to this length
	#
 	n <- max(c(1:length(col))[!is.na(col)])
	length(col) <- n
 	
 	# return this value as an array
	
	col
}

#' Read a single row from an Excel spreadsheet.
#'
#' Sometimes it is necessary to read a single row from an Excel spreadsheet.  A common use case is
#' when the column header information is not on the first row, or there are various rows between 
#' the column header and the actual data.
#'
#' @param f            The Excel file to read
#' @param sheet_name   The Excel sheet name to read
#' @param row_idx      The index of the row to read.  Must be a single value.
#'
#' @return A dataset consisting of a single row.
#'
#' @export

read_row <- function( f, sheet_name, row_idx ) {

	row <- read_xlsx( f,     # if using the xlsx library, these are the arguments: f
		sheet=sheet_name,    # sheetName=sheet_name,
		col_names=FALSE,     # header=FALSE,
		skip=row_idx-1,      # startRow=row_idx,
		n_max=1,             # endRow=row_idx,
							 # colIndex=c(3:99),
							 # stringsAsFactors=FALSE,
		col_types="text"     # colClasses=c( "character")
		)

	# read function has the nasty ability to simply drop the first column if it is empty!!!!
	pos <- 3
	if( row[1] == "TIME" ) {
		pos <- 2
	}
	if( row[1] == "Timepoint" ) {
		pos <- 2
	}
	
	row_strings <- NA
	if( pos < ncol(row)) {
		# convert to array/vector
		row_strings <- as.character(row[,c(pos:ncol(row))])
	}
	row_strings
}

read_animal_id <- function( f, sheet_name ) {

	row <- read_xlsx( f,     # if using the xlsx library, these are the arguments: f
		sheet=sheet_name,    # sheetName=sheet_name,
		col_names=FALSE,     # header=FALSE,
		n_max=1,             # endRow=row_idx,
		col_types="text"     # colClasses=c( "character")
		)

	as.character(row)[10]
}


read_sheet_as_dataframe <- function( f, sheet_name ) {
	cat("  reading: *", sheet_name, "* file: ", f, "\n", sep="" )
		
	#
	# variable names are in the second column
	#
	var_names <- read_column( f, sheet_name, 2 )

	#
	# timepoint names are in the third row
	#
	tp_names <- read_row( f, sheet_name, 3 )
	
	#
	# clock time is in the fourth row
	# warning: this can be messy data.  We've read it as text, but need to clean it up
	#
	dat_df <- NA
	
	clock_tm <- read_row( f, sheet_name, 4 )  # CLOCK TIMES COME FROM row 4
	
	#
	# read main data portion as numberic
	#
	if( !is.na(clock_tm) ) {
	  dat <- read_xlsx( f,     # if using the xlsx library, these are the arguments: read_xlsx2( f,
		sheet=sheet_name,    # sheetName=sheet_name,
		col_names=FALSE,     # header=FALSE,
		skip=4,              # startRow=5,
		n_max=length(var_names),   # endRow=4 + length(var_names),
                              #		colIndex=c(3:12),
							  # stringsAsFactors=FALSE,
		col_types=c( "text" ) # colClasses="character"
		)
		
	  dat_t <- t(dat[,c(3:ncol(dat))])

	  length(tp_names) <- nrow(dat_t)
	  length(clock_tm) <- nrow(dat_t)
	
	  #
	  # swap rows and columns...flip...translate...
	  #

	  dat_df <- as.data.frame(dat_t,stringsAsFactors = FALSE)

	  names(dat_df) <- var_names
	  dat_df$TP <- tp_names
	  dat_df$TM <- clock_tm
	}

	dat_df
}

append_dataframe<- function ( full_df, part, group, animal_id, subject ) {
	part$group <- group
	part$animal_id <- animal_id
	part$file <- subject
	
	if( is.null( full_df ) ) {
		full_df <- part
	} else {
		tryCatch(
			full_df <- rbind( full_df, part ),
			error = function(e) { 
				cat( "ERROR:\n", names(full_df),"\n", names(part), "\n" )
				stop(e) 
			}
		)
	}
	
	full_df 
} 

#
groups <- list.files( chart_dir )

read_all_animals <- function( sheet_name ) {
  full_df <- NULL
  for( group in groups ) {
	cat("group: ", group, "\n" )
	
	group_dir <- paste( chart_dir, group, sep="" )
	group_files <- list.files( group_dir)
	
	for( subject in group_files ) {
		if( length(grep("mean", subject, ignore.case=TRUE )) >= 1 ) {
			# ignore cat("ignoring file: ", subject)
		} else if( length(grep("~", subject )) >= 1 ) {
			# ignore cat("ignoring excel backup file: ", subject)
		} else {
			f <- paste( group_dir, subject, sep="\\" )
			animal_id <- read_animal_id( f, "Vitals Sheet" )
			
			if( is.na( animal_id ) ) {
				cat( "SKIPPING animal ID=", animal_id, " file:", subject, "\n")
			} else {
				part <- read_sheet_as_dataframe( f, sheet_name )
				
				if( !is.na(part) ) {
				  # some SS in labs don't contain initials...just drop that column
				  part[ "INITIALS" ] <- NULL
				
				  # issue with labs
				  part[ "Na" ] <- NULL
				  part[ "Na" ] <- NULL
				  part[ "Na.1" ] <- NULL
				  full_df <- append_dataframe( full_df, part, group, animal_id, subject )
				}
			} 
		}
	}
  }
  
  #
  # post processing
  #

  # reset row numbers...simple cleaning
  rownames(full_df) <- NULL

  # remove columns where colname is NA
  full_df <- full_df[,!is.na(colnames(full_df))]

  # factors will be helpful here
  full_df$TP   <- factor(full_df$TP,   levels=c("BL 1","BL 2", "EH", "ROSC", "ETX", "R30", "R60", "R120", "R180", "R240/D") )

  # convert data columns to numbers…indicating warnings if unexpected data
  options( warn=1 )
  for( i in c(1:(ncol(full_df) - 5)  )) { 
  	  cat("process col: ", i, "\n" )
  
	  u <- full_df[,i]
	  u[u == "-"] <- NA
	  u[u == "off"] <- 0
	  class(u) <- "numeric"
	  full_df[,i] <- u
	  warnings()
  }

  full_df
}

get_cardiac_arrest_event <- function( anno ) {
	ca <- subset( anno, txt =="start cardiac arrest" )
	if( nrow( ca ) == 0 ) {
		ca <- subset( anno, txt =="cardiac arrest" )
	}
	if( nrow( ca ) == 0 ) {
		ca <- subset( anno, txt =="start vfib" )
	}
	if( nrow( ca ) == 0 ) {
		ca <- subset( anno, txt =="start ventricular fibrilation" )
	}
	if( nrow( ca ) == 0 ) {
		ca <- subset( anno, txt =="start ventricular fibrillation" )
	}
	
	ca
}


## and sometimes factors just get in the way!
# cbc_df$WBCs <- as.numeric( as.character(cbc_df$WBCs ))

cbc_df <- read_all_animals( "CBC and Chemistry" )
vitals_df <- read_all_animals( "Vitals Sheet" )

labs_df <- read_all_animals( "Blood gases" )

rb <- subset( labs_df, group != "Injury Control" & TP == "ROSC" )
# specific exclusions
rb <- subset( rb, animal_id != "9676" )
rb <- subset( rb, animal_id != "9682" )


# rename columns for convenience
colnames(vitals_df)[ names( vitals_df ) == "ABP/M" ] <- "MAP"
colnames(vitals_df)[ names( vitals_df ) == "ABP/S" ] <- "SYS"
colnames(vitals_df)[ names( vitals_df ) == "ABP/D" ] <- "DIA"
colnames(vitals_df)[ names( vitals_df ) == "Fentanyl Total" ] <- "FentanylTot"


#
# First graph
#

boxplot( HR ~ TP, data=vitals_df, main="Heart Rate by Timepoint"  )
boxplot( WBCs ~ TP, data=cbc_df, main="White Blood Cells"  )
