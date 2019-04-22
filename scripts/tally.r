
NOT READY!!!

#
# DESCRIPTION:
#     read tally file
#
# PREREQUISITES:
#     load setup.r 
#

tally_file <- paste(protocol_dir, "pH-REBOA Tally.xlsx", sep="\\" )

tally <- read_xlsx( tally_file, "Animal Tally", skip=3 )
tally_df <- as.data.frame(tally,stringsAsFactors=FALSE)

#
# rename some columns
#

names( tally_df )[ names(tally_df) == "Pig#" ] <- "animal_id"
names( tally_df )[ names(tally_df) == "Experimental Date" ] <- "dt"
names( tally_df )[ names(tally_df) == "Wt (kg)" ] <- "weight"
names( tally_df )[ names(tally_df) == "Hemorrhage (mL)" ] <- "hemorrhage"
names( tally_df )[ names(tally_df) == "Blood wt (g)" ] <- "blood_wt"
names( tally_df )[ names(tally_df) == "ROSC (Y/N)" ] <- "ROSC"


#'
#' Fix clock times
#'
#' time of day is already in the data frame 
#'
#' @param tally_df     The tally dataframe read previously
#' @param mydf         A dataframe with columns for time (TM).
#'
#' @return
#'
#' @export

fix_clock_times <- function( tally_df, mydf ) {
	mydf$TM[ mydf$TM == "DEATH" ] <- NA
	mydf$TM[ mydf$TM == "Death" ] <- NA
	
	HR <- as.numeric( mydf$TM ) * 24
	MM <- trunc(60*(HR - trunc(HR))) 
	mydf$TOD <- sprintf( "%2.0f:%0.2d", trunc(HR),MM )
	mydf$TOD[ mydf$TOD == "NA:NA" ] <- ""

	animals <- unique( tally_df$animal_id )

	mydf$dt <- NA
	for( animal in animals ) {
		dt <- subset(tally_df, animal_id==animal)$dt
		mydf$dt[ mydf$animal_id == animal ] <- as.character(dt)	
	}
	mydf$dt <- strptime( paste(mydf$dt, mydf$TOD), "%Y-%m-%d %H:%M" )
	
	mydf
}

vitals_df <- fix_clock_times( tally_df, vitals_df )
cbc_df <- fix_clock_times( tally_df, cbc_df )

