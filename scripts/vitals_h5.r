
#
# REQUIREMENTS:
#         after setup and tally
#


require( rhdf5 )

h5_000_files <- list.files( idea_dir, recursive=TRUE, pattern="0000.h5", full.names=TRUE )
h5_001_files <- list.files( idea_dir, recursive=TRUE, pattern="0001.h5", full.names=TRUE )

find_h5 <- function( animal_id ) {
	ff <- NA
	
	idx <- grep( animal_id, h5_001_files)
	if( length(idx) > 0 ) {
		ff <- h5_001_files[idx]
	} else {
		idx <- grep(animal_id,h5_000_files)
		ff <- h5_000_files[idx]
	}
	
	ff
}

fix_h5_col_names <- function( d ) { 
	newnames <- c()
	oldnames <- colnames(d)
	for( n in c(1:length(oldnames)) ) { 
 		# cat( oldnames[n], "\n" )
 		arr <- strsplit( oldnames[n], "!" )
 		newnames[n] <- arr[[1]][1]
	}

	newnames
}

read_drager_vitals <- function(animal_id) {
	data_files <- find_h5( animal_id )
	
	vitals <- NULL
	for( file in data_files ) {
		d <- h5read(file, "/Drager_1/numerics", bit64conversion='double' )
		colnames(d) <- fix_h5_col_names(d)
		d$dt <- as.POSIXlt(d$tstamp/1000, origin="1970-01-01",tz="America/Chicago")
		
		vitals <- rbind( vitals, d )
	}
	
	vitals
}

read_cco_vitals <- function(animal_id) {
	data_files <- find_h5( animal_id )
	
	vitals <- NULL
	for( file in data_files ) {
		d <- NULL
		try (
			d <- h5read(file, "/Vigilance+II_0/numerics", bit64conversion='double' )
			,TRUE
		)
		if( !is.null(d) ) {
			colnames(d) <- fix_h5_col_names(d)
			d$dt <- as.POSIXlt(d$tstamp/1000, origin="1970-01-01",tz="America/Chicago")
		
			vitals <- rbind( vitals, d )
		}
	}
	
	vitals
}

read_aesculon_vitals <- function(animal_id) {
	data_files <- find_h5( animal_id )
	
	vitals <- NULL
	for( file in data_files ) {
		d <- NULL
		try (
			d <- h5read(file, "/Aesculon_0/numerics", bit64conversion='double' )
			,TRUE
		)
		if( !is.null(d) ) {
			colnames(d) <- fix_h5_col_names(d)
			d$dt <- as.POSIXlt(d$tstamp/1000, origin="1970-01-01",tz="America/Chicago")
		
			vitals <- rbind( vitals, d )
		}
	}
	
	vitals
}

read_annotations <- function( animal_id, pdate = NULL ) {
	pat <- paste( "*", animal_id, "9*", sep="" )
 	animal_dir <- list.files( idea_dir, pattern=pat, full.names=TRUE,  include.dirs=TRUE )
 	# data_dir <- paste( idea_dir, "\\", animal_dir, sep="" )
 	
 	# h5_file <- list.files( animal_dir, pattern="0000.h5", full.names=TRUE, recursive=TRUE )
	# anno <- h5read( h5_file, "Annotations", bit64conversion="double" )

 	txt_files <- list.files( animal_dir, pattern="null_dre_anno_000.dat", full.names=TRUE, recursive=TRUE )
 	if( length(txt_files) == 0 ) {
 		return( NA )
 	}
 	d <- NA
 	for( anno_file in txt_files ) {
 		one_d <- read.csv( anno_file, sep="\t", header=FALSE, stringsAsFactors=FALSE )
 		if( is.na( d ) ) {
 			d <- one_d
 		} else {
 			d <- rbind( d, one_d )
 		}
 	} 	
 	names(d) = c("tstamp","V2","txt")
 	
	d$dt <- strptime(d$tstamp, "%Y-%m-%d %H:%M:%S", tz="America/Chicago" )
	
	if( !is.null(pdate) ) {
		# compute relative time...in minutes
		d$rel_tm <- as.numeric( difftime( d$dt, pdate, units = "sec") ) / 60
	}
	
	tot <- nrow(d)
	
	d
}


