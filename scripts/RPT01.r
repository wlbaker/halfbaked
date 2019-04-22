#
# DESCRIPTION:
#     This is a special purpose script for looking at pressure below the balloon (GP1.SYS) 
#     and changes to the balloon volume.
#
# REQUIREMENTS:
#     Must load setup.r and vitals_h5.r before using this script.
#

require( knitr)
require( ggplot2 )



print_one_animal <- function( group, animal_id ) {
	cat("Processing: ", animal_id, "\n")
	
	v <- read_drager_vitals( animal_id )
	anno <- read_annotations( animal_id )
	if( is.na(anno) ) {
		cat( animal_id, ": ", "missing annotations\n" )
		return()
	}
	cat("got anno\n")
	anno$txt = tolower(anno$txt )
	tp <- subset( anno, txt == "start bleed" )
	if( nrow(tp) == 0) {
		tp <- subset( anno, txt == "start bleeding" )
	}
	if( nrow(tp) == 0) {
		tp <- subset( anno, txt == "satrzt bleed" )  # 7717
	}
	if( nrow(tp) == 0) {
		tp <- subset( anno, txt == "start bleedq" )  # 7716
	}
	if( nrow(tp) == 0) {
		tp <- subset( anno, txt == "start hemorrhage" )  # 7679
	}
	if( nrow(tp) == 0) {
		cat( animal_id, ": ", "missing START BLEED\n" )
		return()
	}
	tm0 <- as.POSIXct( tp$dt )

	# txt == 9309 "insert REBOA"
	# txt == 2875 "INSERT BALLOON"

	dt <- as.difftime( 180, format = "%M", unit="mins" )
	end_tm <- tm0 + dt

	anno$rel_tm <-  difftime( anno$tstamp, tm0, unit="mins" )
	anno$ttm <- as.POSIXct( tm0 + anno$rel_tm )

	v$dt <- as.POSIXct( v$dt )
	v$mins <- difftime( v$dt, tm0, unit="mins" )
	p0 <- ggplot(data=v, aes(x=mins,y=GP1.SYS)) + 
		ylim(0, 180) + xlim(50, 190) +
		geom_point() + 
		geom_point(aes(y=ART.SYS),color="red") + 
		geom_vline(data=anno, aes(xintercept=anno$rel_tm), color="blue") +
		geom_text(data=anno, aes(anno$rel_tm, 150, label=anno$txt), show.legend=FALSE, color="black", angle=90) +
		ylab( "GP1 Systolic" ) + xlab( paste("Group: ", group, "   Animal ID: ", animal_id) )
	
	print( p0 )
}


with_pdf = TRUE
if( with_pdf ) {
	pdfname <- paste( "c:\\tmp", "clc_balloon_adj.pdf", sep="\\" )
	pdf( file=pdfname, paper="USr", width=10, height=7.5 )  # USr for US Rotated, i.e., landscape
}

animal_id = "2975" #  xlim(50, 190)
animal_id = "9309"
animal_id = "2875"
animal_id = "7715"  # group is CLC REBOA 45

clc_45 = subset( tally_df, Group=="CLC REBOA 45")
animals <- clc_45$animal_id
animals <- c("4308", "4309", "4326")
for( animal_id in animals ) {
	print_one_animal( "CLC REBOA 45", animal_id )
}

clc_60 = subset( tally_df, Group=="CLC REBOA 60")
animals <- clc_60$animal_id
animals <- c("7715", "4300", "4306")
for( animal_id in animals ) {
	print_one_animal( "CLC REBOA 60", animal_id )
}

if( with_pdf ) {
	dev.off()
	pdfname <- paste( "c:\\tmp", "manual_balloon_adj.pdf", sep="\\" )
	pdf( file=pdfname, paper="USr", width=10, height=7.5 )  # USr for US Rotated, i.e., landscape
}

pr_45 = subset( tally_df, Group=="Partial REBOA 45")
animals <- pr_45$animal_id
for( animal_id in animals ) {
	print_one_animal( "Partial REBOA 45", animal_id )
}

pr_60 = subset( tally_df, Group=="Partial REBOA 60")
animals <- pr_60$animal_id
for( animal_id in animals ) {
	print_one_animal( "Partial REBOA 60", animal_id )
}

if( with_pdf ) {
	dev.off()
}

# p1 <- ggplot(data=v, aes(x=dt,y=GP1.SYS,group=group,color=group)) + 
#    geom_smooth(method="loess") + guides(colour=FALSE) + 
# scale_color_manual(values=group.colors) + 
	
	 