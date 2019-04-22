#
# DESCRIPTION:
#     This is a special purpose script for looking at pressure below the balloon (GP1.SYS) of two animals
#     to determine if they can be reclassified.
#
# REQUIREMENTS:
#     Must load setup.r and vitals_h5.r before using this script.
#

require( knitr)
require( ggplot2 )


animal_id = "9309"  # "2875"  # 
v <- read_drager_vitals( animal_id )
anno <- read_annotations( animal_id )
anno$txt = tolower(anno$txt )
tp <- subset( anno, txt == "start bleed" )
tm0 <- tp$dt

# txt == 9309 "insert REBOA"
# txt == 2875 "INSERT BALLOON"

dt <- as.difftime( 180, format = "%M", unit="mins" )
end_tm <- tm0 + dt

anno$rel_tm <- dt

p0 <- ggplot(data=v, aes(x=dt,y=GP1.SYS)) + 
	geom_point() + ylim(0, 120) + xlim(tm0, end_tm) +
	geom_text(data=anno, aes(anno$dt, -1, label=anno$txt), show.legend=FALSE, color="black", angle=90) +
	ylab( "GP1 Systolic" ) + xlab(animal_id)

# p1 <- ggplot(data=v, aes(x=dt,y=GP1.SYS,group=group,color=group)) + 
#    geom_smooth(method="loess") + guides(colour=FALSE) + 
# scale_color_manual(values=group.colors) + 
	
	 