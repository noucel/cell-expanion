##################################################################
# Author: Marc Rigau
# Title: Predicting cell expansion profiles after induced stimuli
# Date: Jul 29, 2022
# Updated: Jul 29, 2022
# Version: 1
##################################################################

##################################################################
# LIBRARIES
##################################################################
library( tidyverse ) # Kernel for statistics in science fields.
library(scales)
#library(RColorBrewer); display.brewer.all(colorblindFriendly = T) # Use color blind palette


##################################################################
# Color-blind codes
##################################################################

# Set HEX colors for color blind palette
# http://mkweb.bcgsc.ca/colorblind/palettes.mhtml
cb3 <- c('#009F81', '#00C2F9', '#A40122' )
cb4 <- c('#d4af37', '#009F81', '#00C2F9', '#A40122' )
cb5 <- c('#d4af37', '#009F81', '#00C2F9', '#A40122', '#00C2F9' )
cb6 <- c('#009F81', '#008DF9', '#00C2F9', '#A40122', '#E20134', '#FF6E3A')
cb7 <- c('#9F0162', '#009F81', '#00FCCF', '#8400CD', '#008DF9', '#00C2F9', '#FF6E3A')
cb12 <- c('#9F0162', '#009F81', '#FF5AAF', '#00FCCF', '#8400CD', '#008DF9', '#00C2F9', '#FFB2FD', '#A40122', '#E20134', '#FF6E3A', '#FFC33B')

# Use color blind palette in fills:
# scale_fill_manual(values=cbPalette)

# Use color blind palette in line and points:
# scale_colour_manual(values=cbPalette)

##################################################################
# Load data
##################################################################
# Load table into a dataframe (d)
# Save data in the raw format (d.raw)
d = d.raw <- read.csv( "example-imput-dataset.csv", header = T )

# Data structure
str( d )

# # Convert data formats
# col_names <- c( 'Specimen' )
# d[ ,col_names] <- lapply( d[ ,col_names], factor)


# Subset dataframe
d <- select( d, -c( 'Viability' ) )

# Drop unused levels
d <- droplevels(d)

# Is data balanced?
xtabs( ~ Day + Specimen, d )

# Data overview
head( d )

# Visualise the distribution of datapoints in a histogram
hist( d$Cell.Count, col= 'Skyblue', breaks = 6, xlab = 'Number of cells', main = 'Cell Counts' )


##################################################################
# Plot data
##################################################################

# Personalize a theme
my.theme <- theme( panel.background = element_blank(), strip.background.x = element_blank(), strip.background.y = element_blank(),
                   legend.position = 'bottom', legend.direction = 'horizontal', legend.title = element_blank(), legend.text = element_text( size= 18 ),
                   strip.text = element_text( size = 18, color = "black", angle = 0 ),
                   strip.background = element_rect( fill = "white", colour = "black", size = 1 ),
                   plot.title = element_text( hjust = 0.5, face = "bold", size = 18 ), plot.subtitle = element_text( hjust = 0.5 ),
                   axis.text.x = element_text( size = 14 ), axis.title.x = element_text( size = 18 ),
                   axis.text.y = element_text( size = 14 ), axis.title.y = element_text( hjust = 0.5, angle = 90, size = 18 ) )


# Comparing the two Experiment approaches
ggplot( d, aes( y = Cell.Count, x=Day, group = Donor, color = Donor ) ) +
  facet_grid( ~ Specimen ) +
  geom_line( size = 1.25 ) +
  geom_point( size = 4 ) +
  labs( title = 'In-vitro Expansion of T Cells' ) +
  xlab( 't (days)' ) +
  ylab( '# cells' ) +
  scale_y_continuous( trans = "log10",
                      #                     breaks = 10^c( 1:5 ), limits = c( 10, 100000 ),
                      labels = trans_format( "log10", math_format( 10^.x ) ) ) + 
  scale_colour_manual( values = cb4 ) +
  theme_bw( ) + my.theme




##################################################################
# END SCRIPT
##################################################################









