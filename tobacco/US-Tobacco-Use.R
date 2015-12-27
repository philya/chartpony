library(ggplot2)
library(dplyr)
library(gridExtra)
library(rgdal)
library(rgeos)


load_tus_data <- function(filename) {
    
    # Default col type
    #colClasses <- rep("character", 29)
    
    #colClasses[4]
    
    tus <- read.csv(filename,
                     sep = ',',
                     stringsAsFactors = F,
                     colClasses = c(
                         "Data_Value" = "numeric",
                         "Data_Value_Std_Err" = "numeric",
                         "TopicDesc" = "factor",
                         "LocationAbbr" = "factor",
                         "LocationDesc" = "factor",
                         "Response" = "factor",
                         "Low_Confidence_Limit" = "numeric",
                         "High_Confidence_Limit" = "numeric",
                         "Sample_Size" = "numeric",
                         "Gender" = "factor",
                         "Race" = "factor",
                         "Age" = "factor",
                         "Education" = "factor"
                     )
    )
    
    tus$Year.End <- sapply(tus$YEAR, function(ystr) {max(as.numeric(unlist(strsplit(ystr, '-'))))})
    
    tus
}

#
pipe2011 <- tus %>% filter(TopicDesc == "Pipe Use (Adults)", Response == "Current", Gender == "Overall", Year.End == 2011) %>% arrange(desc(Year.End), desc(Data_Value)) %>% select(YEAR, Year.End, LocationAbbr, LocationDesc, Data_Value, Sample_Size)
cigar2011 <- tus %>% filter(TopicDesc == "Cigar Use (Adults)", Response == "Current", Gender == "Overall", Year.End == 2011) %>% arrange(desc(Year.End), desc(Data_Value)) %>% select(YEAR, Year.End, LocationAbbr, LocationDesc, Data_Value, Sample_Size)




map_text_style <- element_text(family = "Open Sans", size = 16, face = "plain", color="gray15")
map_theme <- theme(
    plot.title = element_text(lineheight=2.8, face="plain", vjust = 3, size = 24),
    text = map_text_style,
    panel.background = element_blank(), 
    panel.margin = unit(2, "cm"),
    strip.background = element_rect(fill=NA),
    strip.text.x = element_text(size=16),
    plot.margin = unit(c(2, 1, 1, 2), "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y=element_text(vjust=4),
    #legend.title = default_text_style,
    legend.title = map_text_style,
    legend.margin = unit(2, "cm"),
    legend.key.size = unit(1, "cm"),
    #legend.key = element_blank(),
    legend.key = element_rect(size=5, color="white"),
    legend.text = map_text_style
    #legend.direction = "horizontal",
    #legend.position = "bottom"
)



# get GeoJSON from https://team.cartodb.com/u/andrew/tables/andrew.us_states_hexgrid/public/map

# Read in GeoJSON ---------------------------------------------------------

us <- readOGR("us_states_hexgrid.geojson", "OGRGeoJSON")

# Get centers of polygns for label placement ------------------------------

centers <- cbind.data.frame(data.frame(gCentroid(us, byid=TRUE), id=us@data$iso3166_2))

# Convert base shapefile into something ggplot can handle -----------------

us_map <- fortify(us, region="iso3166_2")

gg <- ggplot()

# Plot base map -----------------------------------------------------------



# Plot filled polygons ----------------------------------------------------

gg <- gg + geom_map(data=us_map, map=us_map,
                    aes(x=long, y=lat, map_id=id, fill=NULL),
                    color="white", size=1.5)

gg <- gg + geom_map(data=cigar2011, map=us_map,
                    aes(fill=Data_Value, map_id=LocationAbbr), size=1.5, color="white")
# Overlay borders without ugly line on legend -----------------------------

gg <- gg + ggtitle(
    bquote(
        atop(bold(.("Title")), atop("Sub-title"), "")
    )
) 

#gg <- gg + guides(colour = guide_legend("Traffic Source")) 

#gg <- gg + guide_legend("% of population", override.aes = list(colour = NA))
gg <- gg + guides(fill = guide_legend("%", override.aes = list(colour = NA)))



#gg <- gg + geom_map(data=us@data, map=us_map,
#                    aes(map_id=iso3166_2),
#                    fill="#ffffff", alpha=0, color="white",
#                    show_guide=FALSE)

# Place state name in center ----------------------------------------------

gg <- gg + geom_text(data=centers, aes(label=id, x=x, y=y), color="white", size=6, family="Open Sans", fontface = "plain")

# ColorBrewer scale; using distiller for discrete vs continuous -----------

#gg <- gg + scale_fill_distiller(palette="WhOrRd", na.value="#49250D")

# cayenne #941200
# tobacco (deep oak) #49250D

# colourbar

gg <- gg + scale_fill_gradient(low = "#f1f1f1", high = "#941200", na.value = "white", guide ="colourbar")

# coord_map mercator works best for the display ---------------------------

gg <- gg + coord_map()

# Remove chart junk for the “map" -----------------------------------------

gg <- gg + labs(x=NULL, y=NULL)
#gg <- gg + theme_bw()
gg <- gg + map_theme
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_blank())

grid.newpage()

footer <- "Philip Olenyk\ntwitter.com/@polenyk\n\nData Source:\nThe Tobacco Use Supplement to the Current Population Survey\nhttp://appliedresearch.cancer.gov/tus-cps/"

g <- arrangeGrob(gg,
                 
                 bottom = textGrob(footer,
                                   x = 0.95, 
                                   #x = 0.05,
                                   hjust = 1, vjust=0.1,
                                   gp = gpar(
                                       fontface = "plain",
                                       alpha = .5,
                                       fontfamily = "Open Sans",
                                       fontsize = 12)))    
grid.draw(g)
