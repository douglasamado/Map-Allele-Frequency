# Package to make the map of allele frequency
library(RColorBrewer)
library(sf)
library(maptools)
library(rgdal)
library(ggplot2)
library(ggsn)
library(gggibbous)
library(ggrepel)
# Area of Brazil (Polygon)

BR <- readOGR("BRUFE250GC_SIR.shp")
BR1 <- fortify(BR)

# Area for state (Polygon)

mt<- readOGR("51UFE250GC_SIR.shp") # The maps area is downloaded of ibge page (https://www.ibge.gov.br/busca.html?searchword=shapefile)
sp<- readOGR("35UFE250GC_SIR.shp")
ba<- readOGR("29UFE250GC_SIR.shp")
ms<- readOGR("50UFE250GC_SIR.shp")
go<- readOGR("52UFE250GC_SIR.shp")
sc<- readOGR("42UFE250GC_SIR.shp")
rs<- readOGR("43UFE250GC_SIR.shp")

#######################

# Plot map using the ggplot2 package

g<-ggplot() +
  geom_polygon(data = BR, aes(x=long, y = lat, group = group),
                             fill = "lightgray", color = "lightgray", alpha = 0.5) + # This function change the colors of the background and the border line of the map
 # coord_fixed(1.1, expand = TRUE) + # This function leaves the map proportional
  geom_polygon(data = BR, 
               aes(x = long, y = lat, group = group), 
               color = "white", fill = NA, size = 0.3) + # This function allows choose the color and thickness of border lines
  #geom_polygon(data = mt, aes(x = long, y = lat, group = group), fill = "grey", color = "white", size = 0.3) + # Polygon of each state (When line there is hash, is to deactivate the function)
  #geom_polygon(data = sp, aes(x = long, y = lat, group = group), fill = "grey", color = "white", size = 0.3) + 
  #geom_polygon(data = ba, aes(x = long, y = lat, group = group), fill = "grey", color = "white", size = 0.3) +
  #geom_polygon(data = ms, aes(x = long, y = lat, group = group), fill = "grey", color = "white", size = 0.3) +
  #geom_polygon(data = go, aes(x = long, y = lat, group = group), fill = "grey", color = "white", size = 0.3) +   
  geom_polygon(data = sc, aes(x = long, y = lat, group = group), fill = "grey", color = "white", size = 0.3) +
  geom_polygon(data = rs, aes(x = long, y = lat, group = group), fill = "grey", color = "white", size = 0.3) +
  
  # Place georeferenced locations in the map
  geom_point(data = frequency, aes(x = Long, y = Lat), #,shape = factor(ponto), # Put the shape of the dots |  #colour = factor(ponto), # Choose the color of the dots 
             show.legend = FALSE,
             size = 1.5, # Size of the dots
             alpha = 1 # Transparency: The closer to 1, the less transparent 
            ) +
  scale_color_brewer(palette = "Dark2") + # Automatically color of the dots per location
  #scale_colour_manual(values = c("green","#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) + # Manual color of the dots per location
  #labs(colour = "População") + # Put the name in the color legend
  xlab("Longitude") + ylab("Latitude") + # Put the name in the x and y axis
  theme(axis.text = element_text(size = 14), axis.title=element_text(size=14,face="bold"), 
        #legend.text = element_text(size = 14),
        #legend.title = element_text(size = 14)
       ) +
coord_sf(xlim = c(-73, -35), ylim = c(-33, 5), expand = T, datum = st_crs(BR)) +
  geom_text_repel(data=frequency, mapping=aes(x=longitude, y=latitude, label= ponto), direction = "x", vjust = -1.3) + # This function adjust the location of name for each dots
  geom_segment(aes(x=frequency$Long, frequency$Lat, xend = frequency$longitude, yend = frequency$latitude), color = "gray20") + # This function create the line between name and pie chart
  
 scale_fill_manual(values = c("forestgreen", "gold")) +
  ggsn::scalebar(x.min = -70, x.max = -35,
           y.min = -32, y.max = -30,
           dist = 500, dist_unit = "km",
           st.bottom = F, st.color = "gray20", st.dist = 0.3,
           transform = TRUE, model = "WGS84", st.size=4, height=0.3) +
  ggsn::north(x.min = -40, x.max = -35,
              y.min = 0, y.max = 5, scale = 1, symbol = 3) +
  theme_classic() +
  theme(legend.position = c(0.05, 0.2), legend.direction = "vertical", legend.justification = c(0, 0)) # Used to choose the legend position
  
#################################################################
############ Create the map of the allele frequency #############
library(scatterpie)
  
g +
  geom_scatterpie(data = frequency, aes(x=longitude, y=latitude, r=1),  # Pie chart of each dot
                  cols = c("arroz", "milho"), legend_name = "Cultura")
  
  
##########################################################################
# Correct the coordinate to position the pie chart correctly at each dot #

frequency$Long <- frequency$long + c(-2, 2)
frequency$Lat <- frequency$lat + c(2, 2)

