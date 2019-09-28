library(sf)
library(foreign)
library(tidyverse)
library(lwgeom)


options(stringsAsFactors = FALSE)

pt <- data.frame(lat = 18.372642,long = -66.073269)
city <- 'Area Metro'

#-------import road files.----------
files <- list.files(path="featnames", pattern="*.dbf", full.names=TRUE, recursive=FALSE) %>% as.data.frame
names(files) <- c("path")
files$GEOID <- substr(files$path, 19, 23)

allroads <-NULL

#----------combine em all and add suffixes--------------
for (i in 1:nrow(files)) {
  #read in the feature names file, which has road suffixes in it
  featname <- read.dbf(files$path[i],  as.is = TRUE)
  featname$SUFTYPABRV[is.na(featname$SUFTYPABRV)] <- featname$PRETYPABRV[is.na(featname$SUFTYPABRV)]
  featname <- featname %>% dplyr::select(LINEARID, SUFTYPABRV) %>% unique
  
  #read in the roads shapefile as a simple features dataframe
  roads <- read_sf("roads", paste0("tl_2019_", files$GEOID[i], "_roads"))
  roads$len <- st_length(roads)
  
  #join the two 
  temp <- inner_join(roads, featname, by = "LINEARID") 
  
  #merge em all
  if (i==1) {
    allroads <- temp
  }else {
    allroads <- do.call(rbind, list(temp, allroads))
  }
}

#---------subset the roads into a circle 15 miles wide-------
pt <- pt %>% st_as_sf(coords = c("long", "lat"), crs = 4326) %>% st_transform(2163)
circle <- st_buffer(pt, dist = 20000)
circle <- circle %>% st_transform(st_crs(temp))
allroads <- st_intersection(circle, allroads)

#-----------figure out plot colors automagically------
plottype <- allroads %>% select(SUFTYPABRV,len) 
plottype$geometry <- NULL
plottype <- subset(plottype, !is.na(SUFTYPABRV))
plottype <- plottype %>% group_by(SUFTYPABRV) %>% summarise(Length = sum(len)) %>% arrange(-Length) %>% head(8)

#these ones I want to set always
plotcolors <- c('Other' = '#cccccc')

#get what's leftover
findcolors <- plottype$SUFTYPABRV
colors <- c('#59c8e5', '#fed032',  '#4cb580', '#fe9ea5', '#fe4d64', '#0a7abf', '#ff9223', '#2e968c')
#go thru and assign the rest in order
for (i in 1:length(colors)) {
  tempnames <- names(plotcolors)
  plotcolors <- c(plotcolors, colors[i]) 
  names(plotcolors) <- c(tempnames, findcolors[i])
}


#-----------plot----------
suff <- plottype$SUFTYPABRV
allroads$SUFTYPABRV[!(allroads$SUFTYPABRV %in% suff)] <- "Other"

otherroads <- allroads[(allroads$SUFTYPABRV  == "Other"),]
allroads <- allroads[(allroads$SUFTYPABRV  != "Other"),]

blankbg <-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(), axis.title.y=element_blank(),
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())

ggplot() + blankbg + theme(panel.grid.major = element_line(colour = "transparent")) + 
  geom_sf(data=otherroads, size = .45, aes(color=SUFTYPABRV)) + 
  geom_sf(data=allroads, size = .50, aes(color=SUFTYPABRV), alpha = 0.45) + 
  scale_color_manual(values = plotcolors) +
  labs(title = "Carreteras Area Metropolitana, Puerto Rico", 
       caption = "Preparado por Ian Flores Siaca, MDS")

ggsave(paste0("./IndivRoads/", city, ".png"), plot = last_plot(),
       scale = 1, width = 24, height = 24, units = "in",
       dpi = 500)





