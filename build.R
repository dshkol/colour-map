require(readr)
require(dplyr)
require(parallel)

cores <- detectCores() - 4
makeCluster(cores)

# Start by reading in some of the first data set and analyze what we have

setwd("../extracted")

f0 <- "yfcc100m_dataset-0"
d0 <- read_tsv(f0, col_names = FALSE, n_max = 1000)

# interpretation variables on first glance - we'll return to this later

# "X1"  - ?
# "X2"  - user_id 
# "X3"  - username
# "X4"  - timestamp
# "X5"  - ?
# "X6"  - camera
# "X7"  - caption
# "X8"  - ?
# "X9"  - tags
# "X10" - some kind of tag or source
# "X11" - lon
# "X12" - lat
# "X13" - accuracy? (geography)
# "X14" - photo URL
# "X15" - other url (?)
# "X16" - license
# "X17" - license link
# "X18" - ?
# "X19" - ?
# "X20" - ?
# "X21" - ?
# "X22" - file format
# "X23" - ? 

# Let's simplify this. What we need are user_id, username, timestamp, lon, lat, accuracy. These correspond to X2, X3, X4, X11, X12, X13, 
d0 <- d1[c(2,3,4,11,12,13,14)]

# We're also only interested in retaining the photos for which we have geography
d0 <- d1[!is.na(d1$X11),]

# we are left with a file around 1.5% the size of the original and much more manageable

# let's iterate over the rest of the files

# directory has 12 files including the autotags and hash files, dataset has 10 files which start at position 2

names <- dir()
names <- names[2:11]

flickrList <- c()

m1 <- Sys.time()

for (i in 1:length(names)) {
  tmp <- read_tsv(names[i], col_names = FALSE)
  tmp <- tmp[c(2,3,4,11,12,13,14)]
  tmp <- tmp[!is.na(tmp$X11),]
  flickrList[[i]] <- as.data.frame(tmp)
}

m2 <- Sys.time()

fdat <- rbind.fill(flickrList)

m3 <- Sys.time()
names(fdat) <- c("user.id","user.name","timestamp","lon","lat","acc","url")

# Let's reduce the data to pictures taken mostly in Canada
# We will use a single bounding box for now. This is imperfect and will 
# capture a substantial number in bordering US states - but computationally 
# more simple for now. We can resolve the bounding box issue later. 

# Vancouver bbox=-123.27,49.195,-123.020,49.315 per OSM wiki
lonmin <- -123.27   
latmin <-  49.195 
lonmax <- -123.020
latmax <- 49.315

fdat_van <- fdat[fdat$lon > lonmin & fdat$lon < lonmax,]
fdat_van <- fdat_van[fdat_van$lat > latmin & fdat_van$lat < latmax,]

# While we're at it, let's get a couple other Canadian cities

# Montreal bbox
# -73.745615,45.411828,-73.440969,45.586293
lonmin <- -73.746   
latmin <-  45.411 
lonmax <- -73.440
latmax <- 45.587

fdat_mtl <- fdat[fdat$lon > lonmin & fdat$lon < lonmax,]
fdat_mtl <- fdat_mtl[fdat_mtl$lat > latmin & fdat_mtl$lat < latmax,]

# Toronto bbox
# -79.613803,43.603032,-79.225156,43.808504
lonmin <- -79.614   
latmin <-  43.603 
lonmax <- -79.225
latmax <- 43.809

fdat_to <- fdat[fdat$lon > lonmin & fdat$lon < lonmax,]
fdat_to <- fdat_to[fdat_to$lat > latmin & fdat_to$lat < latmax,]

fdat <- NULL
flickrList <- NULL
tmp <- NULL


# 1 by 1 download photos and check colour

require(RImagePalette)
require(rvest)
require(jpeg)

# For each picture, the url connects to the photos page. We need a direct link to the photo itself. 
# General approach
# a <- read_html("http://www.flickr.com/photos/23740710@N04/3648061643/")
# b <- a %>% html_nodes("meta") %>% html_attrs()
# img <- grep("og:image",b)[1]
# jpg <- grep("jpg",b[[img]]) 
# c <- b[[img]][[jpg]]
# # download file as a temp file to read and open
# z <- tempfile()
# download.file(c,z,mode = "wb")
# pic <- readJPEG(z)
# #pq1 <- quantize_image(pic,1)
# #pq3 <- quantize_image(pic,3)
# pp1 <- image_palette(pic,1)
# pp3 <- image_palette(pic,3)
# pp5 <- image_palette(pic,5)
# file.remove(z)

fdat_van200 <- filter(fdat_van) %>% sample_n(.,200)

# function to get the palettes of photos
get_pal <- function(x) {
  a <- read_html(x)
  b <- a %>% html_nodes("meta") %>% html_attrs()
  img <- grep("og:image",b)[1]
  jpg <- grep("jpg",b[[img]]) 
  c <- b[[img]][[jpg]]
  z <- tempfile()
  download.file(c,z,mode = "wb")
  pic <- readJPEG(z)
  pp1 <- image_palette(pic,1)
  pp3 <- image_palette(pic,3)
  pp5 <- image_palette(pic,5)
  pal_list <- list(pp1,pp3,pp5)
  names(pal_list) <- c("Pal1","Pal3","Pal5")
  file.remove(z)
  return(pal_list)
}

# All below is broken
# add a safety to catch errors
get_pal_safe <- failwith(NA,get_pal)

pal200 <- lapply(fdat_van200$url, get_pal_safe)
list_length_index <- lengths(pal200)
pal200df <- as.data.frame(do.call(rbind,lapply(pal200, 'length<-',max(list_length_index))))
names(pal200df) <- c("pal1","pal3","pal5")

# pal200df$pal1 <- as.character(pal200df$pal1)
# pal200df$pal3 <- as.character(pal200df$pal3)
# pal200df$pal5 <- as.character(pal200df$pal5)
# fdat_van200 <- cbind.data.frame(fdat_van200,pal200df)
# write_csv(fdat_van200, "fdat_van200.csv")



# Plot with base graphics
#plot(fdat_van200$lon, fdat_van200$lat, col = fdat_van200$pal1, pch = 19, cex = 2)

# Get second most prominent colour from pal3
# library(stringr)
#fdat_van200$pal3_2 <- str_extract(fdat_van200$pal3, "\\,(.*)\\,")
#fdat_van200$pal3_2 <- str_extract(fdat_van200$pal3, "#......")


fdat_van25k <- filter(fdat_van) %>% sample_n(.,25000)

start_time <- Sys.time()
pal25k <- lapply(fdat_van25k$url, get_pal_safe)
list_length_index <- lengths(pal25k)
pal25kdf <- as.data.frame(do.call(rbind,lapply(pal25k, 'length<-',max(list_length_index))))
names(pal25kdf) <- c("pal1","pal3","pal5")
end_time <- Sys.time()

van25 <- cbind.data.frame(fdat_van25k,pal25kdf)
van25_bak <- van25
van25$pal1 <- as.character(van25$pal1)
van25$pal3 <- as.character(van25$pal3)
van25$pal5 <- as.character(van25$pal5)



# Generate colour matrix

colmat <- matrix(van25$pal1, nrow = 125, ncol = 200)
matpos <- as.data.frame(which(colmat != 0, arr.ind = TRUE))
matpos$pal <- van25$pal1

# Sort colours by converting into HSV
require(colorspace)

# 2 step process:
# 1: convert hex to RGB
# 2: convert RGB to HSV

safe_hex2RGB <- failwith(NA, hex2RGB)
safe_rgb2hsv <- failwith(NA, rgb2hsv)

get_hsv <- function(x) {
  tmp <- safe_hex2RGB(x)
  hsv <- safe_rgb2hsv(matrix(tmp@coords, nrow = 3, ncol =1))
  hsv <- t(hsv)
  return(hsv)
}

get_hsv <- failwith(NA,get_hsv)
# get HSV values
pal25hsl <- sapply(pal25kdf$pal1, get_hsv)

# convert into DF
palhsl <- list()
palhsl$h <- sapply(pal25hsl, "[[",1)
palhsl$s <- sapply(pal25hsl, "[[",2)
palhsl$l <- sapply(pal25hsl, "[[",3)
palhsl <- as.data.frame(palhsl)

# generate PCA
require(FactoMineR)


