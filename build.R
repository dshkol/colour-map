require(readr)
require(plyr)
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

fdat.van <- fdat[fdat$lon > lonmin & fdat$lon < lonmax,]
fdat.van <- fdat.van[fdat.van$lat > latmin & fdat.van$lat < latmax,]

# While we're at it, let's get a couple other Canadian cities

# Montreal bbox
# -73.745615,45.411828,-73.440969,45.586293
lonmin <- -73.746   
latmin <-  45.411 
lonmax <- -73.440
latmax <- 45.587

fdat.mtl <- fdat[fdat$lon > lonmin & fdat$lon < lonmax,]
fdat.mtl <- fdat.mtl[fdat.mtl$lat > latmin & fdat.mtl$lat < latmax,]

# Toronto bbox
# -79.613803,43.603032,-79.225156,43.808504
lonmin <- -79.614   
latmin <-  43.603 
lonmax <- -79.225
latmax <- 43.809

fdat.to <- fdat[fdat$lon > lonmin & fdat$lon < lonmax,]
fdat.to <- fdat.to[fdat.to$lat > latmin & fdat.to$lat < latmax,]

fdat <- NULL
flickrList <- NULL
tmp <- NULL


# 1 by 1 download photos and check colour

require(RImagePalette)
require(readr)

# For each picture, the url connects to the photos page. We need a direct link to the photo itself. 

a <- read_html("http://www.flickr.com/photos/23740710@N04/3648061643/")
b <- a %>% html_nodes("meta") %>% html_attrs()
img <- grep("og:image",b)[1]
jpg <- grep("jpg",b[[img]]) 
c <- b[[img]][[jpg]]
c