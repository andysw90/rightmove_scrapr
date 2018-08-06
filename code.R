library(rvest)
library(openxlsx)
already_viewed_url <- "https://docs.google.com/spreadsheets/d/1OKrCL2WQ6RMoZh7i6bF0o1xNzdU4B8KUwwd4HNFlbvQ/export?format=csv"
already_viewed_raw <- system(paste("curl",already_viewed_url),intern=TRUE)
writeLines(already_viewed_raw,"tmp.csv")
already_viewed <- read.csv("tmp.csv")

n_pages <- 3
urls <- paste0("http://www.rightmove.co.uk/property-for-sale/find.html?locationIdentifier=REGION%5E422&minBedrooms=3&maxPrice=325000&radius=0.25&index=",c(1,24,48,72,96)[1:n_pages])
web_data_by_listing_list <- vector(mode="list",length=n_pages)
property_links_list <- vector(mode="list",length=n_pages)
for(i in 1:n_pages){
  pg <- read_html(urls[i])
  all_links <- html_attr(html_nodes(pg, "a"), "href")
  property_links_list[[i]] <- unique(all_links[grep("/property-for-sale/property-",all_links)])
}
property_links <- unique(paste0("http://www.rightmove.co.uk",unlist(property_links_list)))
strippr <- function(strng){
  strng <- gsub(pattern="\t",replacement = "",x=strng)
  strng <- gsub(pattern="\r",replacement = "",x=strng)
  strng <- gsub(pattern="\n",replacement = "",x=strng)
  strng <- gsub(pattern="\\)",replacement = "",x=strng)
  strng <- gsub(pattern="\\(",replacement = "",x=strng)
  return(strng)
}
gettr <- function(dat,toget){
  # dat <- raw_data
  # toget <- "lon"
  dat2 <- unlist(strsplit(dat,split=toget))[2]
  dat3 <- unlist(strsplit(dat2,split=","))[1]
  dat4 <- gsub(pattern = '"',replacement = "",x=dat3)
  dat5 <- gsub(pattern = ':',replacement = "",x=dat4)
  return(dat5)
}
get_prop_data <- function(url){
  # url <- property_links[1]
  pg <- read_html(url)
  Street <- strippr(pg %>% html_node(".grid-25") %>% html_text())
  if(length(grep(",",Street))>0){
    Street <- unlist(strsplit(Street,split=","))[1]
  }
  all_text <- pg %>% html_text()
  
  station_distance_raw <- strippr(unlist(strsplit(unlist(strsplit(all_text,split="Didcot Parkway"))[2],split=")"))[1])
  Station <- gsub(pattern=" ",replacement="",station_distance_raw)
  
  raw_data <- unlist(strsplit(all_text,split="RIGHTMOVE.ANALYTICS.PageViewTracker.track"))[2]
  URL <- url
  Price <- gettr(raw_data,"prc")
  Location <- data.frame(Lat=gettr(raw_data,"lat"),Long=gettr(raw_data,toget="lon"))
  Added <- gettr(raw_data,"aed")
  Bedrooms <- gettr(raw_data,"bed")
  Postcode <- paste(gettr(raw_data,"pcd1"),gettr(raw_data,"pcd2"),collapse=" ")
  if(length(grep("garage",all_text,ignore.case = TRUE))>0){
    Garage <- 1
  }else{Garage <- 0}
  URL_floorplan_tmp <- gsub(pattern="sale/property-",replacement = "sale/fullscreen/view-floorplan.html?propertyId=",x=url)
  URL_floorplan <- paste0(unlist(strsplit(URL_floorplan_tmp,split=""))[-c((nchar(URL_floorplan_tmp)-4):nchar(URL_floorplan_tmp))],collapse="")
  URL_images_tmp <- gsub(pattern="sale/property-",replacement = "sale/fullscreen/image-gallery.html?propertyId=",x=url)
  URL_images <- paste0(unlist(strsplit(URL_images_tmp,split=""))[-c((nchar(URL_images_tmp)-4):nchar(URL_images_tmp))],collapse="")
  return(list(Street=Street,
              Postcode=Postcode,
              Price=as.numeric(Price),
              Bedrooms=as.numeric(Bedrooms),
              Garage=as.numeric(Garage),
              Added=Added,
              StationDistance=Station,
              URL=URL,URL_images=URL_images,URL_floorplan=URL_floorplan))
}
all_houses <- as.data.frame(t(sapply(property_links,get_prop_data)))
rownames(all_houses) <- NULL
URLcols <- grep("URL",colnames(all_houses))

### Split by viewed and not
wb <- createWorkbook()

viewed_indx <- as.vector(sapply(as.character(already_viewed$Link),function(x) which(all_houses$URL==x)))
not_viewed_indx <- c(1:length(all_houses$URL))[-viewed_indx]

sht_not_viewed <- addWorksheet(wb,"Not_viewed",gridLines=TRUE)
not_viewed_dat <- all_houses[not_viewed_indx,-URLcols]
openxlsx::writeDataTable(wb,sht_not_viewed,not_viewed_dat)
nv_URLs <- as.vector(c(all_houses[not_viewed_indx,URLcols[1]]))
nv_URLs_images <- as.vector(c(all_houses[not_viewed_indx,URLcols[2]]))
nv_URLs_floorplans <- as.vector(c(all_houses[not_viewed_indx,URLcols[3]]))
names(nv_URLs) <- rep("Listing",length(nv_URLs))
class(nv_URLs) <- "hyperlink"
names(nv_URLs_images) <- rep("Images",length(nv_URLs))
class(nv_URLs_images) <- "hyperlink"
names(nv_URLs_floorplans) <- rep("Floorplan",length(nv_URLs))
class(nv_URLs_floorplans) <- "hyperlink"
writeData(wb,sheet=sht_not_viewed, x = nv_URLs, startCol = URLcols[1],startRow = 2)
writeData(wb,sheet=sht_not_viewed, x = nv_URLs_images, startCol = URLcols[2],startRow = 2)
writeData(wb,sheet=sht_not_viewed, x = nv_URLs_floorplans, startCol = URLcols[3],startRow = 2)

sht_viewed <- addWorksheet(wb,"Viewed",gridLines=TRUE)
viewed_dat <- cbind(all_houses[viewed_indx,-URLcols],already_viewed[,-c(1,3,5,8)])

openxlsx::writeDataTable(wb,sht_viewed,viewed_dat)
v_URLs <- as.vector(c(all_houses[viewed_indx,URLcols[1]]))
v_URLs_images <- as.vector(c(all_houses[viewed_indx,URLcols[2]]))
v_URLs_floorplans <- as.vector(c(all_houses[viewed_indx,URLcols[3]]))
names(v_URLs) <- rep("Listing",length(v_URLs))
class(v_URLs) <- "hyperlink"
names(v_URLs_images) <- rep("Images",length(v_URLs))
class(v_URLs_images) <- "hyperlink"
names(v_URLs_floorplans) <- rep("Floorplan",length(v_URLs))
class(v_URLs_floorplans) <- "hyperlink"
writeData(wb,sheet=sht_viewed, x = v_URLs, startCol = dim(viewed_dat)[2]+1,startRow = 2)
writeData(wb,sheet=sht_viewed, x = v_URLs_images, startCol = dim(viewed_dat)[2]+2,startRow = 2)
writeData(wb,sheet=sht_viewed, x = v_URLs_floorplans, startCol = dim(viewed_dat)[2]+3,startRow = 2)

openxlsx::saveWorkbook(wb,paste0("Houses",Sys.Date(),".xlsx"),overwrite=TRUE)


# 
# write.csv(t(df_out),"house_info_auto_2018-04-15.csv")





