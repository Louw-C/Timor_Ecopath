#Loading and reading netCDF data in R to look at Chl a
library(ncdf4)
library(CFtime)
library(lattice)
library(RColorBrewer)

# set path and filename
ncpath <- "/Users/louwclaassens/Desktop/IMOS_Chla_Timor/"
ncname <- "IMOS_aggregation_Long"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "chl_oc3"  # note: Chla refers to Chlorophyl a

# open a netCDF file
ncin <- nc_open(ncfname)
print(ncin)

# get longitude and latitude
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"latitude")
nlat <- dim(lat)
head(lat)

#see how many coordinates there are
print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin,"time")
time
#Figure out what units time are - here they are days since 1 January 1990
tunits <- ncatt_get(ncin,"time","units")
tunits
#Check how many time values there are
nt <- dim(time)
nt

# get Chl a
Chla_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"lon_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(Chla_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

ls()


# decode time
cf <- CFtime(tunits$value, calendar = "proleptic_gregorian", time) # convert time to CFtime class
cf

timestamps <- as_timestamp(cf) # get character-string times
timestamps

class(timestamps)

time_cf <- CFparse(cf, timestamps) # parse the string into date components
time_cf

class(time_cf)

# replace netCDF fill values with NA's
Chla_array[Chla_array==fillvalue$value] <- NA

length(na.omit(as.vector(Chla_array[,,1])))

# get a single slice or layer (January)
m <- 1
Chla_slice <- Chla_array[,,m]
dim(Chla_slice)

# quick map
image(long,lat,Chla_slice, col=rev(brewer.pal(10,"RdBu")))

# levelplot of the slice
grid <- expand.grid(lon=long, lat=lat)
levelplot(Chla_slice ~ long * lat, data=grid, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
