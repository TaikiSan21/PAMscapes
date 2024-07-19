# GFW api testing
install.packages('gfwr')
pak::pkg_install("GlobalFishingWatch/gfwr")
library(gfwr)
?gfw_auth()
token <- yaml::read_yaml('.secrets/secrets.yml')$gfw_token

data("test_shape")

vessGroup <- get_raster(
    spatial_resolution = 'LOW',
    temporal_resolution = 'YEARLY',
    group_by = 'VESSEL_ID',
    start_date = '2021-01-01',
    end_date = '2021-02-01',
    region = test_shape,
    region_source = 'USER_JSON',
    key = token
)
str(hm)

noGroup <- hm

# my ais_west folder is lat 20-50, lon -140 - -110
library(sf)

st_polygon(list(matrix(c(-140, -110, 20, 50), ncol=2)), dim='XY')
# oops poly needs to be
# UR BR BL UL UR
makeBBMatrix <- function(latRange, lonRange) {
    latRange <- range(latRange)
    lonRange <- range(lonRange)
    matrix(
        c(latRange[2], latRange[2], latRange[1], latRange[1], latRange[2],
          lonRange[2], lonRange[1], lonRange[1], lonRange[2], lonRange[2]),
        ncol=2
    )
}
sfc <- (st_sfc(st_polygon(list(makeBBMatrix(c(-140,-110), c(20,50)))), crs=4326))
sf <- st_sf(geometry=sfc)
# 2022 04-25 - 04-28
vessComp <- get_raster(
    spatial_resolution = 'LOW',
    temporal_resolution = 'HOURLY',
    group_by = 'VESSEL_ID',
    start_date = '2022-04-25',
    end_date = '2022-04-28',
    region = sf,
    region_source = 'USER_JSON',
    key = token
)
str(vessComp)
gps <- readr::read_csv(c('tutorial/ADRIFT_017_GPS.csv'), show_col_types = FALSE)
bufferBound <- st_as_sf(gps[,c('Longitude', 'Latitude', 'UTC')],
                        coords=c('Longitude', 'Latitude'), crs=4326) %>%
    # st_bbox() %>%
    # st_as_sfc() %>%
    st_buffer(dist=50e3) %>%
    st_bbox() %>%
    st_as_sfc()
bufferBound <- st_sf(geometry=bufferBound)
bufferVess <- get_raster(
    spatial_resolution = 'LOW',
    temporal_resolution = 'HOURLY',
    group_by = 'VESSEL_ID',
    start_date = '2022-04-22',
    end_date = '2022-04-30',
    region = bufferBound,
    region_source = 'USER_JSON',
    key = token,
    print_request = TRUE
)
str(bufferVess)
ais <- readLocalAIS(gps, aisDir=c('AIS_West'), distance=10e3)
str(ais)

# CURRENT RESULT: A lot of marcad MMSI's are not present from GFW output
# seems bad. May need to ask