# This script is dedicated to analyze scraped Airbnb webpages in order
# to provide some insights about Airbnb pricing, Airbnb location and
# even daily monitor dashboards. 

library(dplyr)
library(ggplot2)
library(RgoogleMaps)
library(ggmap)
library(ggrepel)
library(maps)
library(sp)
library(gstat)
library(gridExtra)
library(RPostgreSQL)
library(reshape2)
library(xtable)

args <- commandArgs(TRUE)
kCityName <- args[1]
kCityNameUnderscored <- gsub(" ", "-", kCityName)
kZoom <- 11
kMonthDays <- 30
kProjectDir <- "/media/sean/disk2/desktop/airbnb-invest"
kPlotDir <- file.path(kProjectDir, paste0("plot/", kCityNameUnderscored))
kDailyFilename <- sprintf("data/scrape_data/%s_80_pages_daily.csv", kCityNameUnderscored)
kSearchFilename <- sprintf("data/scrape_data/%s_80_pages_search.csv", kCityNameUnderscored)
setwd(kProjectDir)

system(paste0("mkdir -p ", kPlotDir))

datDaily <- unique(read.csv(kDailyFilename))
datDaily$date <- as.Date(datDaily$date)

datSearch <- read.csv(kSearchFilename)
datSearch$base_rate <- datSearch$rate
datSearch$rate <- NULL

# Utils
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}

simpleUnderscore <- function(x) {
    return(gsub(" ", "_", tolower(x)))
}

# Evaluation Plots
getListingTable <- function(dedup = TRUE, reviewLb = 0) {
    datGrouped <- datDaily %>%
        group_by(listing_id) %>%
        summarise(rate = mean(rate),
                  occupancy_rate = mean(ifelse(availability == "False", 1, 0)),
                  occupied_days = kMonthDays * occupancy_rate)
    tbl <- merge(datSearch, datGrouped, all = TRUE)
    
    if (dedup) {
        index <- tbl %>%
        arrange(page, counter) %>%
        group_by(listing_id) %>%
        summarise(page = first(page), counter = first(counter))
        result <- unique(index %>% inner_join(tbl) %>% arrange(page, counter))
    } else {
        result <- tbl %>% arrange(page, counter) %>% select(page, counter, everything())
    }

    result <- result[result$reviews_count >= reviewLb, ]
    return(result)
}

getDailyTable <- function(reviewLb = 0) {
    tbl <- getListingTable(reviewLb = reviewLb)
    return(datDaily %>% filter(listing_id %in% tbl$listing_id))
}

plotRateQuantileByDay <- function(percent, reviewLb = 0) {
    # Function that plot rate by date. For given date, it calculates all quantiles
    # for given percentiles in the given listing rates on that day.
    #
    # Takeaways:
    #     1. Shows the rate trends for each percentile. Each percentile could be
    #        treated as listings at certain quality tier.
    #     2. Percentile is relatively robust to outliers.

    title = sprintf("rate quantile by day with reviewLb %s", reviewLb)
    percent.truncated = round(percent, 2)
    colNames = paste0("quantile", floor(100 * percent.truncated))
    dat <- getDailyTable(reviewLb) %>%
        group_by(date) %>%
        do(as.data.frame(as.list(quantile(.$rate, percent.truncated))))
    colnames(dat) <- c("date", colNames)
    dat.melted <- melt(dat, id.vars = c("date"))
    g <- ggplot(aes(x = date, y = value, col = variable), data = dat.melted) +
        geom_line() +
        ggtitle(simpleCap(title))
    ggsave(filename = file.path(kPlotDir, paste0(simpleUnderscore(title), ".png")), g,
           width = 20, height = 10)
}

getLikeliForFTL <- function() {
    # Get likelihood for each listing on whether it's a full time listing or not.
}

plotOcpyByDay <- function() {
    # Get empirical occupancy rate by day.
    #
    # Goal:
    #     1. variation of occupancy rate by day of week.

    title = "Average Occupied Days By Day"
    tbl <- getListingTable(TRUE, 1)

    g <- ggplot(aes(x = date, y = occupied_days), data = tbl) +
        geom_line() +
        geom_abline(intercept = kMonthDays) +
        ggtitle(simpleCap(title)) +
        scale_y_continuous(limits = c(0, kMonthDays))

    
}
plotRateRandomByDay <- function(k) {
    # Same as plotRateQuantileByDay except we pick k random listings from the pool
    # so as to illustrate the pattern of rate trend of actual listings. 
}

bucketizeByRatePercentile <- function(percent) {
    # The purpose of this function is to give you a better understanding of listings
    # at each percentile level by showing you all the listing IDs at that level. It
    # lets you evaluate the predicted daily rate of your potential property by putting
    # it to the right bucket.
}

bucketizeByOcpyPrecentile <- function(percent) {
    # Same as bucketizeByRatePerentile except you look at occupancy this time.
}

getReviewDistribution <- function() {
    # Show distribution of number of reviews. Estimate serious host.

    title <- "host percentage and counts based on number of reviews"
    tbl <- getListingTable(dedup = TRUE, reviewLb = 0)
    tbl <- tbl %>% mutate(bucketizedReviews =
                       ifelse(reviews_count == 0, "0",
                       ifelse(reviews_count <= 10, "0 < reviews <= 10",
                       ifelse(reviews_count <= 20, "10 < reviews <= 20",
                              "20 < reviews"))))
    result <- tbl %>%
        group_by(bucketizedReviews) %>%
        summarise(count = n(),
                  freq = n() / nrow(tbl))
    
    resultXtable <- xtable(result, caption = title)
    print(resultXtable, file = paste0(kPlotDir, "/", simpleUnderscore(title), ".html"),
          type = "html")
}

showListingTable <- function(dedup = TRUE, reviewLb = 0) {
    # Show features per listing in a tabular form.

    title <- sprintf("Listing Table %s and review lowerbound %s",
                     ifelse(dedup, "Dedup", "noDedup"), reviewLb)
    result <- getListingTable(dedup, reviewLb)
    
    resultXtable <- xtable(result, caption = title)
    print(resultXtable, file = paste0(kPlotDir, "/", simpleUnderscore(title), ".html"),
          type = "html")
}

showSummaryStats <- function(percent, reviewLb = 0) {
    # Show a bunch of tables
    #
    # Tables to Show:
    #     1. Quantile of rate
    #     2. Quantile of occupied days and occupancy rate
    #     3. Quantile of monthly gross income

    title <- sprintf("Summary Statistics with reviewLb %s", reviewLb)
    dat <- getListingTable(TRUE, reviewLb)
    result <- dat %>%
        do(data.frame(percent = percent,
                         rate = quantile(.$rate, percent, names = FALSE),
                         occupied_days = quantile(.$occupied_days, percent, names = FALSE),
                         occupancy_rate = quantile(.$occupancy_rate, percent, names = FALSE),
                         monthly_income = quantile(.$occupied_days * .$rate, percent, names = FALSE)))
    resultXtable <- xtable(result, caption = title)
    print(resultXtable, file = paste0(kPlotDir, "/", simpleUnderscore(title), ".html"),
          type = "html")
}

getMap <- function(cityname = kCityName, zoom = 10, data,
                   f = 0.05, is_google = TRUE) {
    if (!missing(data)) {
        
        bbox <- make_bbox(lon = lon, lat = lat, data, f = f)
        if (is_google) {
            map <- get_map(bbox, maptype = "roadmap", source = "google")
        } else {
            map <- get_map(bbox, source = "osm")
        }
        
    } else {
        if (cityname %in% us.cities$name) {
            print("City coordinates provided by data.frame us.cities")
            city_coord <- us.cities %>% filter(name == cityname) %>% select(lat, lon = long)
        } else {
            print("City coordinates provided by geocode")
            city_coord <- geocode(cityname)
        }
        
        map <- get_googlemap(center = c(city_coord$lon, city_coord$lat),
                             zoom = zoom, maptype = 'roadmap')
    }
    return(map)
}

showMapPlots <- function(cityname = kCityName, zoom = 10, calculate_boundary, f = 0.05, is_google = TRUE) {
    
    title = sprintf("Rate and Occupancy Map with args %s", deparse(match.call()))
    dat <- getListingTable()
    
    if (calculate_boundary) {
        map <- getMap(data = dat %>% select(lon = longitude, lat = latitude),
                      f = f,
                      is_google = is_google)
    } else {
        map <- getMap(cityname = cityname, zoom = zoom)
    }

    midRate <- median(dat$rate)
    rateLimits <- c(0, 2 * midRate)
    midOccupancy <- median(dat$occupied_days)
    occupancyLimits <- c(max(0, 2 * midOccupancy - kMonthDays),
                         min(kMonthDays, 2 * midOccupancy))
    # Rate
    e <- parent.frame(n = 2)
    e$gg1 <- ggplot(aes_string(y = "latitude", x = "longitude", color = "rate"), data = dat)   
    g1 <- ggmap(map, base_layer = gg1, alpha = 0.1, darken = c(0.2, 'black')) +
        geom_point(alpha = 1, size = 3) +
        scale_colour_gradient2(midpoint = midRate,
                               low = 'blue', high = 'red', mid = 'white',
                               limits = rateLimits) +
        theme(legend.position = 'top',
              legend.key.width = unit(2, 'cm'))

    # Occupied Days
    e$gg2 <- ggplot(data = dat, aes(y = latitude, x = longitude, color = occupied_days))
    g2 <- ggmap(map, base_layer = gg2, alpha = 0.1, darken = c(0.3, 'black')) +
        geom_point(alpha = 1, size = 3) +
        scale_colour_gradient2(midpoint = midOccupancy,
                               low = 'blue', high = 'red', mid = 'white',
                               limits = occupancyLimits) +
        theme(legend.position = 'top',
              legend.key.width = unit(2, 'cm'))

    g <- grid.arrange(g1, g2, ncol = 2)
    ggsave(filename = file.path(kPlotDir, paste0(simpleUnderscore(title), ".png")), g,
           width = 16, height = 8)
}


plotRateToOccupiedDays <- function(ylimits = c(NA, 250)) {
    g <- ggplot(aes(x = occupied_days, y = rate, label = listing_id), data = dailyAggr) +
        geom_point() +
        geom_text(hjust = 0, nudge_x = 0.1) +
        scale_y_continuous(limits = ylimits) +
        ggtitle("Daily Rate VS Occupied Days")
    return(g)
}

main <- function() {
    plotRateQuantileByDay(seq(0.1, 0.9, by = 0.1), 10)
    plotRateQuantileByDay(seq(0.1, 0.9, by = 0.1), 0)
    showListingTable(TRUE, 10)
    showListingTable(TRUE, 1)
    showListingTable(TRUE)
    showListingTable(FALSE)
    showSummaryStats(seq(0.1, 0.9, by = 0.1))
    showSummaryStats(seq(0.1, 0.9, by = 0.1), 1)
    showSummaryStats(seq(0.1, 0.9, by = 0.1), 5)
    showSummaryStats(seq(0.1, 0.9, by = 0.1), 10)
    getReviewDistribution()
    showMapPlots(calculate_boundary = TRUE, is_google = TRUE, f = 0.1)
    showMapPlots(calculate_boundary = TRUE, is_google = FALSE, f = 0.05)
    showMapPlots(calculate_boundary = TRUE, is_google = FALSE, f = 0.1)
    ## showMapPlots(calculate_boundary = TRUE, is_google = FALSE, f = 0.2)
    ## showMapPlots(calculate_boundary = FALSE, zoom = 10)
    ## showMapPlots(calculate_boundary = FALSE, zoom = 11)
    ## showMapPlots(calculate_boundary = FALSE, zoom = 12)
}

main()
