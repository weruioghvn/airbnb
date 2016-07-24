library(Quandl)
library(ggplot2)
library(dplyr)

Quandl.api_key("UqQP4MikD_dNnx7h4Vu4")

# Constants
kDataDir <- "../data/zillow_data/downloaded/"

loadData <- function() {
    # Get area code from quandl API
    state_codes <<- read.csv("./zillow_data/state_codes.csv", header = TRUE, sep = "|",
                            quote = "\"", stringsAsFactors = FALSE)
    county_codes <<- read.csv("./zillow_data/county_codes.csv", header = TRUE,
                             sep = "|",  quote = "\"", stringsAsFactors = FALSE)
    metro_codes <<- read.csv("./zillow_data/metro_codes.csv", header = TRUE, sep = "|",
                            quote = "\"", stringsAsFactors = FALSE)
    hood_codes <<- read.csv("./zillow_data/hood_codes.csv", header = TRUE, sep = "|",
                           quote = "\"", stringsAsFactors = FALSE)
    city_codes <<- read.csv("./zillow_data/city_codes.csv", header = TRUE, sep = "|",
                           quote = "\"", stringsAsFactors = FALSE)

    # Metropolitan data
    metro_map <<- metro_codes$Code
    f <- function(x) sub(",", "", x)
    names(metro_map) <<- sapply(metro_codes$Region, f)

}

metro2Code <- function(..., type = "SF") {
    metro_name <- c(...)
    metro_names <- names(metro_map)
    for (m in metro_name) {
        if (!(m %in% metro_names)) {
            stop(sprintf("%s is not a valid name for a metropolitan", m))
        }
    }
    return(paste(sprintf("M%05d", metro_map[metro_name]), type, sep = "_"))
}

getData <- function(code, new = FALSE, dir = kDataDir) {
  code_list <- list.files(path = dir)
  if (!new & (paste0(code, ".csv") %in% code_list)) {
    f <- file.path(dir, paste0(code, ".csv"))
    dat <- read.csv(file = f)
    return(dat)
  } else {
    f <- file.path(dir, paste0(code, ".csv"))
    tryCatch(dat <- Quandl(paste0("ZILL/", code)),
             error = function(e)
             {stop(sprintf("%s is not available in zillow API", code))})
    write.csv(dat, file = f, row.names = FALSE)
    return(dat)
  }
}

getPercentChange <- function(x) 100 * (x - 1)
plotPriceTrend <- function(codes, start_date, end_date) {
    title <- "Housing Price"
    dat <- data.frame()
    for (code in codes) {
        d <- getData(code)
        d$code <- code
        dat <- rbind(dat, d)
    }
    
    dat$Date <- as.Date(dat$Date)
    dat$Value <- dat$Value / 1000
    date_format <- "2000-01-01"
    
    if (!missing(end_date)) {
        l <- nchar(end_date)
        full_end_date <- paste0(end_date, substr(date_format, l + 1, 10))
        tryCatch(dat <- dat %>% filter(Date <= as.Date(full_end_date)),
                 error = function(e)
                     stop("Wrong format for end_date!! The format should be XXXX-XX-XX"))
    }

    if (!missing(start_date)) {
        l <- nchar(start_date)
        full_start_date <- paste0(start_date, substr(date_format, l + 1, 10))
        tryCatch(dat <- dat %>% filter(Date >= as.Date(full_start_date)),
                 error = function(e)
                     stop("Wrong format for end_date!! The format should be XXXX-XX-XX"))
    }
   
    g <- ggplot(aes(x = Date, y = Value, color = code), data = dat) + geom_line() +
    scale_x_date(date_breaks = "6 months", date_minor_breaks = "1 month") +
    scale_y_continuous(limits = c(0, NA)) +
    theme(axis.text.x = element_text(size = 13, angle = 90, hjust = 1),
          axis.text.y = element_text(size = 13)) +
    ylab("housing price (thousands)") + xlab("date") + ggtitle(title)
    g
}

plotAnnualRate <- function(codes, start_date, end_date) {
    title <- "Housing Price"
    dat <- data.frame()
    for (code in codes) {
        d <- getData(code)
        d$code <- code
        dat <- rbind(dat, d)
    }
    
    dat$Date <- as.Date(dat$Date)
    dat$Value <- dat$Value / 1000
    date_format <- "2000-01-01"

    # Get data 12 months earlier
    dat_before <- dat %>%
        group_by(code) %>%
        arrange(desc(Date)) %>%
        filter(row_number() > 12) %>%
        ungroup() %>%
        select(-Date, -code) %>% 
        rename(value_before = Value)

    dat_after <- dat %>%
        group_by(code) %>%
        arrange(desc(Date)) %>%
        filter(row_number() <= n() - 12) %>%
        rename(value_after = Value)

    dat_combined <- cbind(dat_before, dat_after)
    dat_new <- dat_combined %>%
        mutate(percent_change = getPercentChange(value_after / value_before))
    
    
    if (!missing(end_date)) {
        l <- nchar(end_date)
        full_end_date <- paste0(end_date, substr(date_format, l + 1, 10))
        tryCatch(dat_new <- dat_new %>% filter(Date <= as.Date(full_end_date)),
                 error = function(e)
                     stop("Wrong format for end_date!! The format should be XXXX-XX-XX"))
    }

    if (!missing(start_date)) {
        l <- nchar(start_date)
        full_start_date <- paste0(start_date, substr(date_format, l + 1, 10))
        tryCatch(dat_new <- dat_new %>% filter(Date >= as.Date(full_start_date)),
                 error = function(e)
                     stop("Wrong format for end_date!! The format should be XXXX-XX-XX"))
    }

    
    g <- ggplot(aes(x = Date, y = percent_change, color = code), data = dat_new) +
        geom_line() +
        scale_x_date(date_breaks = "6 months", date_minor_breaks = "1 month") +
        scale_y_continuous() +
        geom_abline(intercept = 0, slope = 0, alpha = 0.5) +
        theme(axis.text.x = element_text(size = 13, angle = 90, hjust = 1),
              axis.text.y = element_text(size = 13)) +
        ylab("annual increase rate (percent)") + xlab("date") + ggtitle(title)
    g
}

plotMonthRate <- function(codes, start_date, end_date) {
    title <- "Housing Price"
    dat <- data.frame()
    for (code in codes) {
        d <- getData(code)
        d$code <- code
        dat <- rbind(dat, d)
    }
    
    dat$Date <- as.Date(dat$Date)
    dat$Value <- dat$Value / 1000
    date_format <- "2000-01-01"

    # Get data 1 month earlier
    dat_before <- dat %>%
        group_by(code) %>%
        arrange(desc(Date)) %>%
        filter(row_number() > 1) %>%
        ungroup() %>%
        select(-Date, -code) %>% 
        rename(value_before = Value)

    dat_after <- dat %>%
        group_by(code) %>%
        arrange(desc(Date)) %>%
        filter(row_number() <= n() - 1) %>%
        rename(value_after = Value)

    dat_combined <- cbind(dat_before, dat_after)
    dat_new <- dat_combined %>%
        mutate(percent_change = getPercentChange(value_after / value_before))
    
    
    if (!missing(end_date)) {
        l <- nchar(end_date)
        full_end_date <- paste0(end_date, substr(date_format, l + 1, 10))
        tryCatch(dat_new <- dat_new %>% filter(Date <= as.Date(full_end_date)),
                 error = function(e)
                     stop("Wrong format for end_date!! The format should be XXXX-XX-XX"))
    }

    if (!missing(start_date)) {
        l <- nchar(start_date)
        full_start_date <- paste0(start_date, substr(date_format, l + 1, 10))
        tryCatch(dat_new <- dat_new %>% filter(Date >= as.Date(full_start_date)),
                 error = function(e)
                     stop("Wrong format for end_date!! The format should be XXXX-XX-XX"))
    }

    
    g <- ggplot(aes(x = Date, y = percent_change, color = code), data = dat_new) +
        geom_line() +
        scale_x_date(date_breaks = "6 months", date_minor_breaks = "1 month") +
        scale_y_continuous() +
        geom_abline(intercept = 0, slope = 0, alpha = 0.5) +
        theme(axis.text.x = element_text(size = 13, angle = 90, hjust = 1),
              axis.text.y = element_text(size = 13)) +
        ylab("annual increase rate (percent)") + xlab("date") + ggtitle(title)
    g
}

getMetroInfo <- function() {
    dat <- data.frame()
    for (m in names(metro_map)[1:100]) {
        d <- data.frame(name = m, price = getData(metro2Code(m))[1, "Value"])
        dat <- rbind(dat, d)
    }

    dat %>% arrange(price)
}


plotPriceTrend(c("N00622_SF", "S00001_SF"))
plotAnnualRate(c("S00001_SF", "S00002_SF"))
plotMonthRate(c("M00022_SF"))
plotPriceTrend(c("M00022_SF", "M00007_SF"))
plotAnnualRate(c("M00022_SF", "M00007_SF"))

