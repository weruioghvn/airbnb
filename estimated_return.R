library(ggplot2)


# Utility function
number_ticks <- function(n) {function(limits) pretty(limits, n)}

setClass("Investment",
         slots = list(price = "numeric",
                      down = "numeric",
                      furniture = "numeric",
                      renovation = "numeric",
                      apr = "numeric",
                      year = "numeric",
                      taxRate = "numeric",
                      maintenanceFee = "numeric",
                      hoa = "numeric",
                      occupiedDays = "numeric",
                      rate = "numeric",
                      priceLift = "numeric",
                      investYears = "numeric",
                      downPay = "numeric",
                      principal = "numeric",
                      invest = "numeric",
                      mortgagePerYear = "numeric",
                      mortgagePerMonth = "numeric",
                      taxPerYear = "numeric",
                      taxPerMonth = "numeric",
                      managementFee = "numeric",
                      grossIncome = "numeric",
                      netIncome = "numeric",
                      netIncomePerYear = "numeric",
                      capitalGain = "numeric",
                      capRate = "numeric",
                      capitalGainPerYear = "numeric"))

# Initializer for Investment class
setMethod(
    f = "initialize",
    signature = "Investment",
    definition = function(.Object, price, down, furniture, renovation,
                          apr, year, taxRate, maintenanceFee, hoa,
                          occupiedDays, rate, priceLift, investYears) {
        
        downPay = price * down
        principal = price * (1 - down)
        invest = downPay + furniture + renovation
        mortgagePerYear = principal * apr / (1 - 1 / (1 + apr) ^ year)
        mortgagePerMonth = mortgagePerYear / 12
        taxPerYear = price * taxRate
        taxPerMonth = taxPerYear / 12
        managementFee = hoa + maintenanceFee
        grossIncome = rate * occupiedDays
        netIncome = grossIncome - taxPerMonth - managementFee - mortgagePerMonth
        netIncomePerYear = netIncome * 12
        capitalGain = price * ((1 + priceLift) ^ investYears - 1)
        capitalGainPerYear = capitalGain / investYears
        capRate = (netIncomePerYear + capitalGainPerYear) / invest
        
        .Object@price <- price
        .Object@down <- down
        .Object@furniture <- furniture
        .Object@renovation <- renovation
        .Object@apr <- apr
        .Object@year <- year
        .Object@taxRate <- taxRate
        .Object@maintenanceFee <- maintenanceFee
        .Object@hoa <- hoa
        .Object@occupiedDays <- occupiedDays
        .Object@rate <- rate
        .Object@priceLift <- priceLift
        .Object@investYears <- investYears
        .Object@downPay <- downPay
        .Object@principal <- principal
        .Object@invest <- invest
        .Object@mortgagePerYear <- mortgagePerYear
        .Object@mortgagePerMonth <-mortgagePerMonth
        .Object@taxPerYear <- taxPerYear
        .Object@taxPerMonth <- taxPerMonth
        .Object@managementFee <- managementFee
        .Object@grossIncome <- grossIncome
        .Object@netIncome <- netIncome
        .Object@netIncomePerYear <- netIncomePerYear
        .Object@capitalGain <- capitalGain
        .Object@capRate <- capRate
        .Object@capitalGainPerYear <- capitalGainPerYear

        return(.Object)
    }
)

# Print method for Investment object 
setGeneric("print")
setMethod("print",
          c(x = "Investment"),
          function(x) {
              # Get basic investment stats
              
              cat(sprintf(
                  "
                   Cap rate is %s%%.
                   Invest is %s.
                   Net income per year is %s.
                   Capital gain in %s years is %s.
                   Capital gain each year is %s if I sell the house in %s years\n\n",
                  round(100 * x@capRate, 2),
                  x@invest,
                  round(x@netIncomePerYear),
                  x@investYears,
                  round(x@capitalGain),
                  round(x@capitalGainPerYear),
                  x@investYears))
          }
)

# Get price and cap rate relationship

plotPriceRange <- function(inv, lowPrice, highPrice,
                           hoas, downs = seq(0.1, 0.3, by = 0.1)) {
    x <- seq(lowPrice, highPrice, length = 500)
    d <- data.frame(price = numeric(0),
                    capRate = numeric(0),
                    hoa = numeric(0))
    for (hoa in hoas) {
        for(down in downs) {
            for (i in 1:length(x)) {
                inv_new <- new("Investment",
                           price = x[i],
                           down = down,
                           furniture = inv@furniture,
                           renovation = inv@renovation,
                           apr = inv@apr,
                           year = inv@year,
                           taxRate = inv@taxRate,
                           maintenanceFee = inv@maintenanceFee,
                           hoa = hoa,
                           occupiedDays = inv@occupiedDays,
                           rate = inv@rate,
                           priceLift = inv@priceLift,
                           investYears = inv@investYears)
                d <- rbind(d, data.frame(price = x[i],
                                         capRate = inv_new@capRate,
                                         hoa = hoa,
                                         down = down))
            }
        }
    }

    g <- ggplot(aes(x = price, y = capRate, color = as.factor(hoa)), data = d) +
        geom_line() +
        scale_x_continuous(breaks = number_ticks(20)) +
        scale_y_continuous(breaks = number_ticks(20)) +
        ggtitle("Cap Rate V. Price Colored by HOA Fee") +
        facet_wrap(~ down) +
        geom_vline(xintercept = inv@price, linetype = 2) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    g
}


main <- function() {
    # New instance
    inv <- new("Investment",
               price = 150000 / 2,
               down = 0.25,
               furniture = 8000,
               renovation = 10000,
               apr = 0.038,
               year = 30,
               taxRate = 0.031,
               maintenanceFee = 150,
               hoa = 0,
               occupiedDays = 26,
               rate = 85.5,
               priceLift = 0.02,
               investYears = 5)
    print(inv)
    plotPriceRange(inv, 50000, 200000, hoas = 0,
                   downs = c(0.1, 0.2, 0.3, 1))
}

main2 <- function() {
    # New instance
    inv <- new("Investment",
               price = 160888,
               down = 0.3,
               furniture = 8000,
               renovation = 10000,
               apr = 0.038,
               year = 30,
               taxRate = 0.027,
               maintenanceFee = 150,
               hoa = 0,
               occupiedDays = 17,
               rate = 146,
               priceLift = 0.02,
               investYears = 5)

    plotPriceRange(inv, 50000, 200000, hoas = 0,
                   downs = c(0.1, 0.2, 0.3, 1))
}

main3 <- function() {
    # New instance
    inv <- new("Investment",
               price = 160888,
               down = 0.3,
               furniture = 8000,
               renovation = 10000,
               apr = 0.038,
               year = 30,
               taxRate = 0.027,
               maintenanceFee = 150,
               hoa = 0,
               occupiedDays = 16,
               rate = 181,
               priceLift = 0.02,
               investYears = 5)

    plotPriceRange(inv, 50000, 200000, hoas = 0,
                   downs = c(0.1, 0.2, 0.3, 1))
}


## main()

