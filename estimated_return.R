
PRICE = 250000  #
DOWN = 0.3  #
DOWNPAYMENT = PRICE * DOWN
PRINCIPAL = PRICE * (1 - DOWN)
UNITS = 2  #
FURNITURE.PER.UNIT = 8000  #
FURNITURE = FURNITURE.PER.UNIT * UNITS
RENOVATION.PER.UNIT = 0  #
RENOVATION = RENOVATION.PER.UNIT * UNITS
INVEST = DOWNPAYMENT + RENOVATION + FURNITURE
APR = 0.038
YEAR = 30
MORTGAGE.PER.YEAR = PRINCIPAL * APR / (1 - 1/(1 + APR) ^ YEAR)
MORTGAGE.PER.MONTH = MORTGAGE.PER.YEAR / 12
TAXRATE = 0.027  #
TAX.PER.YEAR = PRICE * TAXRATE
TAX.PER.MONTH = TAX.PER.YEAR / 12

# Management Fee
WATER.PER.UNIT = 30
ELECTRICITY.PER.UNIT = 100
HEAT.PER.UNIT = 0
WIFI.PER.UNIT = 30
HOA = 0  #
MANAGEMENT.FEE = WATER.PER.UNIT * UNITS +
                 ELECTRICITY.PER.UNIT * UNITS +
                 HEAT.PER.UNIT * UNITS +
                 WIFI.PER.UNIT * UNITS +
                 HOA

# Return
OCCUPANCY = 25  #
RATE = 150  #
AIRBNB.INCOME = RATE * OCCUPANCY




# Monthly Net Income (Didn't count tax deduction)
NET.INCOME = AIRBNB.INCOME * UNITS -
             TAX.PER.MONTH -
             MANAGEMENT.FEE -
             MORTGAGE.PER.MONTH

NET.INCOME.PER.YEAR = NET.INCOME * 12

# Housing Market
PRICE.LIFT = 0.0 #
YEARS = 5
CAPITAL.GAIN = PRICE * (1 + PRICE.LIFT) ^ YEARS - PRICE

# Metrics
CAP.RATE = NET.INCOME.PER.YEAR / INVEST

print(paste("cap rate is", CAP.RATE))
print(paste("invest is ", INVEST))
print(paste("net income per year is", NET.INCOME.PER.YEAR))
print(paste("capital gain in", YEARS, "years is", round(CAPITAL.GAIN)))
print(paste("capital gain each year is", round(CAPITAL.GAIN / YEARS), "if I sell the house in", YEARS, "years"))
