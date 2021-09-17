
library(MazamaRollUtils)

# Load package air quality data
data("airquality")
t <- airquality$Temp

# Calculate moving maximum of adjacent measurements
a <- MazamaRollUtils::roll_mean(t, n = 3)
b <- seismicRoll::roll_mean(t, n = 3)
c <- zoo::rollapply(t, width = 3, FUN = mean, fill = NA)
d <- RcppRoll::roll_mean(t, n = 3, fill = NA)

all.equal(a, b)
all.equal(a, c)
all.equal(a, d)

