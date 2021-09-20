
library(MazamaRollUtils)

# Load package air quality data
data("airquality")
t <- airquality$Temp

# Calculate moving medianimum of adjacent measurements
a <- MazamaRollUtils::roll_median(t, n = 3)
b <- RcppRoll::roll_median(t, n = 3, fill = NA)
# c <- seismicRoll::roll_median(t, n = 3)
# d <- zoo::rollapply(t, width = 3, FUN = median, fill = NA)

print(all.equal(a, b))
# all.equal(a, c)
# all.equal(a, d)

# -----

a <- MazamaRollUtils::roll_median(t, n = 3, align = -1)
b <- RcppRoll::roll_median(t, n = 3, fill = NA, align = "left")

print(all.equal(a, b))

a <- MazamaRollUtils::roll_median(t, n = 3, align = 1)
b <- RcppRoll::roll_median(t, n = 3, fill = NA, align = "right")

print(all.equal(a, b))
