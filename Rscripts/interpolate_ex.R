#install.packages('zoo')
require(zoo)

missingData = c(10,NA,7,NA,NA,11,NA)
plot(missingData)

#Last observation carried forward
plot(na.locf(missingData),type="l")

#Approx (linear interpolation)
plot(na.approx(missingData),type="l")

#Spline (polynomial interpolation)
plot(na.spline(missingData),type="l")
