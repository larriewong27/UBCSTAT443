## This is the R script for STAT 443 Lab9

gdp <- read.table("gdp-unempl.txt", header=T)

plot.ts(gdp$gdp)

plot.ts(gdp$unempl)

# difference the series

dgdp <- diff(gdp$gdp)
dunempl <- diff(gdp$unempl)

acf(dgdp)
acf(dunempl)
