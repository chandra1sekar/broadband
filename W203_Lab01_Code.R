# Overall questions: Does a trade-off exist between these concepts? Secondarily, consider whether there is evidence for the beneficial effects of open access policies.

# TODO (Eugene): Write up introduction
# TODO (Chandra): look through speed (and maybe price and penetration for more outliers); transformations (e.g. take the logarithm); how to respond - we will have to decide on key variables; look at standard deviation versus average versus median on speed variables
# TODO (Bhuvnesh): take a look at bivariate or even three-way relationships (could tie into overall questions)
# TODO (Eugene): secondary analysis


setwd("~/Desktop/EugeneTang/Grad School/Berkeley/W203_Statistics/Homework/Lab01/Broadband_EDA/")
library(dplyr)
library(car)
convert_to_numeric = function(col) {
  return(as.numeric(sub("[\\$%,]","", col)))
}

df_penetration = read.table("Penetration.csv", header = TRUE, sep = ",") # penetration had one extra line on the bottom
drops_penetration <- c("X") # extra column
df_penetration = df_penetration[ , !(names(df_penetration) %in% drops_penetration)]
df_price = read.table("Price.csv", header = TRUE, sep = ",")
df_speed = read.table("Speed.csv", header = TRUE, sep = ",") 
colnames(df_speed)[2] <- "Country.Code"
# speed had two extra lines on the bottom
# name of country code variable was different in df_speed

# convert to numeric
# make 3 a constant - start of numeric data
df_price[3:length(df_price)] = lapply(df_price[3:length(df_price)], convert_to_numeric)
df_penetration[3:length(df_penetration)] = lapply(df_penetration[3:length(df_penetration)], convert_to_numeric)
df_speed[3:length(df_speed)] = lapply(df_speed[3:length(df_speed)], convert_to_numeric)


summary(df_penetration)
summary(df_price)
summary(df_speed)

df_partial = full_join(df_penetration, df_price, by = c("Country", "Country.Code"))
df_full = full_join(df_partial, df_speed, by = c("Country", "Country.Code"))

colnames(df_full)
summary(df_full)

scatterplotMatrix(~Penetration.per.100.OECD..2008 + Penetration.per.100.OECD..2007 + Household.penetration..OECD + X2G.and.3G.penetration.per.100..OECD + Penetration.per.100.GC + X3G.penetration.per.100 + Growth.in.3G.penetration + Wi.Fi.hotspots..JiWire + Wi.Fi.hotspots.per.100.000..JiWire + Percent.of.population.in.urban.areas, data=df_full, main = "Scatterplot Matrix for Broadband EDA Variables", diagonal="boxplot")

scatterplotMatrix(~Price.for.low.speeds..combined + Price.for.med.speeds..combined + Price.for.high.speeds..combined + Price.for.very.high.speeds..combined, data=df_full, main = "Scatterplot Matrix for Broadband EDA Variables", diagonal="boxplot")

scatterplotMatrix(~Price.for.low.speeds..combined + Price.for.med.speeds..combined + Price.for.high.speeds..combined + Price.for.very.high.speeds..combined, data=df_full, main = "Scatterplot Matrix for Broadband EDA Variables", diagonal="boxplot")

# Poland percent of population in urban areas 162%
# Turkey 3G Prenetration is 0 despite having growth
# Iceland latency s.d.

# Bivariate
scatterplotMatrix(~Price.for.low.speeds..combined + Price.for.med.speeds..combined + Price.for.high.speeds..combined + Price.for.very.high.speeds..combined, data=df_full, main = "Scatterplot Matrix for Broadband EDA Variables")

scatterplotMatrix(~Price.for.med.speeds..combined + Average.latency.speedtest.net + Median.latency..speedtest.net + Average.download.speedtest.net..kbps. + Median.download..speedtest.net..kbps. + Average.upload.speedtest.net..kbps. + Median.upload..speedtest.net..kbps., data=df_full)

scatterplotMatrix(~Price.for.med.speeds..combined + Average.download.speedtest.net..kbps. + Median.download..speedtest.net..kbps. + Average.upload.speedtest.net..kbps. + Median.upload..speedtest.net..kbps., data=df_full) # Price is low, speed is high? Maybe infrastructure plays a role here?

scatterplotMatrix(~Price.for.high.speeds..combined + Average.download.speedtest.net..kbps. + Median.download..speedtest.net..kbps. + Average.upload.speedtest.net..kbps. + Median.upload..speedtest.net..kbps., data=df_full) # Price is low, speed is high? Maybe infrastructure plays a role here?

scatterplotMatrix(~Price.for.med.speeds..combined + Maximum.advertised.speed.OECD..kbps. + Average.advertised.speed.OECD..kbps. + Average.actual.speed..Akamai..kbps., data=df_full)
# maybe not much of a relationship between price and average advertised speed

scatterplotMatrix(~Price.for.med.speeds..combined + Penetration.per.100.OECD..2008 + X2G.and.3G.penetration.per.100..OECD + Growth.in.3G.penetration + Wi.Fi.hotspots..JiWire + Wi.Fi.hotspots.per.100.000..JiWire + Percent.of.population.in.urban.areas, data=df_full)


