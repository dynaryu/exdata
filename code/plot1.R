# You must address the following questions and tasks in your exploratory analysis. For each question/task you will need to make a single plot. Unless specified, you can use any plotting system in R to make your plot.
# # 
# 
# 
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
# 
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
# 
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("../data/summarySCC_PM25.rds")
SCC <- readRDS("../data/Source_Classification_Code.rds")

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
#year_vector <- c(1999, 2002, 2005, 2008)
#total_emissions <- numeric(length(year_vector))

#for (i in seq_along(year_vector)) {
#  total_emissions[i] = sum(NEI$Emissions[NEI$year==year_vector[i]])
#}

total_emission = aggregate(NEI$Emissions, by=list(NEI$year), sum)
names(total_emission) = c("year","total")

# Yes.
png('./plot1.png')
plot(total~year, total_emission, xlab='Year', ylab='Total emission')
dev.off()

# 2. # Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
Baltimore = NEI[NEI$fips=="24510",]
total_emission_Baltimore = aggregate(Baltimore$Emissions, by=list(Baltimore$year), sum)
names(total_emission_Baltimore) = c("year","total")

# Yes.
png('./plot2.png')
plot(total~year, total_emission_Baltimore, xlab='Year', ylab='Total emission')
dev.off()

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
total_emission_by_type_year = aggregate(Baltimore$Emissions, by=list(Baltimore$year, Baltimore$type), sum)
names(total_emission_by_type_year) <- c("year", "type", "total")
