# You must address the following questions and tasks in your exploratory analysis. For each question/task you will need to make a single plot. Unless specified, you can use any plotting system in R to make your plot.
# # 
# 
# 
# 
# 

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("../data/summarySCC_PM25.rds")
SCC <- readRDS("../data/Source_Classification_Code.rds")

library(ggplot2)

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
Baltimore = subset(NEI, NEI$fips=="24510")
total_emission_Baltimore = aggregate(Baltimore$Emissions, by=list(Baltimore$year), sum)
names(total_emission_Baltimore) = c("year","total")

# Yes.
png('./plot2.png')
plot(total~year, total_emission_Baltimore, xlab='Year', ylab='Total emission')
dev.off()

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
Baltimore = subset(NEI, NEI$fips=="24510")
total_emission_by_type = aggregate(Baltimore$Emissions, by=list(Baltimore$year, Baltimore$type), sum)
names(total_emission_by_type) <- c("year", "type", "total")

g <- ggplot(total_emission_by_type, aes(year, total)) + 
     geom_point(aes(pch=type)) +
     xlab('Year') +
     ylab('Total emission')
     #facet_grid(. ~ type)
ggsave('./plot3.png')

# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

which(apply(SCC, 2, function(x) any(grepl("Coal", x))))
#     Short.Name       EI.Sector SCC.Level.Three  SCC.Level.Four 
#              3               4               9              10 
levels(SCC$EI.Sector)[which(grepl("Coal", levels(SCC$EI.Sector)))]
#levels(SCC$ESCC.Level.Three)[which(grepl("Coal", levels(SCC$SCC.Level.Three)))]
#levels(SCC$ESCC.Level.Three)[which(grepl("Coal", levels(SCC$SCC.Level.Three)))]

# names(SCC)
#  [1] "SCC"                 "Data.Category"       "Short.Name"         
#  [4] "EI.Sector"           "Option.Group"        "Option.Set"         
#  [7] "SCC.Level.One"       "SCC.Level.Two"       "SCC.Level.Three"    
# [10] "SCC.Level.Four"      "Map.To"              "Last.Inventory.Year"
# [13] "Created_Date"        "Revised_Date"        "Usage.Notes"      

#grep("ombusti", levels(SCC$SCC.Level.One))
#  [3] "External Combustion Boilers"            
#  [5] "Internal Combustion Engines"            
# [13] "Stationary Source Fuel Combustion"      
coal = levels(SCC$EI.Sector)[which(grepl("Coal", levels(SCC$EI.Sector)))]
#coal_comb = levels(SCC$EI.Sector)[c(13, 18, 23)]

#idx = which(SCC$EI.Sector==levels_EI_sector[13] | SCC$EI.Sector==levels_EI_sector[18] | SCC$EI.Sector==levels_EI_sector[23])

#idx = SCC$EI.Sector %in% coal_comb

scc_coal = subset(SCC$SCC, SCC$EI.Sector %in% coal)
nei_coal = subset(NEI, NEI$SCC %in% scc_coal)
total_emission_coal = aggregate(nei_coal$Emissions, by=list(nei_coal$year), sum)

names(total_emission_coal) = c("year", "total")
png('./plot4.png')
plot(total~year, total_emission_coal, xlab='Year', ylab='Total emission')
dev.off()

# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
#which(apply(SCC, 2, function(x) any(grepl("Motor", x))))
motor = levels(SCC$SCC.Level.Two)[which(grepl("Vehicle", levels(SCC$SCC.Level.Two)))]
scc_motor = SCC$SCC[SCC$SCC.Level.Two %in% motor]

nei_motor = subset(NEI, NEI$SCC %in% scc_motor & NEI$fips=="24510")
total_emission_motor = aggregate(nei_motor$Emissions, by=list(nei_motor$year), sum)
names(total_emission_motor) = c("year", "total")

png('./plot5.png')
plot(total~year, total_emission_motor, xlab='Year', ylab='Total emission', main='Emissions from motor vehicle sources in Baltimore')
dev.off()

#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == 06037). Which city has seen greater changes over time in motor vehicle emissions?
motor = levels(SCC$SCC.Level.Two)[which(grepl("Vehicle", levels(SCC$SCC.Level.Two)))]
scc_motor = SCC$SCC[SCC$SCC.Level.Two %in% motor]

nei_motor = subset(NEI, (NEI$SCC %in% scc_motor) & (NEI$fips=="24510" | NEI$fips=="06037"))
total_emission_motor_by_city = aggregate(nei_motor$Emissions, by=list(nei_motor$year, nei_motor$fips), sum)
names(total_emission_motor_by_city) = c("year", "city", "total")

g <- ggplot(total_emission_motor_by_city, aes(year, total)) +
     geom_point(aes(color=city)) +
     #facet_grid(. ~ city) +
     xlab('Year') +
     ylab('Total emission') +
     scale_color_discrete(name  ="City",
                          breaks=c("06037", "24510"),
                          labels=c("Baltimore", "Los Angeles"))
ggsave('./plot6.png')
