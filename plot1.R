plot1 = function(){
  # 
  # Have total emissions from PM2.5 decreased in the United States 
  # from 1999 to 2008? Using the base plotting system, make a plot 
  # showing the total PM2.5 emission from all sources for each of 
  # the years 1999, 2002, 2005, and 2008.

  # first, let's make sure we are in the right directory
  setwd('~/Documents/Coursera//ExploratoryDataAnalysis//ExData_project2')

  # read in data & code 
  NEI <- readRDS("../data/summarySCC_PM25.rds")
  SCC <- readRDS("../data/Source_Classification_Code.rds")

  # calculate the sum of PM2.5 for each year
  year=c(1999,2002,2005,2008)
  sumPM25 <- function(x) sum(NEI$Emissions[NEI$year==x])
  totalPM25 = sapply(year, sumPM25)

  # now plot
  png(file='plot1.png')

  plot(totalPM25/(10^6),ann=FALSE, xaxt="n")
  lines(totalPM25/(10^6))
  title(main=expression('Total PM'[2.5]*' Emission in the United States from 1999 to 2008'),
     xlab='Year', ylab=expression('Total Emission (10'^6*' tons)'))
  axis(1, at=c(1:4),
     labels=c("1999","2002","2005","2008"))

  dev.off()
}