plot3 = function(){
  
  # Of the four types of sources indicated by the type (point, nonpoint, 
  # onroad, nonroad) variable, which of these four sources have seen 
  # decreases in emissions from 1999–2008 for Baltimore City? 
  # Which have seen increases in emissions from 1999–2008? 
  # Use the ggplot2 plotting system to make a plot answer this question.
  
  # first, let's make sure we are in the right directory
  setwd('~/Documents/Coursera//ExploratoryDataAnalysis//ExData_project2')
  library(ggplot2)

  # read in data & code 
  NEI <- readRDS("../data/summarySCC_PM25.rds")
  SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  # select data just from Baltimore
  Bmore=NEI[NEI$fips == "24510",]
  
  # now plot
  png(file='plot3.png')
  
  g = ggplot(Bmore, aes(year, Emissions))
  g + geom_point() 
  + facet_grid(. ~ type) 
  + geom_smooth(method="lm", se=FALSE, col="steelblue") 
  + labs(list(title = "PM2.5 Emission in Baltimore City by Type", 
              x = "year", y = "Total Emission (10^3 tons)"))
  
  
  dev.off()
  
}