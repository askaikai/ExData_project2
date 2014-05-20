plot3 = function(){
  
  # Of the four types of sources indicated by the type (point, nonpoint, 
  # onroad, nonroad) variable, which of these four sources have seen 
  # decreases in emissions from 1999–2008 for Baltimore City? 
  # Which have seen increases in emissions from 1999–2008? 
  # Use the ggplot2 plotting system to make a plot answer this question.
  #
  # useful page: http://docs.ggplot2.org/0.9.3.1/facet_grid.html
  
  # first, let's make sure we are in the right directory
  setwd('~/Documents/Coursera//ExploratoryDataAnalysis//ExData_project2')
  library(ggplot2)

  # read in data & code 
  NEI <- readRDS("../data/summarySCC_PM25.rds")
  SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  # select data just from Baltimore
  Bmore=NEI[NEI$fips == "24510",]
  Bmore$type=as.factor(Bmore$type)

  # now plot
  png(file='plot3.png')
  
  g = ggplot(Bmore, aes(year, Emissions)) + geom_point() 
  g + facet_grid(.~type) + 
    geom_smooth(method="lm", se=FALSE, col="steelblue") + 
    labs(list(title = "PM2.5 Emission in Baltimore City by Type", 
              x = "year", y = "Total Emission (10^3 tons)")) + 
    scale_x_continuous(breaks = seq(1999, 2008, by = 3),
                     labels=c("1999","2002","2005","2008"))
  
  ### I want to add mean value at each data point... use annotate?
  
  dev.off()
  
}