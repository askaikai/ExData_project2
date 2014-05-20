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
  library(reshape2)

  # read in data & code 
  NEI <- readRDS("../data/summarySCC_PM25.rds")
  SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  # select data just from Baltimore
  Bmore=NEI[NEI$fips == "24510",]
  Bmore$type=as.factor(Bmore$type)
  Bmore$year=as.factor(Bmore$year)
  BmoreSummary=tapply(Bmore$Emissions, INDEX=list(c(Bmore$year),c(Bmore$type)), sum,simplify = TRUE)
  BmoreSummary=as.data.frame(BmoreSummary)
  colnames(BmoreSummary)=levels(Bmore$type)
  rownames(BmoreSummary)=levels(Bmore$year)

  BmoreSummary$year=row.names(BmoreSummary)
  BmoreMelt=melt(BmoreSummary,variable.name='type',value.name='Emissions')
  BmoreMelt$year=as.numeric(BmoreMelt$year)
  BmoreMelt$Emissions=BmoreMelt$Emissions/(10^3)

  # now plot
  g = ggplot(BmoreMelt, aes(year, Emissions)) + geom_point(size=.8) 
  g = g + facet_grid(.~type) + theme_grey(base_size = 4) + 
    geom_smooth(method="lm", se=FALSE, col="steelblue", size=.3) + 
    labs(list(title = "PM2.5 Emission in Baltimore City by Type", 
              x = "year", y = "Total Emission (10^3 tons)")) + 
    scale_x_continuous(limits=c(1998, 2009), breaks = seq(1999, 2008, by = 3),
                     labels=c("1999","2002","2005","2008")) 
  
  ggsave(filename="plot3.png",plot=g,width=8,height=6,units="cm")
}
