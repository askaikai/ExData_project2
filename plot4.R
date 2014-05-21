plot4 = function(){
  
  # Across the United States, how have emissions from coal combustion-related 
  # sources changed from 1999–2008?
  
  # first, let's make sure we are in the right directory
  setwd('~/Documents/Coursera//ExploratoryDataAnalysis//ExData_project2')
  library(ggplot2)
  
  # read in data & code 
  NEI <- readRDS("../data/summarySCC_PM25.rds")
  SCC <- readRDS("../data/Source_Classification_Code.rds")
  
  #### Run this to find out which variables contain the word "Coal"
  #a=matrix(0,dim(SCC)[2],2)
  #a=as.data.frame(a)
  #for (i in 1:dim(SCC)[2]){
  #  a[i,1] = names(SCC)[i]
  #  if(length(grep("Coal", SCC[,i],ignore.case=TRUE))>0) {
  #    a[i,2] = length(grep("Coal", SCC[,i]))
  #  }
  #}
  #print(a)
  ### from here, we know that Short.Name, EI.Sector, SCC.Level.3&4 
  ### have a word "Coal" in them
  
  # select data with "Coal" in them
  thisSCC = SCC$SCC[unique(c(grep("Coal", SCC$Short.Name), grep("Coal", SCC$EI.Sector), 
                              grep("Coal",SCC$SCC.Level.Three),grep("Coal", SCC$SCC.Level.Four)))]
  coalData <- subset(NEI, SCC %in% thisSCC)
  
  year=c(1999,2002,2005,2008)
  sumPM25 <- function(x) sum(coalData$Emissions[coalData$year==x])
  totalPM25 = sapply(year, sumPM25)
  totalPM25 = data.frame(totalPM25)
  colnames(totalPM25)=c("Emissions")
  totalPM25$year=seq(1999, 2008, by = 3)
  totalPM25$Emissions=totalPM25$Emissions/(10^3)
  totalPM25$Emissions=as.numeric(format(round(totalPM25$Emissions, 3), nsmall = 3))

  # now plot
  g = ggplot(totalPM25, aes(year, Emissions)) + geom_point(size=.8)
  g = g + theme_grey(base_size = 5) + 
    geom_smooth(method="lm", se=FALSE, col="steelblue", size=.3) + 
    labs(list(title = "PM2.5 Emission in the US from Coal between 1999 and 2008", 
              x = "year", y = "Total Emission (10^3 tons)")) + 
    scale_x_continuous(limits=c(1998, 2009), breaks = seq(1999, 2008, by = 3),
                       labels=c("1999","2002","2005","2008")) + 
    annotate("text", x=seq(1999, 2008, by = 3), y=c(totalPM25$Emissions-10),
             label=as.character(totalPM25$Emissions),size=1)
  
  ggsave(filename="plot4.png",plot=g,width=8,height=6,units="cm")
  
}