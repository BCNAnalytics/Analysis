if( !require(maptools) ){
  install.packages('maptools')
}
if( !require(RColorBrewer) ){
  install.packages('RColorBrewer')
}
if( !require(classInt) ){
  install.packages('classInt')
}

if( !require(BcnDataAccess) ){
  devtools::install_git('https://github.com/BCNAnalytics/BcnDataAccess')
}



idescat250 <- BcnDataSources$Idescat$Grid250_2001$Map250_2001$getMap()

# plot( idescat250 )
# str( idescat250, max.level = 2 )
# head( slot( idescat250, "data" ) )

IdescatMapPlot <- function (idescatShp, varName = "TOTAL", plotTitle = "Total population in Barcelona", legendTitle = "Population", classN = 5, paletteName = "Blues") {
  # paletteName from the package RColorBrewer
  dataPlot <- idescat250@data[, varName]
  
  dataClust <- na.omit(dataPlot)
  dataClust <- dataClust[ dataClust > 0]
  breakClass <-classIntervals(dataClust, n=classN-1, style="kmeans")
  # Exploring breakpoints
  # plot( breakClass, colourPalette[-1] )
  # print( breakClass )
  breakPoints <- c(-1, breakClass$brks)
  breakNames <- c("Empty", leglabs(breakPoints))
  
  dataPlot[ is.na( dataPlot) ] <- -1
  dataPlot[ dataPlot == 0 ] <- -1
  
  
  # For placing the text: idescat250@bbox
  
  colourPalette <- brewer.pal(classN, paletteName) 
  colorAssing <- colourPalette[findInterval(dataPlot, breakPoints, rightmost.closed = TRUE)]
  
  plot( idescatShp, col = colorAssing )
  legend(x=3649337, y=2068287, legend=breakNames, 
         fill=colourPalette, bty="n", 
         cex = 0.8)
  text(x=3652337, y=2068687, legendTitle, cex=1)
  title(plotTitle)
  text(x=3670337, y=2061087, 
       "2001 census, \n 250m grid. \n Data provided by \n Idescat.", cex=0.8, font = 3)
}

# variable names
colnames(idescat250@data)[9:15]

IdescatMapPlot( idescat250 )

varName = "HOMES"
plotTitle <- "Men population in Barcelona"
legendTitle <- "Men"
IdescatMapPlot( idescat250, varName = varName, plotTitle, legendTitle)

varName = "DONES"
plotTitle <- "Women population in Barcelona"
legendTitle <- "Women"
IdescatMapPlot( idescat250, varName = varName, plotTitle, legendTitle)



