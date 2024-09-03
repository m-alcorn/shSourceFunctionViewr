#load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load(zoo,hydroTSM,tools,tidyverse,utils,jsonlite,SWTools,network,igraph,
       ggraph,tidygraph,networkD3,dplyr,htmlwidgets,shinythemes,bslib,
       install = TRUE,
       update = FALSE)


rf <- paste0(getwd(),"/RawFunctionExport.csv")

s <- read_lines(rf,
                skip = 1
                )
# types <- vector()
# functionNames <- vector()
# strsplit(l,"[*+,()\n////\r ]+")
# for(i in 1:length(s)){
#   l <- s[i]
#   ls <- strsplit(l,"[,]+")
#   types <- append(types,ls[[1]][1])
#   functionNames <- append(functionNames,ls[[1]][2])
# }
# uniqueFunctionTypes <- unique(types)
# uniqueFunctions <- unique(functionNames)
i <- 1
for(i in 1:length(s)){
  if(i==1){
    f.df <- data.frame(name=NA,
                       parentFolder=NA,
                       lastFolder=NA,
                       pathDepth=-NA,
                       functionType=NA,
                       fullName=NA,
                       expression=NA)
  }
  l <- s[i]
  ls <- strsplit(l,"[,]+")
  fType <- ls[[1]][1]
  full <- ls[[1]][2]
  ffn.Split <- strsplit(full,"[//.]+")
  shortName <- paste0("$",ffn.Split[[1]][length(ffn.Split[[1]])])
  pFolder <- ffn.Split[[1]][1]
  expressionCol <- switch(fType,
                          "Function"= 3,
                          "TimeSeriesVariable"= 3,
                          "Pattern Variable" = 4,
                          "Modelled Variable" = 3,
                          "Linear Variable" = 3)
  expr <- ls[[1]][expressionCol]
  
  pDepth <- length(ffn.Split[[1]])
  
  if (pDepth > 1) {
    for(x in 1: (pDepth-1)){
      if(x==1){
        ip <- ffn.Split[[1]][1]
      } else {
        ip <- paste0(ip,".",ffn.Split[[1]][x])
      }
      
    } 
  } else {
    ip <- NA
    print("Found zero depth path")
  }
  
  
   f.df[i,] <- list(shortName,
                    pFolder,
                    ip,
                    pDepth,
                    fType,
                    full,
                    expr)

}



