
#load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load(zoo,hydroTSM,tools,tidyverse,utils,jsonlite,SWTools,network,igraph,
       ggraph,tidygraph,networkD3,dplyr,htmlwidgets,shinythemes,bslib,
       install = TRUE,
       update = FALSE)
#---------------------------------------------------------------------------

checkFileType <- function(f){
  s <- read_lines(f)
  firstText <- strsplit(s[1]," ")[[1]][1]
  if(firstText=="Export"){
    return("newFile")
  } else {
    return("preFile")
  }
}

# rf <- "E:/R/shSourceFunctionViewr/data/RawFunctionExport.csv"
parseFunctionExport <- function(rf){ 
  
  s <- read_lines(rf)
  firstText <- strsplit(s[1]," ")[[1]][1]
  s <- read_lines(rf, skip = 1)
  if(firstText=="Export"){ 
  for(i in 1:length(s)){
    if(i==1){
      f.df <- data.frame(Name=NA,
                         ParentFolder=NA,
                         LastFolder=NA,
                         PathDepth=NA,
                         Type=NA,
                         FullName=NA,
                         Value=NA)
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
    # browser()
    
  }
  
  write_csv(f.df,"./data/FunctionsListFile.csv")
  print("Successfully parsed raw Function Export File...")
  print("Saved data to ....'/data/FunctionsListFile.csv'")
  df <- getData("E:/R/shSourceFunctionViewr/data/FunctionsListFile.csv")
  return(df)
  # browser()
  } else {
    print("Incorrect File Header. Please select Raw Export file..")
    return()
    
  }
  
  
}
getData <- function(f){#f = "FunctionsList.csv"
  # if(file.exists(f)){
  fList <- read_csv(f)
  
  for(i in 1:length(fList$Name)){
    # If first run, start a new data frame to store the output
    if(i==1){
      n <- 0
    }
    # Get the ith values to test
    iName <- fList$Name[i]
    iFullName <- fList$FullName[i]
    iType <- fList$Type[i]
    iDepth <- fList$PathDepth[i]
    iBase <- fList$ParentFolder[i]
    ip <- fList$LastFolder[i]
   
    # Now loop through all the values in the text to find where there is a match
    for(j in 1: length(fList$Value)){
      if(j==1){ jCount <- 1}
      jText <- fList$Value[j]
      jTextSplit <- strsplit(jText,"[*+,()\n////\r ]+")
      
      #Check if your value exists in the Value column
      if(iName %in% jTextSplit[[1]]){
        
        jCount <- jCount + 1
        #Dont include if referenced in its own row
        if(!i==j){
          #Add these values to the dataframe
          # If first run, start a new data frame to store the output
          
          if(n==0){
            
            df <- data.frame(A=iName,
                             B=fList$Name[j],
                             A.Full.Name=iFullName,
                             B.Full.Name=fList$FullName[j],
                             Type=iType,
                             Count=jCount,
                             fPath=ip,
                             group=iBase)
          } else {
            idf <- data.frame(A=iName,
                              B=fList$Name[j],
                              A.Full.Name=iFullName,
                              B.Full.Name=fList$FullName[j],
                              Type=iType,
                              Count=jCount,
                              fPath=ip,
                              group=iBase)
            df <- rbind(df,idf)
          }
          n <- n + 1
          
        } #End Test for autoselect
      } # End test of iName in jValue
    } # End inner search loop (j)
  }# End Search Function (i)
  return(df)
}

 
plotSimpleNetwork <- function(data){
  
  p <-   data %>% 
    # filter(BaseFolder=="$ACT_Net_Take") %>% 
    simpleNetwork(height = "800",       
                  Source = 3,                 # column number of source
                  Target = 4,                 # column number of target
                  linkDistance = 80,          # distance between node. Increase this value to have more space between nodes
                  charge = -100,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                  fontSize = 10,               # size of the node names
                  fontFamily = "serif",       # font og node names
                  linkColour = "#46b99c",        # colour of edges, MUST be a common colour for the whole graph
                  nodeColour = "#ff0080",     # colour of nodes, MUST be a common colour for the whole graph
                  opacity = 0.8,              # opacity of nodes. 0=transparent. 1=no transparency
                  zoom = T                    # Can you zoom on the figure?
    )
  return(p)
  
} 


plotForceNetwork <- function(data){
  sources <- data %>%
    rename(label = A.Full.Name)
  destinations <- data %>%
    rename(label = B.Full.Name)
  nodes <- full_join(sources, destinations, by = "label", relationship = "many-to-many")
 
  # A connection data frame is a list of flows with intensity for each flow
  links <- data.frame(
    source=sources,
    target=destinations,
    group=data$group,
    value=data$Count
  )
  
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(
    name=c(as.character(links$source.label),
           as.character(links$target.label)) %>% unique()
  )
  nodes$nodeID <- index(nodes)-1
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  
  links$IDsource <- match(links$source.label, nodes$name)-1
  links$IDtarget <- match(links$target.label, nodes$name)-1
  nodes$group <- links$group[ match(nodes$name,links$source.label)]
  # Plot
  fn <- forceNetwork(Links = links, Nodes = nodes,
               Source = "IDsource", Target = "IDtarget",
               Value = "value", NodeID = "name",
               opacity = 0.8,zoom = T, Group = "group",
               linkDistance = 80,          # distance between node. Increase this value to have more space between nodes
               charge = -100,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
               legend = TRUE, 
               arrows = TRUE,
               height = "800",
               opacityNoHover = 0.8)
  return(fn)
  
  
}

filterData <- function(s,data){
  d <- data
  d$FD <- 0
  for(j in 1:length(s)){ 
    # sj <- s[j]
    sj <- strsplit(s[j],"[//.]+")
    print(sj)
    for(i in 1: nrow(d)){
      fp.Split <- strsplit(d$fPath[i],"[//.]+")
      sd <- pmatch(sj[[1]],fp.Split[[1]])
      for(k in length(sd)){
        kVal <- sd[k]
        if(!is.na(kVal)){
          d$FD[i] <- d$FD[i]+1
        }
      }
      # print(sd)
      
    }
  }
    
  fd <- filter(d,FD >= 1)
  fd <- fd[,-ncol(fd)] 
  return(fd)
  
}
  
 getFolderPaths <- function(data) {
   # fps <- unique(data$A.Full.Name)
   # Add Base Level Names first
   # browser()
   b <- c(unique(data$group))
   a <- c(b, data$A.Full.Name,data$B.Full.Name)
   a_unique <- unique(a)
   # browser()
   # browser()
   for(i in 1:length(a_unique)){
     # browser()
     iDir <- strsplit(a_unique[i],"[//.]+")
     iFunctionShortName <- paste0(iDir[[1]][length(iDir[[1]])])

     iDepth <- length(iDir[[1]])
     #recreate path without function base name
     # go through list recreating one fewer folder each time
     if (iDepth > 1) {
       for(x in 1: (iDepth-1)){
         if(x==1){
           s <- c(iDir[[1]][1])
           # ip <- iDir[[1]][1]
         } else {
           s <- c(s,paste0(iDir[[1]][x-1],".",iDir[[1]][x]))
           # ip <- paste0(ip,".",iDir[[1]][x])
         }

       }

     } else {
       s <- c(iDir[[1]][1])
       # ip <- iDir[[1]][1]
     }
     if(i==1){
       df <- data.frame(fullPath=s)
       # df <- data.frame(fullPath=ip,functionName=iFunctionShortName, pathDepth=iDepth)
     } else {
       df <- rbind(df,data.frame(fullPath=s))
       # df <- rbind(df,data.frame(fullPath=ip,functionName=iFunctionShortName,pathDepth=iDepth))
     }



   }
   df2 <- data.frame(fullPath=unique(df$fullPath))
   return(df2)


 }

 # f <- "./data/newFunctionsListFile.csv"
 # data <- getData(f)
 # selectList <- getFolderPaths(data)
  