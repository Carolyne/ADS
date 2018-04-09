# Lab 08 - Imbalanced classes

setwd("~/RGU/Semester 02/Advanced Data Science/ADS")

clustData <- function (df,ClassIndex,kmeansClasses = rep(0,unique(df[,ClassIndex]))) {
  # use split function to split the dataset according to the class label
  # a set of dataframes each representing a class label will be stored
  # in dfs list()
  dfs <- split (df, df[,ClassIndex])
  # create empty list
  clustList <- list()
  n <- length(dfs)
  for (i in 1:length(kmeansClasses)){
    # Cluster according to all features excluding the label
    if (kmeansClasses[i]>1 & kmeansClasses[i]< nrow(dfs[[i]])){
      clustList[[i]] <- kmeans(dfs[[i]][,-ClassIndex],kmeansClasses[i])
      #plotcluster(clustList[[i]], clustList[[i]]$cluster)
      dfs[[i]]$cluster <- paste0((dfs[[i]][,ClassIndex]),
                                 "_","c",clustList[[i]]$cluster)
    }
    else {
      dfs[[i]]$cluster = paste0((dfs[[i]][,ClassIndex]),
                                "_c0")
    }
  }
  # put all list elements in a dataframe and return it
  # note that ldply() require the library plyr
  allClusteredElements <- ldply (dfs, data.frame)
  # drop the first column 'id' resulting from ldply
  allClusteredElements <- allClusteredElements[,-1]
  allClusteredElements <- allClusteredElements[,-ClassIndex]
  return(allClusteredElements)
}

df <- read.csv('data/hypthyroid.csv')
df$label <- as.factor(df$label)

# create backup copy of the dataset
library(plyr)
dfc <- df
# store labels (labels are 1st column in original dataset)
# and add them as the last column
tmp <- dfc$label

dfc$label <- NULL
dfc$label <- tmp
#

# notice that ncol(df) was used here because it is the index of the class label
dfc <- clustData(dfc,ncol(df),c(7,0))

barplot(table(dfc$cluster))
