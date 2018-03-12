# Section 1

# Saving the iris dataset to a file to show how streaming can be emulated from a file
write.csv(iris, "data/iris.csv", row.names=FALSE)

df <- read.csv("data/iris.csv")

# Basic exploration of the dataset
nrow(df)
ncol(df)
head(df)

# Display the unique classes, first the number then the names
length(unique(df$Species))
uniqueClasses <- as.character(unique(df$Species))
uniqueClasses

# Show the class distribution
classDis <- as.data.frame(table(df$Species))
names(classDis) <- c("Class", "Count")
classDis

# Check that this matches the total number of observations
sum(classDis$Count)
nrow(df)

# Plot the class distribution
barplot(beside = TRUE, classDis$Count,
        names.arg = classDis$Class, ylim = c(0,6),
        xlab = "Class", ylab="Count")

# Seciton 2

# Using DSD_ReadCSV to read line-by-line from a CSV file in a streaming fashion

if(!require("stream")) install.packages("stream")
library("stream")
if(!require("streamMOA")) install.packages("streamMOA")
library("streamMOA")

# Create the stream. The take param specifies which parts of the dataset to use.
stream <- DSD_ReadCSV("data/iris.csv", header = TRUE, take=c(1:5), class=5, k=3)
dataPoints <- get_points(stream, 25, assignment = TRUE)
dataPoints
# note: get points is taking the first 25 items and leaving a pointer at that position, we can reset this if needed.

reset_stream(stream)
#close_stream(stream)

first100 <- get_points(stream, n=100, class=TRUE)
head(first100)

class(first100)

last50 <- get_points(stream, n=50, class= TRUE)

reset_stream(stream)
dstream <- DSC_DStream(gridsize = 0.5, gaptime = 10000L, lambda = 0.01)
update(dstream, stream, n= 150, verbose = TRUE)

reset_stream(stream)
evaluate(dstream, stream,
         measure = c("purity", "numMicroClusters", "numMacroClusters","SSQ"), n = 150)


# Checking some of the points against classes
reset_stream(stream)
pointsA <- get_points(stream, n=150)
assignmentPointsA <- get_assignment(dstream, pointsA, type = "macro", method="nn")
assignmentPointsA

df <- iris
df$cluster <- assignmentPointsA

head(df)

# Section 3

# Find a method to calculate the clustering quality
table(df$cluster,df$Species)
p <- ggplot(data=df, aes(df$cluster, df$Species)) +  xlab("Clusters") + ylab("Species") + geom_count()+ scale_x_continuous(breaks=c(1:9))
p



