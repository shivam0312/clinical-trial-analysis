# Set seed for reproducible results
set.seed(100)

# Packages
library(tm) # Text mining: Corpus and Document Term Matrix
library(class) # KNN model
library(SnowballC) # Stemming words
library(caret)
library(e1071)
# Read csv with two columns: text and category
data <- read.csv(file.choose(), sep ="\t", header = F)

set.seed(2018)

intrain <- createDataPartition(data$V1,p=.005,list = F)
sample.data <- data[intrain,]
docs <- Corpus(VectorSource(sample.data$V2))


# Clean corpus
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument, language = "english")

# Create dtm
dtm <- DocumentTermMatrix(docs)

# Transform dtm to matrix to data frame - df is easier to work with
mat.df <- as.data.frame(data.matrix(dtm), stringsAsfactors = FALSE)

# Column bind category (known classification)
mat.df <- cbind(mat.df, sample.data$V1)

# Change name of new column to "category"
colnames(mat.df)[ncol(mat.df)] <- "V1"

# Split data by rownumber into two equal portions
train <- sample(nrow(mat.df), ceiling(nrow(mat.df) * .50))
test <- (1:nrow(mat.df))[- train]

# Isolate classifier
cl <- mat.df[, "V1"]

# Create model data and remove "category"
modeldata <- mat.df[,!colnames(mat.df) %in% "V1"]

# Create model: training set, test set, training set classifier
nb.classifier <- naiveBayes(modeldata[train, ],cl[train])
nb.pred <- predict(nb.classifier,modeldata[test, ],type = "class")

# Confusion matrix
conf.mat <- table("Predictions" = nb.pred, Actual = cl[test])
confusionMatrix(conf.mat)

# Accuracy
(accuracy <- sum(diag(conf.mat))/length(test) * 100)
