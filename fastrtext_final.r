
install.packages("fastrtext")
library(fastrtext)

library(caret)

data <- read.csv("A:\\C-DAC\\Project\\python\\capstone-master\\textData\\labeledEligibilitySample.csv",header = F, sep = '\t')

set.seed(2018)

intrain <- createDataPartition(data$V1,p=.95,list = F)
sample.data <- data[intrain,]

set.seed(2018)
sample.intrain <- createDataPartition(sample.data$V1,p=0.7,list = F)

training <- sample.data[sample.intrain,]
validation <- sample.data[-sample.intrain,]

# prepare data
tmp_file_model <- tempfile()

train_labels <- paste0("__label__", training[,"V1"])
train_texts <- tolower(training[,"V2"])
train_to_write <- paste(train_labels, train_texts)
train_tmp_file_txt <- tempfile()
writeLines(text = train_to_write,con = train_tmp_file_txt)

test_labels <- paste0("__label__", validation[,"V1"])
test_texts <- tolower(validation[,"V2"])
test_to_write <- paste(test_labels, test_texts)

# learn model
execute(commands = c("supervised",
                     "-input", train_tmp_file_txt,
                     "-output", tmp_file_model,
                     "-dim", 20, "-lr", 1, "-epoch", 20,
                     "-wordNgrams", 2, "-verbose", 1))

model <- load_model(tmp_file_model)

# prediction are returned as a list with words and probabilities
predictions <- predict(model, sentences = test_to_write)
print(head(predictions, 5))


# Compute accuracy
mean(names(unlist(predictions)) == validation$V1)

# because there is only one category by observation, hamming loss will be the same
get_hamming_loss(as.list(validation$V1), predictions)

# you can get flat list of results when you are retrieving only one label per observation
print(head(predict(model, sentences = test_to_write, simplify = TRUE)))

# free memory
unlink(train_tmp_file_txt)
unlink(tmp_file_model)
rm(model)
gc()
