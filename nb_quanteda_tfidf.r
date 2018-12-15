install.packages("RColorBrewer")
library(RColorBrewer)
library(quanteda)
library(ggplot2)
library(caret)

data <- read.csv("D://Prrojjecct/sample100.csv",header = T,sep=",",stringsAsFactors = F)

data.corpus <- corpus(data$V2)

#attaching the class labels to the corpus message text
docvars(data.corpus)<-data$V1

#subsetting only the Eligible Criteria
eligible.plot <- corpus_subset(data.corpus,docvar1 == "__label__0")
eligible.plot <- dfm(eligible.plot,tolower=T,remove=stopwords(source = "smart"))


#ploting -wordcount -- ELIGIBLE
eligible.col <- brewer.pal(10, "BrBG")
textplot_wordcloud(eligible.plot, min.freq = 16, color = eligible.col)
title("Eligible Wordcloud", col.main = "grey14")


#subsetting only the Not Eligible(NE) Criteria
ne.plot <- corpus_subset(data.corpus,docvar1 == "__label__0")
ne.plot <- dfm(eligible.plot,tolower=T,remove=stopwords(source = "smart"))


#ploting -wordcount -- Not ELIGIBLE
ne.col <- brewer.pal(10, "BrBG")
textplot_wordcloud(ne.plot, min.freq = 16, color = ne.col)
title("Not Eligible Wordcloud", col.main = "grey14")

#splitting Data for training and validation
intrain <- createDataPartition(data$V1,p=0.7,list = F)
training <- data[intrain,]
validation <- data[-intrain,]


data.dfm <- dfm(data.corpus,tolower = F)
data.dfm <- dfm_trim(data.dfm,min_termfreq =5,min_docfreq = 3)
data.dfm <- dfm_tfidf(data.dfm)

head(data.dfm)


data.dfm.train <- data.dfm[1:700,]
data.dfm.test <- data.dfm[701:1000,]

nb.classifier <- textmodel_nb(data.dfm.train,training[,1])
nb.classifier


#Fitted Naive Bayes Model
textmodel_NB <- dfm(x=data.dfm.train,y=training[,1]) 


pred <- predict(nb.classifier,data.dfm.test)
pred


tbl <- table(predicted=pred,actual=validation[,1])
confusionMatrix(tbl)
