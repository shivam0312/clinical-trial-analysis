library(XML)
library(methods)
library(pracma)
library(base)
library(stringr)
#install.packages("textclean")
library(data.table)
library(textclean)
library(tm)

setwd("A://C-DAC//Project//trials_data")
complete.data <- data.frame(label=c(),intervention=c(),condition=c(),criteria=c())

files <- list.files("A://C-DAC//Project//trials_data")

Sys.time()

count <- 0
not_read <- 0

for (i in 1001:3000) {
  count <- count + 1
  print(files[i])
  print(count)
  trial <- xmlParse(files[i])
  rootNode <- xmlRoot(trial)
  
  data <- xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))
  
  trial.data <- data.frame(t(data),row.names=NULL)
  columns <- colnames(trial.data)
  
  conditions <- c()
  interventions <- c()
  criterias <- c()
  
  for(col in columns) {
    if(str_detect(col,"condition([.][:digit:])?$")) {
      x <- trial.data[col]
      conditions <- c(conditions,x$condition[[1]][[1]])
      conditions <- tolower(conditions)
    }
    if(str_detect(col,"intervention([.][:digit:])?$")) { 
      x <- trial.data[col]
      interventions <- c(interventions,x$intervention[[1]][[2]])
      interventions <- tolower(interventions)
    }
  }
  
  relevant <- c("cancer","neoplasm","oma","tumor")
  all.condition <- paste(conditions,collapse = " ")
  all.condition <- strsplit(all.condition," ")
  all.condition <- c(unlist(all.condition))
  if(!any(relevant %in% all.condition)) {
    print("yes")
    not_read <- not_read + 1
    next
  }
  
  trial.condition <- trial.data$condition
  
  eligibility <- strsplit(trial.data$eligibility[[1]][[1]],"\n\n")
  eligibility <- unlist(eligibility,use.names = T)
  eligibility
  
  eligibility <- trimws(eligibility)
  eligibility <- str_replace_all(eligibility,"-  ","")
  eligibility <- str_replace_all(eligibility,"\n[ ]{2,}"," ")
  eligibility <- tolower(eligibility)
  eligibility <- str_replace_all(eligibility," - "," to ")
  eligibility <- replace_number(eligibility)
  eligibility <- str_replace_all(eligibility,"[0123456789]",replace_number)
  eligibility <- str_replace_all(eligibility,"[.,//:(){}]"," ")
  eligibility <- str_replace_all(eligibility,"[ ]{2,}"," ")
  eligibility <- trimws(eligibility)
  eligibility <- str_replace_all(eligibility,c(">" = "greater_than","<" = "less_than","=" = "equal_to"))
  eligibility <- str_replace_all(eligibility,"[\u2265]","greater_than_equal_to")
  eligibility <- str_replace_all(eligibility,"[\u2264]","less_than_equal_to")
  eligibility
  
  label <- '__label__0'
  
  for(c in seq(1,length(eligibility))) {
    if (strcmp(eligibility[c],'inclusion criteria')) {
      label <- '__label__0'
      next
    }
    if (strcmp(eligibility[c],'exclusion criteria')) {
      label <- '__label__1'
      next
    }
    new.criteria <- paste(label,eligibility[c],sep = '\t')
    criterias <- c(criterias,new.criteria)
  }
  
  entries <- expand.grid(condition = conditions,intervention = interventions,criteria = criterias, stringsAsFactors = F)
  out <- strsplit(as.character(entries$criteria),'\t')
  
  entries <- data.frame(entries,do.call(rbind,out))
  entries <- entries[,-3]
  colnames(entries)[c(3,4)]=c("label","criteria")
  entries <- entries[,c(3,2,1,4)]
  
  complete.data <- rbind(complete.data,entries)

}

print(not_read)

Sys.time()

'''try <- paste(complete.data$label,
             paste(paste("study interventions are",complete.data$intervention,sep = " "),
                   paste(complete.data$condition,complete.data$criteria,sep = " and "),sep = " . "),sep = "\t")
try <- data.frame(try)

write.csv(try,"A://C-DAC//Project//Data PreProcess//sample//sample_data.csv",col.names = F,row.names = F)

Sys.time()'''