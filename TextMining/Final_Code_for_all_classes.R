library("e1071")
library("tm")
library("plyr")
library("Rcpp")
library("RcppEigen")
library("RTextTools")
library("stringr")
setwd("/home/rahul/Documents/BIS/Assignment3/20_newsgroup")

alt.atheism <- Corpus( DirSource("alt.atheism"))
alt.atheism.train <- alt.atheism[1:300]
#alt.atheism.train[[1]]

comp.graphics <- Corpus( DirSource("comp.graphics"))
comp.graphics.train <- comp.graphics[1:300]
#comp.graphics.train[[1]]

comp.os.ms.windows.misc <- Corpus( DirSource("comp.os.ms-windows.misc"))
comp.os.ms.windows.misc.train <- comp.os.ms.windows.misc[1:300]
#comp.os.ms.windows.misc.train[[1]]

comp.sys.ibm.pc.hardware <- Corpus( DirSource("comp.sys.ibm.pc.hardware"))
comp.sys.ibm.pc.hardware.train <- comp.sys.ibm.pc.hardware[1:300]
#comp.sys.ibm.pc.hardware.train[[1]]

comp.sys.mac.hardware <- Corpus( DirSource("comp.sys.mac.hardware"))
comp.sys.mac.hardware.train <- comp.sys.mac.hardware[1:300]
#comp.sys.mac.hardware.train[[1]]

comp.windows.x <- Corpus( DirSource("comp.windows.x"))
comp.windows.x.train <- comp.windows.x[1:300]
#comp.windows.x.train[[1]]

misc.forsale <- Corpus( DirSource("misc.forsale"))
misc.forsale.train <- misc.forsale[1:300]
#misc.forsale.train[[1]]

rec.autos <- Corpus( DirSource("rec.autos"))
rec.autos.train <- rec.autos[1:300]
#rec.autos.train[[1]]

rec.motorcycles <- Corpus( DirSource("rec.motorcycles"))
rec.motorcycles.train <- rec.motorcycles[1:300]
#rec.motorcycles.train[[1]]

rec.sport.baseball <- Corpus( DirSource("rec.sport.baseball"))
rec.sport.baseball.train <- rec.sport.baseball[1:300]
#rec.sport.baseball.train[[1]]

sci.electronics <- Corpus( DirSource ("sci.electronics"))
sci.electronics.train<-sci.electronics[1:300]
#sci.electronics.train[[1]]

rec.sport.hockey<- Corpus( DirSource ("rec.sport.hockey"))
rec.sport.hockey.train<-rec.sport.hockey[1:300]
#rec.sport.hockey.train[[1]]

sci.crypt<- Corpus( DirSource ("sci.crypt"))
sci.crypt.train<-sci.crypt[1:300]
#sci.crypt.train[[1]]

sci.med<- Corpus( DirSource ("sci.med"))
sci.med.train<-sci.med[1:300]
#sci.med.train[[1]]

sci.space<- Corpus( DirSource ("sci.space"))
sci.space.train<-sci.space[1:300]
#sci.space.train[[1]]

soc.religion.christian<- Corpus( DirSource ("soc.religion.christian"))
soc.religion.christian.train<-soc.religion.christian[1:300]
#soc.religion.christian.train[[1]]

talk.politics.guns<- Corpus( DirSource ("talk.politics.guns"))
talk.politics.guns.train<-talk.politics.guns[1:300]
#talk.politics.guns.train[[1]]

talk.politics.mideast<- Corpus( DirSource ("talk.politics.mideast"))
talk.politics.mideast.train<-talk.politics.mideast[1:300]
#talk.politics.mideast.train[[1]]

talk.politics.misc<- Corpus( DirSource ("talk.politics.misc"))
talk.politics.misc.train<-talk.politics.misc[1:300]
#talk.politics.misc.train[[1]]

talk.religion.misc<- Corpus( DirSource ("talk.religion.misc"))
talk.religion.misc.train<-talk.religion.misc[1:300]
#talk.religion.misc.train[[1]]

train_corpus <- c(alt.atheism.train,comp.graphics.train,comp.os.ms.windows.misc.train,comp.sys.ibm.pc.hardware.train,
                  comp.sys.mac.hardware.train,comp.windows.x.train,misc.forsale.train, rec.autos.train,
                  rec.motorcycles.train,rec.sport.baseball.train,sci.electronics.train,rec.sport.hockey.train,
                  sci.crypt.train,sci.med.train,sci.space.train, soc.religion.christian.train,talk.politics.guns.train,
                  talk.politics.mideast.train,talk.politics.misc.train,talk.religion.misc.train)

train_corpus_pre<-tm_map(train_corpus,  PlainTextDocument)
train_corpus_pre<-tm_map(train_corpus_pre, content_transformer(tolower))
train_corpus_pre<-tm_map(train_corpus_pre, removeNumbers)
train_corpus_pre<-tm_map(train_corpus_pre, removePunctuation)
train_corpus_pre<-tm_map(train_corpus_pre,  removeWords,stopwords('english'))
train_corpus_pre<-tm_map(train_corpus_pre, stripWhitespace);
train_corpus_pre[[1]]
term.document.train<- DocumentTermMatrix(train_corpus_pre,control=list(minWordLength=3, minDocFreq=5))
term.document.train<-removeSparseTerms(term.document.train,0.998)
dtm.train <- as.data.frame(inspect(term.document.train))
dim(dtm.train)

class <- c(rep("alt.atheism",300), rep("comp.graphics",300), rep("comp.os.ms.windows.misc",300), 
           rep("comp.sys.ibm.pc.hardware",300), rep("comp.sys.mac.hardware",300), rep("comp.windows.x",300), 
           rep("misc.forsale",300), rep("rec.autos",300), rep("rec.motorcycles",300), 
           rep("rec.sport.baseball",300), rep("sci.electronics",300), rep("rec.sport.hockey",300), 
           rep("sci.crypt",300), rep("sci.med",300), rep("sci.space",300), rep("soc.religion.christian",300), 
           rep("talk.politics.guns",300), rep("talk.politics.mideast",300), rep("talk.politics.misc",300), 
           rep("talk.religion.misc",300))

naive_classifier = naiveBayes(dtm.train, as.factor(class))

#preparing test data set
alt.atheism.test <- alt.atheism[301:400]
comp.graphics.test <- comp.graphics[301:400]
comp.os.ms.windows.misc.test <- comp.os.ms.windows.misc[301:400]
comp.sys.ibm.pc.hardware.test <- comp.sys.ibm.pc.hardware[301:400]
comp.sys.mac.hardware.test <- comp.sys.mac.hardware[301:400]
comp.windows.x.test <- comp.windows.x[301:400]
misc.forsale.test <- misc.forsale[301:400]
rec.autos.test <- rec.autos[301:400]
rec.motorcycles.test <- rec.motorcycles[301:400]
rec.sport.baseball.test <- rec.sport.baseball[301:400]
sci.electronics.test <- sci.electronics[301:400]
rec.sport.hockey.test <- rec.sport.hockey[301:400]
sci.crypt.test <- sci.crypt[301:400]
sci.med.test <- sci.med[301:400]
sci.space.test <- sci.space[301:400]
soc.religion.christian.test <- soc.religion.christian[301:400]
talk.politics.guns.test <- talk.politics.guns[301:400]
talk.politics.mideast.test <- talk.politics.mideast[301:400]
talk.politics.misc.test <- talk.politics.misc[301:400]
talk.religion.misc.test <- talk.religion.misc[301:400]


test_data <- c(alt.atheism.test,comp.graphics.test,comp.os.ms.windows.misc.test,comp.sys.ibm.pc.hardware.test,
               comp.sys.mac.hardware.test,comp.windows.x.test, misc.forsale.test,rec.autos.test,
               rec.motorcycles.test,rec.sport.baseball.test,sci.electronics.test,rec.sport.hockey.test,
               sci.crypt.test,sci.med.test,sci.space.test,soc.religion.christian.test,talk.politics.guns.test,
               talk.politics.mideast.test, talk.politics.misc.test,talk.religion.misc.test)

test_pre<-tm_map(test_data,  PlainTextDocument)
test_pre<-tm_map(test_pre, content_transformer(tolower))
test_pre<-tm_map(test_pre, removeNumbers)
test_pre<-tm_map(test_pre, removePunctuation)
test_pre<-tm_map(test_pre,  removeWords,stopwords('english'))
test_pre<-tm_map(test_pre, stripWhitespace);
test_corpus <- DocumentTermMatrix(test_pre,control=list(minWordLength=2, minDocFreq=5))
test_corpus <-removeSparseTerms(test_corpus,0.998)
test <- as.data.frame(inspect(test_corpus))
dim(test)
#alt.atheis.test-data.frame <-as.data.frame(alt.atheism.test)
#alt.atheis.test.data.frame <-data.frame(text=unlist(sapply(alt.atheism.test, '[')), stringsAsFactors=F)
#alt.atheis.test.data.frame
predictions <- predict(naive_classifier, test, type="class")
dim(predictions)
class_test <- c(rep("alt.atheism",100), rep("comp.graphics",100), rep("comp.os.ms.windows.misc",100), 
                rep("comp.sys.ibm.pc.hardware",100), rep("comp.sys.mac.hardware",100), rep("comp.windows.x",100), 
                rep("misc.forsale",100), rep("rec.autos",100), rep("rec.motorcycles",100), 
                rep("rec.sport.baseball",100), rep("sci.electronics",100), rep("rec.sport.hockey",100), 
                rep("sci.crypt",100), rep("sci.med",100), rep("sci.space",100), rep("soc.religion.christian",100), 
                rep("talk.politics.guns",100), rep("talk.politics.mideast",100), rep("talk.politics.misc",100), 
                rep("talk.religion.misc",100))

table(class_test,predictions)

#dtm.sci.rel <- cbind( dtm.sci.rel, class)
#ncol(dtm.sci.rel)
#comp.graphics.train <- comp.graphics[1:300]