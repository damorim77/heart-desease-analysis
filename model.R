PATH <- "~/Documents/Projects/Heart Disease/"
setwd(PATH)

##-----------------------------------------------------------------
## Leitura e configuração do dataset
##-----------------------------------------------------------------

heart <- read.csv("heart.csv")

heart.scale  <- with (heart, data.frame(age,trestbps,chol, thalach,oldpeak))
heart.factor <- with (heart, data.frame(sex,cp,fbs,restecg,exang,slope,ca,thal,target))
heart.factor <- data.frame(lapply( heart.factor , function(x) { return(factor(x))  } ))

# Criação de dummies

if(! require("dummies")) { install.packages("dummies") }; library("dummies")

heart.new <- heart.scale
heart.new$target <- heart.factor$target

for( c in  colnames( heart.factor) )
{
  if( c != "target" )
    heart.new <-  cbind( heart.new, dummies::dummy( data = heart.factor, x = c  )  )
}

##-----------------------------------------------------------------
## Separação de base de treino e teste
##-----------------------------------------------------------------

set.seed(65421)

s <- sample(NROW(heart.new))

proportion <- .6666
upper_bound <- s[ 0: as.integer(length(s)*proportion) ]
lower_bound <- s[ (as.integer(length(s)*proportion)+1) : NROW(heart.new)  ]

train <- data.frame( heart.new[ upper_bound , ] )
test  <- data.frame( heart.new[ lower_bound , ] )

NROW(train)
NROW(test)

##-----------------------------------------------------------------
## Gerador de modelos
##-----------------------------------------------------------------

source("model_generator.R")

get.model <- function(train, test, fn, level, file = "")
{
  models <- generate.models(train,test,level,fn)
  
  if( file != "")
    write.table(models, paste(PATH,file), sep=";", row.names = FALSE)
  
  return(models)
}


logit_model <- function(f){ return( glm( f , family = binomial(link="logit"), data = train) ) }

get.model(train, test, logit_model, 4, "logit.txt")




if(! require("e1071")) { install.packages("e1071") }; library("e1071")

bayes_model <- function(f){ return( naiveBayes( f, data = train )  ) }

get.model(train, test, bayes_model, 4, "bayes.txt")


svm_model <- function(f){ return(  svm( f, data = train ) ) }

get.model(train, test, svm_model, 4, "svm.txt")




if(! require("party")) { install.packages("party") }; library("party")

tree_model <- function(f){ return( ctree( f, data = train ) )}

get.model(train, test, tree_model, 4, "tree.txt")



if(! require("randomForest")) { install.packages("randomForest") }; library("randomForest")

forest_model <- function(f){ return( randomForest( f, data = train ) ) }

get.model(train,test, forest_model, 4, "forest.txt")



if(! require("MASS")) { install.packages("MASS") }; library("MASS")

lda_model <- function(f){ return( lda( f , data = train, CV = FALSE ) ) }

get.model(train, test, lda_model, 4, "lda.txt")


if(! require("nnet")) { install.packages("nnet") }; library("nnet")

nnet_model <- function(f){ return(  nnet( f , data = train, size= 10 )  ) }

get.model( train, test, nnet_model, 4, 'nnet.txt'  )



##-----------------------------------------------------------------
## Teste de modelos selecionados - performance na reamostragem
##-----------------------------------------------------------------

set.seed(65421)

source("model_generator.R")

results <- NULL

if(! require("e1071")) { install.packages("e1071") }; library("e1071")
model <- function (tr)
{
  return(naiveBayes(target ~ cp2+slope2+ca0+thal2, data=tr))
}

results <- rbind(results, test.resample.model( model , heart.new, f="naiveBayes: target ~ cp2+slope2+ca0+thal2" ))

#write.table(results, paste(PATH,"naiveBayes - target-cp2+slope2+ca0+thal2 .txt"), sep=";", row.names = FALSE)


####

if(! require("e1071")) { install.packages("e1071") }; library("e1071")
model <- function (tr)
{
  return(naiveBayes(target ~ sex0+cp0+slope2+ca0, data=tr))
}

results <- rbind(results, test.resample.model( model , heart.new, f="naiveBayes: target ~ sex0+cp0+slope2+ca0"  ))

#write.table(results, paste(PATH,"naiveBayes - target-sex0+cp0+slope2+ca0 .txt"), sep=";", row.names = FALSE)


####

if(! require("nnet")) { install.packages("nnet") }; library("nnet")
model <- function (tr)
{
  return( nnet(target ~ cp2+ca0+thal1+thal2, size=10, data=tr))
}

results <- rbind(results, test.resample.model( model , heart.new, f="neuralNet: target ~ cp2+ca0+thal1+thal2"  ))

#write.table(results, paste(PATH,"nnet - target-cp2+ca0+thal1+thal2 .txt"), sep=";", row.names = FALSE)


####

if(! require("nnet")) { install.packages("nnet") }; library("nnet")
model <- function (tr)
{
  return( nnet(target ~ cp0+ca1+thal2, size=10, data=tr))
}

results <- rbind(results, test.resample.model( model , heart.new, f="neuralNet: target ~ cp0+ca1+thal2"  ))

#write.table(results, paste(PATH,"nnet - target-cp0+ca1+thal2 .txt"), sep=";", row.names = FALSE)


####

if(! require("randomForest")) { install.packages("randomForest") }; library("randomForest")
model <- function (tr)
{
  return( randomForest(target ~ cp2+ca0+thal1+thal2, data=tr))
}

results <- rbind(results, test.resample.model( model , heart.new , f="randomForest: target ~ cp2+ca0+thal1+thal2" ))

#write.table(results, paste(PATH,"forest - target-cp2+ca0+thal1+thal2 .txt"), sep=";", row.names = FALSE)


####

if(! require("nnet")) { install.packages("nnet") }; library("nnet")
model <- function (tr)
{
  return( nnet(target ~ cp0+ca1+thal2, size=10, data=tr))
}

results <- rbind(results, test.resample.model( model , heart.new, f="neuralNet: target ~ cp0+ca1+thal2"  ))

#write.table(results, paste(PATH,"nnet - age+cp0+ca0+thal2 .txt"), sep=";", row.names = FALSE)

write.table(results, paste(PATH,"resample_results.txt"), sep=";", row.names = FALSE)

##-----------------------------------------------------------------
## Tuning to modelo selecionado - naiveBayes(target ~ cp2+slope2+ca0+thal2)
##-----------------------------------------------------------------

set.seed(65421)
source("model_generator.R")

s <- sample(NROW(heart.new))

proportion <- .6666
upper_bound <- s[ 0: as.integer(length(s)*proportion) ]
lower_bound <- s[ (as.integer(length(s)*proportion)+1) : NROW(heart.new)  ]

train <- data.frame( heart.new[ upper_bound , ] )
test  <- data.frame( heart.new[ lower_bound , ] )

if(! require("e1071")) { install.packages("e1071") }; library("e1071")
model <- function (f)
{
  return(naiveBayes(f, data=train))
}


models <- generate.models(train,test,1,model,f="target ~ cp2+slope2+ca0+thal2 + ca2 + cp3 + restecg2 +")


##-----------------------------------------------------------------
## Teste do modelo final - target ~ cp2+slope2+ca0+thal2+cp3
##-----------------------------------------------------------------

results <- NULL

if(! require("e1071")) { install.packages("e1071") }; library("e1071")
model <- function (tr)
{
  return(naiveBayes(target ~ cp2+slope2+ca0+thal2+cp3, data=tr))
}

results <- rbind(results, test.resample.model( model , heart.new, f="naiveBayes: target ~ cp2+slope2+ca0+thal2+cp3" ))
write.table(results, paste(PATH,"resample_final_model.txt"), sep=";", row.names = FALSE)
