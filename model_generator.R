
generate.models <- function(train, test, levels, fn, f=NULL){
  
  col.names <- colnames(df)  
  col.length <- length(col.names)
  
  cols <- colnames(train)
  cols <- cols[ ! cols %in% "target"]
  n <- length(cols)
  
  f <- ifelse( is.null(f) == TRUE, "target ~ ", f  )
  
  outputs <- NULL
  
  if(! require("caret")) { install.packages("caret") }; library("caret")
  
  for(l in 1:levels)
  {
    
    id <- combn(1:n, l, simplify= FALSE)
    
    formulas <- sapply(id,function(i)
      paste(f,paste(cols[i],collapse="+"))
    )

    for(f in formulas)
    {
      print(f)
      
      fit <- fn(as.formula(f))
      pred <- predict(fit, newdata=test)
      
      if(is.factor(pred) == TRUE){
        p <- factor(pred)
      } else if( is.list(pred) == TRUE) {
        p <- as.factor(ifelse( pred$x <0.5 ,0 , 1  ))
      } else{
        p <- as.factor(ifelse( pred <0.5 ,0 , 1  ))
      }
      
      con <- confusionMatrix(p, test$target)
      
      row <- data.frame(
        f,
        con$table[2,2],
        con$table[1,1],
        con$table[2,1],
        con$table[1,2],
        con$overall["Accuracy"],
        con$byClass["Sensitivity"], 
        con$byClass["Specificity"] 
      )
      
      colnames(row) <- c(
        "Formula",
        "TP",
        "TN",
        "FP",
        "FN",
        "Accuracy",
        "Sensitivity",
        "Specificity"
      )
      
      row.names(row) <- NULL
      
      outputs <- rbind( outputs, row )
      
    }
    
  }
  
  return(outputs)
  
  
}




test.resample.model <- function(fn, df, size=100, proportion =.666, f="")
{
  if(! require("caret")) { install.packages("caret") }; library("caret")
  
  outputs = NULL
  
  for (i in 1:size)
  {
    s <- sample(NROW(df))
    
    upper_bound <- s[ 0: as.integer(length(s)*proportion) ]
    lower_bound <- s[ (as.integer(length(s)*proportion)+1) : NROW(df)  ]
    
    train <- data.frame( df[ upper_bound , ] )
    test  <- data.frame( df[ lower_bound , ] )
    
    fit <- fn(train)
    
    pred <- predict(fit, newdata=test)
    
    if(is.factor(pred) == TRUE){
      p <- factor(pred)
    } else if( is.list(pred) == TRUE) {
      p <- as.factor(ifelse( pred$x <0.5 ,0 , 1  ))
    } else{
      p <- as.factor(ifelse( pred <0.5 ,0 , 1  ))
    }
    
    con <- confusionMatrix(p, test$target)
    
    row <- data.frame(
      f,
      con$table[2,2],
      con$table[1,1],
      con$table[2,1],
      con$table[1,2],
      con$overall["Accuracy"],
      con$byClass["Sensitivity"], 
      con$byClass["Specificity"] 
    )
    
    colnames(row) <- c(
      "Formula",
      "TP",
      "TN",
      "FP",
      "FN",
      "Accuracy",
      "Sensitivity",
      "Specificity"
    )
    
    row.names(row) <- NULL
    
    outputs <- rbind( outputs, row )
    
    
  }
  
  return( outputs )
}
