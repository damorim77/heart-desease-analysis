PATH <- "~/Documents/Projects/Heart Disease/"
setwd(PATH)

##-----------------------------------------------------------------
## Leitura e configuração do dataset
##-----------------------------------------------------------------

heart <- read.csv("heart.csv")

heart.scale  <- with (heart, data.frame(age,trestbps,chol, thalach,oldpeak))
heart.factor <- with (heart, data.frame(sex,cp,fbs,restecg,exang,slope,ca,thal,target))
heart.factor <- data.frame(lapply( heart.factor , function(x) { return(factor(x))  } ))
  
##-----------------------------------------------------------------
## Análise univariada
##-----------------------------------------------------------------

mapply(hist,heart.scale,main=colnames(heart.scale),xlab="x") 

mapply(plot,heart.factor,main=colnames(heart.factor))

mapply(boxplot,heart.scale,horizontal=TRUE, main = colnames(heart.scale))

lapply( colnames(heart.scale) , function(col){
  qqnorm(sort( heart.scale[[col]] ), main=col); 
  qqline(sort( heart.scale[[col]] ))
} )

lapply(heart.factor, function(x){
  t <- sort(table( x ), decreasing = TRUE)  
  p <- prop.table(t)
  
  df <- data.frame(t,p)
  rownames(df) <- df[,1]

  df <- df[ , c(2,4)  ]
  colnames(df) <- c("count", "perc")

  df$cumsum <- cumsum(df$perc)
  
  return( df)
})

lapply( colnames(heart.scale), function(var) {
  stats <- c( mean(heart[[var]]), median(heart[[var]]), sd(heart[[var]]), min(heart[[var]]), max(heart[[var]]) )
  names <- c( "Mean", "Median", "SD", "Min", "Max")

  df <- data.frame(stats)
  rownames(df) <- names
  colnames(df) <- var
  return(df)
})


##-----------------------------------------------------------------
## Análise bivariada
##-----------------------------------------------------------------

if (! require("corrplot") ){ install.packages("corrplot") }; library("corrplot")
M <- cor(heart.scale)
corrplot(M, method = "number")

## Associação entre variáveis categóricas e target

if(! require("rcompanion")) { install.packages("rcompanion") }; library("rcompanion")

lapply( heart.factor[,1:8] , function(x) {  
  cramerV(table(x, heart.factor$target ))  
})

## Associação entre variáveis contínuas e o target

if (! require("ltm") ){ install.packages("ltm") }; library("ltm")

lapply( heart.scale, function(x) {
  biserial.cor(x, heart.factor$target  )
})

## Plots separados por target/não-target

lapply( colnames(heart.factor) , function(x) {
  plot( heart.factor[[x]] ~ heart.factor$target, main=x)
} )

lapply( colnames(heart.scale) , function(x) {
  plot( heart.scale[[x]] ~ heart.factor$target, main=x, horizontal = TRUE)
} )

##-----------------------------------------------------------------
## Testes estatísticos
##-----------------------------------------------------------------

## Normalidade

if (! require("nortest") ){ install.packages("nortest") }; library("nortest")

lapply( heart.scale, function(x){
  return(lillie.test(x))
})

## Diferenças de média entre os grupos target e não-target nas variáveis escalares
## (Quais variáveis melhor distinguem os grupos?)

lapply(heart.scale, function(x){
  t.test( subset(x, heart$target == 1  ),  subset(x, heart$target == 0  ) )
})

## Associação entre variáveis categóricas e target

lapply( heart.factor[,1:8] , function(x) {  

  tb <- table( x , heart.factor$target )
  df <- data.frame(tb)
  count <- NROW( df [ df$Freq < 5 , ]  )
  
  # Se não possui células com pelo menos 5 observações
  if( count > 0)
  {
    test <- fisher.test( table( x , heart.factor$target ))  
  }
  else
  {
    test <- chisq.test( table( x , heart.factor$target ))     
  }
  
  return(test)
})

#if (! require("psych") ){ install.packages("psych") }; library("psych")

## Teste de adequação da amostra para uma análise fatorial /PCA

### Kaiser-Meyer-Olkin (precisa ter overall KMO > 0.5)
#M <- cor(heart.scale)
#KMO(M)

### Teste de esfericidade de Bartlett (precisa ter p-value < 0.05 )
#cortest.bartlett(M)


## Principal components analysis
#pcfit <- prcomp(heart.scale, center = TRUE, retx = TRUE, scale = TRUE)
#summary(pcfit)
#screeplot(pcfit, type="lines")





