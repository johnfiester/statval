#' Validate a Partition
#' 
#' Function creates a train and test partition with specified split percentage
#' and then validates the partition across all variables.
#' 
#' Train and Test data frames are output via a list--along with p-values--and can 
#' be unpacked using their respective unpack functions.
#' @param df The data frame to be partitioned
#' @param targetVar The target variable of data frame entered as df$target
#' @param split Specified percent of data for training set
#' @param pval Specified p-value for statistical tests
#' @return List of training and testing partitions along with p-values
#' @export
statval <- function(df, targetVar, split, pval){
  
  p.vals <- rep(NA, length(df))
  seed.val <- 1
  
  repeat{
    
    set.seed(seed.val)
    
    index <- caret::createDataPartition(y = targetVar, p = split, list = F)
    
    train <- df[index,]; test <- df[-index,]
    train$which <- "train"; test$which <- "test"
    data.all <- rbind(train, test)
    
    for(i in 1:length(df)){
      
      if(is.factor(df[,i]) == TRUE){
        
        tab_a <- table(train[,i])
        tab_b <- table(test[,i])
        MAT <- rbind(tab_a, tab_b)
        p.vals[i] <- chisq.test(MAT, correct = FALSE)$p.value
        
      }
      else{
        
        p.vals[i] <- kruskal.test(df[,i] ~ which, data = data.all)$p.value
        
      }
    }
    
    if(sum(p.vals < pval) == 0) {
      break
    }
    
    seed.val <- seed.val +1
  }
  
  return(list(train, test, p.vals))
  
}


#' Unpack the Training Data Set
#' 
#' Unpacks the training set as data frame resulting from the statval function
#' 
#' @param statVal.saved User defined name given to the return of statval
#' @return Data frame of the validated training set
#' @export
unpack.train <- function(statVal.saved){
  
  as.data.frame(statVal.saved[1])
  
}

#' Unpack the Testing Data Set
#' 
#' Unpacks the testing set as data frame resulting from the statval function
#' 
#' @param statVal.saved User defined name given to the return of statval
#' @return Data frame of the validated testing set
#' @export
unpack.test <- function(statVal.saved){
  
  as.data.frame(statVal.saved[2])
  
}
