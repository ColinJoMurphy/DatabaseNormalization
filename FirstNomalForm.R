library(data.table)
library(stringi)
library(tidyr)



normalize.1nf <- function(dataframe, ordering = FALSE, order_var_name = NULL){
  
  # Check input data structure
  stopifnot('data argument must be of class data.frame' = 'data.frame' %in% class(dataframe))
  dataframe<- as.data.frame(dataframe)
  
  # Convert to data.table
  setDT(dataframe)
  
  # Check variable names are unique and fix any issues
  if (length(unique(names(dataframe))) == length(names(dataframe))){
    message('Column names are unique')
  } else {
    dups <- names(dataframe)[duplicated(names(dataframe))]
      name <- data.table('dups' = dups)
      name[, coln := which(names(dataframe) %in% dups)[-1], by = dups]
      name[, n := 1:.N, by = dups][,new := paste0(dups, '.', as.character(n))]
      setnames(dataframe, name$coln, name$new)
      message('Duplicate column names found and renamed, column names now unique')
    }
  
  # Add variable for ordering value
  if (ordering){
    if (is.null(order_var_name)){
      dataframe[, rank := 1:nrow(.SD)]
      message('Ordering values set as \'rank\' column')
    } else {
      dataframe[, eval(order_var_name) := 1:nrow(.SD)]
      message(paste0('Ordering values set as \'', order_var_name, '\' column'))
    }
  } else {message('Row ordering does not exist')}
  
  # Check to make sure all columns contain single entity entries
  if ('list' %in% dataframe[, lapply(.SD, class)]){
    ls <- which(dataframe[, lapply(.SD, class)] == 'list')
      if (length(ls) == 1){
        unnested <- unnest_longer(dataframe, all_of(ls))
        message('Columns with nonsingular entries have been unnested')
        return(unnested)
      } else {
        unnested <- dataframe
        for (n in 1:length(ls)){
          unnested <- unnest_longer(unnested, all_of(n))
        }
        message('Columns with nonsingular entries have been unnested')
        return(unnested)
      }
  } else {message('All columns have single value entries')}
}
