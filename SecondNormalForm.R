library(data.table)
library(stringi)
library(tidyr)

normalize.2nf <- function(dataframe, non_candidate_keys = NULL){
  
  # Check input data structure
  stopifnot('dataframe argument must be of class data.frame' = 'data.frame' %in% class(dataframe))
  dataframe<- as.data.frame(dataframe)
  
  # Convert to data.table
  setDT(dataframe)
  
  # Check for a single attribute key
  singlekeys <- dataframe[, 
                          lapply(.SD, function(c){length(unique(c))})
                          ][1,
                            sapply(.SD, function(c){max(c) == nrow(dataframe)})
                            ]
  singlekeys <- names(dataframe)[singlekeys]
  
  # Remove specified non keys
  singlekeys <- setdiff(singlekeys,non_candidate_keys)
 
  keycombos <- 
    
  # Find 2 composite candidate keys
  doublekeys <- list()
  for (x in 1:(ncol(dataframe) - 1)){
    comp1 <- rep(names(dataframe[, ..x]), (ncol(dataframe) - x))
    comp2 <- names(dataframe)[(x + 1):ncol(dataframe)]
    doublekeys <- c(doublekeys, list(data.table(comp1, comp2)))
  }
  doublekeys <- rbindlist(doublekeys)
  
  
  # Define helper function to generate combinations of columns
  get.keycombinations <- function(data, n, single_keys = NULL){
    p <- combn(names(data), n) %>%
            as.data.table()
    setnames(p, names(p), as.character(1:ncol(p)))
    p <- melt(p, measure.vars  = names(p))
    p[, variable := as.numeric(variable)]
    p
  }
  
  # Define helper function to test if a set of columns are a super key 
  test.superkey<- function(x, data){
     all(data[,
              .N == 1, 
              by = c(p[variable == x, value])
     ][,
       V1
     ])
   } 
   
   
  
  # Generate T/F column indicating if column set is a super key
  p[, 
    superkey := lapply(1:max(variable), test.superkey, data = data),
    by = variable]
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   # Define helper function that checks if a set of cols are a super key for 
  # a given data set
  get.superkeys <- function(i, n, keytablename, data){
    stopifnot(class(keytablename) == 'character')
    
    # Generate code for 'by' clause that indexes each column of the given row 
    # in the key combination table
    cols <- c()
    for (x in 1:n){
      cols <- c(cols, paste0(keytablename, '[eval(', i,')][[', x, ']]'))
    }                      
    
    # Iterate through columns to get their names
    colnames <- unname(sapply(cols, function(p) eval(parse(text = p))))
    
    # Test superkey criteria
    all(data[,
    .N == 1, 
    by = eval(colnames)
    ][,
      V1
      ])
  }
  
  # Use helper function to find all double key super keys        
  doublekeys[, superkey := lapply(1:nrow(.SD), get.superkeys, n = 2, keytablename = 'doublekeys', data = dataframe)]    
  doublekeys <- doublekeys[superkey == T]
  

  
  
    
    
    
    
    
    
  
  
  
  
  
}
