
#get rid of columns: batch_id, name & AGS of full_overview
reduceOverview = function(){
  
  #get rid of columns: batch_id, name & AGS
  data <- full_overview[, 6:(ncol(full_overview)-1)]
  data$AvrgWithChild = as.double(data$AvrgWithChild)
  data$AvrgWithSenior = as.double(data$AvrgWithSenior)
  data$OSGov = NULL
  return(data)
}

#function to create correlation matrix of full_overview
corMatrix = function(){
  
  data = reduceOverview()
  
  result <- cor(data)
  
  #round to the fourth decimal place
  result <- round(result, 4)
  
  return(result)
}