plotInfectionsMunBox = function(){
  plot_ly(type = "box", x = full_overview$name, y = full_overview$totalInfections)
}

plotInfectionsRegBezBox = function(){
  plot_ly(type = "box", x = full_overview$RegBez, y = full_overview$totalInfections)
}

plotCorMatrix = function(){
  matrix = corMatrix()
  plot = corrplot(matrix, type= 'upper', order = 'AOE',
           tl.col = 'black', tl.srt = 90)
  
  plot
}


plotCorTable = function(valueA, valueB){
  
  names = colnames(full_overview)
  
  a = match(valueA, names)
  b = match(valueB, names)
  
  cor = cor.test(full_overview[, a], full_overview[, b])
  
  return(cor)
}



plotCorLinReg = function(valueA, valueB){
  
  names = colnames(full_overview)
  
  a = match(valueA, names)
  b = match(valueB, names)
  
  frame = data.frame(FirstValue = full_overview[, a], SecondValue = full_overview[, b])
  
  plot = ggplot(data = frame, aes(x = FirstValue, y = SecondValue)) +
    geom_point() +
    geom_smooth(method = "lm")
  
  plot
}








