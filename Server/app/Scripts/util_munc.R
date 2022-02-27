#----------------------retrieveInfAge(input$selected_munc)-----------------------
retrieveImage = function(selected_munc, plotType){

  filename <- normalizePath(file.path('./images', plotType,
                                      paste(selected_munc, '.jpg', sep='')))

  
  # Return a list containing the filename
  list(src = filename)
}

#------------------------R-Value------------------------------------------------
RValueCalculation = function(munc){
  #pulling the outbreak and infection data
  outbreak_frame = (munc %>% filter(type == "outbreak"))
  infection_frame = (munc %>% filter(type == "infection"))
  
  #create new frame which has every infected.id and the time when the infected got infected
  null_vector = vector(,(nrow(outbreak_frame) + (nrow(infection_frame))))
  infected_frame = data.frame("infected.id" = null_vector, "infection_time" = NA, "caused_infections" = c(0))
  for(i in 1:(nrow(outbreak_frame)+nrow(infection_frame))){
    if(i<=nrow(outbreak_frame)){
      infected_frame$infected.id[i] = outbreak_frame$infected.id[i]
      infected_frame$infection_time[i] = substr(outbreak_frame$simulation_time[i],1,10)
    }
    else{
      infected_frame$infected.id[i] = infection_frame$infected.id[i-nrow(outbreak_frame)]
      infected_frame$infection_time[i] = substr(infection_frame$simulation_time[i-nrow(outbreak_frame)],1,10)
    }
  }
  
  
  #sort the data frame based on the simulation_time
  infected_frame =  infected_frame[order(infected_frame$infection_time),]
  
  #rvalue per day
  r_frame = data.frame("date" = unique(infected_frame$infection_time), "rvalue" =c(0), "number_of_new_infections_this_day" = c(0))
  
  #'calcuate how many people an individual has caused during the viewed infection
  #'viewing the outbreak_frame is not necessary, since they were not infected by anyone
  for(i in 1:nrow(infected_frame)){
    infector = infected_frame$infected.id[i]
    for(j in 1:nrow(infection_frame)){
      #viewed is used to skip not relevant infections which were caused previous to the infectors infection
      viewed = j
      if(i >nrow(outbreak_frame)){
        viewed = j+i-nrow(outbreak_frame)
        if(viewed > nrow(infection_frame)){
          break
        }
      }
      #go to next infector if the infected.id is equal to the infector -> infector has been infected again = new infection
      if(infector == infection_frame$infected.id[viewed]){
        break
      }
      #see whom the infector has infected and calcualte total sum
      if(infector == infection_frame$infecting.id[viewed]){
        infected_frame$caused_infections[i] = infected_frame$caused_infections[i]+1
      }
    }
    
    #add infections to rvalue column of r_frame
    for(z in 1:nrow(r_frame)){
      if(r_frame$date[z] == infected_frame$infection_time[i]){
        r_frame$rvalue[z] = r_frame$rvalue[z] + infected_frame$caused_infections[i]
        r_frame$number_of_new_infections_this_day[z]= r_frame$number_of_new_infections_this_day[z]+1
        break
      }
    }
  }
  
  #calculate rvalue per day
  for(i in 1:nrow(r_frame)){
    r_frame$rvalue[i] = r_frame$rvalue[i]/r_frame$number_of_new_infections_this_day[i]
  }
  
  
  
  return(r_frame)
  
}

#-----------------------------Cases Over Time for municipality------------------------------
timeCases = function(i){
    selected_munc = link_data$Name[i]
    print(selected_munc)
    
    #search for link for municipality information in link_population_data
    link = (link_data %>% filter(Name == selected_munc))$Link
    
    #get municipality data from Sciebo
    munc = (loadMunc(link))$median_dataset
    
    
    #reduce the simulation_time to only hold info about the day of infection
    munc$simulation_time = substr(munc$simulation_time , 1, 10)
    
    #######################Population Info########################################
    #search for link for municipality information in link_population_data
    link_pop = (link_population_data %>% filter(Name == selected_munc))$Link
    population = LoadPopulationInfo(link_pop)
    
    
    
    ####################R-Value###################################################
    rvalues = RValueCalculation(munc)
    rvalues$date <- as.Date(rvalues$date)
    
    munc$simulation_time <- as.Date(munc$simulation_time)
    
    
    #infections per time
    p1 = ggplot(data = munc, aes(simulation_time)) +
      geom_histogram(
        fill = 'red',
        col = 'black',
        alpha = 0.5,
        stat="count",
      ) #+ 
      #scale_x_discrete(guide = guide_axis(angle = 75)) 
    
    #infections per place type
    p2 = ggplot(data = munc, aes(place_type)) +
      geom_histogram(
        fill = 'red',
        col = 'black',
        alpha = 0.5,
        stat="count",
      ) +
      scale_x_discrete(guide = guide_axis(angle = 90))
    
    
    #infections per age
    p3 = ggplot(data = population, aes(age)) +
      geom_histogram(data = population, aes(age), fill = 'red', alpha = 0.5, col = 'black') +
      geom_histogram(data = munc, aes(infected.attributes.Age), fill = 'red', col = 'black', alpha = 0.7) +
      labs(x = 'Age') +
      theme(legend.position="right") 
    
    #rvalue plot
    p4 = ggplot(rvalues, aes(x=date, y =rvalue)) +
      geom_line() +
      xlab('Date')
    
    
    data = getInfected(munc) %>% 
      filter(Age>18 & Age<65)  #filter out seniors and Students
    
    
    p5 = plot_ly(x = data$Employment_Status, type = "histogram") %>% 
      layout(title = "Infections by Employment Status",
             xaxis = list (title = "Employment Status"),
             yaxis = list (title = "Occurances"),
             bargap = 0.1
      )
    
    #housesizes
    households = data.frame(ID = NA, Size = NA)
    
    smaller_overview = full_overview[match(unique(full_overview$name), full_overview$name),]
    munc = smaller_overview[smaller_overview$name == selected_munc, ]
    AGS = munc$AGS
    
    #getting the household info only for the viewed municipality through AGS-Code
    household_info = household_info_list[household_info_list$AGS == AGS, ]
    
    #create histogram
    p6 = ggplot(data = household_info, aes(size)) +
      geom_histogram(data = household_info, aes(size), fill = 'red', alpha = 0.5, col = 'black', bins = 6) +
      labs(x = 'size') +
      theme(legend.position="right")
    
    
    annotations = list(
      list( 
        x = 0.2,  
        y = 1.0,  
        text = "Daily Infections",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ), 
      list( 
        x = 0.8,  
        y = 1.0,  
        text = "Infections per Place Type",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ), 
      list( 
        x = 0.2,  
        y = 0.63,  
        text = "Infections by Age",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ), 
      list( 
        x = 0.8,  
        y = 0.63,  
        text = "Daily R-Value",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ), 
      list( 
        x = 0.2,  
        y = 0.30,  
        text = "Infections by Employment Status",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ), 
      list( 
        x = 0.8,  
        y = 0.30,  
        text = "Number of Household Sizes",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      )
    )
  
    
    
    fig = subplot(p1, p2, p3, p4, p5, p6, nrows = 3, margin = 0.05) %>%
      layout(plot_bgcolor='#e5ecf6', 
             xaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'), 
             yaxis = list( 
               zerolinecolor = '#ffff', 
               zerolinewidth = 2, 
               gridcolor = 'ffff'),
             autosize = F,
             width = 1500, height = 1000,
             annotations = annotations,
             title = ""
      )
    
    
    
  
    
    return(fig)
    
    #saveWidget(fig, paste(paste('data/htmlGraphics/Munc-', selected_munc, sep = ""), '.html', sep = ""), selfcontained = F, libdir = "lib")
    
    
  
  
}

#---------------------------muncMap---------------------------------------------
muncMap = function(munc){
 
  p1 = createDensityMap(munc)
  p2 = createMap(munc)
    
  
  annotations = list(
    list( 
      x = 0.2,  
      y = 1.0,  
      text = "Infections Density",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ), 
    list( 
      x = 0.8,  
      y = 1.0,  
      text = "Infections on Map",  
      xref = "paper",  
      yref = "paper",  
      xanchor = "center",  
      yanchor = "bottom",  
      showarrow = FALSE 
    ))
  
  fig = subplot(p1, p2, nrows = 1, margin = 0.05) %>%
    layout(plot_bgcolor='#e5ecf6', 
           xaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'), 
           yaxis = list( 
             zerolinecolor = '#ffff', 
             zerolinewidth = 2, 
             gridcolor = 'ffff'),
           autosize = F,
           width = 1500, height = 1000,
           annotations = annotations,
           title = ""
    )
  
  
  
  
  
  return(fig) 
}




#------------------------------LoadMunc-----------------------------------------
#Function for loading and cleaning up data from a given Sciebo Link
#Returns a list with 
#[[1]] = simulation result (all 50 Simulations)
#[[2]] = median Dataset
loadMunc = function(link){
  res = read.csv(paste(link, "/download", sep =""))
  
  res$place_type = str_replace(res$place_type,"fast_food","leisure")
  res$place_type = str_replace(res$place_type,"bank", "workplace")
  res = res%>% 
    group_by(Batch_ID) %>% 
    mutate(run_infections = n()) %>% 
    ungroup()
  
  number_of_runs = length(unique(res$Batch_ID))
  
  #Calculate median for median data set. Due to number of runs the highest
  #possible odd number of Runs is sampled randomly for median calculation
  
  #check if number of runs is odd
  if (number_of_runs %% 2 == 0){number_of_runs = number_of_runs -1}
  
  median = median(
    sample_n(
      unique(
        data.frame(Batch_ID = res$Batch_ID,total_inf=res$run_infections)
      )
      ,number_of_runs)$total_inf
  )  
  
  median = filter(res, run_infections == median)
  #check if there are more than one run in this dataset, if so use the first
  if (length(unique(median$Batch_ID)) > 1){
    median = filter(median, Batch_ID == median$Batch_ID[1])
  }
  
  res = list (full_data = res, median_dataset = median)
  
}

#------------------------------LoadPopulationInfo-----------------------------------------
#Function for loading and cleaning up data from a given Sciebo Link
#Returns a list with 
#[[0]] = household_id
#[[1]] = municipality_id 
#[[2]] = age
#[[3]] = x_coord
#[[4]] = y_coord
LoadPopulationInfo = function(link){
  
  res = read.csv2(paste(link, "/download", sep =""))
  res$HLA = NULL
  res$HSC = NULL
  res$HSI = NULL
  #res$SEX = NULL
  res$AG2 = NULL
  #res$EMP = NULL
  
  
  #Lat and Lon must be the same for each household individual from the same household
  before = 0
  for(i in 1: nrow(res)){
    if(res$household_id[i] == before){
      res$X_coord[i] = res$X_coord[i-1]
      res$Y_coord[i] = res$Y_coord[i-1]
    }
    before = res$household_id[i]
  }
  
  return(res)
}



#-----------------------------Creating an saving plots for Municipalities--------------------
#not used during shiny dashboard just manually once
plotAgeDistribution = function(selected_munc){
  #search for link for municipality information in link_population_data
  Link = (link_population_data %>% filter(Name == selected_munc))$Link
  
  #get municipality data from Sciebo
  popInfo = LoadPopulationInfo(Link)
  

  
  
  # 1. Open jpeg file
  jpeg(paste(paste("images/AgeDistPlot/", selected_munc, sep = ''), '.jpg', sep = ''), width = 350, height = 350)
  # 2. Create the plot
  plot = ggplot(data = popInfo, aes(age)) +
    geom_histogram(
      fill = 'red',
      col = 'black',
      alpha = 0.5
    )
  
  print(plot)
  # 3. Close the file
  dev.off()
  
  
}


saveAgeDistPlot = function(){
  for(i in 1:nrow(link_population_data)){
    plotAgeDistribution(link_population_data$Name[i])
  }
}

########################################
plotInfPerAge = function(selected_munc){
  #search for link for municipality information in link_population_data
  Link = (link_population_data %>% filter(Name == selected_munc))$Link
  
  #search for link for municipality information in link_data (for simulation results)
  Link2 = (link_data %>% filter(Name == selected_munc))$Link
  
  #median data of simulation runs for municipality
  median_data = (loadMunc(Link2))$median_dataset
  
  #population
  population = LoadPopulationInfo(Link)
  
  # 1. Open jpeg file
  jpeg(paste(paste("images/InfPerAge/", selected_munc, sep = ''), '.jpg', sep = ''), width = 350, height = 350)
  # 2. Create the plot
  #create histogram
  plot = ggplot(data = population, aes(age)) +
    geom_histogram(data = population, aes(age), fill = 'red', alpha = 0.5, col = 'black') +
    geom_histogram(data = median_data, aes(infected.attributes.Age), fill = 'red', col = 'black', alpha = 0.7) +
    labs(x = 'Age') +
    theme(legend.position="right")
  
  print(plot)
  # 3. Close the file
  dev.off()
  
}

saveInfPerAgePlot = function(){
  for(i in 1:nrow(link_data)){
    plotInfPerAge(link_data$Name[i])
  }
}


#########################################
#percentages per age
plotPercPerAge = function(selected_munc){
  frame = data.frame(age = NA, variable = NA, value = NA) #variable is either count or percentage
  
  #search for litnk for municipality information in link_population_data
  Link = (link_population_data %>% filter(Name == selected_munc))$Link
  
  #search for link for municipality information in link_data (for simulation results)
  Link2 = (link_data %>% filter(Name == selected_munc))$Link
  
  #median data of simulation runs for municipality
  median_data = (loadMunc(Link2))$median_dataset
  
  #population
  population = LoadPopulationInfo(Link)
  
  for(i in 1:max(population$age)){
    count = length(which(population$age == i))
    perc = length(which(median_data$infected.attributes.Age == i)) / count
    if(i == 1) {
      frame$age = i
      frame$variable = 'percentage'
      frame$value = perc
    }
    else{
      frame[nrow(frame) +1, ] = c(i, 'percentage', perc) 
    }
  }
  
  # 1. Open jpeg file
  jpeg(paste(paste("images/InfPercPerAge/", selected_munc, sep = ''), '.jpg', sep = ''), width = 350, height = 350)
  # 2. Create the plot
  #create histogram
  plot = plot(frame$age, frame$value, type = "b", pch = 19,
              col = 'red', xlab = 'Age', ylab = 'Percentage of Infections')

  
  print(plot)
  # 3. Close the file
  dev.off()
  
  
}


saveInfPercPerAgePlot = function(){
  for(i in 1:nrow(link_data)){
    plotPercPerAge(link_data$Name[i])
  }
}



#---------------------------getTimeCases-----------------------------------------
getTimeCases = function(munc){
  #file = 'C:/Users/charl/OneDrive/Desktop/ShinyDashboardCovidSimulation/data/htmlGraphics/Munc-'
  #file = paste(file, munc, sep = "")
  #file = paste(file, '.html', sep = "")
  
  #print(file)
  variable = sym(paste("Z_", munc, sep=""))
  
  eval(variable)
  #return(fig)#includeHTML(file))
}


#---------------------------getMuncMaps-----------------------------------------
getMuncMaps = function(munc){
  #file = 'C:/Users/charl/OneDrive/Desktop/ShinyDashboardCovidSimulation/data/htmlGraphics/Munc-'
  #file = paste(file, munc, sep = "")
  #file = paste(file, '.html', sep = "")
  
  #print(file)
  variable = sym(paste("Z_Maps_", munc, sep=""))
  
  eval(variable)
  #return(fig)#includeHTML(file))
}

#---------------------------getInfected-----------------------------------------
#function for geting a list of infected persons and their attributes ()
getInfected = function(data){

  data = filter(data, type == "infection") #remove outbreak cases
  

  infected = data.frame(
    ID = data$infected.id,
    Age = data$infected.attributes.Age,
    Employment_Status = data$infected.attributes.Employment.Status
  )
  
  infecting = data.frame(
    ID = data$infecting.id,
    Age = data$infecting.attributes.Age,
    Employment_Status = data$infecting.attributes.Employment.Status
  )
  
  return(unique(rbind(infected, infecting)))
  
}











