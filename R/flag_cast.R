
flag_cast<- function(combined_data){
  bongo <- combined_data %>%  as_tibble() %>%  dplyr::group_by(consecutive) %>%  dplyr::mutate(direction=if_else(`V. Câble`>0, "descent", "ascent"),
                                                                                               flag_odd = if_else(
                                                                                                 lead(direction,2)==direction | lag(direction,2) ==direction, "no",
                                                                                                 if_else(lead(direction,2)!=direction & lag(direction,2)!= direction, "yes", NA)),
                                                                                               direction=if_else(flag_odd=="yes", lead(direction), direction),
                                                                                               direction01=as.numeric(as.factor(direction)),
                                                                                               directiondetect=c(NA,diff(direction01))) %>%  dplyr::ungroup()
  
  allcast <- tibble()
  allbongs <- tibble()
  depth_mean <-  tibble()
  for(i in unique(bongo$consecutive)){
    
    bongoc <-  bongo %>%  filter(consecutive == i, !is.na(Profondeur)) 
    
    cablecheck<- bongoc  %>% dplyr::group_by(consecutive, station) %>%  dplyr::summarize(cablecheck=mean(abs(`V. Câble`))) %>%  
      mutate(cablecheck=replace_na(cablecheck, 0))
      if(cablecheck$cablecheck>0){
      
      flag<- c(0,which(bongoc$directiondetect!=0)) 
      flagdf<- cbind(start=flag,end=lead(flag, default=nrow(bongoc)), cast=1:length(flag)) %>%  as_tibble() %>%  
        mutate(count= (end-(start)),
               type=c("descent",bongoc[which(bongoc$directiondetect!=0),"direction"]$direction)) 
      
      
      library(splitstackshape) #for expandRows
      bongoc$cast<- expandRows(flagdf, "count")$cast
      
      bongodepth <-  bongoc %>%  dplyr::group_by(consecutive,station, cast) %>%  dplyr::summarize(max(Profondeur)) %>% 
        dplyr::group_by(consecutive, station) %>%  dplyr::summarize(meandepth=mean(`max(Profondeur)`),
                                                                    maxdepth=max(`max(Profondeur)`))
      
      
      ggplot(bongoc, aes(x=Chrono, y=Profondeur,color=as.factor(cast))) +geom_point()
      
      # Define a function to process each subset of data
      
      
      # Apply the function to each unique cast and combine results
      output <- ddply(bongoc, .(cast), function(subset) process_cast(subset)) %>%
        bind_rows() %>%  mutate(consecutive=i)
  } else{ 
      outNA<-  bind_cols(NA,NA, i, NA,NA, NA,NA); colnames(outNA) = c("cast","direction","consecutive", "station","depth","Volume2" ,"Flow") 
      output<-  outNA
      
      outdNA<-  bind_cols(i,NA,NA); colnames(outdNA) =c("consecutive","station","meandepth")
      bongodepth<-  outdNA
      
      
    }
    
    depth_mean= bind_rows(depth_mean, bongodepth)
    
    allbongs<- bind_rows(allbongs, output)
    allcast <- bind_rows(allcast, bongoc)
    
    }

  return(list(allbongs, depth_mean, allcast))
}

