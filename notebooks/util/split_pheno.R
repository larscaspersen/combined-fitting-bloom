split_pheno <- function(pheno, 
                        spec, 
                        target_col,
                        n_obs_min = 20,
                        share_cal = 0.75,
                        r = 1,
                        seed = 123456789){
  
  adamedor_sum <- pheno %>% 
    group_by(species, cultivar) %>% 
    summarise(n = n(),
              locations = length(unique(location)),
              countries = length(unique(country)))
  
  #subset sweet cherry data
  
  species_cult <- adamedor_sum %>% 
    filter(species == spec, 
           n >= n_obs_min) %>% 
    dplyr::pull(cultivar)
  
  pheno$target_col <- pheno[,target_col]
  
  species_sub <- pheno %>% 
    filter(species == spec,
           cultivar %in% species_cult) %>% 
    drop_na(target_col) %>% 
    mutate(begin_flowering_f50 = lubridate::mdy(target_col)) %>% 
    mutate(doy_begin = lubridate::yday(begin_flowering_f50))
  
  pheno_cal_list <- pheno_val_list <- list()
  cultivars <- unique(species_sub$cultivar)
  
  set.seed(seed)
  
  for(cult in cultivars){
    
    pheno_cal_list[[cult]] <- pheno_val_list[[cult]] <- c()
    
    #check which and how many locations
    overview_df <- species_sub %>% 
      dplyr::filter(cultivar == cult) %>% 
      group_by(location) %>% 
      summarise(n = n()) %>% 
      mutate(n_cal = floor(n * share_cal),
             n_val = ceiling(n * (1 - share_cal)))
    
    for(i in 1:r){
      pheno_cal_list[[cult]][[i]] <- data.frame()
      pheno_val_list[[cult]][[i]] <- data.frame()
      
      #for each location decide how much we take for training and calibration
      for(loc in overview_df$location){
        #extract years with observations
        pheno_years <- species_sub %>% 
          dplyr::filter(cultivar == cult, location == loc) %>% 
          reframe(pheno_years = unique(year)) %>% 
          dplyr::pull(pheno_years)
        
        
        #decide which years belong to calibration and validation
        cal_years <- sort(sample(x = pheno_years, 
                                 size = overview_df$n_cal[overview_df$location == loc], 
                                 replace = FALSE))
        
        val_years <- pheno_years[!pheno_years %in% cal_years]
        
        
        #extract corresponding phenology data
        pheno_cal <- species_sub %>% 
          dplyr::filter(location == loc, cultivar == cult, year %in% cal_years) %>% 
          dplyr::pull(doy_begin)
        
        pheno_val <- species_sub %>% 
          dplyr::filter(location == loc, cultivar == cult, year %in% val_years) %>% 
          dplyr::pull(doy_begin)
        
        
        if(length(pheno_cal) != 0){
          #add phenology information to list
          pheno_cal_list[[cult]][[i]] <- rbind(pheno_cal_list[[cult]][[i]],
                                               data.frame(location = loc,
                                                          year = cal_years,
                                                          pheno = pheno_cal))
        }
        if(length(pheno_val) != 0){
          pheno_val_list[[cult]][[i]] <- rbind(pheno_val_list[[cult]][[i]],
                                               data.frame(location = loc,
                                                          year = val_years,
                                                          pheno = pheno_val))
        }
        

      }
      pheno_cal_list[[cult]] <- bind_rows(pheno_cal_list[[cult]], .id = 'r')
      pheno_val_list[[cult]] <- bind_rows(pheno_val_list[[cult]], .id = 'r')
    }
  }
  
  pheno_cal_list <- bind_rows(pheno_cal_list, .id = 'cultivar')
  pheno_val_list <- bind_rows(pheno_val_list, .id = 'cultivar')
  
  pheno_cal_list$split <- 'calibration'
  pheno_val_list$split <- 'validation'
  
  return(rbind(pheno_cal_list,
               pheno_val_list))
  
}