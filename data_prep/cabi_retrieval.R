#-----------------------------
#
#  Capital Bikeshare (CaBi) Ride Data Scraper Function
#
#  Purpose: Get and store data from CaBi
#
#  Created by: Nadav Rindler 
#
#  Created on: 2017-10-17
#
#-----------------------------

##----Import Libraries-----
require(RSQLite)
require(logging)
require(stringr)
require(zoo)

##----Define the get-weather-data function-----
get_cabi_data = function(yq, yq2, logger=NA, db_conn=NA){
  
  # Build HTML Link String
  site_prefix = 'https://s3.amazonaws.com/capitalbikeshare-data/'
  site_suffix = ''
  cabi_links = vector()
  cabi_csv = vector()
  for(l in 1:length(yq)){
    if(l<match("2015-Q1",yq)){
      cabi_links[l] = paste0(site_prefix, yq[l], '-cabi-trip-history-data.zip',site_suffix)
      cabi_csv[l] = paste0(yq[l],'-cabi-trip-history-data.csv')
    } else if(l==match("2015-Q3",yq)){
      cabi_links[l] = paste0(site_prefix, yq[l], '-cabi-trip-history-data.zip',site_suffix)
      cabi_csv[l] = paste0(yq[l],'-cabi-trip-history-data.csv')
    } else if(l<match("2016-Q2",yq)){
      cabi_links[l] = paste0(site_prefix, yq[l], '-cabi-trip-history-data.zip',site_suffix)
      cabi_csv[l] = paste0(yq[l],'-Trips-History-Data.csv')
    } else if(l==match("2016-Q3",yq)){
      cabi_links[l] = paste0(site_prefix, yq[l], '-cabi-trips-history-data.zip',site_suffix)
      cabi_csv[l] = paste0(yq[l],'-Trips-History-Data-1.csv')
    } else{
      cabi_links[l] = paste0(site_prefix, yq[l], '-cabi-trips-history-data.zip',site_suffix)
      cabi_csv[l] = paste0(yq[l],'-Trips-History-Data.csv')
    }
  }
  
  #Account for second CSV file for 2016-Q3
  yq = c(yq,"2016-Q3")
  yq2 = c(yq2,"201632")
  cabi_links = c(cabi_links,paste0(site_prefix, '2016-Q3', '-cabi-trips-history-data.zip',site_suffix))
  cabi_csv = c(cabi_csv,paste0('2016-Q3','-Trips-History-Data-2.csv'))
  
  # Initialize final data frame
  cabi_list = list()
  
  # Get Data
  for (l in 1:length(cabi_links)){
    temp = tempfile() #initialize temporary file
    print(paste('Getting link',cabi_links[l]))
    # Log each attempt
    if (is.function(logger)){
      loginfo(paste('Getting link',cabi_links[l]),logger=logger)
    }
    
    cabi_info = tryCatch(download.file(cabi_links[l],temp)
                            ,error = function(e){
                              
                              print(paste('Error getting',cabi_links[l])) # Output Error on Screen/Console
                              
                              if(is.function(logger)){loginfo(paste('Error getting',cabi_links[l])
                                                              ,logger=logger)} # Store Error in Log
                              
                            })
    cabi_data = read.csv(unz(temp, cabi_csv[l]), stringsAsFactors=FALSE)
    unlink(temp)
    
    # Post Retrieval Data Cleanup
    
    # Clean up field names
    names(cabi_data) <- gsub("\\.", "_", names(cabi_data))
    
    # Combine
    cabi_list[[l]] = cabi_data
    
    #save(cabi_data, file=paste0("Raw_Data/cabi_raw",yq2[l],".Rdata"))
    
  } # End loop through each quarter's CaBi data zip file link (l)
  
  # Log ending time
  if(is.function(logger)){
    loginfo('All done!',logger=logger)
  }
  
  names(cabi_list) = yq
  save(cabi_list, file="Raw_Data/cabi_raw.Rdata")
  
  #return(cabi_list)
}

if(interactive()){
  ##----Setup Test Logger-----
  basicConfig()
  addHandler(writeToFile, file="Raw_Data/cabi_retrieval_log.log", level='DEBUG')  
  
  ##----Test Parameters----
  yq = format(as.yearqtr(seq(from=as.Date('2010-10-01'),to=as.Date('2017-01-01'),by='quarter')),'%Y-Q%q')
  yq2 = format(as.yearqtr(yq, format='%Y-Q%q'), '%Y%q')
  
  ##----Run function----
  get_cabi_data(yq, yq2)
}