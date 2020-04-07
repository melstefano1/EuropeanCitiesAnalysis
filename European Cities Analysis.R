getwd()
setwd("C:/Users/Stefano/Dropbox/University/NCI/Project/Working Folder")

install.packages("jsonlite")
library(jsonlite)
library(magrittr)


#######################The following codes have been writen as the API password is not active anymore
api_Cities <- read.csv(file="Cities.csv", header = TRUE, sep=',')
api_CityPrices <- read.csv(file = "CityPrices.csv", header = TRUE, sep = ',')
api_CityCrime <- read.csv(file = "CityCrime.csv", header = TRUE, sep = ',')
api_CityHealthcare <- read.csv(file = "CityHealthcare.csv", header = TRUE, sep = ',')
api_CityPollution <- read.csv(file = "CityPollution.csv", header = TRUE, sep = ',')
api_CityTraffic <- read.csv(file = "CityTraffic.csv", header = TRUE, sep = ',')
api_Currency <- read.csv(file = "Currency.csv", header = TRUE, sep = ',')
####################################################################################################

#Upload EU contries
EU.List<-c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia",
           "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
           "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania","Slovakia", "Slovenia", 
           "Spain", "Sweden", "United Kingdom")





####################### DATA WEB SCRAPING THROUGH API
#API - Cities
          api_Cities <- as.data.frame(fromJSON("https://www.numbeo.com/api/cities?api_key=5k5h3in8zdgnv3"))
          head(df.cities)
          write.csv(api_Cities, file="Cities.csv", row.names=TRUE)
 
#Filtering the cities dataframe just by the first column (city names)
cities.list <- api_Cities[,2:3]
eu.cities <- subset(cities.list, cities.list$cities.country %in% EU.List)
class(EU.List)

#Combine cities and countries together
list<-c(paste(eu.cities$cities.city, eu.cities$cities.country, sep = ","))
list


#API City Prices
          eu.cities$cities.city <- stringr::str_replace(eu.cities$cities.city,' ','%20')
          eu.cities$cities.country <- stringr::str_replace(eu.cities$cities.country,' ','%20')
          eu.cities$apilink <- paste0("https://www.numbeo.com/api/city_prices?api_key=5k5h3in8zdgnv3&query=",
                                      eu.cities$cities.city,',',eu.cities$cities.country)
        
          
          # for loop # settings
          api_df <- eu.cities
          api_CityPrices <- data.frame()
          
          # for loop run
          for (i in 1:nrow(api_df)) { 
            tryCatch({
              paste("Working on",i, "of", nrow(api_df))
              
              #custom operations
              temp <- as.data.frame(fromJSON(api_df$apilink[i]))
              temp$request_id <- api_df$request_id[i]
              api_CitiesPrices <- rbind(api_CitiesPrices, temp)
              rm(temp)
              
              
            }, error=function(e){ 
              err = i
              i = i+1
              paste("Error on",err, "go to", i, "of", nrow(api_df))
            })
            
          }
          write.csv(api_CitiesPrices, file="City_Prices.csv", row.names=TRUE)



#API City Crime
          eu.cities$cities.city <- stringr::str_replace(eu.cities$cities.city,' ','%20')
          eu.cities$cities.country <- stringr::str_replace(eu.cities$cities.country,' ','%20')
          eu.cities$apilink <- paste0("https://www.numbeo.com/api/city_crime?api_key=5k5h3in8zdgnv3&query=",
                                      eu.cities$cities.city,',',eu.cities$cities.country)
          #eu.country<- distinct(eu.cities, cities.country,.keep_all= TRUE) #Filter for distinct countries
          eu.cities$request_id <- rownames(eu.cities)
          
          testdf <- rbind(eu.cities[535,],eu.cities[10,],eu.cities[841,])
          
          # for loop ####
          # for loop # settings
          api_df <- eu.cities # change df to relevant one
          api_CityCrime <- data.frame()
          
          # for loop run
          for (i in 1:nrow(api_df)) { 
            tryCatch({
              paste("Working on",i, "of", nrow(api_df))
              
              #custom operations
              temp <- as.data.frame(fromJSON(api_df$apilink[i]))
              temp$request_id <- api_df$request_id[i]
              api_CityCrime <- rbind(api_CityCrime, temp)
              rm(temp)
              
            }, error=function(e){ 
              err = i
              i = i+1
              paste("Error on",err, "go to", i, "of", nrow(api_df))
            })
            }          
          write.csv(api_CityCrime, file="CityCrime.csv", row.names=TRUE) #To check if it works
          
#API City Healtcare
          eu.cities$cities.city <- stringr::str_replace(eu.cities$cities.city,' ','%20')
          eu.cities$cities.country <- stringr::str_replace(eu.cities$cities.country,' ','%20')
          eu.cities$apilink <- paste0("https://www.numbeo.com/api/city_healthcare?api_key=5k5h3in8zdgnv3&query=",
                                      eu.cities$cities.city,',',eu.cities$cities.country)
          #eu.country<- distinct(eu.cities, cities.country,.keep_all= TRUE) #Filter for distinct countries
          #eu.cities$request_id <- rownames(eu.cities)
          #rm(eu.cities)
          testdf <- rbind(eu.cities[5292,], subset(eu.cities, request_id==5292))
      
          # for loop ####
          # for loop # settings
          api_df <- eu.cities # change df to relevant one
          api_CityHealtcare <- data.frame()
          Rome <-subset(eu.cities, request_id==5292)
          eu.cities <-subset(eu.cities, request_id!=5292)
          eu.cities <- rbind(Rome, eu.cities)
          temp <- as.data.frame(fromJSON("https://www.numbeo.com/api/city_healthcare?api_key=5k5h3in8zdgnv3&query=Rome"))
          api_df <- eu.cities
          
          # for loop run
          for (i in 1:nrow(api_df)) { 
            tryCatch({
              paste("Working on",i, "of", nrow(api_df))
              #i=2
              #custom operations
              temp <- as.data.frame(fromJSON(api_df$apilink[i]))
            
             
              results<-temp
              results$skill_and_competency <- ifelse(is.null(temp$skill_and_competency), NA, temp$skill_and_competency)
              results$cost <- ifelse(is.null(temp$cost), NA, temp$cost)
              results$responsiveness_waitings <- ifelse(is.null(temp$responsiveness_waitings), NA, temp$responsiveness_waitings)
              results$index_healthcare <- ifelse(is.null(temp$index_healthcare), NA, temp$index_healthcare)
              results$speed <- ifelse(is.null(temp$speed), NA, temp$speed)
              results$accuracy_and_completeness <- ifelse(is.null(temp$accuracy_and_completeness), NA, temp$accuracy_and_completeness)
              results$friendliness_and_courtesy <- ifelse(is.null(temp$friendliness_and_courtesy), NA, temp$friendliness_and_courtesy)
              results$insurance_type.Employer.Sponsored <- ifelse(is.null(temp$insurance_type.Employer.Sponsored), NA, temp$insurance_type.Employer.Sponsored)
              results$insurance_type.Private <- ifelse(is.null(temp$insurance_type.Private), NA, temp$insurance_type.Private)
              results$insurance_type.Public <- ifelse(is.null(temp$insurance_type.Public), NA, temp$insurance_type.Public)
              results$insurance_type.None <- ifelse(is.null(temp$insurance_type.None), NA, temp$insurance_type.None)
              results$modern_equipment <- ifelse(is.null(temp$modern_equipment), NA, temp$modern_equipment)
              results$name <- ifelse(is.null(temp$name), NA, paste(as.character(temp$name)))
              results$monthLastUpdate <- ifelse(is.null(temp$monthLastUpdate), NA, temp$monthLastUpdate)
              results$location <- ifelse(is.null(temp$location), NA, temp$location)
              results$contributors <- ifelse(is.null(temp$contributors), NA, temp$contributors)
              results$yearLastUpdate <- ifelse(is.null(temp$yearLastUpdate), NA, temp$yearLastUpdate)
              results$city_id <- ifelse(is.null(temp$city_id), NA, temp$city_id)
              results$request_id <- ifelse(is.null(temp$request_id), NA, temp$request_id)
              
              
              results<-results[,c("name", "city_id", "skill_and_competency", "cost", 
                                  "responsiveness_waitings", "index_healthcare", "speed",
                                  "accuracy_and_completeness", "friendliness_and_courtesy", 
                                  "insurance_type.Employer.Sponsored",
                                  "insurance_type.Private", "insurance_type.Public", 
                                  "insurance_type.None", "modern_equipment", 
                                  "monthLastUpdate", "location", "contributors", 
                                  "yearLastUpdate", "request_id")]
              
              api_CityHealtcare <- rbind(api_CityHealtcare, results)
              rm(temp, results)
              
              
            }, error=function(e){ 
              err = i
              i = i+1
              paste("Error on",err, "go to", i, "of", nrow(api_df))
            })
            
          }          
          write.csv(api_CityHealtcare, file="CityHealtcare.csv", row.names=TRUE) #To check if it works
          
          

#API City Pollution
          eu.cities$cities.city <- stringr::str_replace(eu.cities$cities.city,' ','%20')
          eu.cities$cities.country <- stringr::str_replace(eu.cities$cities.country,' ','%20')
          eu.cities$apilink <- paste0("https://www.numbeo.com/api/city_pollution?api_key=5k5h3in8zdgnv3&query=",
                                      eu.cities$cities.city,',',eu.cities$cities.country)
          #eu.country<- distinct(eu.cities, cities.country,.keep_all= TRUE) #Filter for distinct countries
          #eu.cities$request_id <- rownames(eu.cities)
          #rm(eu.cities)
          testdf <- rbind(eu.cities[1,], subset(eu.cities, request_id==5292))
          
          # for loop ####
          # for loop # settings
          api_df <- eu.cities # change df to relevant one
          api_CityPollution <- data.frame()
          
          # for loop run
          for (i in 1:nrow(api_df)) { 
            tryCatch({
              paste("Working on",i, "of", nrow(api_df))
              
              #custom operations
              temp <- as.data.frame(fromJSON(api_df$apilink[i]))
              temp$request_id <- api_df$request_id[i]
              
              results<-temp
              results$green_and_parks_quality <- ifelse(is.null(temp$green_and_parks_quality), NA, temp$green_and_parks_quality)
              results$pm2.5 <- ifelse(is.null(temp$pm2.5), NA, temp$pm2.5)
              results$comfortable_to_spend_time <- ifelse(is.null(temp$comfortable_to_spend_time), NA, temp$comfortable_to_spend_time)
              results$pm10 <- ifelse(is.null(temp$pm10), NA, temp$pm10)
              results$air_quality <- ifelse(is.null(temp$air_quality), NA, temp$air_quality)
              results$garbage_disposal_satisfaction <- ifelse(is.null(temp$garbage_disposal_satisfaction), NA, temp$garbage_disposal_satisfaction)
              results$index_pollution <- ifelse(is.null(temp$index_pollution), NA, temp$index_pollution)
              results$drinking_water_quality_accessibility <- ifelse(is.null(temp$drinking_water_quality_accessibility), NA, temp$drinking_water_quality_accessibility)
              results$name <- ifelse(is.null(temp$name), NA, paste(as.character(temp$name)))
              results$monthLastUpdate <- ifelse(is.null(temp$monthLastUpdate), NA, temp$monthLastUpdate)
              results$clean_and_tidy <- ifelse(is.null(temp$clean_and_tidy), NA, temp$clean_and_tidy)
              results$noise_and_light_pollution <- ifelse(is.null(temp$noise_and_light_pollution), NA, temp$noise_and_light_pollution)
              results$contributors <- ifelse(is.null(temp$contributors), NA, temp$contributors)
              results$yearLastUpdate <- ifelse(is.null(temp$yearLastUpdate), NA, temp$yearLastUpdate)
              results$water_pollution <- ifelse(is.null(temp$water_pollution), NA, temp$water_pollution)
              results$contributors <- ifelse(is.null(temp$contributors), NA, temp$contributors)
              
              results<-results[,c("name","city_id","request_id",
                                  "green_and_parks_quality","pm2.5","comfortable_to_spend_time",
                                  "pm10","air_quality","garbage_disposal_satisfaction",
                                  "index_pollution","drinking_water_quality_accessibility","monthLastUpdate",
                                  "clean_and_tidy","noise_and_light_pollution","contributors",
                                  "yearLastUpdate","water_pollution")]
              
              api_CityPollution <- rbind(api_CityPollution, results)
              rm(temp, results)
              
              
            }, error=function(e){ 
              err = i
              i = i+1
              paste("Error on",err, "go to", i, "of", nrow(api_df))
            })
            
          }          
          write.csv(api_CityPollution, file="CityPollution.csv", row.names=TRUE) #To check if it works        
          
          
  

#API City Traffic (Traffic data have not been used as there were to many missing data)
          eu.cities$cities.city <- stringr::str_replace(eu.cities$cities.city,' ','%20')
          eu.cities$cities.country <- stringr::str_replace(eu.cities$cities.country,' ','%20')
          eu.cities$apilink <- paste0("https://www.numbeo.com/api/city_traffic?api_key=5k5h3in8zdgnv3&query=",
                                      eu.cities$cities.city,',',eu.cities$cities.country)
          #eu.country<- distinct(eu.cities, cities.country,.keep_all= TRUE) #Filter for distinct countries
          #eu.cities$request_id <- rownames(eu.cities)
          #rm(eu.cities)
          testdf <- rbind(eu.cities[1,], subset(eu.cities, request_id==5292))
          
          # for loop ####
          # for loop # settings
          api_df <- eu.cities # change df to relevant one
          api_CityTraffic <- data.frame()
          
          # for loop run
          for (i in 1:nrow(api_df)) { 
            tryCatch({
              paste("Working on",i, "of", nrow(api_df))
      
              #custom operations
              temp <- as.data.frame(fromJSON(api_df$apilink[i]))
              temp$request_id <- api_df$request_id[i]
              
              results<-temp
              results$analyze.using.Tram.Streetcar.time_waiting  <- ifelse(is.null(temp$analyze.using.Tram.Streetcar.time_waiting ), NA, temp$analyze.using.Tram.Streetcar.time_waiting)
              results$analyze.using.Tram.Streetcar.time_driving <- ifelse(is.null(temp$analyze.using.Tram.Streetcar.time_driving), NA, temp$analyze.using.Tram.Streetcar.time_driving)
              results$analyze.using.Tram.Streetcar.time_tram  <- ifelse(is.null(temp$analyze.using.Tram.Streetcar.time_tram ), NA, temp$analyze.using.Tram.Streetcar.time_tram )
              results$analyze.using.Tram.Streetcar.time_other <- ifelse(is.null(temp$analyze.using.Tram.Streetcar.time_other), NA, temp$analyze.using.Tram.Streetcar.time_other)
              results$analyze.using.Tram.Streetcar.distance  <- ifelse(is.null(temp$analyze.using.Tram.Streetcar.distance ), NA, temp$analyze.using.Tram.Streetcar.distance )
              results$analyze.using.Tram.Streetcar.time_bike <- ifelse(is.null(temp$analyze.using.Tram.Streetcar.time_bike), NA, temp$analyze.using.Tram.Streetcar.time_bike)
              results$analyze.using.Tram.Streetcar.time_train <- ifelse(is.null(temp$analyze.using.Tram.Streetcar.time_train), NA, temp$analyze.using.Tram.Streetcar.time_train)
              results$analyze.using.Tram.Streetcar.time_walking <- ifelse(is.null(temp$analyze.using.Tram.Streetcar.time_walking), NA, temp$analyze.using.Tram.Streetcar.time_walking)
              results$analyze.using.Tram.Streetcar.time_motorbike <- ifelse(is.null(temp$analyze.using.Tram.Streetcar.time_motorbike), NA, paste(as.character(temp$analyze.using.Tram.Streetcar.time_motorbike)))
              results$analyze.using.Tram.Streetcar.count <- ifelse(is.null(temp$analyze.using.Tram.Streetcar.count), NA, temp$analyze.using.Tram.Streetcar.count)
              results$analyze.using.Tram.Streetcar.time_bus <- ifelse(is.null(temp$analyze.using.Tram.Streetcar.time_bus), NA, temp$analyze.using.Tram.Streetcar.time_bus)
              results$analyze.using.Train.Metro.time_waiting <- ifelse(is.null(temp$analyze.using.Train.Metro.time_waiting), NA, temp$analyze.using.Train.Metro.time_waiting)
              results$analyze.using.Train.Metro.time_driving  <- ifelse(is.null(temp$analyze.using.Train.Metro.time_driving ), NA, temp$analyze.using.Train.Metro.time_driving )
              results$analyze.using.Train.Metro.time_tram <- ifelse(is.null(temp$analyze.using.Train.Metro.time_tram), NA, temp$analyze.using.Train.Metro.time_tram)
              results$analyze.using.Train.Metro.time_other  <- ifelse(is.null(temp$analyze.using.Train.Metro.time_other ), NA, temp$analyze.using.Train.Metro.time_other )
              results$analyze.using.Train.Metro.distance <- ifelse(is.null(temp$analyze.using.Train.Metro.distance), NA, temp$analyze.using.Train.Metro.distance)
              results$analyze.using.Train.Metro.time_bike  <- ifelse(is.null(temp$analyze.using.Train.Metro.time_bike ), NA, temp$analyze.using.Train.Metro.time_bike)
              results$analyze.using.Train.Metro.time_train  <- ifelse(is.null(temp$analyze.using.Train.Metro.time_train ), NA, temp$analyze.using.Train.Metro.time_train )
              results$analyze.using.Train.Metro.time_motorbike  <- ifelse(is.null(temp$analyze.using.Train.Metro.time_motorbike ), NA, temp$analyze.using.Train.Metro.time_motorbike )
              results$analyze.using.Train.Metro.time_walking <- ifelse(is.null(temp$analyze.using.Train.Metro.time_walking), NA, temp$analyze.using.Train.Metro.time_walking)
              results$analyze.using.Train.Metro.count  <- ifelse(is.null(temp$analyze.using.Train.Metro.count ), NA, temp$analyze.using.Train.Metro.count )
              results$analyze.using.Train.Metro.time_bus <- ifelse(is.null(temp$analyze.using.Train.Metro.time_bus), NA, temp$analyze.using.Train.Metro.time_bus)
              results$overall_average_analyze.time_waiting  <- ifelse(is.null(temp$overall_average_analyze.time_waiting ), NA, temp$overall_average_analyze.time_waiting )
              results$overall_average_analyze.time_driving  <- ifelse(is.null(temp$overall_average_analyze.time_driving ), NA, temp$overall_average_analyze.time_driving )
              results$overall_average_analyze.time_tram <- ifelse(is.null(temp$overall_average_analyze.time_tram), NA, paste(as.character(temp$overall_average_analyze.time_tram)))
              results$overall_average_analyze.time_other  <- ifelse(is.null(temp$overall_average_analyze.time_other ), NA, temp$overall_average_analyze.time_other )
              results$overall_average_analyze.distance  <- ifelse(is.null(temp$overall_average_analyze.distance ), NA, temp$overall_average_analyze.distance )
              results$overall_average_analyze.time_bike <- ifelse(is.null(temp$overall_average_analyze.time_bike), NA, temp$overall_average_analyze.time_bike)
              results$overall_average_analyze.time_train   <- ifelse(is.null(temp$overall_average_analyze.time_train  ), NA, temp$overall_average_analyze.time_train  )
              results$overall_average_analyze.time_motorbike <- ifelse(is.null(temp$overall_average_analyze.time_motorbike), NA, temp$overall_average_analyze.time_motorbike)
              results$overall_average_analyze.time_walking   <- ifelse(is.null(temp$overall_average_analyze.time_walking  ), NA, temp$overall_average_analyze.time_walking  )
              results$overall_average_analyze.count <- ifelse(is.null(temp$overall_average_analyze.count), NA, temp$overall_average_analyze.count)
              results$overall_average_analyze.time_bus  <- ifelse(is.null(temp$overall_average_analyze.time_bus ), NA, temp$overall_average_analyze.time_bus)
              results$analyze.using.Bus.Trolleybus.time_waiting <- ifelse(is.null(temp$analyze.using.Bus.Trolleybus.time_waiting), NA, temp$analyze.using.Bus.Trolleybus.time_waiting)
              results$analyze.using.Bus.Trolleybus.time_driving  <- ifelse(is.null(temp$analyze.using.Bus.Trolleybus.time_driving ), NA, temp$analyze.using.Bus.Trolleybus.time_driving )
              results$analyze.using.Bus.Trolleybus.time_tram <- ifelse(is.null(temp$analyze.using.Bus.Trolleybus.time_tram), NA, temp$analyze.using.Bus.Trolleybus.time_tram)
              results$analyze.using.Bus.Trolleybus.time_other  <- ifelse(is.null(temp$analyze.using.Bus.Trolleybus.time_other ), NA, temp$analyze.using.Bus.Trolleybus.time_other )
              results$analyze.using.Bus.Trolleybus.distance <- ifelse(is.null(temp$analyze.using.Bus.Trolleybus.distance), NA, temp$analyze.using.Bus.Trolleybus.distance)
              results$analyze.using.Bus.Trolleybus.time_bike <- ifelse(is.null(temp$analyze.using.Bus.Trolleybus.time_bike), NA, temp$analyze.using.Bus.Trolleybus.time_bike)
              results$analyze.using.Bus.Trolleybus.time_train <- ifelse(is.null(temp$analyze.using.Bus.Trolleybus.time_train), NA, temp$analyze.using.Bus.Trolleybus.time_train)
              results$analyze.using.Bus.Trolleybus.time_motorbike <- ifelse(is.null(temp$analyze.using.Bus.Trolleybus.time_motorbike), NA, paste(as.character(temp$analyze.using.Bus.Trolleybus.time_motorbike)))
              results$analyze.using.Bus.Trolleybus.time_walking <- ifelse(is.null(temp$analyze.using.Bus.Trolleybus.time_walking), NA, temp$analyze.using.Bus.Trolleybus.time_walking)
              results$analyze.using.Bus.Trolleybus.count <- ifelse(is.null(temp$analyze.using.Bus.Trolleybus.count), NA, temp$analyze.using.Bus.Trolleybus.count)
              results$analyze.using.Bus.Trolleybus.time_bus <- ifelse(is.null(temp$analyze.using.Bus.Trolleybus.time_bus), NA, temp$analyze.using.Bus.Trolleybus.time_bus)
              results$index_co2_emission  <- ifelse(is.null(temp$index_co2_emission ), NA, temp$index_co2_emission )
              results$analyze.using.Car.time_waiting <- ifelse(is.null(temp$analyze.using.Car.time_waiting), NA, temp$analyze.using.Car.time_waiting)
              results$analyze.using.Car.time_driving  <- ifelse(is.null(temp$analyze.using.Car.time_driving ), NA, temp$analyze.using.Car.time_driving )
              results$analyze.using.Car.time_tram <- ifelse(is.null(temp$analyze.using.Car.time_tram), NA, temp$analyze.using.Car.time_tram)
              results$analyze.using.Car.time_other  <- ifelse(is.null(temp$analyze.using.Car.time_other ), NA, temp$analyze.using.Car.time_other)
              results$analyze.using.Car.distance  <- ifelse(is.null(temp$analyze.using.Car.distance ), NA, temp$analyze.using.Car.distance )
              results$analyze.using.Car.time_bike  <- ifelse(is.null(temp$analyze.using.Car.time_bike ), NA, temp$analyze.using.Car.time_bike )
              results$analyze.using.Car.time_train <- ifelse(is.null(temp$analyze.using.Car.time_train), NA, temp$analyze.using.Car.time_train)
              results$analyze.using.Car.time_motorbike  <- ifelse(is.null(temp$analyze.using.Car.time_motorbike ), NA, temp$analyze.using.Car.time_motorbike )
              results$analyze.using.Car.time_walking   <- ifelse(is.null(temp$analyze.using.Car.time_walking  ), NA, temp$analyze.using.Car.time_walking  )
              results$analyze.using.Car.count <- ifelse(is.null(temp$analyze.using.Car.count), NA, temp$analyze.using.Car.count)
              results$analyze.using.Car.time_bus   <- ifelse(is.null(temp$analyze.using.Car.time_bus  ), NA, temp$analyze.using.Car.time_bus  )
              results$primary_means_percentage_map.Walking <- ifelse(is.null(temp$primary_means_percentage_map.Walking), NA, temp$primary_means_percentage_map.Walking)
              results$primary_means_percentage_map.Tram.Streetcar  <- ifelse(is.null(temp$primary_means_percentage_map.Tram.Streetcar ), NA, temp$primary_means_percentage_map.Tram.Streetcar)
              results$primary_means_percentage_map.Train.Metro <- ifelse(is.null(temp$primary_means_percentage_map.Train.Metro), NA, temp$primary_means_percentage_map.Train.Metro)
              results$primary_means_percentage_map.Car  <- ifelse(is.null(temp$primary_means_percentage_map.Car ), NA, temp$primary_means_percentage_map.Car )
              results$primary_means_percentage_map.Bus.Trolleybus <- ifelse(is.null(temp$primary_means_percentage_map.Bus.Trolleybus), NA, temp$primary_means_percentage_map.Bus.Trolleybus)
              results$primary_means_percentage_map.Working.from.Home  <- ifelse(is.null(temp$primary_means_percentage_map.Working.from.Home ), NA, temp$primary_means_percentage_map.Working.from.Home )
              results$primary_means_percentage_map.Bike <- ifelse(is.null(temp$primary_means_percentage_map.Bike), NA, temp$primary_means_percentage_map.Bike)
              results$primary_means_percentage_map.Motorbike <- ifelse(is.null(temp$primary_means_percentage_map.Motorbike), NA, temp$primary_means_percentage_map.Motorbike)
              results$index_time_exp <- ifelse(is.null(temp$index_time_exp), NA, temp$index_time_exp)
              results$index_time <- ifelse(is.null(temp$index_time), NA, paste(as.character(temp$index_time)))
              results$index_traffic <- ifelse(is.null(temp$index_traffic), NA, temp$index_traffic)
              results$name <- ifelse(is.null(temp$name ), NA, paste(as.character(temp$name)))
              results$index_inefficiency <- ifelse(is.null(temp$index_inefficiency), NA, temp$index_inefficiency)
              results$analyze.using.Walking.time_waiting  <- ifelse(is.null(temp$analyze.using.Walking.time_waiting ), NA, temp$analyze.using.Walking.time_waiting )
              results$analyze.using.Walking.time_driving <- ifelse(is.null(temp$analyze.using.Walking.time_driving), NA, temp$analyze.using.Walking.time_driving)
              results$analyze.using.Walking.time_tram  <- ifelse(is.null(temp$analyze.using.Walking.time_tram ), NA, temp$analyze.using.Walking.time_tram )
              results$analyze.using.Walking.time_other <- ifelse(is.null(temp$analyze.using.Walking.time_other), NA, temp$analyze.using.Walking.time_other)
              results$analyze.using.Walking.distance  <- ifelse(is.null(temp$analyze.using.Walking.distance ), NA, temp$analyze.using.Walking.distance)
              results$analyze.using.Walking.time_bike  <- ifelse(is.null(temp$analyze.using.Walking.time_bike ), NA, temp$analyze.using.Walking.time_bike )
              results$analyze.using.Walking.time_train  <- ifelse(is.null(temp$analyze.using.Walking.time_train ), NA, temp$analyze.using.Walking.time_train )
              results$analyze.using.Walking.time_motorbike <- ifelse(is.null(temp$analyze.using.Walking.time_motorbike), NA, temp$analyze.using.Walking.time_motorbike)
              results$analyze.using.Walking.time_walking  <- ifelse(is.null(temp$analyze.using.Walking.time_walking ), NA, temp$analyze.using.Walking.time_walking )
              results$analyze.using.Walking.count  <- ifelse(is.null(temp$analyze.using.Walking.count ), NA, temp$analyze.using.Walking.count )
              results$analyze.using.Walking.time_bus  <- ifelse(is.null(temp$analyze.using.Walking.time_bus ), NA, temp$analyze.using.Walking.time_bus )
              results$contributors <- ifelse(is.null(temp$contributors), NA, temp$contributors)
              results$city_id  <- ifelse(is.null(temp$city_id ), NA, temp$city_id )
              
              
              results<-results[,c("name","city_id", 
                                  "analyze.using.Tram.Streetcar.time_waiting", 
                                  "analyze.using.Tram.Streetcar.time_driving", 
                                  "analyze.using.Tram.Streetcar.time_tram",
                                  "analyze.using.Tram.Streetcar.time_other", 
                                  "analyze.using.Tram.Streetcar.distance", 
                                  "analyze.using.Tram.Streetcar.time_bike", 
                                  "analyze.using.Tram.Streetcar.time_train", 
                                  "analyze.using.Tram.Streetcar.time_motorbike", 
                                  "analyze.using.Tram.Streetcar.time_walking", 
                                  "analyze.using.Tram.Streetcar.count", 
                                  "analyze.using.Tram.Streetcar.time_bus",
                                  "analyze.using.Train.Metro.time_waiting",
                                  "analyze.using.Train.Metro.time_driving",
                                  "analyze.using.Train.Metro.time_tram",
                                  "analyze.using.Train.Metro.time_other", 
                                  "analyze.using.Train.Metro.distance", 
                                  "analyze.using.Train.Metro.time_bike",
                                  "analyze.using.Train.Metro.time_train", 
                                  "analyze.using.Train.Metro.time_motorbike", 
                                  "analyze.using.Train.Metro.time_walking", 
                                  "analyze.using.Train.Metro.count", 
                                  "analyze.using.Train.Metro.time_bus", 
                                  "overall_average_analyze.time_waiting", 
                                  "overall_average_analyze.time_driving", 
                                  "overall_average_analyze.time_tram",
                                  "overall_average_analyze.time_other",
                                  "overall_average_analyze.distance",
                                  "overall_average_analyze.time_bike", 
                                  "overall_average_analyze.time_train", 
                                  "overall_average_analyze.time_motorbike",
                                  "overall_average_analyze.time_walking", 
                                  "overall_average_analyze.count", 
                                  "overall_average_analyze.time_bus", 
                                  "analyze.using.Bus.Trolleybus.time_driving", 
                                  "analyze.using.Bus.Trolleybus.time_tram", 
                                  "analyze.using.Bus.Trolleybus.time_other", 
                                  "analyze.using.Bus.Trolleybus.distance", 
                                  "analyze.using.Bus.Trolleybus.time_train",
                                  "analyze.using.Bus.Trolleybus.time_motorbike",
                                  "analyze.using.Bus.Trolleybus.time_walking",
                                  "analyze.using.Bus.Trolleybus.count",
                                  "analyze.using.Bus.Trolleybus.time_bus", 
                                  "index_co2_emission", 
                                  "analyze.using.Car.time_waiting",
                                  "analyze.using.Car.time_driving",
                                  "analyze.using.Car.time_tram",
                                  "analyze.using.Car.time_other", 
                                  "analyze.using.Car.distance", 
                                  "analyze.using.Car.time_bike",
                                  "analyze.using.Car.time_train", 
                                  "analyze.using.Car.time_motorbike", 
                                  "analyze.using.Car.time_walking",
                                  "analyze.using.Car.count", 
                                  "analyze.using.Car.time_bus", 
                                  "primary_means_percentage_map.Walking",
                                  "primary_means_percentage_map.Tram.Streetcar",
                                  "primary_means_percentage_map.Train.Metro",
                                  "primary_means_percentage_map.Car", 
                                  "primary_means_percentage_map.Bus.Trolleybus", 
                                  "primary_means_percentage_map.Working.from.Home",
                                  "primary_means_percentage_map.Bike", 
                                  "primary_means_percentage_map.Motorbike", 
                                  "index_time_exp",
                                  "index_time", 
                                  "index_traffic", 
                                  "index_inefficiency",
                                  "analyze.using.Walking.time_waiting",
                                  "analyze.using.Walking.time_driving",
                                  "analyze.using.Walking.time_tram", 
                                  "analyze.using.Walking.time_other", 
                                  "analyze.using.Walking.distance",
                                  "analyze.using.Walking.time_bike", 
                                  "analyze.using.Walking.time_train", 
                                  "analyze.using.Walking.time_motorbike",
                                  "analyze.using.Walking.time_walking", 
                                  "analyze.using.Walking.count", 
                                  "analyze.using.Walking.time_bus",
                                  "contributors")]
              
              api_CityTraffic <- rbind(api_CityTraffic, results)
              rm(temp, results)
              
              
            }, error=function(e){ 
              err = i
              i = i+1
              paste("Error on",err, "go to", i, "of", nrow(api_df))
            })
            
          }          
          write.csv(api_CityTraffic, file="CityTraffic.csv", row.names=TRUE) #To check if it works    
          
          
          
           
          
          
          
          
#Cut data to be exported into MySQL
          install.packages("tidyverse")
          library(dplyr)
          api_CitiesPricesLim15<-subset(api_CitiesPrices, api_CitiesPrices$contributors>=15)
          pricename <- distinct(api_CitiesPricesLim15[c("prices.item_id")])
          pricename
       

          
#API - Currency
          api_Currency <- as.data.frame(fromJSON("https://www.numbeo.com/api/currency_exchange_rates?api_key=5k5h3in8zdgnv3"))          
          write.csv(api_Currency, file="Currency.csv", row.names=TRUE)
                    
                           
#Push data to MySQL (DOESN'T WORK AS IT SEEMS THERE IS SOME COMPATIBILITY ISSUE BETWEEN R AND MYSQL)
          install.packages("RMySQL")
          library(RMySQL)
          
                      ch_base <- dbConnect(MySQL(),  
                                           user="root", 
                                           password="password", 
                                           dbname="europeranking",
                                           host="localhost")
                      dbListTables(ch_base)                   
                      
                      rs = dbSendQuery(ch_base, )
                      # read from MySQL
                      rs <- as.dataframe(dbSendQuery(ch_base,"select * from european_cities limit 5;"))
                      MySQL_Cities <- data.table(fetch(rs, n=-1))
                      
                      # write to mysql
                      dbWriteTable(ch_base, "cities" , value=api_cities, row.names = TRUE, append=TRUE) 
                      
                      dbWriteTable(ch_base, "citiesprices" , value=api_CitiesPrices, row.names = TRUE, append=TRUE)

       names(DataMartRaw)       
       
       
       
######################### DATA CLEANING AND DATA MANIPULATION FOR ANALYSIS                      
#Read data from MySQL
 
      DataMartRaw <-read.csv(file="DataMartRaw.csv", header = TRUE, sep=',') 
      rownames(DataMartRaw)<- DataMartRaw$city 
      DataMartRaw<- subset(DataMartRaw, DataMartRaw$contributors_crime>=15& 
                          DataMartRaw$contributors_pollution>=15&
                          DataMartRaw$contributors_prices>=15&
                          DataMartRaw$contributors_he>=15)
      
    
      coordinates <- subset(DataMartRaw, select = c(longitude, latitude))  
      CityInfo <- subset(DataMartRaw, select = c(city_id,
                                                  country,
                                                  city))
      DataMartRaw<- subset(DataMartRaw, select = -c(city_id, 
                                              country,
                                              latitude,
                                              longitude,
                                              city))
#Extracting and manipulation Prices data   
      DataMartPrice <- subset(DataMartRaw, select = c(contributors_prices,
                                                      meal_inexpensive,
                                                      mid_range_meal_for_2_people,          
                                                      mcdonalds,                          
                                                      domesticbeer05,                         
                                                      importedbeer033,                         
                                                      coke033,                               
                                                      water,                                
                                                      milk,                                 
                                                      whitebread,                              
                                                      eggs,                                   
                                                      localcheese,                             
                                                      water15,                           
                                                      wine_mid_rang,                           
                                                      domesticbeermarkets,                    
                                                      importedbeermarkets,                     
                                                      marlboro,                               
                                                      One_wayTicketLocalTransport,             
                                                      chickenbreasts,                         
                                                      monthlypasstransportation,               
                                                      gasoline, 
                                                      taxistart,                               
                                                      taxi1hour,                              
                                                      apples,                                  
                                                      oranges,                                
                                                      potato,                                  
                                                      lettuce,                                
                                                      cappuccino,                              
                                                      rice,                                   
                                                      tomato,                                  
                                                      banana,                                 
                                                      onion,                                  
                                                      beefround,
                                                      UtilitiesMontly,                        
                                                      onemin_PrepaidMobileTariff,              
                                                      internetunlimiteddata,                  
                                                      fitnessclub,                             
                                                      tennis,                                 
                                                      cinema,                                  
                                                      onepairjeans,                           
                                                      onesummerdresszara,                      
                                                      onepairnikemid_range,                   
                                                      onepairmenleather, 
                                                      volkswagengolf, 
                                                      toyotacorolla,
                                                      onebedroomcitycentrerentmonth,          
                                                      onebedroomoutsidecentrerentmonth,        
                                                      threebedroomscitycentrerentmonth,       
                                                      threebedroomsoutsidecentrerentmonth,     
                                                      preschool,                               
                                                      primaryschool,                     
                                                      mortgageinterestrateperc,
                                                      pricesquaremeterbuyaptcitycentre,        
                                                      pricesquaremeterbuyaptoutsidecentre,
                                                      netsalary))
                                                      
  
#Replace missing values with the median of the variable
      DataMartPrice[DataMartPrice==0] <- NA
      
      a <- median(DataMartPrice$primaryschool, na.rm = TRUE)
      b <- median(DataMartPrice$pricesquaremeterbuyaptoutsidecentre, na.rm = TRUE)
      
      DataMartPrice$primaryschool[is.na(DataMartPrice$primaryschool)]<-a
      DataMartPrice$pricesquaremeterbuyaptoutsidecentre[is.na(DataMartPrice$pricesquaremeterbuyaptoutsidecentre)]<-b
     
 
#Index creation where the bigger is the value, the better is the purchase power
         DataMartPrice$meal_inexpensive <- DataMartPrice$netsalary/DataMartPrice$meal_inexpensive
         DataMartPrice$mid_range_meal_for_2_people <- DataMartPrice$netsalary/DataMartPrice$mid_range_meal_for_2_people
         DataMartPrice$mcdonalds <- DataMartPrice$netsalary/DataMartPrice$mcdonalds
         DataMartPrice$domesticbeer05 <- DataMartPrice$netsalary/DataMartPrice$domesticbeer05
         DataMartPrice$importedbeer033 <- DataMartPrice$netsalary/DataMartPrice$importedbeer033
         DataMartPrice$coke033 <- DataMartPrice$netsalary/DataMartPrice$coke033
         DataMartPrice$water <- DataMartPrice$netsalary/DataMartPrice$water
         DataMartPrice$milk <- DataMartPrice$netsalary/DataMartPrice$milk
         DataMartPrice$whitebread <- DataMartPrice$netsalary/DataMartPrice$whitebread
         DataMartPrice$eggs <- DataMartPrice$netsalary/DataMartPrice$eggs
         DataMartPrice$localcheese <- DataMartPrice$netsalary/DataMartPrice$localcheese
         DataMartPrice$water15 <- DataMartPrice$netsalary/DataMartPrice$water15
         DataMartPrice$wine_mid_rang <- DataMartPrice$netsalary/DataMartPrice$wine_mid_rang
         DataMartPrice$domesticbeermarkets <- DataMartPrice$netsalary/DataMartPrice$domesticbeermarkets
         DataMartPrice$importedbeermarkets <- DataMartPrice$netsalary/DataMartPrice$importedbeermarkets
         DataMartPrice$marlboro <- DataMartPrice$netsalary/DataMartPrice$marlboro
         DataMartPrice$One_wayTicketLocalTransport <- DataMartPrice$netsalary/DataMartPrice$One_wayTicketLocalTransport
         DataMartPrice$chickenbreasts <- DataMartPrice$netsalary/DataMartPrice$chickenbreasts
         DataMartPrice$monthlypasstransportation <- DataMartPrice$netsalary/DataMartPrice$monthlypasstransportation
         DataMartPrice$gasoline <- DataMartPrice$netsalary/DataMartPrice$gasoline
         DataMartPrice$taxistart <- DataMartPrice$netsalary/DataMartPrice$taxistart
         DataMartPrice$taxi1hour <- DataMartPrice$netsalary/DataMartPrice$taxi1hour
         DataMartPrice$apples <- DataMartPrice$netsalary/DataMartPrice$apples
         DataMartPrice$oranges <- DataMartPrice$netsalary/DataMartPrice$oranges
         DataMartPrice$potato <- DataMartPrice$netsalary/DataMartPrice$potato
         DataMartPrice$lettuce <- DataMartPrice$netsalary/DataMartPrice$lettuce
         DataMartPrice$cappuccino <- DataMartPrice$netsalary/DataMartPrice$cappuccino
         DataMartPrice$rice <- DataMartPrice$netsalary/DataMartPrice$rice
         DataMartPrice$tomato <- DataMartPrice$netsalary/DataMartPrice$tomato
         DataMartPrice$banana <- DataMartPrice$netsalary/DataMartPrice$banana
         DataMartPrice$onion <- DataMartPrice$netsalary/DataMartPrice$onion
         DataMartPrice$UtilitiesMontly <- DataMartPrice$netsalary/DataMartPrice$UtilitiesMontly
         DataMartPrice$onemin_PrepaidMobileTariff <- DataMartPrice$netsalary/DataMartPrice$onemin_PrepaidMobileTariff
         DataMartPrice$internetunlimiteddata <- DataMartPrice$netsalary/DataMartPrice$internetunlimiteddata
         DataMartPrice$fitnessclub <- DataMartPrice$netsalary/DataMartPrice$fitnessclub
         DataMartPrice$tennis <- DataMartPrice$netsalary/DataMartPrice$tennis
         DataMartPrice$cinema <- DataMartPrice$netsalary/DataMartPrice$cinema
         DataMartPrice$onepairjeans <- DataMartPrice$netsalary/DataMartPrice$onepairjeans
         DataMartPrice$onesummerdresszara <- DataMartPrice$netsalary/DataMartPrice$onesummerdresszara
         DataMartPrice$onepairnikemid_range <- DataMartPrice$netsalary/DataMartPrice$onepairnikemid_range
         DataMartPrice$onepairmenleather <- DataMartPrice$netsalary/DataMartPrice$onepairmenleather
         DataMartPrice$volkswagengolf <- DataMartPrice$netsalary/DataMartPrice$volkswagengolf
         DataMartPrice$toyotacorolla <- DataMartPrice$netsalary/DataMartPrice$toyotacorolla
         DataMartPrice$onebedroomcitycentrerentmonth <- DataMartPrice$netsalary/DataMartPrice$onebedroomcitycentrerentmonth
         DataMartPrice$onebedroomoutsidecentrerentmonth <- DataMartPrice$netsalary/DataMartPrice$onebedroomoutsidecentrerentmonth
         DataMartPrice$threebedroomscitycentrerentmonth <- DataMartPrice$netsalary/DataMartPrice$threebedroomscitycentrerentmonth
         DataMartPrice$threebedroomsoutsidecentrerentmonth <- DataMartPrice$netsalary/DataMartPrice$threebedroomsoutsidecentrerentmonth
         DataMartPrice$preschool <- DataMartPrice$netsalary/DataMartPrice$preschool
         DataMartPrice$primaryschool <- DataMartPrice$netsalary/DataMartPrice$primaryschool
         DataMartPrice$pricesquaremeterbuyaptcitycentre <- DataMartPrice$netsalary/DataMartPrice$pricesquaremeterbuyaptcitycentre
         DataMartPrice$pricesquaremeterbuyaptoutsidecentre <- DataMartPrice$netsalary/DataMartPrice$pricesquaremeterbuyaptoutsidecentre
         DataMartPrice$beefround <- DataMartPrice$netsalary/DataMartPrice$beefround
         
 
#Normalization of the dataset (prices)
         
 DataMartNorm <- as.data.frame(lapply(DataMartPrice, scale))
         
 
#Calculation of means across variables (between rows) 
 DataMartNorm$basic_expenses <- rowMeans(subset(DataMartNorm, select = c(meal_inexpensive,
                                                                         mid_range_meal_for_2_people,          
                                                                         mcdonalds,                          
                                                                         domesticbeer05,                         
                                                                         importedbeer033,                         
                                                                         coke033,                               
                                                                         water,                                
                                                                         milk,                                 
                                                                         whitebread,                              
                                                                         eggs,                                   
                                                                         localcheese,                             
                                                                         water15,                           
                                                                         wine_mid_rang,                           
                                                                         domesticbeermarkets,                    
                                                                         importedbeermarkets,                     
                                                                         marlboro,                               
                                                                         One_wayTicketLocalTransport,             
                                                                         chickenbreasts,                         
                                                                         monthlypasstransportation,               
                                                                         gasoline, 
                                                                         taxistart,                               
                                                                         taxi1hour,                              
                                                                         apples,                                  
                                                                         oranges,                                
                                                                         potato,                                  
                                                                         lettuce,                                
                                                                         cappuccino,                              
                                                                         rice,                                   
                                                                         tomato,                                  
                                                                         banana,                                 
                                                                         onion,                                  
                                                                         beefround,
                                                                         UtilitiesMontly,                        
                                                                         onemin_PrepaidMobileTariff,              
                                                                         internetunlimiteddata,                  
                                                                         fitnessclub,                             
                                                                         tennis,                                 
                                                                         cinema,                                  
                                                                         onepairjeans,                           
                                                                         onesummerdresszara,                      
                                                                         onepairnikemid_range,                   
                                                                         onepairmenleather)), na.rm = TRUE)
#Aggregation of similar variables to reduce the number of features
      DataMartNorm$buy_car <- rowMeans(subset(DataMartNorm, select = c(volkswagengolf, 
                                                                        toyotacorolla)), na.rm = TRUE)
      DataMartNorm$rent_apartment <- rowMeans(subset(DataMartNorm, select = c(onebedroomcitycentrerentmonth,          
                                                                         onebedroomoutsidecentrerentmonth,        
                                                                         threebedroomscitycentrerentmonth,       
                                                                         threebedroomsoutsidecentrerentmonth)), na.rm = TRUE)
      DataMartNorm$school_ind <- rowMeans(subset(DataMartNorm, select = c(preschool,                               
                                                                           primaryschool)), na.rm = TRUE)
      DataMartNorm$buy_apartment <- rowMeans(subset(DataMartNorm, select = c(pricesquaremeterbuyaptcitycentre,        
                                                                        pricesquaremeterbuyaptoutsidecentre)), na.rm = TRUE)
 
      DataMartNorm <- as.data.frame(lapply(DataMartNorm, scale))
      
      DataMartPriceFinished <- subset(DataMartNorm, select = c(basic_expenses,
                                                                        buy_car,
                                                                        rent_apartment,
                                                                        school_ind,
                                                                        buy_apartment,
                                                                        mortgageinterestrateperc))
 
     
    

 
#Cancellation of variables already aggregated in order to work with the remaining variables
      DataMartFil <- subset(DataMartRaw, select = -c(contributors_prices,
                                                      meal_inexpensive,
                                                      mid_range_meal_for_2_people,          
                                                      mcdonalds,                          
                                                      domesticbeer05,                         
                                                      importedbeer033,                         
                                                      coke033,                               
                                                      water,                                
                                                      milk,                                 
                                                      whitebread,                              
                                                      eggs,                                   
                                                      localcheese,                             
                                                      water15,                           
                                                      wine_mid_rang,                           
                                                      domesticbeermarkets,                    
                                                      importedbeermarkets,                     
                                                      marlboro,                               
                                                      One_wayTicketLocalTransport,             
                                                      chickenbreasts,                         
                                                      monthlypasstransportation,               
                                                      gasoline, 
                                                      taxistart,                               
                                                      taxi1hour,                              
                                                      apples,                                  
                                                      oranges,                                
                                                      potato,                                  
                                                      lettuce,                                
                                                      cappuccino,                              
                                                      rice,                                   
                                                      tomato,                                  
                                                      banana,                                 
                                                      onion,                                  
                                                      beefround,
                                                      UtilitiesMontly,                        
                                                      onemin_PrepaidMobileTariff,              
                                                      internetunlimiteddata,                  
                                                      fitnessclub,                             
                                                      tennis,                                 
                                                      cinema,                                  
                                                      onepairjeans,                           
                                                      onesummerdresszara,                      
                                                      onepairnikemid_range,                   
                                                      onepairmenleather, 
                                                      volkswagengolf, 
                                                      toyotacorolla,
                                                      onebedroomcitycentrerentmonth,          
                                                      onebedroomoutsidecentrerentmonth,        
                                                      threebedroomscitycentrerentmonth,       
                                                      threebedroomsoutsidecentrerentmonth,     
                                                      preschool,                               
                                                      primaryschool,                     
                                                      mortgageinterestrateperc,
                                                      pricesquaremeterbuyaptcitycentre,        
                                                      pricesquaremeterbuyaptoutsidecentre,
                                                      netsalary))
      
      
      
#Normalize the dataframe (Pollution, Healtcare, Crime)
#DataMartPriceFinished and DataMartFil have to be normalized with the same scale      

      DataMartFil <- as.data.frame(lapply(DataMartFil, scale))
      
      DataMartFil$safe_alone_ind <- rowMeans(DataMartFil[c('safe_alone_night', 'safe_alone_daylight')], na.rm=FALSE)  #ok
      
      DataMartFil$theft_robbery_ind <- rowMeans(DataMartFil[c('problem_property_crimes', 
                                                                    'worried_mugged_robbed',
                                                                    'worried_car_stolen',
                                                                    'worried_home_broken',
                                                                    'worried_things_car_stolen')], na.rm=FALSE) #Inverted
      
      DataMartFil$attacked_insulted_skin_religion <- rowMeans(DataMartFil[c('worried_insulted', 
                                                                                  'worried_attacked',
                                                                                  'worried_skin_ethnic_religion')], na.rm=FALSE) #Inverted
      
      DataMartFil$healthcare_speed <- rowMeans(DataMartFil[c('responsiveness_waitings', 'speed')], na.rm=FALSE) #ok
      
      DataMartFil$healthcare_insurance <- rowMeans(DataMartFil[c('insurance_type_Employer_Sponsored', 
                                                                 'insurance_type_Private',
                                                                 'insurance_type_Public',
                                                                 'insurance_type_None')], na.rm=FALSE) #Ok (to double check)
      
      DataMartFil$healthcare_competency_equipment <- rowMeans(DataMartFil[c('skill_and_competency', 
                                                                            'accuracy_and_completeness',
                                                                            'friendliness_and_courtesy',
                                                                            'modern_equipment',
                                                                            'location')], na.rm=FALSE) #Ok
      
      
      DataMartFil$pollution_environment_quality <- rowMeans(DataMartFil[c('green_and_parks_quality_pollution', 
                                                                          'comfortable_to_spend_time_pollution',
                                                                          'garbage_disposal_satisfaction_pollution',
                                                                          'drinking_water_quality_accessibility',
                                                                          'clean_and_tidy')], na.rm=FALSE) #ok
      
      DataMartFil$pollution_pm10_pm25_ind <- rowMeans(DataMartFil[c('pm10_pollution', 
                                                                    'pm25_pollution')], na.rm=FALSE) #ok 
      
      DataMartFil <- as.data.frame(lapply(DataMartFil, scale))
      
#Deleting all rows which are not needed anynore     
      DataMartFil<- subset(DataMartFil, select = -c(contributors_crime,
                                       contributors_pollution,
                                       contributors_he,
                                       index_safety,
                                       index_crime,
                                       index_healthcare,
                                       index_pollution,
                                       worried_attacked,
                                       problem_property_crimes,
                                       safe_alone_night,
                                       worried_skin_ethnic_religion,
                                       worried_car_stolen,
                                       worried_home_broken,
                                       worried_things_car_stolen,
                                       safe_alone_daylight,
                                       worried_insulted,
                                       problem_violent_crimes,
                                       worried_mugged_robbed,
                                       skill_and_competency,
                                       responsiveness_waitings,
                                       accuracy_and_completeness,
                                       responsiveness_waitings,
                                       speed,
                                       insurance_type_Employer_Sponsored,
                                       insurance_type_Private,
                                       insurance_type_Public,
                                       insurance_type_None,
                                       friendliness_and_courtesy,
                                       modern_equipment,
                                       location,
                                       green_and_parks_quality_pollution,
                                       comfortable_to_spend_time_pollution,
                                       garbage_disposal_satisfaction_pollution,
                                       drinking_water_quality_accessibility,
                                       pm10_pollution,
                                       pm25_pollution,
                                       clean_and_tidy
                                       ))
    
    DataMart <- cbind(CityInfo, DataMartPriceFinished, DataMartFil) 
    DataMart <- subset(DataMart, select = -c(1,2,3))
 
    
    
################# CLUSTERING ANALYSIS WITH K-MEANS           
    DataMart
    set.seed(456)
    result<-kmeans(DataMart,3)
    result$size
    Centres<-as.data.frame(result$centers)
    

    
    install.packages("factoextra")
    library(factoextra)
    
    
    distance<-get_dist(DataMart)
    set.seed(456)
    k<-kmeans(DataMart, centers = 3, nstart = 20)
    fviz_cluster(k, data=DataMart)
    print(k)
    
    CityInfo$cluster <- result$cluster  
    DataMartOutPut1<-cbind(CityInfo, coordinates, DataMart)
    write.csv(DataMartOutPut1, file="DataMartOutPut1.csv", row.names=TRUE)
    Centres$cluster <-row.names(Centres)
    
    
    
    
######### ELBOW METHOD (within groups - Heterogeneity)
    install.packages("ggplot2")
    library(ggplot2)
    install.packages("purrr")
    library(purrr)
    
    library(gridExtra)
    
    set.seed(123)
    wss<-function(k){
      kmeans(DataMart, k, nstart = 10)$tot.withinss
    }
    k.values<-1:15
    wss_values<-map_dbl(k.values, wss)
    plot(k.values, wss_values,
         type = "b", pch=19, frame=TRUE,
         xlab = "Number of Clusters",
         ylab = "Total within-clusters sum of squares")
    
########## CLUSTERING ANALSYS WITH HIERARCHICAL CLUSTERING

    d <- dist(DataMart, method = "euclidean") # distance matrix
    fit <- hclust(distance, method="ward")
    plot(fit, cex = 0.9, hang = -1) # display dendogram
    groups <- cutree(fit, k=5) # cut tree into n clusters
    # draw dendogram with red borders around the 5 clusters
    rect.hclust(fit, k=3, border="red")
    rect.hclust(fit, k=5, border="blue")
   
    
########### CENTROIDS ANALYSIS    
#Change the variables sign as I need the higher the better
    Centres$problem_corruption_bribery <- Centres$problem_corruption_bribery *(-1)
    Centres$theft_robbery_ind <- Centres$theft_robbery_ind *(-1)
    Centres$level_of_crime <- Centres$level_of_crime *(-1)
    Centres$crime_increasing <- Centres$crime_increasing *(-1)
    Centres$attacked_insulted_skin_religion <- Centres$attacked_insulted_skin_religion *(-1)
    Centres$problem_drugs <- Centres$problem_drugs *(-1)
    Centres$water_pollution <- Centres$water_pollution *(-1)
    Centres$noise_and_light_pollution <- Centres$noise_and_light_pollution *(-1)
    Centres$pollution_pm10_pm25_ind <- Centres$pollution_pm10_pm25_ind *(-1)

    write.csv(Centres, file="centres.csv", row.names=TRUE)
    
##################### CLUSTERS COMPARISON ANALYSIS    
    
    cluster1<-subset(Centres, Centres$cluster ==1)
    cluster2<-subset(Centres, Centres$cluster ==2)
    cluster3<-subset(Centres, Centres$cluster ==3)

    rm(testdf1)
    
    testdf1 <- as.data.frame(cbind(names(cluster1),t(cluster1),t(cluster2),t(cluster3)),row.names = FALSE)
    names(testdf1) <- c('Variables','Cluster1','Cluster2','Cluster3')
    testdf1 <- subset(testdf1, Variables !="cluster")
    
    
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'problem_corruption_bribery')),5]<-1
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'theft_robbery_ind')),5]<-1
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'level_of_crime')),5]<-1
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'safe_alone_ind')),5]<-1
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'crime_increasing')),5]<-1
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'attacked_insulted_skin_religion')),5]<-1
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'problem_drugs')),5]<-1
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'school_ind')),5]<-2
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'buy_apartment')),5]<-2
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'rent_apartment')),5]<-2
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'basic_expenses')),5]<-2
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'buy_car')),5]<-2
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'mortgageinterestrateperc')),5]<-2
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'healthcare_insurance')),5]<-3
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'healthcare_competency_equipment')),5]<-3
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'cost')),5]<-3
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'healthcare_speed')),5]<-3
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'water_pollution')),5]<-4
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'noise_and_light_pollution')),5]<-4
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'pollution_environment_quality')),5]<-4
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'pollution_pm10_pm25_ind')),5]<-4
    testdf1[row.names.data.frame(subset(testdf1, testdf1$Variables== 'air_quality_pollution')),5]<-4
    names(testdf1)[5] <- "Groups"
    
    write.csv(testdf1, file="testdf1.csv", row.names=TRUE)
    
    
######## CLASSIFICATION WITH K-NN
    DataMartKnn<-DataMart
    
    DataMartKnn$clusters <- result$cluster
    
    
    DataMartKnn_train <- DataMartKnn[1:70,]
    DataMartKnn_test <- DataMartKnn[71:105,]
    
    
    DataMartKnn_train_cluster <-DataMartKnn_train[,23]
    DataMartKnn_test_cluster <- DataMartKnn_test[,23]
    
    DataMartKnn_train <- subset(DataMartKnn_train, select = -c(clusters))
    DataMartKnn_test <- subset(DataMartKnn_test, select = -c(clusters))
    
    install.packages("class", dependencies = TRUE)
    library(class)
    Prediction<-knn(train = DataMartKnn_train, test = DataMartKnn_test, cl= DataMartKnn_train_cluster, k=12)
    Prediction
    
    
    install.packages("gmodels", dependencies = TRUE)
    library(gmodels)
    CrossTable(x=Prediction, y=DataMartKnn_test_cluster, prop.chisq = FALSE)
    
    
    
    
############# CLASSIFICATION WITH SVM (SUPPORT VECTOR MACHINE)
    install.packages("kernlab")
    library(kernlab)
    
    
    DataMartLM<-DataMart
    DataMartLM$clusters <- result$cluster
    DataMartLM<-DataMartLM[,c(23, 1:22)]
    
    DataMartLM_train <- DataMartLM[1:70,]
    DataMartLM_test <- DataMartLM[71:105,]
    test<-DataMartLM_test[,1]
    names(test)[1]<- "clusters"
    DataMartLM_test <- as.vector(DataMartLM_test[,-1])
    
    
    set.seed(12345)
    model_svm <- ksvm(clusters ~ basic_expenses+             
                     buy_car+               
                     rent_apartment+           
                     school_ind+         
                     buy_apartment+       
                     mortgageinterestrateperc+     
                     level_of_crime+        
                     crime_increasing+              
                     problem_corruption_bribery+    
                     problem_drugs+                  
                     cost+                           
                     air_quality_pollution+          
                     noise_and_light_pollution+       
                     water_pollution+           
                     safe_alone_ind+             
                     theft_robbery_ind+             
                     attacked_insulted_skin_religion+ 
                     healthcare_speed+               
                     healthcare_insurance+           
                     healthcare_competency_equipment+
                     pollution_environment_quality+   
                     pollution_pm10_pm25_ind,
                     data=DataMartLM_train,
                     kernel="rbfdot",
                     C=1)
    pred <- round(predict(model_svm, DataMartLM_test),0)
    
    head(pred)
    table(pred, test)
    
    agreement <- pred == test
    table(agreement)    
    
    
    
    
    
    
    
    
    
    
    
 