#Read CSV into R
mydata <- read.csv(file="/Users/tianshan/Desktop/10minswalk.csv", header=TRUE, sep=",")
dim(mydata)

library(spdep)

#lif, air conditioner, furnish, view(garden, panoramic, street)
group2 <- cbind.data.frame(mydata["latitude"],mydata["longitude"],mydata["price_per_sqm"], mydata["apartment"], mydata["number_of_half_rooms"], mydata["number_of_whole_rooms"], mydata["floor"])
print("group I: apartment, number_of_half_rooms, number_of_whole_rooms, floor")
group2clean <- na.omit(group2)
dim(group2clean)
head(group2clean)

#log price
group2clean["logprice"] <- log(group2clean["price_per_sqm"])
f2 <- logprice ~ apartment + number_of_half_rooms + number_of_whole_rooms + floor
m2 <- lm(f2, data = group2clean)
summary(m2)

#spatial lag regression
group2clean$residuals <- residuals(m2)
#print(group1clean$residuals)

group2.coord <- cbind(group2clean$latitude, group2clean$longitude)
#head(coord)
group2.knea <- knearneigh(group2.coord, longlat = TRUE)
group2.nb <- knn2nb(group2.knea)
group2.lw <- nb2listw(group2.nb)
moran.mc(group2clean$residuals, group2.lw, 999)
group2.slag = lagsarlm(f2, data = group2clean, group2.lw)
summary(group2.slag)

group2clean$residuals <- residuals(group2.slag)
moran.mc(group2clean$residuals, group2.lw, 999)

#Lagrange Multiplier Test Statistics for Spatial Autocorrelation 
group2.lagrange <- lm.LMtests(m2, group2.lw, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(group2.lagrange)

#
#
#lif, air conditioner, furnish, view(garden, panoramic, street), heating_gas, heating_wall_heating, heating_combination, heating_ceiling, heating_central_heating, heating_electric, heating_floor, heating_circulating, heating_district
group3 <- cbind.data.frame(mydata["latitude"],mydata["longitude"],mydata["price_per_sqm"], mydata["apartment"], mydata["number_of_half_rooms"], mydata["number_of_whole_rooms"], mydata["floor"], mydata["lift"], mydata["air_conditioner"], mydata["furnish_furnished"], mydata["view_garden"], mydata["view_panoramic"], mydata["view_street"], mydata["heating_gas"], mydata["heating_wall_heating"], mydata["heating_combination"], mydata["heating_ceiling"], mydata["heating_central_heating"], mydata["heating_electric"], mydata["heating_floor"], mydata["heating_circulating"], mydata["heating_district"])
print("group I: apartment, number_of_half_rooms, number_of_whole_rooms, floor, lif, air conditioner, furnish, view(garden, panoramic, street), heating")
group3clean <- na.omit(group3)
dim(group3clean)

#log price
group3clean["logprice"] <- log(group3clean["price_per_sqm"])
f3 <- logprice ~ apartment + number_of_half_rooms + number_of_whole_rooms + floor + lift + air_conditioner + furnish_furnished + view_garden + view_panoramic + view_street + heating_gas + heating_wall_heating + heating_combination + heating_ceiling + heating_central_heating + heating_electric + heating_floor + heating_circulating + heating_district
m3 <- lm(f3, data = group3clean)
summary(m3)

#spatial lag regression
group3clean$residuals <- residuals(m3)
#print(group1clean$residuals)

group3.coord <- cbind(group3clean$latitude, group3clean$longitude)
#head(coord)
group3.knea <- knearneigh(group3.coord, longlat = TRUE)
group3.nb <- knn2nb(group3.knea)
group3.lw <- nb2listw(group3.nb)
moran.mc(group3clean$residuals, group3.lw, 999)
group3.slag = lagsarlm(f3, data = group3clean, group3.lw)
summary(group3.slag)

group3clean$residuals <- residuals(group3.slag)
moran.mc(group3clean$residuals, group3.lw, 999)

#Lagrange Multiplier Test Statistics for Spatial Autocorrelation 
group3.lagrange <- lm.LMtests(m3, group3.lw, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(group3.lagrange)

#
#
#lif, air conditioner, furnish, view, heating, district
group4 <- cbind.data.frame(mydata["latitude"],mydata["longitude"],mydata["price_per_sqm"], mydata["apartment"], mydata["number_of_half_rooms"], mydata["number_of_whole_rooms"], mydata["floor"], mydata["lift"], mydata["air_conditioner"], mydata["furnish_furnished"], mydata["view_garden"], mydata["view_panoramic"], mydata["view_street"], mydata["heating_gas"], mydata["heating_wall_heating"], mydata["heating_combination"], mydata["heating_ceiling"], mydata["heating_central_heating"], mydata["heating_electric"], mydata["heating_floor"], mydata["heating_circulating"], mydata["heating_district"], mydata["district_i"], mydata["district_ii"], mydata["district_iii"], mydata["district_iv"], mydata["district_v"], mydata["district_vi"], mydata["district_vii"], mydata["district_viii"], mydata["district_ix"], mydata["district_x"], mydata["district_xi"], mydata["district_xii"], mydata["district_xiii"], mydata["district_xiv"], mydata["district_xv"], mydata["district_xvi"], mydata["district_xvii"], mydata["district_xviii"], mydata["district_xix"], mydata["district_xx"], mydata["district_xxi"], mydata["district_xxii"], mydata["district_xxiii"])
print("group IIII: apartment, number_of_half_rooms, number_of_whole_rooms, floor, lif, air conditioner, furnish, view(garden, panoramic, street), heating, district")
group4clean <- na.omit(group4)
dim(group4clean)

#log price
group4clean["logprice"] <- log(group4clean["price_per_sqm"])
f4 <- logprice ~ apartment + number_of_half_rooms + number_of_whole_rooms + floor + lift + air_conditioner + furnish_furnished + view_garden + view_panoramic + view_street + heating_gas + heating_wall_heating + heating_combination + heating_ceiling + heating_central_heating + heating_electric + heating_floor + heating_circulating + heating_district + district_i + district_ii + district_iii + district_iv + district_v + district_vi + district_vii + district_viii + district_ix + district_x + district_xi + district_xii + district_xiii + district_xiv + district_xv + district_xvi + district_xvii + district_xviii + district_xix + district_xx + district_xxi + district_xxii + district_xxiii
m4 <- lm(f4, data = group4clean)
summary(m4)

#spatial lag regression
group4clean$residuals <- residuals(m4)
#print(group1clean$residuals)

group4.coord <- cbind(group4clean$latitude, group4clean$longitude)
#head(coord)
group4.knea <- knearneigh(group4.coord, longlat = TRUE)
group4.nb <- knn2nb(group4.knea)
group4.lw <- nb2listw(group4.nb)
moran.mc(group4clean$residuals, group4.lw, 999)
group4.slag = lagsarlm(f4, data = group4clean, group4.lw)
summary(group4.slag)

group4clean$residuals <- residuals(group4.slag)
moran.mc(group4clean$residuals, group4.lw, 999)

#Lagrange Multiplier Test Statistics for Spatial Autocorrelation 
group4.lagrange <- lm.LMtests(m4, group4.lw, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(group4.lagrange)

#
#
#lif, air conditioner, furnish, view(garden, panoramic, street)
group5 <- cbind.data.frame(mydata["latitude"],mydata["longitude"],mydata["price_per_sqm"], mydata["apartment"], mydata["number_of_half_rooms"], mydata["number_of_whole_rooms"], mydata["floor"], mydata["lift"], mydata["air_conditioner"], mydata["furnish_furnished"], mydata["view_garden"], mydata["view_panoramic"], mydata["view_street"])
print("group V: apartment, number_of_half_rooms, number_of_whole_rooms, floor, lif, air conditioner, furnish, view(garden, panoramic, street)")
group5clean <- na.omit(group5)
dim(group5clean)

#log price
group5clean["logprice"] <- log(group5clean["price_per_sqm"])
f5 <- logprice ~ apartment + number_of_half_rooms + number_of_whole_rooms + floor + lift + air_conditioner + furnish_furnished + view_garden + view_panoramic + view_street 
m5 <- lm(f5, data = group5clean)
summary(m5)

#spatial lag regression
group5clean$residuals <- residuals(m5)
#print(group1clean$residuals)

group5.coord <- cbind(group5clean$latitude, group5clean$longitude)
#head(coord)
group5.knea <- knearneigh(group5.coord, longlat = TRUE)
group5.nb <- knn2nb(group5.knea)
group5.lw <- nb2listw(group5.nb)
moran.mc(group5clean$residuals, group5.lw, 999)
group5.slag = lagsarlm(f5, data = group5clean, group5.lw)
summary(group5.slag)

group5clean$residuals <- residuals(group5.slag)
moran.mc(group5clean$residuals, group5.lw, 999)

#Lagrange Multiplier Test Statistics for Spatial Autocorrelation 
group5.lagrange <- lm.LMtests(m5, group5.lw, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(group5.lagrange)

#
#
#lif, air conditioner, furnish, view(garden, panoramic, street), shop
group6 <- cbind.data.frame(mydata["latitude"], mydata["longitude"], mydata["price_per_sqm"], mydata["apartment"], mydata["number_of_half_rooms"], mydata["number_of_whole_rooms"], mydata["floor"], mydata["lift"], mydata["air_conditioner"], mydata["furnish_furnished"], mydata["view_garden"], mydata["view_panoramic"], mydata["view_street"], mydata["shop_art_music_hobbies"], mydata["shop_beauty"], mydata["shop_clothing_shoes_accessories"], mydata["shop_convenience_shop"], mydata["shop_department_store"], mydata["shop_discount_store_charity"], mydata["shop_food_beverages"], mydata["shop_stationery_gifts_books_newspapers"], mydata["shop_supermarket"], mydata["shop_travel_agency"])
print("group VI: apartment, number_of_half_rooms, number_of_whole_rooms, floor, lif, air conditioner, furnish, view(garden, panoramic, street), shop")
group6clean <- na.omit(group6)
dim(group6clean)

#log price
group6clean["logprice"] <- log(group6clean["price_per_sqm"])
f6 <- logprice ~ apartment + number_of_half_rooms + number_of_whole_rooms + floor + lift + air_conditioner + furnish_furnished + view_garden + view_panoramic + view_street + shop_art_music_hobbies + shop_beauty + shop_clothing_shoes_accessories + shop_convenience_shop + shop_department_store + shop_discount_store_charity + shop_food_beverages + shop_stationery_gifts_books_newspapers + shop_supermarket + shop_travel_agency
m6 <- lm(f6, data = group6clean)
summary(m6)

#spatial lag regression
group6clean$residuals <- residuals(m6)
#print(group1clean$residuals)

group6.coord <- cbind(group6clean$latitude, group6clean$longitude)
#head(coord)
group6.knea <- knearneigh(group6.coord, longlat = TRUE)
group6.nb <- knn2nb(group6.knea)
group6.lw <- nb2listw(group6.nb)
moran.mc(group6clean$residuals, group6.lw, 999)
group6.slag = lagsarlm(f6, data = group6clean, group6.lw)
summary(group6.slag)

group6clean$residuals <- residuals(group6.slag)
moran.mc(group6clean$residuals, group6.lw, 999)

#Lagrange Multiplier Test Statistics for Spatial Autocorrelation 
group6.lagrange <- lm.LMtests(m6, group6.lw, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(group6.lagrange)

#
#
#lif, air conditioner, furnish, view(garden, panoramic, street), shop, station
group7 <- cbind.data.frame(mydata["latitude"],mydata["longitude"],mydata["price_per_sqm"], mydata["apartment"], mydata["number_of_half_rooms"], mydata["number_of_whole_rooms"], mydata["floor"], mydata["lift"], mydata["air_conditioner"], mydata["furnish_furnished"], mydata["view_garden"], mydata["view_panoramic"], mydata["view_street"], mydata["metro_station"], mydata["bus_station"])
print("group VII: apartment, number_of_half_rooms, number_of_whole_rooms, floor, lif, air conditioner, furnish, view(garden, panoramic, street), station")
group7clean <- na.omit(group7)
dim(group7clean)

#log price
group7clean["logprice"] <- log(group7clean["price_per_sqm"])
f7 <- logprice ~ apartment + number_of_half_rooms + number_of_whole_rooms + floor + lift + air_conditioner + furnish_furnished + view_garden + view_panoramic + view_street + metro_station + bus_station
m7 <- lm(f7, data = group7clean)
summary(m7)

#spatial lag regression
group7clean$residuals <- residuals(m7)
#print(group1clean$residuals)

group7.coord <- cbind(group7clean$latitude, group7clean$longitude)
#head(coord)
group7.knea <- knearneigh(group7.coord, longlat = TRUE)
group7.nb <- knn2nb(group7.knea)
group7.lw <- nb2listw(group7.nb)
moran.mc(group7clean$residuals, group7.lw, 999)
group7.slag = lagsarlm(f7, data = group7clean, group7.lw)
summary(group7.slag)

group7clean$residuals <- residuals(group7.slag)
moran.mc(group7clean$residuals, group7.lw, 999)

#Lagrange Multiplier Test Statistics for Spatial Autocorrelation 
group7.lagrange <- lm.LMtests(m7, group7.lw, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(group7.lagrange)

#
#
#lif, air conditioner, furnish, view(garden, panoramic, street), distance, shop, station
group8 <- cbind.data.frame(mydata["latitude"],mydata["longitude"],mydata["price_per_sqm"], mydata["apartment"], mydata["number_of_half_rooms"], mydata["number_of_whole_rooms"], mydata["floor"], mydata["lift"], mydata["air_conditioner"], mydata["furnish_furnished"], mydata["view_garden"], mydata["view_panoramic"], mydata["view_street"], mydata["tourism_gallery"], mydata["tourism_guest_house"], mydata["tourism_hostel"], mydata["tourism_hotel"], mydata["tourism_information"], mydata["tourism_motel"], mydata["tourism_museum"])
print("group VIII: apartment, number_of_half_rooms, number_of_whole_rooms, floor, lif, air conditioner, furnish, view(garden, panoramic, street), tourism")
group8clean <- na.omit(group8)
dim(group8clean)

#log price
group8clean["logprice"] <- log(group8clean["price_per_sqm"])
f8 <- logprice ~ apartment + number_of_half_rooms + number_of_whole_rooms + floor + lift + air_conditioner + furnish_furnished + view_garden + view_panoramic + view_street + tourism_gallery + tourism_guest_house + tourism_hostel + tourism_hotel + tourism_information + tourism_motel + tourism_museum
m8 <- lm(f8, data = group8clean)
summary(m8)

#spatial lag regression
group8clean$residuals <- residuals(m8)
#print(group1clean$residuals)

group8.coord <- cbind(group8clean$latitude, group8clean$longitude)
#head(coord)
group8.knea <- knearneigh(group8.coord, longlat = TRUE)
group8.nb <- knn2nb(group8.knea)
group8.lw <- nb2listw(group8.nb)
moran.mc(group8clean$residuals, group8.lw, 999)
group8.slag = lagsarlm(f8, data = group8clean, group8.lw)
summary(group8.slag)

group8clean$residuals <- residuals(group8.slag)
moran.mc(group8clean$residuals, group8.lw, 999)

#Lagrange Multiplier Test Statistics for Spatial Autocorrelation 
group8.lagrange <- lm.LMtests(m8, group8.lw, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(group8.lagrange)

#
#
#lif, air conditioner, furnish, view(garden, panoramic, street), distance, shop, station, tourism, amenity
group9 <- cbind.data.frame(mydata["latitude"],mydata["longitude"],mydata["price_per_sqm"], mydata["apartment"], mydata["number_of_half_rooms"], mydata["number_of_whole_rooms"], mydata["floor"], mydata["lift"], mydata["air_conditioner"], mydata["furnish_furnished"], mydata["view_garden"], mydata["view_panoramic"], mydata["view_street"], mydata["amenity_sustenance"], mydata["amenity_financial"], mydata["amenity_healthcare"], mydata["amenity_entertainment"], mydata["amenity_religion"], mydata["amenity_police"], mydata["amenity_government"], mydata["amenity_vending_machine"], mydata["amenity_parking"])
print("group VIIII: amenity")
group9clean <- na.omit(group9)
dim(group9clean)

#log price
group9clean["logprice"] <- log(group9clean["price_per_sqm"])
f9 <- logprice ~ apartment + number_of_half_rooms + number_of_whole_rooms + floor + lift + air_conditioner + furnish_furnished + view_garden + view_panoramic + view_street + amenity_sustenance + amenity_financial + amenity_healthcare + amenity_entertainment + amenity_religion + amenity_police + amenity_government + amenity_vending_machine + amenity_parking
m9 <- lm(f9, data = group9clean)
summary(m9)

#spatial lag regression
group9clean$residuals <- residuals(m9)
#print(group1clean$residuals)

group9.coord <- cbind(group9clean$latitude, group9clean$longitude)
#head(coord)
group9.knea <- knearneigh(group9.coord, longlat = TRUE)
group9.nb <- knn2nb(group9.knea)
group9.lw <- nb2listw(group9.nb)
moran.mc(group9clean$residuals, group9.lw, 999)
group9.slag = lagsarlm(f9, data = group9clean, group9.lw)
summary(group9.slag)

group9clean$residuals <- residuals(group9.slag)
moran.mc(group9clean$residuals, group9.lw, 999)

#Lagrange Multiplier Test Statistics for Spatial Autocorrelation 
group9.lagrange <- lm.LMtests(m9, group9.lw, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(group9.lagrange)

#
#
#lif, air conditioner, furnish, view(garden, panoramic, street), distance, shop, station, tourism, amenity
group10 <- cbind.data.frame(mydata["latitude"],mydata["longitude"],mydata["price_per_sqm"], mydata["apartment"], mydata["number_of_half_rooms"], mydata["number_of_whole_rooms"], mydata["floor"], mydata["lift"], mydata["air_conditioner"], mydata["furnish_furnished"], mydata["view_garden"], mydata["view_panoramic"], mydata["view_street"], mydata["Crimes"])
print("group X")
group10clean <- na.omit(group10)
dim(group10clean)

#log price
group10clean["logprice"] <- log(group10clean["price_per_sqm"])
f10 <- logprice ~ apartment + number_of_half_rooms + number_of_whole_rooms + floor + lift + air_conditioner + furnish_furnished + view_garden + view_panoramic + view_street + Crimes
m10 <- lm(f10, data = group10clean)
summary(m10)

#spatial lag regression
group10clean$residuals <- residuals(m10)
#print(group1clean$residuals)

group10.coord <- cbind(group10clean$latitude, group10clean$longitude)
#head(coord)
group10.knea <- knearneigh(group10.coord, longlat = TRUE)
group10.nb <- knn2nb(group10.knea)
group10.lw <- nb2listw(group10.nb)
moran.mc(group10clean$residuals, group10.lw, 10)
group10.slag = lagsarlm(f10, data = group10clean, group10.lw)
summary(group10.slag)

group10clean$residuals <- residuals(group10.slag)
moran.mc(group10clean$residuals, group10.lw, 10)

#Lagrange Multiplier Test Statistics for Spatial Autocorrelation 
group10.lagrange <- lm.LMtests(m10, group10.lw, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(group10.lagrange)



