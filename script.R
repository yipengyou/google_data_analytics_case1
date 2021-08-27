install.packages("tidyverse")
install.packages("geosphere")



library(geosphere)
library(tidyverse)

data1 <- read.csv("C:/Users/YP/Desktop/divvy_tripdata/Important/202007-divvy-tripdata.csv")
data1 <-data.frame(data1)
data2 <- read.csv("C:/Users/YP/Desktop/divvy_tripdata/Important/202008-divvy-tripdata.csv")
data2 <- data.frame(data2)
df <- bind_rows(data1, data2)
remove(data1, data2)
data3 <- read.csv("C:/Users/YP/Desktop/divvy_tripdata/Important/202009-divvy-tripdata.csv")
df <- bind_rows(df, data3)
remove(data3)
data4 <- read.csv("C:/Users/YP/Desktop/divvy_tripdata/Important/202010-divvy-tripdata.csv")
df <- bind_rows(df, data4)
remove(data4)
data5 <- read.csv("C:/Users/YP/Desktop/divvy_tripdata/Important/202011-divvy-tripdata.csv")
df <- bind_rows(df, data5)
remove(data5)
data6 <- read.csv("C:/Users/YP/Desktop/divvy_tripdata/Important/202012-divvy-tripdata.csv")
df <- bind_rows(df, data6)
remove(data6)
data7 <- read.csv("C:/Users/YP/Desktop/divvy_tripdata/Important/202101-divvy-tripdata.csv")
df <- bind_rows(df, data7)
remove(data7)
data8 <- read.csv("C:/Users/YP/Desktop/divvy_tripdata/Important/202102-divvy-tripdata.csv")
df <- bind_rows(df, data8)
remove(data8)
data9 <- read.csv("C:/Users/YP/Desktop/divvy_tripdata/Important/202103-divvy-tripdata.csv")
df <- bind_rows(df, data9)
remove(data9)
data10 <- read.csv("C:/Users/YP/Desktop/divvy_tripdata/Important/202104-divvy-tripdata.csv")
df <- bind_rows(df, data10)
remove(data10)
data11 <- read.csv("C:/Users/YP/Desktop/divvy_tripdata/Important/202105-divvy-tripdata.csv")
df <- bind_rows(df, data11)
remove(data11)

distance <- vector(, length(df[["start_lat"]]))

for (i in 1:length(df[["start_lat"]])){
  distance[i] <- distHaversine(c(df[["start_lng"]][i], df[["start_lat"]][i]), c(df[["end_lng"]][i], df[["end_lat"]][i]))
}

for (a in 1:length(distance)){
  distance[a] <- round(distance[a], digits = 2)
}

df$distance <- distance

casuals <- vector()
members <- vector()


for (x in 1:length(df[["distance"]])){
  if (df[["member_casual"]][x] == "casual"){
    casuals[length(casuals)+1] <- df[["distance"]][x]
  }
  else{
    members[length(members)+1] <- df[["distance"]][x]
  }
}

casuals <- na.omit(casuals)
members <- na.omit(members)
casuals <- data.frame(casuals)
members <- data.frame(members)
ggplot(data = casuals) + geom_area(stat="bin", bins = 100, mapping = aes(x = casuals))
ggplot(data = members) + geom_area(stat="bin", bins = 100, mapping = aes(x = members))
