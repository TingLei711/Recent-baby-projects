df<-read.csv("C:\\Users\\49109\\OneDrive\\??????\\intermed stats model analytics\\fraudTrain.csv")
summary(df)
amt_df<-df$amt
amt_df[amt_df>quantile(amt_df, 0.1)|amt_df<quantile(amt_df,0.9)]
boxplot(amt_df)
?mode
mode(df$trans_date_trans_time)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
v<-df$trans_date_trans_time
result<-getmode(v)
print(result)

merchant<-df$merchant
print(getmode(merchant))

category<-df$category
print(getmode(category))

amt<-df$amt
print(getmode(amt))


first<-df$first
print(getmode(first))

last<-df$last
print(getmode(last))

library(dplyr)
left_join(df$first,df$last,by="name")

name<-paste(df$first, df$last)
print(getmode(name))

gender<-df$gender
print(getmode(gender))

street<-df$street
print(getmode(street))

city<-df$city
print(getmode(city))

state<-df$state
print(getmode(state))


address<-paste(df$street,df$city,df$state)
print(getmode(address))

zip<-df$zip
print(getmode(zip))

lat<-df$lat
print(getmode(lat))

city_pop<-df$city_pop
print(getmode(city_pop))

job<-df$job
print(getmode(job))

dob<-df$dob
print(getmode(dob))

unitx<-df$unix_time
print(getmode(unitx))

count(getmode(unitx))

cc<-df$cc_num
print(getmode(cc))
getmode(df$cc_num)

getmode(df$merch_lat)

getmode(df$merch_long)

median(df$trans_date_trans_time)

median(df$zip)

median(df$lat)

median(df$long)

median(df$city_pop)

median(df$dob)

median(df$unix_time)

median(df$merch_lat)

median(df$merch_long)

mean(df$trans_date_trans_time)
summary(df$city_pop)

range(df$amt)
range(df$zip)
range(df$lat)
range(df$long)
range(df$city_pop)
range(df$unix_time)
range(df$merch_lat)
range(df$merch_long)

var(df$trans_date_trans_time)
var(df$merchant)
var(df$amt)
sd(df$amt)
var(df$city_pop)
sd(df$city_pop)
mean(df$amt)
sample(df$amt,10,replace=T)
