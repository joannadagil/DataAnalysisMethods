d202308 = read.csv("dane/apartments_pl_2023_08.csv",header=T)
d202309 = read.csv("dane/apartments_pl_2023_09.csv",header=T)
d202310 = read.csv("dane/apartments_pl_2023_10.csv",header=T)
d202311 = read.csv("dane/apartments_pl_2023_11.csv",header=T)
d202312 = read.csv("dane/apartments_pl_2023_12.csv",header=T)
d202401 = read.csv("dane/apartments_pl_2024_01.csv",header=T)
d202402 = read.csv("dane/apartments_pl_2024_02.csv",header=T)
d202403 = read.csv("dane/apartments_pl_2024_03.csv",header=T)
d202404 = read.csv("dane/apartments_pl_2024_04.csv",header=T)
d202405 = read.csv("dane/apartments_pl_2024_05.csv",header=T)
d202406 = read.csv("dane/apartments_pl_2024_06.csv",header=T)

pliki <- dir(path = "dane/", pattern = "\\.csv$", full.names = TRUE)
lista <- lapply(pliki, read.csv, header = TRUE)
d_all <- do.call(rbind, lista)
#head(dane)

#par(mfrow=c(1,1))
par(mfrow=c(3,4))

sort(table(d202308$city), decreasing=TRUE)[1:5]
sort(table(d202309$city), decreasing=TRUE)[1:5]
sort(table(d202310$city), decreasing=TRUE)[1:5]
sort(table(d202311$city), decreasing=TRUE)[1:5]
sort(table(d202312$city), decreasing=TRUE)[1:5]
sort(table(d202401$city), decreasing=TRUE)[1:5]
sort(table(d202402$city), decreasing=TRUE)[1:5]
sort(table(d202403$city), decreasing=TRUE)[1:5]
sort(table(d202404$city), decreasing=TRUE)[1:5]
sort(table(d202405$city), decreasing=TRUE)[1:5]
sort(table(d202406$city), decreasing=TRUE)[1:5]

sort(table(d_all$city), decreasing=TRUE)[1:5]

barplot(sort(table(d202308$city), decreasing=TRUE)[1:5], main="2023/08")
barplot(sort(table(d202309$city), decreasing=TRUE)[1:5], main="2023/09")
barplot(sort(table(d202310$city), decreasing=TRUE)[1:5], main="2023/10")
barplot(sort(table(d202311$city), decreasing=TRUE)[1:5], main="2023/11")
barplot(sort(table(d202312$city), decreasing=TRUE)[1:5], main="2023/12")
barplot(sort(table(d202401$city), decreasing=TRUE)[1:5], main="2024/01")
barplot(sort(table(d202402$city), decreasing=TRUE)[1:5], main="2024/02")
barplot(sort(table(d202403$city), decreasing=TRUE)[1:5], main="2024/03")
barplot(sort(table(d202404$city), decreasing=TRUE)[1:5], main="2024/04")
barplot(sort(table(d202405$city), decreasing=TRUE)[1:5], main="2024/05")
barplot(sort(table(d202406$city), decreasing=TRUE)[1:5], main="2024/06")

barplot(sort(table(d_all$city), decreasing=TRUE)[1:5], main="2023/08-2024/06")
#hist(X1,main="jakis podpis",xlab="cos podpisac",col=2)

# tabela z wybranymi danymi z wybranego miasta
tabela_miasto <- subset(
  d_all,
  city == "krakow",
  select = c(city, price, squareMeters, rooms, poiCount, centreDistance, hasBalcony)
)

# podgląd tabeli
View(tabela_miasto)

hist(tabela_miasto$price,
     main = "Rozkład cen mieszkań",
     xlab = "Cena",
     col = "lightblue",
     border = "white")
