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

tabela_miasto$hasBalcony <- ifelse(tabela_miasto$hasBalcony == "yes", 1, 0)


par(mfrow=c(1,1))
hist(tabela_miasto$price,
     main = "Rozkład cen mieszkań",
     xlab = "Cena",
     col = "lightblue",
     border = "white")

# -----------------------------------------------------------------
## STATYSTYKI OPISOWE
# średnia, mediana, minimum, maksimum, odchylenie standardowe, skośność
# ----------------------------------------------------------------

# jeśli trzeba, odkomentuj:
# install.packages("moments")
# install.packages("knitr")

library(moments)
library(knitr)

zmienne_numeryczne <- tabela_miasto[, c("price", "squareMeters", "rooms",
                                        "poiCount", "centreDistance", "hasBalcony")]

statystyki <- data.frame(
  Srednia = round(sapply(zmienne_numeryczne, mean, na.rm = TRUE), 2),
  Mediana = round(sapply(zmienne_numeryczne, median, na.rm = TRUE), 2),
  Minimum = round(sapply(zmienne_numeryczne, min, na.rm = TRUE), 2),
  Maksimum = round(sapply(zmienne_numeryczne, max, na.rm = TRUE), 2),
  OdchylenieStandardowe = round(sapply(zmienne_numeryczne, sd, na.rm = TRUE), 2),
  Skosnosc = round(sapply(zmienne_numeryczne, skewness, na.rm = TRUE), 2)
)

print(statystyki)

# druk do tabeli w latexie 
kable(statystyki, format = "latex", booktabs = TRUE,
      caption = "Statystyki opisowe zmiennych wykorzystanych w analizie", format.args = list(scientific = FALSE, big.mark = " "))

# -----------------------------------------------------------------
## PODSTAWOWA WIZUALIZACJA
# 
# ----------------------------------------------------------------


png("report/podstawowa_wizualizacja_price.png", width = 1000, height = 700, pointsize = 20)
hist(zmienne_numeryczne$price,
     main = "",
     xlab = "Cena",
     ylab = "Liczebność")
dev.off()

png("report/podstawowa_wizualizacja_squareMeters.png", width = 1000, height = 700, pointsize = 20)
hist(zmienne_numeryczne$squareMeters,
     main = "",
     xlab = "Powierzchnia [m²]",
     ylab = "Liczebność")
dev.off()

png("report/podstawowa_wizualizacja_rooms.png", width = 1000, height = 700, pointsize = 20)
barplot(table(zmienne_numeryczne$rooms),
        main = "",
        xlab = "Liczba pokoi",
        ylab = "Liczebność")
dev.off()

png("report/podstawowa_wizualizacja_poiCount.png", width = 1000, height = 700, pointsize = 20)
hist(zmienne_numeryczne$poiCount,
     main = "",
     xlab = "Liczba punktów zainteresowania",
     ylab = "Liczebność")
dev.off()

png("report/podstawowa_wizualizacja_centreDistance.png", width = 1000, height = 700, pointsize = 20)
hist(zmienne_numeryczne$centreDistance,
     main = "",
     xlab = "Odległość od centrum",
     ylab = "Liczebność")
dev.off()

png("report/podstawowa_wizualizacja_hasBalcony.png", width = 1000, height = 700, pointsize = 20)
barplot(table(zmienne_numeryczne$hasBalcony),
        names.arg = c("nie", "tak"),
        main = "",
        xlab = "Balkon",
        ylab = "Liczebność")
dev.off()


## USUNIĘCIE DANYCH Z WYNAJMÓW

tabela_miasto <- tabela_miasto[tabela_miasto$price >= 255000, ]


## USUNIECIE WARTOSCI ODSTAJACYCH
# -------------------------------
# KWANTYLE
# -------------------------------
kwantyle <- lapply(zmienne_numeryczne, function(x) {
  quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
})

print(kwantyle)

# -------------------------------
# FUNKCJA IQR
# -------------------------------
usun_outliery <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  dolna <- Q1 - 1.5 * IQR
  gorna <- Q3 + 1.5 * IQR
  
  x[x < dolna | x > gorna] <- NA
  return(x)
}

# -------------------------------
# USUWANIE OBSERWACJI ODSTAJĄCYCH
# -------------------------------

tabela_clean <- tabela_miasto

tabela_clean$price <- usun_outliery(tabela_clean$price)
tabela_clean$squareMeters <- usun_outliery(tabela_clean$squareMeters)
tabela_clean$rooms <- usun_outliery(tabela_clean$rooms)
tabela_clean$poiCount <- usun_outliery(tabela_clean$poiCount)
tabela_clean$centreDistance <- usun_outliery(tabela_clean$centreDistance)

# usunięcie wierszy z NA
tabela_clean <- na.omit(tabela_clean)

# -------------------------------
# WYKRESY PUDEŁKOWE
# -------------------------------

png("report/boxplot_price.png", width = 1000, height = 700)
boxplot(tabela_miasto$price, main="Cena - przed czyszczeniem")
dev.off()

png("report/boxplot_price_clean.png", width = 1000, height = 700)
boxplot(tabela_clean$price, main="Cena - po usunięciu outlierów")
dev.off()

png("report/boxplot_squareMeters.png", width = 1000, height = 700)
boxplot(tabela_miasto$squareMeters, main="Metraż - przed czyszczeniem")
dev.off()

png("report/boxplot_squareMeters_clean.png", width = 1000, height = 700)
boxplot(tabela_clean$squareMeters, main="Metraż - po usunięciu outlierów")
dev.off()

png("report/boxplot_rooms.png", width = 1000, height = 700)
boxplot(tabela_miasto$rooms, main="Pokoje - przed czyszczeniem")
dev.off()

png("report/boxplot_rooms_clean.png", width = 1000, height = 700)
boxplot(tabela_clean$rooms, main="Pokoje - po usunięciu outlierów")
dev.off()

png("report/boxplot_poiCount.png", width = 1000, height = 700)
boxplot(tabela_miasto$poiCount, main="Liczba punktów zainteresowania - przed czyszczeniem")
dev.off()

png("report/boxplot_poiCoint_clean.png", width = 1000, height = 700)
boxplot(tabela_clean$poiCount, main="Liczba punktów zainteresowania - po usunięciu outlierów")
dev.off()

png("report/boxplot_centreDistance.png", width = 1000, height = 700)
boxplot(tabela_miasto$centreDistance, main="Odległość od centrum - przed czyszczeniem")
dev.off()

png("report/boxplot_centreDistance_clean.png", width = 1000, height = 700)
boxplot(tabela_clean$centreDistance, main="Odległość od centrum - po usunięciu outlierów")
dev.off()


# -------------------------------
# STATYSTYKI PO USUNIĘCIU OUTLIERÓW
# -------------------------------
options(scipen = 999)

zmienne_clean <- tabela_clean[, c("price", "squareMeters", "rooms",
                                  "poiCount", "centreDistance", "hasBalcony")]

statystyki_clean <- data.frame(
  Srednia = round(sapply(zmienne_clean, mean, na.rm = TRUE), 2),
  Mediana = round(sapply(zmienne_clean, median, na.rm = TRUE), 2),
  Minimum = round(sapply(zmienne_clean, min, na.rm = TRUE), 2),
  Maksimum = round(sapply(zmienne_clean, max, na.rm = TRUE), 2),
  OdchylenieStandardowe = round(sapply(zmienne_clean, sd, na.rm = TRUE), 2),
  Skosnosc = round(sapply(zmienne_clean, skewness, na.rm = TRUE), 2)
)

print(statystyki_clean)


kable(statystyki_clean, format = "latex", booktabs = TRUE,
      caption = "Statystyki opisowe po usunięciu obserwacji odstających",
      format.args = list(scientific = FALSE, big.mark = " "))

nrow(tabela_miasto)
nrow(tabela_clean)
print(100*nrow(tabela_clean)/
      nrow(tabela_miasto)
)


zmienne_numeryczne <- tabela_miasto[, c("price", "squareMeters", "rooms",
                                                 "poiCount", "centreDistance", "hasBalcony")]
tinytex::install_tinytex()
library(moments)
library(knitr)

zmienne_numeryczne <- tabela_miasto[, c("price", "squareMeters", "rooms",
                                        "poiCount", "centreDistance", "hasBalcony")]

statystyki <- data.frame(
  Srednia = round(sapply(zmienne_numeryczne, mean, na.rm = TRUE), 2),
  Mediana = round(sapply(zmienne_numeryczne, median, na.rm = TRUE), 2),
  Minimum = round(sapply(zmienne_numeryczne, min, na.rm = TRUE), 2),
  Maksimum = round(sapply(zmienne_numeryczne, max, na.rm = TRUE), 2),
  OdchylenieStandardowe = round(sapply(zmienne_numeryczne, sd, na.rm = TRUE), 2),
  Skosnosc = round(sapply(zmienne_numeryczne, skewness, na.rm = TRUE), 2)
)

print(statystyki)

# druk do tabeli w latexie 
kable(statystyki, format = "latex", booktabs = TRUE,
      caption = "Statystyki opisowe zmiennych wykorzystanych w analizie", format.args = list(scientific = FALSE, big.mark = " "))

png("report/podstawowa_wizualizacja_price_sama_sprzedaz.png", width = 1000, height = 700, pointsize = 20)
hist(zmienne_numeryczne$price,
     main = "",
     xlab = "Cena",
     ylab = "Liczebność")
dev.off()

png("report/podstawowa_wizualizacja_squareMeters_sama_sprzedaz.png", width = 1000, height = 700, pointsize = 20)
hist(zmienne_numeryczne$squareMeters,
     main = "",
     xlab = "Powierzchnia [m²]",
     ylab = "Liczebność")
dev.off()

png("report/podstawowa_wizualizacja_rooms_sama_sprzedaz.png", width = 1000, height = 700, pointsize = 20)
barplot(table(zmienne_numeryczne$rooms),
        main = "",
        xlab = "Liczba pokoi",
        ylab = "Liczebność")
dev.off()

png("report/podstawowa_wizualizacja_poiCount_sama_sprzedaz.png", width = 1000, height = 700, pointsize = 20)
hist(zmienne_numeryczne$poiCount,
     main = "",
     xlab = "Liczba punktów zainteresowania",
     ylab = "Liczebność")
dev.off()

png("report/podstawowa_wizualizacja_centreDistance_sama_sprzedaz.png", width = 1000, height = 700, pointsize = 20)
hist(zmienne_numeryczne$centreDistance,
     main = "",
     xlab = "Odległość od centrum",
     ylab = "Liczebność")
dev.off()

png("report/podstawowa_wizualizacja_hasBalcony_sama_sprzedaz.png", width = 1000, height = 700, pointsize = 20)
barplot(table(zmienne_numeryczne$hasBalcony),
        names.arg = c("nie", "tak"),
        main = "",
        xlab = "Balkon",
        ylab = "Liczebność")
dev.off()

# # ---------------------------------------
# ## STANDARYZACJA
# # ---------------------------------------
# png("report/histogram_price_z_uskokiem.png", width = 1920, height = 1080, res = 300, pointsize = 8)
# 
# par(mfrow = c(1,1), mar = c(5, 4, 1, 1))
# 
# hist(
#   tabela_miasto$price,
#   breaks = c(seq(0, 10000, by = 10000),
#              seq(255000, max(tabela_miasto$price, na.rm = TRUE) + 50000, by = 50000)),
#   freq = FALSE,
#   main = "",
#   xlab = "Cena",
#   ylab = "Gęstość"
# )
# 
# abline(v = 10000, lty = 2)
# abline(v = 255000, lty = 2)
# 
# dev.off()
# 
# 
# # ----------
# 
# library(knitr)
# 
# # 6 obserwacji przed uskokiem
# przed_uskokiem <- tabela_miasto[tabela_miasto$price <= 10000, ]
# przed_uskokiem <- przed_uskokiem[order(przed_uskokiem$price, decreasing = TRUE), ]
# przed_uskokiem <- head(przed_uskokiem, 6)
# 
# # 6 obserwacji po uskoku
# po_uskoku <- tabela_miasto[tabela_miasto$price >= 255000, ]
# po_uskoku <- po_uskoku[order(po_uskoku$price, decreasing = FALSE), ]
# po_uskoku <- head(po_uskoku, 6)
# 
# 
# # połączenie tabel
# tabela_uskok <- rbind(przed_uskokiem, po_uskoku)
# 
# # wygodniejsza kolejność kolumn
# tabela_uskok <- tabela_uskok[, c("price", "squareMeters", "rooms",
#                                  "poiCount", "centreDistance", "hasBalcony")]
# 
# print(tabela_uskok)
# 
# # eksport do LaTeX
# kable(
#   tabela_uskok,
#   format = "latex",
#   booktabs = TRUE,
#   caption = "Przykładowe obserwacje bezpośrednio przed i po uskoku w rozkładzie zmiennej price",
#   format.args = list(scientific = FALSE, big.mark = " ")
# )
# 
# 
# # -------------
# 
# tabela_miasto$price_m2 <- ifelse(
#   tabela_miasto$price_original >= 50000,
#   tabela_miasto$price_original / tabela_miasto$squareMeters,
#   tabela_miasto$price_original
# )
# 
# hist(tabela_miasto$price_m2)
# 
# 
# head(tabela_miasto[order(tabela_miasto$price_m2), 
#                    c("price_original", "squareMeters", "price_m2", "rooms", "centreDistance")], 20)
# 
# 
# 
# 
# png("report/histogram_price_z_podzialem.png", width = 1920, height = 1080, res = 300, pointsize = 8)
# 
# # podział na dwie grupy
# nie_podzielone <- tabela_miasto$price_original[tabela_miasto$price_original < 50000]
# podzielone <- tabela_miasto$price_original[tabela_miasto$price_original >= 50000] / 
#   tabela_miasto$squareMeters[tabela_miasto$price_original >= 50000]
# 
# # wspólne przedziały
# wspolne_breaks <- seq(
#   floor(min(c(nie_podzielone, podzielone), na.rm = TRUE) / 1000) * 1000,
#   ceiling(max(c(nie_podzielone, podzielone), na.rm = TRUE) / 1000) * 1000,
#   by = 1000
# )
# 
# hist(
#   nie_podzielone,
#   breaks = wspolne_breaks,
#   col = rgb(0, 0, 1, 0.4),
#   border = "white",
#   main = "",
#   xlab = "Cena za m²",
#   ylab = "Liczebność",
#   xlim = range(wspolne_breaks)
# )
# 
# hist(
#   podzielone,
#   breaks = wspolne_breaks,
#   col = rgb(1, 0, 0, 0.4),
#   border = "white",
#   add = TRUE,
#   xlim = range(wspolne_breaks)
# )
# 
# legend(
#   "topright",
#   legend = c("Niepodzielone", "Podzielone"),
#   fill = c(rgb(0, 0, 1, 0.4), rgb(1, 0, 0, 0.4)),
#   bty = "n"
# )
# dev.off()

# -------------------------------------------------------------------------
## BRAKI DANYCH
# -------------------------------------------------------------------------

colSums(is.na(tabela_miasto[, c("price", "squareMeters", "rooms",
                                "poiCount", "centreDistance", "hasBalcony")]))

# 1. Wybór kolumn
zmienne <- c("price", "squareMeters", "rooms", "poiCount", "centreDistance", "hasBalcony")
dane_temp <- tabela_clean[, zmienne]

# 2. Bezpieczna konwersja na liczby
# Zamieniamy wszystko na tekst, a potem na czynniki i liczby - to zadziała dla TRUE/FALSE oraz "yes"/"no"
dane_temp$hasBalcony <- as.numeric(as.factor(as.character(dane_temp$hasBalcony)))

# Upewniamy się, że wszystko jest numeryczne
dane_numeryczne <- as.data.frame(lapply(dane_temp, function(x) as.numeric(as.character(x))))

# 3. Usuwamy wiersze z brakami danych (NA) - kluczowe PO konwersji
dane_clean <- na.omit(dane_numeryczne)

# 4. Usuwamy kolumny, które mają stałą wartość (zero variance)
# Standaryzacja takich kolumn zawsze wyrzuca NaN
zmiennosc <- sapply(dane_clean, sd)
dane_final <- dane_clean[, zmiennosc > 0]

if(ncol(dane_final) < ncol(dane_clean)) {
  warning("Usunięto kolumny o zerowej zmienności, aby uniknąć błędów NaN.")
}

# 5. Standaryzacja
dane_skalowane <- scale(dane_final)

# 6. Sprawdzenie końcowe - jeśli tu jest TRUE, kmeans zadziała
if(any(is.na(dane_skalowane)) | any(is.infinite(dane_skalowane))) {
  stop("Dane nadal zawierają NA lub Inf. Sprawdź tabelę wejściową.")
}

# 7. Pętla dla metody łokcia
wss <- (nrow(dane_skalowane)-1)*sum(apply(dane_skalowane, 2, var))
for (i in 2:10) {
  set.seed(123)
  wss[i] <- sum(kmeans(dane_skalowane, centers=i, nstart=25)$withinss)
}

# Wykres
plot(1:10, wss, type="b", pch=19, xlab="Liczba klastrów", ylab="WSS")



# 1. Wykonanie ostatecznego k-means dla 3 klastrów
set.seed(123)
wynik_kmeans <- kmeans(dane_skalowane, centers = 3, nstart = 25)

# 2. Stworzenie zmiennej centra_skupien
# Używamy oryginalnych danych (dane_clean), żeby średnie były w złotówkach i metrach, a nie w skali -3 do 3
centra_skupien <- aggregate(dane_clean, 
                            by = list(Klaster = wynik_kmeans$cluster), 
                            FUN = mean)

# 3. Wyświetlenie wyników do wpisania w LaTeX
print(round(centra_skupien, 2))


# 1. Pobieramy reprezentatywną próbkę (100 obserwacji)
# Robimy to na ustandaryzowanych danych, aby wynik był spójny z K-means
set.seed(123)
idx <- sample(1:nrow(dane_skalowane), 100)
probowka <- dane_skalowane[idx, ]

# 2. Obliczamy macierz odległości euklidesowych
d <- dist(probowka, method = "euclidean")

# 3. Wykonujemy klastrowanie hierarchiczne metodą Warda
# TO JEST TWOJE BRAKUJĄCE 'fit_h'
fit_h <- hclust(d, method = "ward.D2")

# 4. Teraz generujemy plik PNG
png("dendrogram.png", width = 800, height = 600)
plot(fit_h, 
     main = "Dendrogram - Analiza hierarchiczna (n=100)", 
     xlab = "Indeks obserwacji", 
     ylab = "Odległość (Wysokość)",
     sub = "Metoda Warda")
# Opcjonalnie: dodaj czerwone ramki pokazujące podział na 3 grupy
rect.hclust(fit_h, k = 3, border = "red")
dev.off()
