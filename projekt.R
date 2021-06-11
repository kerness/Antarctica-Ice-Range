###################################################################

#     ANTARCTICA ICE RANGE
#     Modelowanie w naukach o Ziemi AGH 2021 - projekt zaliczeniowy 
#     Data ukończenia: 11.06.2021
#     Autorzy: Maciej Bąk, Hubert Długosz, Filip Giermek

###################################################################

# zapis wykresów plotly: https://github.com/plotly/orca#installation

library(plotly)
library(rsm)
library(sp)
library(tidyr)

# Wczytanie danych dotyczących zasięgu lodu w obszarze Antarktydy
# Dane to kolejne długości geograficzne w kolumnach i odpowiadające im szerokości geograficzne dla kolejnych dat
data <- read.csv(file = 'data/daily_ice_edge.csv')

# ------------------------------------------------------------------------
# RYSOWANIE WYKRESÓW ZASIĘGU POKRYWY LODOWEJ DLA KOLEJNYCH WIERSZY (DAT)
# ------------------------------------------------------------------------
# przygotowwanie danych do stworzenia wykresów - usunięcie kolmny z datą
data_df <- as.data.frame(data)
data_df <- subset(data_df , select=-Date)

# wektor zawierający kąty długości geograficznej
ang = c(0:360)

# plotIce - rysuje wykres biegunowy zasięgu lodu dla danej daty
# datę przyjmuje jako liczbę typu int
plotIce <- function(dayNumber, dataSource) {
  row <- dataSource[dayNumber,]
  row <- abs(row)
  row <- as.numeric(row)
  # "odwrócenie" współrzędnych
  row = 90 -row

  name <- paste("fig",dayNumber, sep = "_")
  name <- paste(name, "png", sep = ".")
  print(name)
  
  fig <- plot_ly(
    type = 'scatterpolar',
    mode = 'lines',
    r = row,
    theta = ang,
    mode = 'markers'
  )
  dirName = deparse(substitute(dataSource))
  # zapis wykresu do odpowiedniego kataogu o nazwie takiej jak dataSource
  orca(fig, paste("charts/", dirName, "/",name, sep=""))
}
# ------------------------------------------------------------------------




# ------------------------------------------------------------------------
# SZUKANIE NAJMNIEJSZEGO ZASIĘGU POKRYWY LODOWEJ POPRZEZ ZNALEZIENIE MINIMUM W KAŻDEJ KOLUMNIE
# ------------------------------------------------------------------------
minimalIceRange <- vector()
data_df_abs <- abs(data_df)
# przypisanie wartości max (czyli tak naprawdę min ze względu na abs)
for (colNum in 1:ncol(data_df)) {
  minimalIceRange[colNum] <- max(dane_df_abs[,colNum])
}
minimalIceRange <- as.numeric(minimalIceRange)
# "odwórcenie" współrzednych
minimalIceRange <- 90 - minimalIceRange
# wektor kątów
ang = c(0:360)
# stworzenie wykresu
fig <- plot_ly(
  type = 'scatterpolar',
  mode = 'lines',
  r = minimalIceRange,
  theta = ang,
  mode = 'markers'
)

fig <- fig %>%
  layout(polar = list(angularaxis = list(rotation = 90, direction  = 'clockwise')))
# margines dla tytułu
mrg <- list(l = 50, r = 50, b = 50, t = 50, pad = 20)
fig <- fig %>%
  layout(title = "Minimalny zasięg pokrywy lodowej Antarktydy dla lat 1978-2009",
         xaxis = list(title = ""),
         yaxis = list(title = ""),
         margin = mrg)

# zapis
orca(fig, 'charts/minimalIceRange.png')
# ------------------------------------------------------------------------




# ------------------------------------------------------------------------
# MODEL
# ------------------------------------------------------------------------
# przygotowanie zmiennej reprezentującej czas 
TimeSeq_1 <- seq(1,3180, 2)
# uwzględnienienie tego, że w pewnym momencie dane są co jeden, a nie dwa dni
TimeSeq_2 <- seq(3180,9530+1590-1, 1)
TimeSeq <- c(TimeSeq_1, TimeSeq_2)

# funkcja szukająca modelu dla danego kąta długości geograficznej
findModel <- function(long, data) {
  angle <- data[,long+1]
  # 2*pi*t/T
  xc<-cos(2*pi*TimeSeq/366)
  xs<-sin(2*pi*TimeSeq/366)
  # dopasowanie modelu
  fit <- lm(angle~xc+xs)
  
  # wykres z modelem i danymi
  plot(TimeSeq,angle, xlim=c(1, 9000))
  lines(TimeSeq,predict(fit), col="red")
  
  result <- predict(fit)
  return(result)
}

# zmienna na wygenerowane modele
models <- matrix(ncol=361, nrow=9530)
for (i in c(0:361)) {
  # wywoływanie funkcji modelującej i dopisanie wszystkich modeli do macierzy
  models[,i] <- findModel(i, data)
}

resModel <- data.frame(models)
# zmiana nazw kolumn na takie reprezentujące kąty od 0 do 360
colnames(resModel) <- c(0:360)

# Sprawdzenie poprawności wygenerowanego modelu i rysowania danych rzeczywistych
i <- 1
plotIce(i, models)
plotIce(i, data_df)
# ------------------------------------------------------------------------




# ------------------------------------------------------------------------
# FUNKCJA RYSUJĄCA ZASIĘG RZECZYWISTY I MODEL NA JEDNYM WYKRESIE POLARNYM
# ------------------------------------------------------------------------
# funkcja zbierająca wszystkie poprzednie operacje - rysuje zasięg rzeczywisty i model na tym samym wykresie 
# dayNumber - data jako int; dataSource1 to zasięg rzeczywisty; dataSource2 zasięg wymodelowany; dates - wektor z datami 26-Oct-1978 
plotDoubleIce <- function(dayNumber, dataSource_1, dataSource_2, dates) {
  ang <- c(0:360)
  row_1 <- dataSource_1[dayNumber,]
  row_2 <- dataSource_2[dayNumber,]
  row_1 <- abs(row_1)
  row_2 <- abs(row_2)
  row_1 <- as.numeric(row_1)
  row_2 <- as.numeric(row_2)
  # "odwrócenie" współrzędnych
  row_1 = 90 -row_1
  row_2 = 90 -row_2
  # nazwy wykresów 
  name <- paste("fig", dayNumber, sep = "_")
  name <- paste(name, "png", sep = ".")
  print(name)

  fig_1 <- plot_ly(
    type = 'scatterpolar',
    mode = 'lines',
    r = row_1,
    theta = ang,
    mode = 'markers',
    name = 'Zasięg rzeczywisty'
  )
  
  fig_2 <- plot_ly(
    type = 'scatterpolar',
    mode = 'lines',
    r = row_2,
    theta = ang,
    mode = 'markers',
    name = 'Zasięg wymodelowany'
  )
  
  fig <- subplot(fig_1, fig_2)

  # range - zakres osi poziomej
  fig <- fig %>%
    layout(
      polar = list(
        angularaxis = list(
          rotation = 90,
          direction  = 'clockwise'),
        radialaxis = list(
          range = c(0,40)
        )
      )
    )
  
  currentDate <- dates[i]
  
  fig <- fig %>% 
    layout(
      legend=list(
        title=list(
          text=paste("<b>", currentDate, "</b>", sep="")
        )
      )
    )
  
  dirName <- deparse(substitute(dataSource_2))
  # orca zapisuje rezultat na dysku 
  orca(fig, paste("charts/", dirName, "/",name, sep=""))
}


# użycie funkcji poltDoubleIce dla modelu opartego na funkcjach trygonometrycznych
data_dates <- data[,1]
data_dates <- as.character(data_dates)
# generowanie wszystkich klatek filmu - zasięg rzeczywisty + model
# z tego powstanie film przy pomocy ffmpeg
for (i in c(1:9530)) {
  plotDoubleIce(i, data_df, resModel, data_dates)
}
# ------------------------------------------------------------------------




# ------------------------------------------------------------------------
# WYKRESY MODELU Z FUNKCJAMI TRYGONOMETRYCZNYMI
# ------------------------------------------------------------------------
# przygotowanie zmiennej reprezentującej czas z uwzględnieniem tego, że w pewnym momencie dane są co jeden, a nie dwa dni
TimeSeq_1 <- seq(1,3180, 2)
TimeSeq_2 <- seq(3180, 9530+1590-1, 1)
TimeSeq <- c(TimeSeq_1, TimeSeq_2)

# funkcja szukająca modelu dla danego kąta długości geograficznej i rysuje wykres 
findModelPlot  <- function(long, data) {
  angle <- data[,long+1]
  # 2*pi*t/T
  xc<-cos(2*pi*TimeSeq/366)
  xs<-sin(2*pi*TimeSeq/366)
  fit <- lm(angle~xc+xs)
  
  plot(TimeSeq,angle, xlim=c(1, 9000))
  lines(TimeSeq,predict(fit), col="red")
  
  result <- predict(fit)
  return(result)
  
}

findModelPlot (1, data)
# ------------------------------------------------------------------------




# ------------------------------------------------------------------------
# MODEL ROZSZERZONY ZASIĘGU LODU
# ------------------------------------------------------------------------
startDayNumber <- 1
dayNumber <- 183
resInterpolation <- matrix( , nrow = 0, ncol = 361)

# modelowanie pierwszej częsci zestawu danych gdzie dane występują z dwudniowym interwałem
# zmiana interwału następuje w 9 roku zakresu danych
# dane modelowane są dla każdego roku z osobna - jedna iteracja to jeden rok

for (i in c(1:9)) {
  
  interval <- 183
  
  # po 1590 wierszu (9 iteracja) dane są co jeden dzień
  if(i == 9) {
    # żeby nie wyjść poza dwudniowy interwał
    dayNumber <- dayNumber - 57
    interval <- 126
  }
  
  actualData<-as.matrix(data[startDayNumber:dayNumber,2:362])
  grid<-expand.grid(1:interval,1:361)
  dataGrid<-data.frame(z=as.vector(actualData),x=grid[,1],y=grid[,2])
  # dopasowanie modelu: wielomian dwóch zmiennych stopnia 25
  fit <- lm(z ~ poly(x, y, degree = 25), data = dataGrid)
  pred<-predict(fit)
  resTemp<-matrix(nrow=interval,ncol=361,pred)
  
  print(i)
  
  resInterpolation <- rbind(resInterpolation, resTemp)
  # przejście do następnego roku
  startDayNumber <- startDayNumber + 183
  dayNumber <- dayNumber + 183
}
# backup ;)
resInterpolation_1_1590 <- resInterpolation


# od 1590 wiersza, dane co jeden dzien
startDayNumber <- 1590 + 1
dayNumber <- 1590 + 365
# 22 lata = 22 iteracje
for (i in c(1:22)) {
  
  interval <- 365
  
  # w 22 iteracji kończą się dane
  if(i == 22) {
    # żeby nie wyjść poza dane
    dayNumber <- dayNumber - 90 
    interval <- 275
  }
  
  actualData<-as.matrix(data[startDayNumber:dayNumber,2:362])
  grid<-expand.grid(1:interval,1:361)
  dataGrid<-data.frame(z=as.vector(actualData),x=grid[,1],y=grid[,2])
  # dopasowanie modelu: wielomian dwóch zmiennych stopnia 25
  fit <- lm(z ~ poly(x, y, degree = 25), data = dataGrid)
  pred<-predict(fit)
  resTemp<-matrix(nrow=interval,ncol=361,pred)
  
  resInterpolation <- rbind(resInterpolation, resTemp)
  
  # przejście do następnego roku
  startDayNumber <- startDayNumber + 365
  dayNumber <- dayNumber + 365
}
# ------------------------------------------------------------------------




# ------------------------------------------------------------------------
# RYSOWANIE OBRAZU (REPREZENTACJI GRAFICZNEJ) DANYCH I MODELU
# ------------------------------------------------------------------------
actualData<-as.matrix(data[1:9530,2:362])

par(mfrow=c(1,2))
image(actualData,yaxt="n",xaxt="n",xlab="czas",ylab="azymuty")
axis(1, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*9530,0))
axis(2, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*360,0))

image(resInterpolation,yaxt="n",xaxt="n",xlab="czas",ylab="azymuty")
axis(1, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*9530,0))
axis(2, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*360,0))
# ------------------------------------------------------------------------




# ------------------------------------------------------------------------
# RYSOWANIE MODELU ROZSZERZONEGO I ZAPIS DANYCH
# ------------------------------------------------------------------------
for (i in c(1:9530)) {
  plotDoubleIce(i, data_df, resInterpolation, daily_ice_edge_dates)
}

write.csv(resInterpolation,"results/interpolation/resInterpolation.csv", row.names = FALSE)
# ------------------------------------------------------------------------