
  
#   TODO 
# - 2 punkt anmiacja od nowa DONE
# 
# - 3 punkt anmiacja IN PROGRESSS
# 
# - posprzątać w kodzie! 
#   - sprawko 
# - spać

#Wczytanie danych z plików .csv


daily_ice_edge <- read.csv(file = 'data/daily_ice_edge.csv')


#Rysowanie wykresów zasięgu pokrywy lodowej dla kolejnych wierszy (dat)


# biblioteka do wykresów
library(plotly)
# przygotowwanie danych do stworzenia wykresów - usunięcie kolmny z datą
daily_ice_edge_df <- as.data.frame(daily_ice_edge)
daily_ice_edge_df <- subset(daily_ice_edge_df , select=-Date)

# wektor kątów
ang = c(0:360)
# WERSJA FUNKCJI UNIEZALEŻNIONA OD DANYCH - przyjmuje dowolne dane w formacie macierzy
# dzięki czemu można jej użyć też później
# zapisuje wyniki w odpowednich katalogach
plotIce <- function(angle, dataSource) {
  row <- dataSource[angle,]
  # delete -
  row <- abs(row)
  row <- as.numeric(row)
  # "odwrócenie" współrzędnych
  row = 90 -row
  # nazwy wykresów
  name <- paste("fig",angle, sep = "_")
  name <- paste(name, "png", sep = ".")
  print(name)
  # stworzenie wykresu
  fig <- plot_ly(
    type = 'scatterpolar',
    mode = 'lines',
    r = row,
    theta = ang,
    mode = 'markers'
  )
  # nazwa dataSource jako string
  dirName = deparse(substitute(dataSource))
  # zapis wykresu do odpowedniego kataogu o odpowedniej nazwie
  orca(fig, paste("charts/", dirName, "/",name, sep=""))
}
```

#Szukanie najmniejszego zasięgu pokrywy lodowej poprzez znalezienie minimum w każdej kolumnie i stworzenie mapy z tych wartości


# wektor na dane
minimalIceRange <- vector()
# wartośc bezwzględna
daily_ice_edge_df_abs <- abs(daily_ice_edge_df)
# przypisanie wartości max (czyli tak naprawdę min ze względu na abs)
for (colNum in 1:ncol(daily_ice_edge_df)) {
  minimalIceRange[colNum] <- max(daily_ice_edge_df_abs[,colNum])
}
minimalIceRange <- as.numeric(minimalIceRange)
# odwórcenie współrzednych
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
# parametry wykresu
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



#MODEL

# przygotowanie zmiennej reprezentującej czas z uwzględnieniem tego, że w pewnym momencie dane są co jeden, a nie dwa dni
TimeSeq_1 <- seq(1,3180, 2)
TimeSeq_2 <- seq(3180,9530+1590-1, 1)
TimeSeq <- c(TimeSeq_1, TimeSeq_2)

# funkcja szukająca modelu dla danego kąta długości geograficznej
findModel <- function(long) {
  angle <- daily_ice_edge[,long+1]
  # 2*pi*t/T
  xc<-cos(2*pi*TimeSeq/366)
  xs<-sin(2*pi*TimeSeq/366)
  fit <- lm(angle~xc+xs)
  
  #plot(TimeSeq,angle, xlim=c(1, 9000))
  #lines(TimeSeq,predict(fit), col="red")
  
  result <- predict(fit)
  return(result)
  
}

# zmienna na wygenerowane modele
models <- matrix(ncol=361, nrow=9530)
for (i in c(0:361)) {
  # wywólanie funkcji modelującej i dopisanie wszystkich modeli do macierzy
  models[,i] <- findModel(i)
}
# konwersja do dataFrame
resModel <- data.frame(models)
# zmiana nazw kolumn na takie reprezentujące kąty od 0 do 360
colnames(resModel) <- c(0:360)


#Rysowanie wymodelwoanego zasięgu


for (i in c(400:402)) {
  plotIce(i, models)
}

for (i in c(400:402)) {
  plotIce(i, daily_ice_edge_df)
}

# funkcja zbierająca wsztskie porpzednie operacje - rysuje zasięg rzeczywisty i model na tym samy wykresie 
plotDoubleIce <- function(dayNumber, dataSource_1, dataSource_2, dates) {
  ang <- c(0:360)
  row_1 <- dataSource_1[dayNumber,]
  row_2 <- dataSource_2[dayNumber,]
  # usunięcie minusów
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
  # stworzenie wykresu
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
  date <- dates[i]
  
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
  
  orca(fig, paste("charts/", dirName, "/",name, sep=""))
  
}



# wektor dat
daily_ice_edge_dates <- daily_ice_edge[,1]
# to string
daily_ice_edge_dates <- as.character(daily_ice_edge_dates)

# generowanie wszystkich klatek filmu - zasięg rzeczywisty + model
# z tego powstanie film przy pomocy ffmpeg
for (i in c(1:9530)) {
  plotDoubleIce(i, daily_ice_edge_df, resModel, daily_ice_edge_dates)
}


#Wykres punktów bez dopasowenego sinusa



# przygotowanie zmiennej reprezentującej czas z uwzględnieniem tego, że w pewnym momencie dane są co jeden, a nie dwa dni
TimeSeq_1 <- seq(1,3180, 2)
TimeSeq_2 <- seq(3180, 9530+1590-1, 1)
TimeSeq <- c(TimeSeq_1, TimeSeq_2)

# funkcja szukająca modelu dla danego kąta długości geograficznej
findModelPlot <- function(long) {
  angle <- daily_ice_edge[,long+1]
  # 2*pi*t/T
  xc<-cos(2*pi*TimeSeq/366)
  xs<-sin(2*pi*TimeSeq/366)
  fit <- lm(angle~xc+xs)
  
  plot(TimeSeq,angle, xlim=c(1, 9000))
  #lines(TimeSeq,predict(fit), col="red")
  
  result <- predict(fit)
  return(result)
  
}

findModelPlot(1)


#Wykres punktów z dopasowanym sinusem


# przygotowanie zmiennej reprezentującej czas z uwzględnieniem tego, że w pewnym momencie dane są co jeden, a nie dwa dni
TimeSeq_1 <- seq(1,3180, 2)
TimeSeq_2 <- seq(3180, 9530+1590-1, 1)
TimeSeq <- c(TimeSeq_1, TimeSeq_2)

# funkcja szukająca modelu dla danego kąta długości geograficznej
findModelPlot <- function(long) {
  angle <- daily_ice_edge[,long+1]
  # 2*pi*t/T
  xc<-cos(2*pi*TimeSeq/366)
  xs<-sin(2*pi*TimeSeq/366)
  fit <- lm(angle~xc+xs)
  
  plot(TimeSeq,angle, xlim=c(1, 9000))
  lines(TimeSeq,predict(fit), col="red")
  
  result <- predict(fit)
  return(result)
  
}

findModelPlot(1)


#PUNKT TRZECI


library(rsm)
library(plotly)

dane <- read.csv("data/daily_ice_edge.csv")

im<-as.matrix(dane[1:183,2:362])
gr<-expand.grid(1:183,1:361)
df<-data.frame(z=as.vector(im),x=gr[,1],y=gr[,2])

fit <- lm(z ~ poly(x, y, degree = 25), data = df)
pred<-predict(fit)
resInterpolation<-matrix(nrow=183,ncol=361,pred)
par(mfrow=c(2,2))

# image(im,yaxt="n",xaxt="n",xlab="dni",ylab="azymuty")
# axis(1, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*366,0))
# axis(2, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*360,0))
#  
# image(res,yaxt="n",xaxt="n",xlab="dni",ylab="azymuty")
# axis(1, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*366,0))
# axis(2, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*360,0))

resInterpolation[,361] <- resInterpolation[,1]
# wektor dat
daily_ice_edge_dates <- daily_ice_edge[,1]
# to string
daily_ice_edge_dates <- as.character(daily_ice_edge_dates)

for (i in c(1:183)) {
  plotDoubleIce(i, daily_ice_edge_df, resInterpolation, daily_ice_edge_dates)
}




#Więcej danych do interpolacji

library(rsm)
library(plotly)

dane <- read.csv("data/daily_ice_edge.csv")

im<-as.matrix(dane[1:1000,2:362])
gr<-expand.grid(1:1000,1:361)
df<-data.frame(z=as.vector(im),x=gr[,1],y=gr[,2])

fit <- lm(z ~ poly(x, y, degree = 25), data = df)
pred<-predict(fit)
resInterpolation<-matrix(nrow=500,ncol=361,pred)
par(mfrow=c(2,2))

# image(im,yaxt="n",xaxt="n",xlab="dni",ylab="azymuty")
# axis(1, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*366,0))
# axis(2, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*360,0))
#  
# image(res,yaxt="n",xaxt="n",xlab="dni",ylab="azymuty")
# axis(1, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*366,0))
# axis(2, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*360,0))

resInterpolation[,361] <- resInterpolation[,1]
# wektor dat
daily_ice_edge_dates <- daily_ice_edge[,1]
# to string
daily_ice_edge_dates <- as.character(daily_ice_edge_dates)

for (i in c(1:183)) {
  plotDoubleIce(i, daily_ice_edge_df, resInterpolation, daily_ice_edge_dates)
}



library(rsm)

dane <- read.csv("data/daily_ice_edge.csv")


startDayNumber <- 1
dayNumber <- 183
resInterpolation <- matrix( , nrow = 0, ncol = 361)

for (i in c(1:9)) {
  
  interval <- 183
  
  if(i == 9) {
    # po 1590 dane sa co jeden dzien
    dayNumber <- dayNumber - 57
    interval <- 126
  }
  
  im<-as.matrix(dane[startDayNumber:dayNumber,2:362])
  gr<-expand.grid(1:interval,1:361)
  df<-data.frame(z=as.vector(im),x=gr[,1],y=gr[,2])
  
  fit <- lm(z ~ poly(x, y, degree = 25), data = df)
  pred<-predict(fit)
  resTemp<-matrix(nrow=interval,ncol=361,pred)
  
  print(i)
  print(startDayNumber)
  print(dayNumber)
  
  #View(resInterpolation)
  #View(resTemp)
  
  resInterpolation <- rbind(resInterpolation, resTemp)
  
  
  startDayNumber <- startDayNumber + 183
  dayNumber <- dayNumber + 183
  
  
  
}

View(resInterpolation)

par(mfrow=c(1,2))

im<-as.matrix(dane[1:1590,2:362])




#############
resInterpolation_1_1590 <- resInterpolation
##############

############# oD 1590 wiersza, dane co jeden dzien:

startDayNumber <- 1590 + 1
dayNumber <- 1590 + 365

for (i in c(1:22)) {
  
  interval <- 365
  
  if(i == 22) {
    # po 1590 dane sa co jeden dzien
    dayNumber <- dayNumber - 90 
    interval <- 275
    print(dayNumber)
    print(interval)
  }
  
  im<-as.matrix(dane[startDayNumber:dayNumber,2:362])
  gr<-expand.grid(1:interval,1:361)
  df<-data.frame(z=as.vector(im),x=gr[,1],y=gr[,2])
  
  fit <- lm(z ~ poly(x, y, degree = 25), data = df)
  pred<-predict(fit)
  resTemp<-matrix(nrow=interval,ncol=361,pred)
  
  print(i)
  print(startDayNumber)
  print(dayNumber)
  
  #View(resInterpolation)
  #View(resTemp)
  
  resInterpolation <- rbind(resInterpolation, resTemp)
  
  
  startDayNumber <- startDayNumber + 365
  dayNumber <- dayNumber + 365
  
}

im<-as.matrix(dane[1:9530,2:362])


# wizualizacja modelu
# View(resInterpolation)
# image(im,yaxt="n",xaxt="n",xlab="dni",ylab="azymuty")
# axis(1, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*9530,0))
# axis(2, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*360,0))
# 
# image(resInterpolation,yaxt="n",xaxt="n",xlab="dni",ylab="azymuty")
# axis(1, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*9530,0))
# axis(2, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*360,0))

# eksport
#write.csv(resInterpolation,"results/interpolation/resInterpolation.csv", row.names = FALSE)



for (i in c(1:183)) {
  plotDoubleIce(i, daily_ice_edge_df, resInterpolation, daily_ice_edge_dates)
}
