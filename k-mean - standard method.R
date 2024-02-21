
### macierz wejściowa

data <- matrix(sample(-100:100, 150, replace = TRUE) ,nrow = 30, ncol = 5, byrow = TRUE) 
# jest to przykładowa seria danych do naszego programu z losowymi liczbami z przedziału [-100; 100]

### liczba naszych grup

k <- 3 
# na obecną chwilę za zmienna k (określająca ilość naszych grup) podaję liczbę całkowitą

### funkcja standaryzująca macierz wejściową

standard <- function(x){
  
  st <- c()
  mn <- c()
  
  for (i in 1:ncol(x)){
    st[i] <- sd(x[,i])
    mn[i] <- mean(x[,i])
  }
  
  sdata <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  for (c in 1:ncol(x)){
    sdata[,c] <- (x[,c] - mn[c])/st[c]
  }
  return(sdata)
} 
# funkcja standard(x) standaryzuje zbiór danych według wzoru (wartość[r, c] - odchylenie standardowe[, c])/(średnia[, c])

standdata <- standard(data)

### wstępne zdefiniowanie macierzy zawierającej współrzędne środków naszych grup

kpoint <- matrix(0, nrow = k, ncol = ncol(standdata))
codecol <- matrix(1:k, nrow = k, ncol = 1)
kpoint <- cbind(kpoint, codecol) 
# definiuję macierz zerową z dodatkową ostatnią kolumną zawierającą indeks każdej z k-średnich 
# (jest to kolumna z wartościami numerycznymi dla łatwiejszego filtrowania w kolejnych krokach) 

### losowanie wartości współrzędnych (aby nico przyspieszyć ten proces, liczby będą zawierać się w przedziale [min, max] dla każdej cechy)

mn <- c()
mx <- c()


for (i in 1:ncol(standdata)){
  mn[i] <- min(standdata[,i])
  mx[i] <- max(standdata[,i])
}


for (c in 1:(ncol(kpoint) - 1)) {
  for (r in 1:nrow(kpoint)){
    kpoint[r,c] <- runif(1, min = mn[c], max = mx[c])
  }} 
# z rozkładu jednostajnego losuję liczby z przedziału [min; max], w którym zawierać się będą początkowo wylosowane współrzędne
# (jest to ułatwienie mojego procesu, jeśli jest to błędne to zmienię)


### stworzenie drugiej macierzy wyjściowej (pierwsza to kpoint), początkowo zawiera tylko numer obiektu w pierwszej kolumnie, ostania kolumna jest przeznaczona na kod grupy

mout <- cbind(matrix(1:nrow(standdata), byrow = TRUE, ncol = 1, nrow = nrow(standdata)),
              matrix(0, ncol = (ncol(standdata) + 1), nrow = nrow(standdata)))

### pierwszy punkt właściwej procedury k-średnich

dist <- c()
md <- matrix(0, ncol = ncol(standdata), nrow = nrow(standdata))

for (r in 1:nrow(standdata)){
  for (c in 1:nrow(kpoint)){
    dist[c] <- sqrt(sum((standdata[r,] - kpoint[c,-ncol(kpoint)])^2))
    index <- which.min(dist)
    md[r, ] <- dist[index]
    mout[r,ncol(mout)] <- kpoint[index,ncol(kpoint)]
  }
  mout[r,2:(ncol(mout) -1)] <- md[r,]
} 
# liczę odległość każdego elementu zestandaryzowanej macierzy do odpowiednich współrzędnych kpoint, najkrótsza odległość zostaje przypisana do macierzy mout i nadajemy jej kod najblizszej grupy

### drugi punkt właściwej procedury k-średnich 

XDist <- matrix(0, nrow = nrow(standdata), ncol = ncol(standdata)) 
# macierz zerowa będzie ona nadpisywana początkowymi wartościami mout (wymagana do warunku przerwania pętli while (różnica między XDist a mout równa zero))
kpoint2 <- kpoint 
# tworzę dodatkową zmienna kpoint2 aby na końcu porównać jak zmieniły się współrzędne 
nsteps <- 0 
# ta zmienna powie nam po ilu iteracjach przerwana została pętla while

while (sum(mout[,-c(1, ncol(mout))] - XDist) != 0) {

  XDist <- mout[,-c(1, ncol(mout))]
  indexes <- mout[,ncol(mout)]

  for (i in 1:nrow(kpoint)){
    for (c in 1:ncol(standdata)){
      filter <- which(indexes == i)
      kpoint2[i,c] <- mean(standdata[filter, c]) # w tym miejscu nie jestem pewny co w sytuacji, gdy np. jednej z k-średnich nie będzie w początkowym etapie 
    }
  } 
  # filtrując daną grupę, liczę średnią arytmetyczną dla każdych współrzędnych w kolumnie i nadpisuję kpoint2 z nowymi współrzędnymi

### trzeci i czwarty punkt właściwej procedury k-średnich

  dist2 <- c()

  for (r in 1:nrow(standdata)){
    for (i in 1:nrow(kpoint2)){
      dist2[i] <- sqrt(sum((standdata[r,] - kpoint2[i, -ncol(kpoint2)]) ** 2))
      if (mout[r,2] > min(dist2)){
        mout[r, ncol(mout)] <- which.min(dist2)
        mout[r,2:(ncol(mout) - 1)] <- min(dist2)
      }
    }
  } 
  # sprawdzam czy odległość każdego z obiektów między standdata i kpoint2 jest większa czy mniejsza niż wcześniej policzona odległość w mout, jeśli jest mniejsza to przesuwam do nowej grupy
  
  nsteps <- nsteps + 1
  print(nsteps)
}

### piąty etap właściewj procedury k-średnich to zamknięcie punktów 2-4 w pętli while 

### wynikiem naszego programu są dwie macierze:

#   - mout (pierwsza kolumna to numer obiektu ,
#           następnie m kolumn to powtarzająca się w kolumnach 2:(m+1) odleglość danego punktu standdata do najbliższych współrzędnych grupy,
#           ostatnia kolumna to indeks grupy do której należy dany obiekt)

#   - kpoint2 (pierwsze m kolumn to współrzędne k-średniej,
#              ostatnia kolumna to indeks każdej z k-średnich)

mout
kpoint2








