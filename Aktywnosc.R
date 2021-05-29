#Tworzenie wykresów porównujących aktywność użhywtkowników forów 
#w oparciu o ilość dodanych postów, pytań, głosów i linków w okreslonym czasie

#Potrzebne biblioteki:
library(stringi)
library(sqldf)
library(GGplot2)

aktywnosc1 <- function(nazwa_religii) {
  #Funckja, która tworzy wykresy aktywności dla jednej religii
  
  #Ustalanie odpowiednich ramek, nie mam pomysłu jak dalej zrobić, żeby
  #jakoś ustalać ramki po nazwie religii, tak żeby przypisać jakimś zmiennym
  #wewnętrznym np Posts, PosLinks, Votes odpowiednie ramki i na nich pracować
  #w funkcji, pomysł do przemyślenia
  przyrostek <- stri_extract_all_regex(nazwa_religii, "[A-Z]")
  
  #Wybranie z ramek rekordów z danego okresu
  P <- Posts[grepl("2015", Posts$CreationDate), ]
  L <- PostLinks[grepl("2015", PostLinks$CreationDate), ]
  V <- Votes[grepl("2015", Votes$CreationDate), ]
  
  #Tworzenie nowej kolumny w ramce, która z daty przechowuje tylko rok i miesiąc
  #lub rok miesiąc i dizeń, aby sprawdzać ilość danej w aktywności w ciągu dnia 
  #lub miesiąca, w zależności jak się ustawi regexpy
  pomP <- P$CreationDate
  pomP <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomP, "2015(-)[0-9]{2}"))))
  colnames(pomP)[1] = "date"
  
  pomL <- L$CreationDate
  pomL <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomL, "2015(-)[0-9]{2}"))))
  colnames(pomL)[1] = "date"
  
  pomV <- V$CreationDate
  pomV <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomV, "2015(-)[0-9]{2}"))))
  colnames(pomV)[1] = "date"
 
  
  #Lączenie danych ramek i utworzonej kolumny "date".
  #W przypadku ramki Posts, rozbicie na dwie odzielne: Questions i Answers
  #Wybór kolumn "date" i jednej dowolnej którą można pogrupować wyniki względem 
  #daty 
  P <- cbind(P, pomP)
  P <- P[c("PostTypeId", "date")]
  rownames(P) <- NULL
  Questions <- P[P$PostTypeId == 1]
  Answers <- P[P$PostTypeId == 2]
  
  Links <- cbind(L, pomL)
  Links <- Links[c("Id", "date")]
  
  Votes <- cbind(V, pomV)
  Votes <- Votes[c("Id", "date")]
  
  #Agregowanie każdej ramki z użyciem kochanego SQL
  Questions <- sqldf("SELECT COUNT(*) AS Questions, date
              FROM questions
              GROUP BY date")
  
  Answers <- sqldf("SELECT COUNT(*) AS Answers, date
              FROM Answers
              GROUP BY date")
  
  Links <- sqldf("SELECT COUNT(*) AS Links, date
              FROM Links
              GROUP BY date")
  
  Votes <- sqldf("SELECT COUNT(*) AS Votes, date
              FROM Votes
              GROUP BY date")
  
  # Łączenie wszystkiego w jedną ramkę po kolumnie "date"
  Final <- merge(
            merge(
              merge(Questions, Answers, by = "date"), 
              Links, by = "date"), 
            Votes, by = "date")
  
  #Według tego co się doczytałem, wykres z wieloma krzywymi ładnie rysuje 
  #ggplot, ale niestety jeszcze nie rozgryzłem jak to robi
  ggplot()
  
  # Można też zapisać te wykresy do pliku, żeby dodawać je po prostu
  # do prezentacji
          
}

aktywnosc2 <- fuction() {
  #Funkcja, która porównuje aktywność wszystkich forów względem jednej
  # "zmiennej"
  # Zastanawiałem się w sumie czy po prostu w tej funkcji nie zrobić wszystkich
  #wykresów na raz, bo nie mam pomysłu jak uzależnić tę funkcje od parametru, 
  #podobnie zresztą w tej pierwszej funkcji
  #Dlatego tutaj tylko porownanie dla Votes
  
  #Postępuję podobnie jak w pierwszej funckji
  
  B <- BVotes[grepl("20.", BVotes$CreationDate), ]
  pomB <- B$CreationDate
  pomB <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomB, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomB)[1] = "date"
  Buddhism <- cbind(B, pomB)
  Buddhism <- Buddhism[c("Id", "date")]
  Buddhism <- sqldf("SELECT COUNT(*) AS Buddhism, date
              FROM Buddhism
              GROUP BY date")
  
  
  C <- CVotes[grepl("20.", CVotes$CreationDate), ]
  pomC <- C$CreationDate
  pomC <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomC, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomC)[1] = "date"
  Christianity <- cbind(C, pomC)
  Christianity <- Christianity[c("Id", "date")]
  Christianity <- sqldf("SELECT COUNT(*) AS Christianity, date
              FROM Christianity
              GROUP BY date")
  
  H <- HVotes[grepl("20.", HVotes$CreationDate), ]
  pomH <- H$CreationDate
  pomH <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomH, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomH)[1] = "date"
  Hinduism <- cbind(H, pomH)
  Hinduism <- Hinduism[c("Id", "date")]
  Hinduism <- sqldf("SELECT COUNT(*) AS Hinduism, date
              FROM Hinduism
              GROUP BY date")
  
  I <- IVotes[grepl("20.", IVotes$CreationDate), ]
  pomI <- I$CreationDate
  pomI <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomI, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomI)[1] = "date"
  Islam <- cbind(I, pomI)
  Islam <- Islam[c("Id", "date")]
  Islam <- sqldf("SELECT COUNT(*) AS Islam, date
              FROM Islam
              GROUP BY date")
  
  J <- JVotes[grepl("20.", JVotes$CreationDate), ]
  pomJ <- J$CreationDate
  pomJ <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomJ, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomJ)[1] = "date"
  Judaism <- cbind(J, pomJ)
  Judaism <- Judaism[c("Id", "date")]
  Judaism <- sqldf("SELECT COUNT(*) AS Judaism, date
              FROM Judaism
              GROUP BY date")
  
  CaloscVotes <- merge(
                    merge(
                      merge(
                        merge(Buddhism, Christianity, by = "date"), 
                               Hinduism, by = "date"), 
                                Islam, by ="date"), 
                                  Judaism, by = "date" )
  
  #Ponownie ggplot, ale nie umiem jeszcze
  ggplot()
  
  
}

