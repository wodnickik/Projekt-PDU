#Tworzenie wykresów porównujących aktywność użhywtkowników forów 
#w oparciu o ilość dodanych postów, pytań, głosów i linków w okreslonym czasie
# Funkcje nic nie zwracają, ale zapisują odpowiednie ramki danych i mapki 
# do plików, więc przy wykonywaniu trzeba uważać gdzie się zapisują i czy
# przypadkiem nie dodajemy je na repozytorium

religion_colors <- c("#FF8133", "#FFE900", "#0094C6", "#1A6318", "#89043D")

#Potrzebne biblioteki:
library(stringi)
library(sqldf)
library(ggplot2)

aktywnosc_buddhsim <- function() {
  #Funkcja, która tworzy wykresy aktywności dla jednej religii

  #Wybranie z ramek rekordów z danego okresu
  P <- BPosts[grepl("20.", BPosts$CreationDate), ]
  L <- BPostLinks[grepl("20.", BPostLinks$CreationDate), ]
  V <- BVotes[grepl("20.", BVotes$CreationDate), ]
  C <- BComments[grepl("20.", BComments$CreationDate), ]
  
  #Tworzenie nowej kolumny w ramce, która z daty przechowuje tylko rok i miesiąc
  #lub rok miesiąc i dzień, aby sprawdzać ilość danej w aktywności w ciągu dnia 
  #lub miesiąca, w zależności jak się ustawi regexpy
  pomP <- P$CreationDate
  pomP <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomP, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomP)[1] <- "date"
  
  pomL <- L$CreationDate
  pomL <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomL, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomL)[1] <- "date"
  
  pomV <- V$CreationDate
  pomV <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomV, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomV)[1] <- "date"
  
  pomC <- C$CreationDate
  pomC <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomC, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomC)[1] <- "date"
 
  
  #Łączenie danych ramek i utworzonej kolumny "date".
  #W przypadku ramki Posts, rozbicie na dwie oddzielne: Questions i Answers
  #Wybór kolumn "date" i jednej dowolnej którą można pogrupować wyniki względem 
  #daty 
  P <- cbind(P, pomP)
  P <- P[c("PostTypeId", "date")]
  rownames(P) <- NULL
  Questions <- P[P$PostTypeId == 1, ]
  Answers <- P[P$PostTypeId == 2, ]
  
  Links <- cbind(L, pomL)
  Links <- Links[c("Id", "date")]
  
  Votes <- cbind(V, pomV)
  Votes <- Votes[c("Id", "date")]
  
  Comments <- cbind(C, pomC)
  Comments <- Comments[c("Id", "date")]
  
  #Agregowanie każdej ramki z użyciem kochanego SQL
  Questions <- sqldf("SELECT COUNT(*) AS questions, date
              FROM Questions
              GROUP BY date")
  
  Answers <- sqldf("SELECT COUNT(*) AS answers, date
              FROM Answers
              GROUP BY date")
  
  Links <- sqldf("SELECT COUNT(*) AS links, date
              FROM Links
              GROUP BY date")
  
  Votes <- sqldf("SELECT COUNT(*) AS votes, date
              FROM Votes
              GROUP BY date")
  
  Comments <- sqldf("SELECT COUNT(*) AS comments, date
              FROM Comments
              GROUP BY date")
  
  # Łączenie wszystkiego w jedną ramkę po kolumnie "date"
  Final <- merge(
            merge(
              merge(
                merge(Questions, Answers, by = "date"), 
                Links, by = "date"), 
              Votes, by = "date"),
            Comments, by = "date")
  
  #Zapis ramki do pliku 
  write.csv(Final, "dane/posrednie/Buddhism_Akt.csv")
  
  #Rysowanie wykresu 
  ggplot(data = Final, aes(x = date)) + 
    scale_color_manual(values = religion_colors) +
    geom_line(aes(y = questions, color = "Pytania", group = 1)) +
    geom_line(aes(y = answers, color = "Odpowiedzi", group = 1)) +
    geom_line(aes(y = votes, color = "Głosy", group = 1)) +
    geom_line(aes(y = links, color = "Linki", group = 1)) +
    geom_line(aes(y = comments, color = "Komentarze", group = 1)) +
    labs(x = "Okres", angle = 90, y = "Liczba", 
         title = "Wykres różnej aktywności od czasu dla forum buddhism.stackexchange.com") +
    theme(axis.text.x = element_text(colour = 'black', size = 8, angle = 90))
  
  #Zapis wykresu
  ggsave("prezentacja/Wykresy/wyk_akt_B.png", width=60, height = 20, units = "cm")
  
  invisible(NULL)
          
}

aktywnosc_christianity <- function() {
  #Funkcja, która tworzy wykresy aktywności dla jednej religii
  
  #Wybranie z ramek rekordów z danego okresu
  P <- CPosts[grepl("20.", CPosts$CreationDate), ]
  L <- CPostLinks[grepl("20.", CPostLinks$CreationDate), ]
  V <- CVotes[grepl("20.", CVotes$CreationDate), ]
  C <- CComments[grepl("20.", CComments$CreationDate), ]
  
  #Tworzenie nowej kolumny w ramce, która z daty przechowuje tylko rok i miesiąc
  #lub rok miesiąc i dzień, aby sprawdzać ilość danej w aktywności w ciągu dnia 
  #lub miesiąca, w zależności jak się ustawi regexpy
  pomP <- P$CreationDate
  pomP <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomP, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomP)[1] <- "date"
  
  pomL <- L$CreationDate
  pomL <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomL, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomL)[1] <- "date"
  
  pomV <- V$CreationDate
  pomV <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomV, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomV)[1] <- "date"
  
  pomC <- C$CreationDate
  pomC <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomC, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomC)[1] <- "date"
  
  
  #Łączenie danych ramek i utworzonej kolumny "date".
  #W przypadku ramki Posts, rozbicie na dwie oddzielne: Questions i Answers
  #Wybór kolumn "date" i jednej dowolnej którą można pogrupować wyniki względem 
  #daty 
  P <- cbind(P, pomP)
  P <- P[c("PostTypeId", "date")]
  rownames(P) <- NULL
  Questions <- P[P$PostTypeId == 1, ]
  Answers <- P[P$PostTypeId == 2, ]
  
  Links <- cbind(L, pomL)
  Links <- Links[c("Id", "date")]
  
  Votes <- cbind(V, pomV)
  Votes <- Votes[c("Id", "date")]
  
  Comments <- cbind(C, pomC)
  Comments <- Comments[c("Id", "date")]
  
  #Agregowanie każdej ramki z użyciem kochanego SQL
  Questions <- sqldf("SELECT COUNT(*) AS questions, date
              FROM Questions
              GROUP BY date")
  
  Answers <- sqldf("SELECT COUNT(*) AS answers, date
              FROM Answers
              GROUP BY date")
  
  Links <- sqldf("SELECT COUNT(*) AS links, date
              FROM Links
              GROUP BY date")
  
  Votes <- sqldf("SELECT COUNT(*) AS votes, date
              FROM Votes
              GROUP BY date")
  
  Comments <- sqldf("SELECT COUNT(*) AS comments, date
              FROM Comments
              GROUP BY date")
  
  # Łączenie wszystkiego w jedną ramkę po kolumnie "date"
  Final <- merge(
    merge(
      merge(
        merge(Questions, Answers, by = "date"), 
        Links, by = "date"), 
      Votes, by = "date"),
    Comments, by = "date")
  
  #Zapis ramki do pliku 
  write.csv(Final, "dane/posrednie/Christianity_Akt.csv")
  
  #Rysowanie wykresu 
  ggplot(data = Final, aes(x = date)) + 
    scale_color_manual(values = religion_colors) +
    geom_line(aes(y = questions, color = "Pytania", group = 1)) +
    geom_line(aes(y = answers, color = "Odpowiedzi", group = 1)) +
    geom_line(aes(y = votes, color = "Głosy", group = 1)) +
    geom_line(aes(y = links, color = "Linki", group = 1)) +
    geom_line(aes(y = comments, color = "Komentarze", group = 1)) +
    labs(x = "Okres", angle = 90, y = "Liczba", 
         title = "Wykres różnej aktywności od czasu dla forum christianity.stackexchange.com") +
    theme(axis.text.x = element_text(colour = 'black', size = 8, angle = 90))
  
  #Zapis wykresu
  ggsave("prezentacja/wykresy/wyk_akt_C.png", width=30, height = 10, units = "cm")
  
  invisible(NULL)
  
}

aktywnosc_hinduism <- function() {
  #Funkcja, która tworzy wykresy aktywności dla jednej religii
  
  #Wybranie z ramek rekordów z danego okresu
  P <- HPosts[grepl("20.", HPosts$CreationDate), ]
  L <- HPostLinks[grepl("20.", HPostLinks$CreationDate), ]
  V <- HVotes[grepl("20.", HVotes$CreationDate), ]
  C <- HComments[grepl("20.", HComments$CreationDate), ]
  
  #Tworzenie nowej kolumny w ramce, która z daty przechowuje tylko rok i miesiąc
  #lub rok miesiąc i dzień, aby sprawdzać ilość danej w aktywności w ciągu dnia 
  #lub miesiąca, w zależności jak się ustawi regexpy
  pomP <- P$CreationDate
  pomP <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomP, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomP)[1] <- "date"
  
  pomL <- L$CreationDate
  pomL <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomL, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomL)[1] <- "date"
  
  pomV <- V$CreationDate
  pomV <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomV, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomV)[1] <- "date"
  
  pomC <- C$CreationDate
  pomC <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomC, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomC)[1] <- "date"
  
  
  #Łączenie danych ramek i utworzonej kolumny "date".
  #W przypadku ramki Posts, rozbicie na dwie oddzielne: Questions i Answers
  #Wybór kolumn "date" i jednej dowolnej którą można pogrupować wyniki względem 
  #daty 
  P <- cbind(P, pomP)
  P <- P[c("PostTypeId", "date")]
  rownames(P) <- NULL
  Questions <- P[P$PostTypeId == 1, ]
  Answers <- P[P$PostTypeId == 2, ]
  
  Links <- cbind(L, pomL)
  Links <- Links[c("Id", "date")]
  
  Votes <- cbind(V, pomV)
  Votes <- Votes[c("Id", "date")]
  
  Comments <- cbind(C, pomC)
  Comments <- Comments[c("Id", "date")]
  
  #Agregowanie każdej ramki z użyciem kochanego SQL
  Questions <- sqldf("SELECT COUNT(*) AS questions, date
              FROM Questions
              GROUP BY date")
  
  Answers <- sqldf("SELECT COUNT(*) AS answers, date
              FROM Answers
              GROUP BY date")
  
  Links <- sqldf("SELECT COUNT(*) AS links, date
              FROM Links
              GROUP BY date")
  
  Votes <- sqldf("SELECT COUNT(*) AS votes, date
              FROM Votes
              GROUP BY date")
  
  Comments <- sqldf("SELECT COUNT(*) AS comments, date
              FROM Comments
              GROUP BY date")
  
  # Łączenie wszystkiego w jedną ramkę po kolumnie "date"
  Final <- merge(
    merge(
      merge(
        merge(Questions, Answers, by = "date"), 
        Links, by = "date"), 
      Votes, by = "date"),
    Comments, by = "date")
  
  #Zapis ramki do pliku 
  write.csv(Final, "dane/posrednie/Hinduism_Akt.csv")
  
  #Rysowanie wykresu 
  ggplot(data = Final, aes(x = date)) + 
    scale_color_manual(values = religion_colors) +
    geom_line(aes(y = questions, color = "Pytania", group = 1)) +
    geom_line(aes(y = answers, color = "Odpowiedzi", group = 1)) +
    geom_line(aes(y = votes, color = "Głosy", group = 1)) +
    geom_line(aes(y = links, color = "Linki", group = 1)) +
    geom_line(aes(y = comments, color = "Komentarze", group = 1)) +
    labs(x = "Okres", angle = 90, y = "Liczba", 
         title = "Wykres różnej aktywności od czasu dla forum hindusim.stackexchange.com") +
    theme(axis.text.x = element_text(colour = 'black', size = 8, angle = 90))
  
  #Zapis wykresu
  ggsave("prezentacja/wykresy/wyk_akt_H.png", width=30, height = 10, units = "cm")
  
  invisible(NULL)
  
}

aktywnosc_islam <- function() {
  #Funkcja, która tworzy wykresy aktywności dla jednej religii
  
  #Wybranie z ramek rekordów z danego okresu
  P <- IPosts[grepl("20.", IPosts$CreationDate), ]
  L <- IPostLinks[grepl("20.", IPostLinks$CreationDate), ]
  V <- IVotes[grepl("20.", IVotes$CreationDate), ]
  C <- IComments[grepl("20.", IComments$CreationDate), ]
  
  #Tworzenie nowej kolumny w ramce, która z daty przechowuje tylko rok i miesiąc
  #lub rok miesiąc i dzień, aby sprawdzać ilość danej w aktywności w ciągu dnia 
  #lub miesiąca, w zależności jak się ustawi regexpy
  pomP <- P$CreationDate
  pomP <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomP, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomP)[1] <- "date"
  
  pomL <- L$CreationDate
  pomL <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomL, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomL)[1] <- "date"
  
  pomV <- V$CreationDate
  pomV <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomV, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomV)[1] <- "date"
  
  pomC <- C$CreationDate
  pomC <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomC, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomC)[1] <- "date"
  
  
  #Łączenie danych ramek i utworzonej kolumny "date".
  #W przypadku ramki Posts, rozbicie na dwie oddzielne: Questions i Answers
  #Wybór kolumn "date" i jednej dowolnej którą można pogrupować wyniki względem 
  #daty 
  P <- cbind(P, pomP)
  P <- P[c("PostTypeId", "date")]
  rownames(P) <- NULL
  Questions <- P[P$PostTypeId == 1, ]
  Answers <- P[P$PostTypeId == 2, ]
  
  Links <- cbind(L, pomL)
  Links <- Links[c("Id", "date")]
  
  Votes <- cbind(V, pomV)
  Votes <- Votes[c("Id", "date")]
  
  Comments <- cbind(C, pomC)
  Comments <- Comments[c("Id", "date")]
  
  #Agregowanie każdej ramki z użyciem kochanego SQL
  Questions <- sqldf("SELECT COUNT(*) AS questions, date
              FROM Questions
              GROUP BY date")
  
  Answers <- sqldf("SELECT COUNT(*) AS answers, date
              FROM Answers
              GROUP BY date")
  
  Links <- sqldf("SELECT COUNT(*) AS links, date
              FROM Links
              GROUP BY date")
  
  Votes <- sqldf("SELECT COUNT(*) AS votes, date
              FROM Votes
              GROUP BY date")
  
  Comments <- sqldf("SELECT COUNT(*) AS comments, date
              FROM Comments
              GROUP BY date")
  
  # Łączenie wszystkiego w jedną ramkę po kolumnie "date"
  Final <- merge(
    merge(
      merge(
        merge(Questions, Answers, by = "date"), 
        Links, by = "date"), 
      Votes, by = "date"),
    Comments, by = "date")
  
  #Zapis ramki do pliku 
  write.csv(Final, "dane/posrednie/Islam_Akt.csv")
  
  #Rysowanie wykresu 
  ggplot(data = Final, aes(x = date)) + 
    scale_color_manual(values = religion_colors) +
    geom_line(aes(y = questions, color = "Pytania", group = 1)) +
    geom_line(aes(y = answers, color = "Odpowiedzi", group = 1)) +
    geom_line(aes(y = votes, color = "Głosy", group = 1)) +
    geom_line(aes(y = links, color = "Linki", group = 1)) +
    geom_line(aes(y = comments, color = "Komentarze", group = 1)) +
    labs(x = "Okres", angle = 90, y = "Liczba", 
         title = "Wykres różnej aktywności od czasu dla forum islam.stackexchange.com") +
    theme(axis.text.x = element_text(colour = 'black', size = 8, angle = 90))
  
  #Zapis wykresu
  ggsave("prezentacja/wykresy/wyk_akt_I.png", width=30, height = 10, units = "cm")
  
  invisible(NULL)
  
}

aktywnosc_judaism <- function() {
  #Funkcja, która tworzy wykresy aktywności dla jednej religii
  
  #Wybranie z ramek rekordów z danego okresu
  P <- JPosts[grepl("20.", JPosts$CreationDate), ]
  L <- JPostLinks[grepl("20.", JPostLinks$CreationDate), ]
  V <- JVotes[grepl("20.", JVotes$CreationDate), ]
  C <- JComments[grepl("20.", JComments$CreationDate), ]
  
  #Tworzenie nowej kolumny w ramce, która z daty przechowuje tylko rok i miesiąc
  #lub rok miesiąc i dzień, aby sprawdzać ilość danej w aktywności w ciągu dnia 
  #lub miesiąca, w zależności jak się ustawi regexpy
  pomP <- P$CreationDate
  pomP <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomP, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomP)[1] <- "date"
  
  pomL <- L$CreationDate
  pomL <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomL, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomL)[1] <- "date"
  
  pomV <- V$CreationDate
  pomV <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomV, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomV)[1] <- "date"
  
  pomC <- C$CreationDate
  pomC <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomC, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomC)[1] <- "date"
  
  
  #Łączenie danych ramek i utworzonej kolumny "date".
  #W przypadku ramki Posts, rozbicie na dwie oddzielne: Questions i Answers
  #Wybór kolumn "date" i jednej dowolnej którą można pogrupować wyniki względem 
  #daty 
  P <- cbind(P, pomP)
  P <- P[c("PostTypeId", "date")]
  rownames(P) <- NULL
  Questions <- P[P$PostTypeId == 1, ]
  Answers <- P[P$PostTypeId == 2, ]
  
  Links <- cbind(L, pomL)
  Links <- Links[c("Id", "date")]
  
  Votes <- cbind(V, pomV)
  Votes <- Votes[c("Id", "date")]
  
  Comments <- cbind(C, pomC)
  Comments <- Comments[c("Id", "date")]
  
  #Agregowanie każdej ramki z użyciem kochanego SQL
  Questions <- sqldf("SELECT COUNT(*) AS questions, date
              FROM Questions
              GROUP BY date")
  
  Answers <- sqldf("SELECT COUNT(*) AS answers, date
              FROM Answers
              GROUP BY date")
  
  Links <- sqldf("SELECT COUNT(*) AS links, date
              FROM Links
              GROUP BY date")
  
  Votes <- sqldf("SELECT COUNT(*) AS votes, date
              FROM Votes
              GROUP BY date")
  
  Comments <- sqldf("SELECT COUNT(*) AS comments, date
              FROM Comments
              GROUP BY date")
  
  # Łączenie wszystkiego w jedną ramkę po kolumnie "date"
  Final <- merge(
    merge(
      merge(
        merge(Questions, Answers, by = "date"), 
        Links, by = "date"), 
      Votes, by = "date"),
    Comments, by = "date")
  
  #Zapis ramki do pliku 
  write.csv(Final, "dane/posrednie/Judaism_Akt.csv")
  
  #Rysowanie wykresu 
  ggplot(data = Final, aes(x = date)) + 
    scale_color_manual(values = religion_colors) +
    geom_line(aes(y = questions, color = "Pytania", group = 1)) +
    geom_line(aes(y = answers, color = "Odpowiedzi", group = 1)) +
    geom_line(aes(y = votes, color = "Głosy", group = 1)) +
    geom_line(aes(y = links, color = "Linki", group = 1)) +
    geom_line(aes(y = comments, color = "Komentarze", group = 1)) +
    labs(x = "Okres", angle = 90, y = "Liczba", 
         title = "Wykres różnej aktywności od czasu dla forum judaism.stackexchange.com") +
    theme(axis.text.x = element_text(colour = 'black', size = 8, angle = 90))
  
  #Zapis wykresu
  ggsave("prezentacja/wykresy/wyk_akt_J.png", width=30, height = 10, units = "cm")
  
  invisible(NULL)
  
}
