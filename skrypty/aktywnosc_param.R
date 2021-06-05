#Tworzenie ramek danych i wykresów porównujących aktywność na różnych forach
#pod względem kolejnych paramtetrów
# Funkcje nic nie zwracają, ale zapisują odpowiednie ramki danych i mapki 
# do plików, więc przy wykonywaniu trzeba uważać gdzie się zapisują i czy
# przypadkiem nie dodajemy je na repozytorium

religion_colors <- c("#FF8133", "#FFE900", "#0094C6", "#1A6318", "#89043D")

#Potrzebne biblioteki:
library(stringi)
library(sqldf)
library(ggplot2)

aktywnosc_Questions <- function() {
  #Funkcja, która porównuje aktywność wszystkich forów pod względem 
  # parametru Questions
  
  #Tworzę ramki danych zawierające informacje dotyczące liczby pytań w danym
  #okresie dla każdej religii
  
  B <- BPosts[BPosts$PostTypeId == 1, ]
  B <- B[grepl("20.", B$CreationDate), ]
  pomB <- B$CreationDate
  pomB <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomB, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomB)[1] = "date"
  Buddhism <- cbind(B, pomB)
  Buddhism <- Buddhism[c("Id", "date")]
  Buddhism <- sqldf("SELECT COUNT(*) AS buddhism, date
              FROM Buddhism
              GROUP BY date")
  
  
  C <- CPosts[CPosts$PostTypeId == 1, ]
  C <- C[grepl("20.", C$CreationDate), ]
  pomC <- C$CreationDate
  pomC <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomC, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomC)[1] = "date"
  Christianity <- cbind(C, pomC)
  Christianity <- Christianity[c("Id", "date")]
  Christianity <- sqldf("SELECT COUNT(*) AS christianity, date
              FROM Christianity
              GROUP BY date")
  
  H <- HPosts[HPosts$PostTypeId == 1, ]
  H <- H[grepl("20.", H$CreationDate), ]
  pomH <- H$CreationDate
  pomH <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomH, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomH)[1] = "date"
  Hinduism <- cbind(H, pomH)
  Hinduism <- Hinduism[c("Id", "date")]
  Hinduism <- sqldf("SELECT COUNT(*) AS hinduism, date
              FROM Hinduism
              GROUP BY date")
  
  I <- IPosts[IPosts$PostTypeId == 1, ]
  I <- I[grepl("20.", I$CreationDate), ]
  pomI <- I$CreationDate
  pomI <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomI, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomI)[1] = "date"
  Islam <- cbind(I, pomI)
  Islam <- Islam[c("Id", "date")]
  Islam <- sqldf("SELECT COUNT(*) AS islam, date
              FROM Islam
              GROUP BY date")
  
  J <- JPosts[JPosts$PostTypeId == 1, ]
  J <- J[grepl("20.", J$CreationDate), ]
  pomJ <- J$CreationDate
  pomJ <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomJ, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomJ)[1] = "date"
  Judaism <- cbind(J, pomJ)
  Judaism <- Judaism[c("Id", "date")]
  Judaism <- sqldf("SELECT COUNT(*) AS judaism, date
              FROM Judaism
              GROUP BY date")
  
  #Łączę ramki w jedną reprezentującą i porównującą wszytskie fora
  Final <- merge(
    merge(
      merge(
        merge(Buddhism, Christianity, by = "date"), 
        Hinduism, by = "date"), 
      Islam, by ="date"), 
    Judaism, by = "date" ) 
  
  #Zapis ramki do pliku
  write.csv(Final, "dane/posrednie/Questions_Akt.csv", row.names = FALSE)
  
  #Rysowanie wykresu
  ggplot(data = Final, aes(x = date)) + 
    scale_color_manual(values = religion_colors) +
    geom_line(aes(y = buddhism, group = 1, color = "Buddyzm")) +
    geom_line(aes(y = christianity, group = 1, color = "Chrześcijaństwo")) +
    geom_line(aes(y = hinduism, group = 1, color = "Hinduizm")) +
    geom_line(aes(y = islam, group = 1, color = "Islam")) +
    geom_line(aes(y = judaism, group = 1, color = "Judaizm")) +
  labs(x = "Okres", angle = 90, y = "Liczba", 
       title = "Wykres zadanych pytań dla różnych religii") +
    theme(axis.text.x = element_text(colour = 'black', size = 8, angle = 90))
  
  #Zapis wykresu do pliku:
  ggsave("prezentacja/wykresy/wyk_akt_que.png", width=30, height = 10, units = "cm")
  
  invisible(NULL)

}


aktywnosc_Answers <- function() {
  #Funkcja, która porównuje aktywność wszystkich forów pod względem 
  # parametru Answers
  
  #Tworzę ramki danych zawierające informacje dotyczące liczby odpowiedzi w danym
  #okresie dla każdej religii
  
  B <- BPosts[BPosts$PostTypeId == 2, ]
  B <- B[grepl("20.", B$CreationDate), ]
  pomB <- B$CreationDate
  pomB <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomB, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomB)[1] = "date"
  Buddhism <- cbind(B, pomB)
  Buddhism <- Buddhism[c("Id", "date")]
  Buddhism <- sqldf("SELECT COUNT(*) AS buddhism, date
              FROM Buddhism
              GROUP BY date")
  
  
  C <- CPosts[CPosts$PostTypeId == 2, ]
  C <- C[grepl("20.", C$CreationDate), ]
  pomC <- C$CreationDate
  pomC <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomC, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomC)[1] = "date"
  Christianity <- cbind(C, pomC)
  Christianity <- Christianity[c("Id", "date")]
  Christianity <- sqldf("SELECT COUNT(*) AS christianity, date
              FROM Christianity
              GROUP BY date")
  
  H <- HPosts[HPosts$PostTypeId == 2, ]
  H <- H[grepl("20.", H$CreationDate), ]
  pomH <- H$CreationDate
  pomH <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomH, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomH)[1] = "date"
  Hinduism <- cbind(H, pomH)
  Hinduism <- Hinduism[c("Id", "date")]
  Hinduism <- sqldf("SELECT COUNT(*) AS hinduism, date
              FROM Hinduism
              GROUP BY date")
  
  I <- IPosts[IPosts$PostTypeId == 2, ]
  I <- I[grepl("20.", I$CreationDate), ]
  pomI <- I$CreationDate
  pomI <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomI, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomI)[1] = "date"
  Islam <- cbind(I, pomI)
  Islam <- Islam[c("Id", "date")]
  Islam <- sqldf("SELECT COUNT(*) AS islam, date
              FROM Islam
              GROUP BY date")
  
  J <- JPosts[JPosts$PostTypeId == 2, ]
  J <- J[grepl("20.", J$CreationDate), ]
  pomJ <- J$CreationDate
  pomJ <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomJ, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomJ)[1] = "date"
  Judaism <- cbind(J, pomJ)
  Judaism <- Judaism[c("Id", "date")]
  Judaism <- sqldf("SELECT COUNT(*) AS judaism, date
              FROM Judaism
              GROUP BY date")
  
  #Łączę ramki w jedną reprezentującą i porównującą wszytskie fora
  Final <- merge(
    merge(
      merge(
        merge(Buddhism, Christianity, by = "date"), 
        Hinduism, by = "date"), 
      Islam, by ="date"), 
    Judaism, by = "date" ) 
  
  #Zapis ramki do pliku
  write.csv(Final, "dane/posrednie/Answers_Akt.csv", row.names = FALSE)
  
  #Rysowanie wykresu
  ggplot(data = Final, aes(x = date)) + 
    scale_color_manual(values = religion_colors) +
    geom_line(aes(y = buddhism, group = 1, color = "Buddyzm")) +
    geom_line(aes(y = christianity, group = 1, color = "Chrześcijaństwo")) +
    geom_line(aes(y = hinduism, group = 1, color = "Hinduizm")) +
    geom_line(aes(y = islam, group = 1, color = "Islam")) +
    geom_line(aes(y = judaism, group = 1, color = "Judaizm")) +
  labs(x = "Okres", angle = 90, y = "Liczba", 
       title = "Wykres udzielonych odpowiedzi dla różnych religii") +
    theme(axis.text.x = element_text(colour = 'black', size = 8, angle = 90))
  
  #Zapis wykresu do pliku:
  ggsave("prezentacja/wykresy/wyk_akt_ans.png", width=30, height = 10, units = "cm")
  
  invisible(NULL)
}


aktywnosc_Links <- function() {
  #Funkcja, która porównuje aktywność wszystkich forów pod względem 
  # parametru PostLinks
  
  #Tworzę ramki danych zawierające informacje dotyczące liczby linków w danym
  #okresie dla każdej religii
  
  B <- BPostLinks[grepl("20.", BPostLinks$CreationDate), ]
  pomB <- B$CreationDate
  pomB <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomB, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomB)[1] = "date"
  Buddhism <- cbind(B, pomB)
  Buddhism <- Buddhism[c("Id", "date")]
  Buddhism <- sqldf("SELECT COUNT(*) AS buddhism, date
              FROM Buddhism
              GROUP BY date")
  
  
  C <- CPostLinks[grepl("20.", CPostLinks$CreationDate), ]
  pomC <- C$CreationDate
  pomC <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomC, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomC)[1] = "date"
  Christianity <- cbind(C, pomC)
  Christianity <- Christianity[c("Id", "date")]
  Christianity <- sqldf("SELECT COUNT(*) AS christianity, date
              FROM Christianity
              GROUP BY date")
  
  H <- HPostLinks[grepl("20.", HPostLinks$CreationDate), ]
  pomH <- H$CreationDate
  pomH <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomH, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomH)[1] = "date"
  Hinduism <- cbind(H, pomH)
  Hinduism <- Hinduism[c("Id", "date")]
  Hinduism <- sqldf("SELECT COUNT(*) AS hinduism, date
              FROM Hinduism
              GROUP BY date")
  
  I <- IPostLinks[grepl("20.", IPostLinks$CreationDate), ]
  pomI <- I$CreationDate
  pomI <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomI, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomI)[1] = "date"
  Islam <- cbind(I, pomI)
  Islam <- Islam[c("Id", "date")]
  Islam <- sqldf("SELECT COUNT(*) AS islam, date
              FROM Islam
              GROUP BY date")
  
  J <- JPostLinks[grepl("20.", JPostLinks$CreationDate), ]
  pomJ <- J$CreationDate
  pomJ <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomJ, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomJ)[1] = "date"
  Judaism <- cbind(J, pomJ)
  Judaism <- Judaism[c("Id", "date")]
  Judaism <- sqldf("SELECT COUNT(*) AS judaism, date
              FROM Judaism
              GROUP BY date")
  
  #Łączę ramki w jedną reprezentującą i porównującą wszytskie fora
  Final <- merge(
    merge(
      merge(
        merge(Buddhism, Christianity, by = "date"), 
        Hinduism, by = "date"), 
      Islam, by ="date"), 
    Judaism, by = "date" ) 
  
  #Zapis ramki do pliku
  write.csv(Final, "dane/posrednie/Links_Akt.csv", row.names = FALSE)
  
  #Rysowanie wykresu
  ggplot(data = Final, aes(x = date)) + 
    scale_color_manual(values = religion_colors) +
    geom_line(aes(y = buddhism, group = 1, color = "Buddyzm")) +
    geom_line(aes(y = christianity, group = 1, color = "Chrześcijaństwo")) +
    geom_line(aes(y = hinduism, group = 1, color = "Hinduizm")) +
    geom_line(aes(y = islam, group = 1, color = "Islam")) +
    geom_line(aes(y = judaism, group = 1, color = "Judaizm")) +
  labs(x = "Okres", angle = 90, y = "Liczba", 
       title = "Wykres liczby odnośników do postów dla różnych religii") +
    theme(axis.text.x = element_text(colour = 'black', size = 8, angle = 90))
  
  #Zapis wykresu do pliku:
  ggsave("prezentacja/wykresy/wyk_akt_lin.png", width=30, height = 10, units = "cm")
  
  invisible(NULL)
}


aktywnosc_votes <- function() {
  #Funkcja, która porównuje aktywność wszystkich forów pod względem 
  # parametru Votes
  
  #Tworzę ramki danych zawierające informacje dotyczące liczby głosów w danym
  #okresie dla każdej religii
  
  B <- BVotes[grepl("20.", BVotes$CreationDate), ]
  pomB <- B$CreationDate
  pomB <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomB, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomB)[1] = "date"
  Buddhism <- cbind(B, pomB)
  Buddhism <- Buddhism[c("Id", "date")]
  Buddhism <- sqldf("SELECT COUNT(*) AS buddhism, date
              FROM Buddhism
              GROUP BY date")
  
  
  C <- CVotes[grepl("20.", CVotes$CreationDate), ]
  pomC <- C$CreationDate
  pomC <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomC, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomC)[1] = "date"
  Christianity <- cbind(C, pomC)
  Christianity <- Christianity[c("Id", "date")]
  Christianity <- sqldf("SELECT COUNT(*) AS christianity, date
              FROM Christianity
              GROUP BY date")
  
  H <- HVotes[grepl("20.", HVotes$CreationDate), ]
  pomH <- H$CreationDate
  pomH <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomH, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomH)[1] = "date"
  Hinduism <- cbind(H, pomH)
  Hinduism <- Hinduism[c("Id", "date")]
  Hinduism <- sqldf("SELECT COUNT(*) AS hinduism, date
              FROM Hinduism
              GROUP BY date")
  
  I <- IVotes[grepl("20.", IVotes$CreationDate), ]
  pomI <- I$CreationDate
  pomI <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomI, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomI)[1] = "date"
  Islam <- cbind(I, pomI)
  Islam <- Islam[c("Id", "date")]
  Islam <- sqldf("SELECT COUNT(*) AS islam, date
              FROM Islam
              GROUP BY date")
  
  J <- JVotes[grepl("20.", JVotes$CreationDate), ]
  pomJ <- J$CreationDate
  pomJ <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomJ, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomJ)[1] = "date"
  Judaism <- cbind(J, pomJ)
  Judaism <- Judaism[c("Id", "date")]
  Judaism <- sqldf("SELECT COUNT(*) AS judaism, date
              FROM Judaism
              GROUP BY date")
  
  #Łączę ramki w jedną reprezentującą i porównującą wszystkie fora
  Final <- merge(
    merge(
      merge(
        merge(Buddhism, Christianity, by = "date"), 
        Hinduism, by = "date"), 
      Islam, by ="date"), 
    Judaism, by = "date" ) 
  
  #Zapis ramki do pliku
  write.csv(Final, "dane/posrednie/Votes_Akt.csv", row.names = FALSE)
  
  #Rysowanie wykresu
  ggplot(data = Final, aes(x = date)) + 
    scale_color_manual(values = religion_colors) +
    geom_line(aes(y = buddhism, group = 1, color = "Buddyzm")) +
    geom_line(aes(y = christianity, group = 1, color = "Chrześcijaństwo")) +
    geom_line(aes(y = hinduism, group = 1, color = "Hinduizm")) +
    geom_line(aes(y = islam, group = 1, color = "Islam")) +
    geom_line(aes(y = judaism, group = 1, color = "Judaizm")) +
  labs(x = "Okres", angle = 90, y = "Liczba", 
       title = "Wykres liczby oddanych głosów dla różnych religii") +
    theme(axis.text.x = element_text(colour = 'black', size = 8, angle = 90))
  
  #Zapis wykresu do pliku:
  ggsave("prezentacja/wykresy/wyk_akt_vot.png", width=30, height = 10, units = "cm")
  
  invisible(NULL)
}


aktywnosc_Comments <- function() {
  #Funkcja, która porównuje aktywność wszystkich forów pod względem 
  # parametru Comments
  
  #Tworzę ramki danych zawierające informacje dotyczące liczby komentarzy w danym
  #okresie dla każdej religii
  
  B <- BComments[grepl("20.", BComments$CreationDate), ]
  pomB <- B$CreationDate
  pomB <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomB, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomB)[1] = "date"
  Buddhism <- cbind(B, pomB)
  Buddhism <- Buddhism[c("Id", "date")]
  Buddhism <- sqldf("SELECT COUNT(*) AS buddhism, date
              FROM Buddhism
              GROUP BY date")
  
  
  C <- CComments[grepl("20.", CComments$CreationDate), ]
  pomC <- C$CreationDate
  pomC <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomC, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomC)[1] = "date"
  Christianity <- cbind(C, pomC)
  Christianity <- Christianity[c("Id", "date")]
  Christianity <- sqldf("SELECT COUNT(*) AS christianity, date
              FROM Christianity
              GROUP BY date")
  
  H <- HComments[grepl("20.", HComments$CreationDate), ]
  pomH <- H$CreationDate
  pomH <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomH, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomH)[1] = "date"
  Hinduism <- cbind(H, pomH)
  Hinduism <- Hinduism[c("Id", "date")]
  Hinduism <- sqldf("SELECT COUNT(*) AS hinduism, date
              FROM Hinduism
              GROUP BY date")
  
  I <- IComments[grepl("20.", IComments$CreationDate), ]
  pomI <- I$CreationDate
  pomI <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomI, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomI)[1] = "date"
  Islam <- cbind(I, pomI)
  Islam <- Islam[c("Id", "date")]
  Islam <- sqldf("SELECT COUNT(*) AS islam, date
              FROM Islam
              GROUP BY date")
  
  J <- JComments[grepl("20.", JComments$CreationDate), ]
  pomJ <- J$CreationDate
  pomJ <- as.data.frame(matrix(
    unlist(stri_extract_all_regex(pomJ, "20[0-9]{2}(-)[0-9]{2}"))))
  colnames(pomJ)[1] = "date"
  Judaism <- cbind(J, pomJ)
  Judaism <- Judaism[c("Id", "date")]
  Judaism <- sqldf("SELECT COUNT(*) AS judaism, date
              FROM Judaism
              GROUP BY date")
  
  #Łączę ramki w jedną reprezentującą i porównującą wszytskie fora
  Final <- merge(
    merge(
      merge(
        merge(Buddhism, Christianity, by = "date"), 
        Hinduism, by = "date"), 
      Islam, by ="date"), 
    Judaism, by = "date" ) 
  
  #Zapis ramki do pliku
  write.csv(Final, "dane/posrednie/Comments_Akt.csv", row.names = FALSE)
  
  #Rysowanie wykresu
  ggplot(data = Final, aes(x = date)) + 
    scale_color_manual(values = religion_colors) +
    geom_line(aes(y = buddhism, group = 1, color = "Buddyzm")) +
    geom_line(aes(y = christianity, group = 1, color = "Chrześcijaństwo")) +
    geom_line(aes(y = hinduism, group = 1, color = "Hinduizm")) +
    geom_line(aes(y = islam, group = 1, color = "Islam")) +
    geom_line(aes(y = judaism, group = 1, color = "Judaizm")) +
  labs(x = "Okres", angle = 90, y = "Liczba", 
       title = "Wykres liczby ododanych komentarzy dla różnych religii") +
    theme(axis.text.x = element_text(colour = 'black', size = 8, angle = 90))
  
  #Zapis wykresu do pliku:
  ggsave("prezentacja/wykresy/wyk_akt_com.png", width=30, height = 10, units = "cm")
  
  invisible(NULL)
}