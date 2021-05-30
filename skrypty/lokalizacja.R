# Funkcje potrzebne do stworzenia ramek danych dla każdego forum, zawierające
#zliczonych użytkowników z każdego kraju

#Przydatne pakiety:
library(maps)
library(rworldmap)

rozmieszczenie_B <- function() {
  #funkcja tworząca ramkę danych, w której dla forum buddhism zliczono ilość
  #użytkowników z poszczególnych krajów
  
  #Założenie: użytkownicy są szukani po nazwie kraju, z którego pochodzą. 
  #Nie uwzględniamy tych, którzy podali tylko miasto, lub jakieś inne 
  #bezwartościowe dane
  
  
  #Wektora, który zawiera nazwy 149 krajów świata (potrzebny pakiet "rworldmap")
  countries <- countryExData[, 2]
  
  #Wektor, który reprezentujące kolumnę "Location" w ramce
  location <- BUsers$Location
  
  #Inicjalizacja ramki 
  Zliczanie <- data.frame(Country = "", Count = 0)
  
  #Pętla, w której chodzimy po krajach w wektorze countries. Dla każdego kraju 
  #sprawdzane są wystąpienia jego nazwy w wektorze location i sumowane,
  #by uzyskać liczbę ich wszystkich wystąpień. Następnie tworzona jest 
  #pomocnicza ramka i łączona wierszowo do ramki Zliczanie. Wynikiem jest
  #ramka, w której każdemu krajowi przypisana jest liczba użytkowników z tego
  #kraju
  
  for (country in countries) {
    count <- sum(sapply(location, FUN = function(x) stri_count_regex(x, country)))
    pom <- data.frame(Country = country, Count = count)
    Zliczanie <- rbind(Zliczanie, pom)
  }
  
  #Trzeba się pozbyć pierwszego wiersza, który służył tylko do inicjalizacji
  Zliczanie <- Zliczanie[-1, ]
  
  #Zapis do pliku
  write.csv(Zliczanie, "Rozmieszczenie_B.csv")
  
}


rozmieszczenie_C <- function() {
  #funkcja tworząca ramkę danych, w której dla forum christianity zliczono ilość
  #użytkowników z poszczególnych krajów
  
  #Założenie: użytkownicy są szukani po nazwie kraju, z którego pochodzą. 
  #Nie uwzględniamy tych, którzy podali tylko miasto, lub jakieś inne 
  #bezwartościowe dane
  
  
  #Wektora, który zawiera nazwy 149 krajów świata (potrzebny pakiet "rworldmap")
  countries <- countryExData[, 2]
  
  #Wektor, który reprezentujące kolumnę "Location" w ramce
  location <- CUsers$Location
  
  #Inicjalizacja ramki 
  Zliczanie <- data.frame(Country = "", Count = 0)
  
  #Pętla, w której chodzimy po krajach w wektorze countries. Dla każdego kraju 
  #sprawdzane są wystąpienia jego nazwy w wektorze location i sumowane,
  #by uzyskać liczbę ich wszystkich wystąpień. Następnie tworzona jest 
  #pomocnicza ramka i łączona wierszowo do ramki Zliczanie. Wynikiem jest
  #ramka, w której każdemu krajowi przypisana jest liczba użytkowników z tego
  #kraju
  
  for (country in countries) {
    count <- sum(sapply(location, FUN = function(x) stri_count_regex(x, country)))
    pom <- data.frame(Country = country, Count = count)
    Zliczanie <- rbind(Zliczanie, pom)
  }
  
  #Trzeba się pozbyć pierwszego wiersza, który służył tylko do inicjalizacji
  Zliczanie <- Zliczanie[-1, ]
  
  #Zapis do pliku
  write.csv(Zliczanie, "Rozmieszczenie_C.csv")
  
}

rozmieszczenie_H <- function() {
  #funkcja tworząca ramkę danych, w której dla forum hinduism zliczono ilość
  #użytkowników z poszczególnych krajów
  
  #Założenie: użytkownicy są szukani po nazwie kraju, z którego pochodzą. 
  #Nie uwzględniamy tych, którzy podali tylko miasto, lub jakieś inne 
  #bezwartościowe dane
  
  
  #Wektora, który zawiera nazwy 149 krajów świata (potrzebny pakiet "rworldmap")
  countries <- countryExData[, 2]
  
  #Wektor, który reprezentujące kolumnę "Location" w ramce
  location <- HUsers$Location
  
  #Inicjalizacja ramki 
  Zliczanie <- data.frame(Country = "", Count = 0)
  
  #Pętla, w której chodzimy po krajach w wektorze countries. Dla każdego kraju 
  #sprawdzane są wystąpienia jego nazwy w wektorze location i sumowane,
  #by uzyskać liczbę ich wszystkich wystąpień. Następnie tworzona jest 
  #pomocnicza ramka i łączona wierszowo do ramki Zliczanie. Wynikiem jest
  #ramka, w której każdemu krajowi przypisana jest liczba użytkowników z tego
  #kraju
  
  for (country in countries) {
    count <- sum(sapply(location, FUN = function(x) stri_count_regex(x, country)))
    pom <- data.frame(Country = country, Count = count)
    Zliczanie <- rbind(Zliczanie, pom)
  }
  
  #Trzeba się pozbyć pierwszego wiersza, który służył tylko do inicjalizacji
  Zliczanie <- Zliczanie[-1, ]
  
  #Zapis do pliku
  write.csv(Zliczanie, "Rozmieszczenie_H.csv")
  
}

rozmieszczenie_I <- function() {
  #funkcja tworząca ramkę danych, w której dla forum islam zliczono ilość
  #użytkowników z poszczególnych krajów
  
  #Założenie: użytkownicy są szukani po nazwie kraju, z którego pochodzą. 
  #Nie uwzględniamy tych, którzy podali tylko miasto, lub jakieś inne 
  #bezwartościowe dane
  
  
  #Wektora, który zawiera nazwy 149 krajów świata (potrzebny pakiet "rworldmap")
  countries <- countryExData[, 2]
  
  #Wektor, który reprezentujące kolumnę "Location" w ramce
  location <- IUsers$Location
  
  #Inicjalizacja ramki 
  Zliczanie <- data.frame(Country = "", Count = 0)
  
  #Pętla, w której chodzimy po krajach w wektorze countries. Dla każdego kraju 
  #sprawdzane są wystąpienia jego nazwy w wektorze location i sumowane,
  #by uzyskać liczbę ich wszystkich wystąpień. Następnie tworzona jest 
  #pomocnicza ramka i łączona wierszowo do ramki Zliczanie. Wynikiem jest
  #ramka, w której każdemu krajowi przypisana jest liczba użytkowników z tego
  #kraju
  
  for (country in countries) {
    count <- sum(sapply(location, FUN = function(x) stri_count_regex(x, country)))
    pom <- data.frame(Country = country, Count = count)
    Zliczanie <- rbind(Zliczanie, pom)
  }
  
  #Trzeba się pozbyć pierwszego wiersza, który służył tylko do inicjalizacji
  Zliczanie <- Zliczanie[-1, ]
  
  #Zapis do pliku
  write.csv(Zliczanie, "Rozmieszczenie_I.csv")
  
}

rozmieszczenie_J <- function() {
  #funkcja tworząca ramkę danych, w której dla forum judaism zliczono ilość
  #użytkowników z poszczególnych krajów
  
  #Założenie: użytkownicy są szukani po nazwie kraju, z którego pochodzą. 
  #Nie uwzględniamy tych, którzy podali tylko miasto, lub jakieś inne 
  #bezwartościowe dane
  
  
  #Wektora, który zawiera nazwy 149 krajów świata (potrzebny pakiet "rworldmap")
  countries <- countryExData[, 2]
  
  #Wektor, który reprezentujące kolumnę "Location" w ramce
  location <- BUsers$Location
  
  #Inicjalizacja ramki 
  Zliczanie <- data.frame(Country = "", Count = 0)
  
  #Pętla, w której chodzimy po krajach w wektorze countries. Dla każdego kraju 
  #sprawdzane są wystąpienia jego nazwy w wektorze location i sumowane,
  #by uzyskać liczbę ich wszystkich wystąpień. Następnie tworzona jest 
  #pomocnicza ramka i łączona wierszowo do ramki Zliczanie. Wynikiem jest
  #ramka, w której każdemu krajowi przypisana jest liczba użytkowników z tego
  #kraju
  
  for (country in countries) {
    count <- sum(sapply(location, FUN = function(x) stri_count_regex(x, country)))
    pom <- data.frame(Country = country, Count = count)
    Zliczanie <- rbind(Zliczanie, pom)
  }
  
  #Trzeba się pozbyć pierwszego wiersza, który służył tylko do inicjalizacji
  Zliczanie <- Zliczanie[-1, ]
  
  #Zapis do pliku
  write.csv(Zliczanie, "Rozmieszczenie_I.csv")
  
}
