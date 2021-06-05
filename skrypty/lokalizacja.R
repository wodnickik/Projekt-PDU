# Funkcje potrzebne do stworzenia ramek danych dla każdego forum, zawierające
#zliczonych użytkowników z każdego kraju
# Funkcje nic nie zwracają, ale zapisują odpowiednie ramki danych i mapki 
# do plików, więc przy wykonywaniu trzeba uważać gdzie się zapisują i czy
# przypadkiem nie dodajemy je na repozytorium

#Przydatne pakiety:
library(maps)
library(rworldmap) 
library(ggmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)


rozmieszczenie_B <- function() {
  #funkcja tworząca ramkę danych, w której dla forum buddhism zliczono ilość
  #użytkowników z poszczególnych krajów
  
  #Założenie: użytkownicy są szukani po nazwie kraju, z którego pochodzą. 
  #Nie uwzględniamy tych, którzy podali tylko miasto, lub jakieś inne 
  #bezwartościowe dane
  
  
  #Ramka world zawiera wszystkie dane o krajach i wszystkie dane graficzne 
  #potrzebne do rysowania mapy, wektor countries zawiera nazwy wszystkich państw
  #świata
  world <- ne_countries(scale = "medium", returnclass = "sf")
  countries <- unique(world$name_long)
  
  
  #Wektor reprezentujący kolumnę "Location" w ramce
  location <- na.omit(BUsers$Location)
  
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
  
  #Zapis do pliku ramki z rozmieszczeniem
  write.csv(Zliczanie, "dane/posrednie/Rozmieszczenie_B.csv")
  
  #Połączenie z ramką world, aby uzyskać dane do rysowania mapki
  world_map <- merge(world, Zliczanie, by.y = "Country", by.x="name_long")
  
  #rysowanie wykresu i kolorowanie skalą kolorów w zależności od liczby 
  #użytkowników w danym kraju
  ggplot(data = world_map) +
    geom_sf() +
    ggtitle("Mapa rozmieszczenia użytkowników forum buddhism.stackexchange.com") +
    geom_sf(aes(fill = Count)) +
    scale_fill_viridis_c(alpha = 1, guide = "colourbar", 
                         option = "G", trans = "sqrt", direction = -1)
  
  #Zapis mapki do pliku
  ggsave("prezentacja/wykresy/Mapa_B.png", width = 20, height = 10, units = "cm")
  
  invisible(NULL)
}


rozmieszczenie_C <- function() {
  #funkcja tworząca ramkę danych, w której dla forum christianity zliczono ilość
  #użytkowników z poszczególnych krajów
  
  #Założenie: użytkownicy są szukani po nazwie kraju, z którego pochodzą. 
  #Nie uwzględniamy tych, którzy podali tylko miasto, lub jakieś inne 
  #bezwartościowe dane
  
  
  #Ramka world zawiera wszystkie dane o krajach i wszystkie dane graficzne 
  #potrzebne do rysowania mapy, wektor countries zawiera nazwy wszystkich państw
  #świata
  world <- ne_countries(scale = "medium", returnclass = "sf")
  countries <- unique(world$name_long)
  
  
  #Wektor reprezentujący kolumnę "Location" w ramce
  location <- na.omit(CUsers$Location)
  
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
  write.csv(Zliczanie, "dane/posrednie/Rozmieszczenie_C.csv")
  
  #Połączenie z ramką world, aby uzyskać dane do rysowania mapki
  world_map <- merge(world, Zliczanie, by.y = "Country", by.x="name_long")
  
  #rysowanie wykresu i kolorowanie skalą kolorów w zależności od liczby 
  #użytkowników w danym kraju
  ggplot(data = world_map) +
    geom_sf() +
    ggtitle("Mapa rozmieszczenia użytkowników forum christianity.stackexchange.com") +
    geom_sf(aes(fill = Count)) +
    scale_fill_viridis_c(alpha = 1, guide = "colourbar", 
                         option = "G", trans = "sqrt", direction = -1)
  
  #Zapis mapki do pliku
  ggsave("prezentacja/wykresy/Mapa_C.png", width = 20, height = 10, units = "cm")
  
  
  
  invisible(NULL)
}

rozmieszczenie_H <- function() {
  #funkcja tworząca ramkę danych, w której dla forum hinduism zliczono ilość
  #użytkowników z poszczególnych krajów
  
  #Założenie: użytkownicy są szukani po nazwie kraju, z którego pochodzą. 
  #Nie uwzględniamy tych, którzy podali tylko miasto, lub jakieś inne 
  #bezwartościowe dane
  
  
  #Ramka world zawiera wszystkie dane o krajach i wszystkie dane graficzne 
  #potrzebne do rysowania mapy, wektor countries zawiera nazwy wszystkich państw
  #świata
  world <- ne_countries(scale = "medium", returnclass = "sf")
  countries <- unique(world$name_long)
  
  
  #Wektor reprezentujący kolumnę "Location" w ramce
  location <- na.omit(HUsers$Location)
  
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
  write.csv(Zliczanie, "dane/posrednie/Rozmieszczenie_H.csv")
  
  #Połączenie z ramką world, aby uzyskać dane do rysowania mapki
  world_map <- merge(world, Zliczanie, by.y = "Country", by.x="name_long")
  
  #rysowanie wykresu i kolorowanie skalą kolorów w zależności od liczby 
  #użytkowników w danym kraju
  ggplot(data = world_map) +
    geom_sf() +
    ggtitle("Mapa rozmieszczenia użytkowników forum hinduism.stackexchange.com") +
    geom_sf(aes(fill = Count)) +
    scale_fill_viridis_c(alpha = 1, guide = "colourbar", 
                         option = "G", trans = "sqrt", direction = -1)
  
  #Zapis mapki do pliku
  ggsave("prezentacja/wykresy/Mapa_H.png", width = 20, height = 10, units = "cm")
  
  invisible(NULL)
}

rozmieszczenie_I <- function() {
  #funkcja tworząca ramkę danych, w której dla forum islam zliczono ilość
  #użytkowników z poszczególnych krajów
  
  #Założenie: użytkownicy są szukani po nazwie kraju, z którego pochodzą. 
  #Nie uwzględniamy tych, którzy podali tylko miasto, lub jakieś inne 
  #bezwartościowe dane
  
  
  #Ramka world zawiera wszystkie dane o krajach i wszystkie dane graficzne 
  #potrzebne do rysowania mapy, wektor countries zawiera nazwy wszystkich państw
  #świata
  world <- ne_countries(scale = "medium", returnclass = "sf")
  countries <- unique(world$name_long)
  
  
  #Wektor reprezentujący kolumnę "Location" w ramce
  location <- na.omit(IUsers$Location)
  
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
  write.csv(Zliczanie, "dane/posrednie/Rozmieszczenie_I.csv")
  
  #Połączenie z ramką world, aby uzyskać dane do rysowania mapki
  world_map <- merge(world, Zliczanie, by.y = "Country", by.x="name_long")
  
  #rysowanie wykresu i kolorowanie skalą kolorów w zależności od liczby 
  #użytkowników w danym kraju
  ggplot(data = world_map) +
    geom_sf() +
    ggtitle("Mapa rozmieszczenia użytkowników forum islam.stackexchange.com") +
    geom_sf(aes(fill = Count)) +
    scale_fill_viridis_c(alpha = 1, guide = "colourbar", 
                         option = "G", trans = "sqrt", direction = -1)
  
  #Zapis mapki do pliku
  ggsave("prezentacja/wykresy/Mapa_I.png", width = 20, height = 10, units = "cm")
  
  invisible(NULL)
}

rozmieszczenie_J <- function() {
  #funkcja tworząca ramkę danych, w której dla forum judaism zliczono ilość
  #użytkowników z poszczególnych krajów
  
  #Założenie: użytkownicy są szukani po nazwie kraju, z którego pochodzą. 
  #Nie uwzględniamy tych, którzy podali tylko miasto, lub jakieś inne 
  #bezwartościowe dane
  
  
  #Ramka world zawiera wszystkie dane o krajach i wszystkie dane graficzne 
  #potrzebne do rysowania mapy, wektor countries zawiera nazwy wszystkich państw
  #świata
  world <- ne_countries(scale = "medium", returnclass = "sf")
  countries <- unique(world$name_long)
  
  
  #Wektor reprezentujący kolumnę "Location" w ramce
  location <- na.omit(BUsers$Location)
  
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
  write.csv(Zliczanie, "dane/posrednie/Rozmieszczenie_I.csv")
  
  #Połączenie z ramką world, aby uzyskać dane do rysowania mapki
  world_map <- merge(world, Zliczanie, by.y = "Country", by.x="name_long")
  
  #rysowanie wykresu i kolorowanie skalą kolorów w zależności od liczby 
  #użytkowników w danym kraju
  ggplot(data = world_map) +
    geom_sf() +
    ggtitle("Mapa rozmieszczenia użytkowników forum judaism.stackexchange.com") +
    geom_sf(aes(fill = Count)) +
    scale_fill_viridis_c(alpha = 1, guide = "colourbar", 
                         option = "G", trans = "sqrt", direction = -1)
  
  #Zapis mapki do pliku
  ggsave("prezentacja/wykresy/Mapa_J.png", width = 20, height = 10, units = "cm")
  
  invisible(NULL)
}
