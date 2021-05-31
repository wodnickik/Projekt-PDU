# Zestawienie zbiorczej aktywności na forach

library(dplyr)
library(ggplot2)

summarise_religions <- function(){
    # W funkcji tworzona jest ramka danych Religions
    # Religions ma 5 wierszy, po jednym na każdą religię
    # Zmienne to sumaryczna ilość użytkowników, pytań, odpowiedzi, itp.
    # W ramce znajduje się dodatkowa kolumny
    # Adherants = liczba wyznawców (dane z wikipedii)
    # Funkcja zwraca utworzoną ramkę Religions
    
    # Stworzenie ramki, ręczne wpisanie danych z wikipedii
    Religions <- data.frame(
        Religion = c("Buddhism", "Christisnity", "Hinduism", "Islam", "Judaism"),
        Adherents = c(506*(10**6), 2.382*(10**9), 1.161*(10**9), 1.907*(10**9), 14.7*(10**6))
    )
    rownames(Religions) <- c("Buddhism", "Christisnity", "Hinduism", "Islam", "Judaism")
    
    # Zliczanie odpowiednich statystyk
    Religions$Users <- c(
                        length(pull(filter(BUsers, Id!=-1), Id)),
                        length(pull(filter(CUsers, Id!=-1), Id)),
                        length(pull(filter(HUsers, Id!=-1), Id)),
                        length(pull(filter(IUsers, Id!=-1), Id)),
                        length(pull(filter(JUsers, Id!=-1), Id))
                        )
    
    Religions$Questions <- c(
                            length(pull(filter(BPosts, PostTypeId==1), Id)),
                            length(pull(filter(CPosts, PostTypeId==1), Id)),
                            length(pull(filter(HPosts, PostTypeId==1), Id)),
                            length(pull(filter(IPosts, PostTypeId==1), Id)),
                            length(pull(filter(JPosts, PostTypeId==1), Id))
                            )
    
    Religions$Answers <- c(
                          length(pull(filter(BPosts, PostTypeId==2), Id)),
                          length(pull(filter(CPosts, PostTypeId==2), Id)),
                          length(pull(filter(HPosts, PostTypeId==2), Id)),
                          length(pull(filter(IPosts, PostTypeId==2), Id)),
                          length(pull(filter(JPosts, PostTypeId==2), Id))
                          )
    
    Religions$PostLinks <- c(
                            length(pull(filter(BPostLinks), Id)),
                            length(pull(filter(CPostLinks), Id)),
                            length(pull(filter(HPostLinks), Id)),
                            length(pull(filter(IPostLinks), Id)),
                            length(pull(filter(JPostLinks), Id))
                            )
    
    Religions$Comments <- c(
                           length(pull(filter(BComments), Id)),
                           length(pull(filter(CComments), Id)),
                           length(pull(filter(HComments), Id)),
                           length(pull(filter(IComments), Id)),
                           length(pull(filter(JComments), Id))
                           )
    
    Religions$Votes <- c(
                        length(pull(filter(BVotes), Id)),
                        length(pull(filter(CVotes), Id)),
                        length(pull(filter(HVotes), Id)),
                        length(pull(filter(IVotes), Id)),
                        length(pull(filter(JVotes), Id))
                        )
    
    # Zwrócenie ramki
    Religions
}

Religions <- summarise_religions()

# rysowanie wykresu
# do przemyślenia czy damy radę zamknąć to w funkcję
# dla reszty religii automatycznie

# własna paleta kolorów
# trzeba wybrać jakieś ładne, które zawsze będą takie same z konkretną religią
religion_colors <- c("#99F7AB", "#F0EC57", "#79A9D1", "#FF8C42", "#89043D")
# inicjacja wykresu
Users_plot <- ggplot(data = Religions)
# zmiana palety kolorów
Users_plot <- Users_plot + scale_fill_manual(values = religion_colors)
# dodanie słupków
Users_plot <- Users_plot + geom_bar(mapping = aes(x = Religion, y = Users, fill=Religion), color = "black", stat = "identity")
# pokazanie wykresu
Users_plot
