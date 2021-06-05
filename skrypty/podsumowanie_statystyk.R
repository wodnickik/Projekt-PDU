# Zestawienie zbiorczej aktywności na forach
# ile było danego rodzaju aktywności na forum w sumie, bez podziału na okresy czasowe

library(dplyr)
library(ggplot2)
library(scales)

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
    
    # zapisanie ramki
    write.csv(Religions, "dane/posrednie/Religions_summary.csv")
    
    # Zwrócenie ramki
    Religions
}

draw_summary <- function(){
    # w funkcji eysowane i zapisywane są wykresy
    # zestawiające odpowiednie statystyki
    
    Religions <- summarise_religions()
    
    # rysowanie wykresu
    # do przemyślenia czy damy radę zamknąć to w funkcję
    # dla reszty religii automatycznie
    
    # własna paleta kolorów
    # trzeba wybrać jakieś ładne, które zawsze będą takie same z konkretną religią
    religion_colors <- c("#FF8133", "#FFE900", "#0094C6", "#1A6318", "#89043D")
    
    # users
    Users_plot <- ggplot(data = Religions) +
        scale_fill_manual(values = religion_colors) +
        geom_bar(mapping = aes(x = Religion, y = Users, fill=Religion), color = "black", stat = "identity") +
        labs(x = "Religia", angle = 90, y = "Liczba",
             title = "Liczba użytkowników na odpowiednich forach.") +
        theme(
            legend.title = element_text( size = 16),
            legend.text = element_text( size = 14),
            plot.title = element_text(size=22)
        )
    ggsave(filename = "prezentacja/wykresy/sum_users.png", plot = Users_plot)
    
    # questions
    Questions_plot <- ggplot(data = Religions) +
        scale_fill_manual(values = religion_colors) +
        geom_bar(mapping = aes(x = Religion, y = Questions, fill=Religion), color = "black", stat = "identity") +
        labs(x = "Religia", angle = 90, y = "Liczba",
             title = "Liczba pytań na odpowiednich forach.") +
        theme(
            legend.title = element_text( size = 16),
            legend.text = element_text( size = 14),
            plot.title = element_text(size=22)
        )
    ggsave(filename = "prezentacja/wykresy/sum_questions.png", plot = Questions_plot)
    
    # answers
    Answers_plot <- ggplot(data = Religions) +
        scale_fill_manual(values = religion_colors) +
        geom_bar(mapping = aes(x = Religion, y = Answers, fill=Religion), color = "black", stat = "identity") +
        labs(x = "Religia", angle = 90, y = "Liczba",
             title = "Liczba odpowiedzi na odpowiednich forach.") +
        theme(
            legend.title = element_text( size = 16),
            legend.text = element_text( size = 14),
            plot.title = element_text(size=22)
        )
    ggsave(filename = "prezentacja/wykresy/sum_answers.png", plot = Answers_plot)
    
    # links
    Links_plot <- ggplot(data = Religions) +
        scale_fill_manual(values = religion_colors) +
        geom_bar(mapping = aes(x = Religion, y = PostLinks, fill=Religion), color = "black", stat = "identity") +
        labs(x = "Religia", angle = 90, y = "Liczba",
             title = "Liczba linków na odpowiednich forach.") +
        theme(
            legend.title = element_text( size = 16),
            legend.text = element_text( size = 14),
            plot.title = element_text(size=22)
        )
    ggsave(filename = "prezentacja/wykresy/sum_links.png", plot = Links_plot)
    
    # comments
    Comments_plot <- ggplot(data = Religions) +
        scale_fill_manual(values = religion_colors) +
        geom_bar(mapping = aes(x = Religion, y = Comments, fill=Religion), color = "black", stat = "identity") +
        labs(x = "Religia", angle = 90, y = "Liczba",
             title = "Liczba komentarzy na odpowiednich forach.") +
        theme(
            legend.title = element_text( size = 16),
            legend.text = element_text( size = 14),
            plot.title = element_text(size=22)
        )
    ggsave(filename = "prezentacja/wykresy/sum_comments.png", plot = Comments_plot)
    
    # votes
    Votes_plot <- ggplot(data = Religions) +
        scale_fill_manual(values = religion_colors) +
        geom_bar(mapping = aes(x = Religion, y = Votes, fill=Religion), color = "black", stat = "identity") +
        labs(x = "Religia", angle = 90, y = "Liczba",
             title = "Liczba głosów na odpowiednich forach.") +
        theme(
            legend.title = element_text( size = 16),
            legend.text = element_text( size = 14),
            plot.title = element_text(size=22)
        )
    ggsave(filename = "prezentacja/wykresy/sum_votes.png", plot = Votes_plot)
}

draw_religions <- function(){
    # w funkcji rysowany i zapisywany jest jeden wykres
    # wykres porównuje liczbość wszystkich religii
    
    # inicjujemy ramkę i własne kolory
    Religions <- summarise_religions()
    religion_colors <- c("#FF8133", "#FFE900", "#0094C6", "#1A6318", "#89043D")
    
    # rysujemy wykres
    Pie_religions <- ggplot(Religions, aes(x = "", y = Adherents, fill=Religion)) +
        scale_fill_manual(values = religion_colors) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        theme_void() + 
        geom_text(aes(x=1.7, y = cumsum(Adherents)[length(Adherents)] - cumsum(Adherents) + Adherents/2, 
                      label = percent(Adherents/sum(Adherents))), size=5) +
        labs(title = "Stosunkowa ilość wyznawców.") +
        theme(
            legend.title = element_text( size = 16),
            legend.text = element_text( size = 14),
            plot.title = element_text(size=22)
        )
    ggsave(filename = "prezentacja/wykresy/pie_religions.png", plot = Pie_religions)
}
