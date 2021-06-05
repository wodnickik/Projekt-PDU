library(dplyr)
library(ggplot2)

time_to_answer <- function(Posts){
    # Funkcja oblicza czas uzyskania odpowiedzi na pytania
    # jako argument przyjmowana jest ramka z postami
    # zwracan jest ramka z czasami odpowiedzi na każdy z postów który jest pytaniem
    
    
    # wybierami pytania i potrzebne kolumny
    Questions <- Posts %>%
        filter(PostTypeId == 1) %>%
        select(Id, AcceptedAnswerId, CreationDate)
    
    # wybieramy odpowiedzi i potrzebne kolumny
    Answers <- Posts %>%
        filter(PostTypeId == 2) %>%
        select(Id, CreationDate)
    
    # łączymy i porządkujemy ramki
    QandA <- inner_join(x = Questions, y = Answers, by = c("AcceptedAnswerId" = "Id"))
    QandA <- rename(QandA, QId=Id, AId=AcceptedAnswerId, QCreationDate=CreationDate.x, ACreationDate=CreationDate.y)
    
    # obliczamy czas pomiędzy napisaniem pytania a napisaniem odpowiedzi
    format <- "%Y-%m-%dT%H:%M:%S"
    QandA <- QandA %>%
        mutate(TimeToAnswer=(difftime(
            as.POSIXlt(ACreationDate, format=format, tz="UTC"),
            as.POSIXlt(QCreationDate, format=format, tz="UTC"),
            units="hours"
            )))
}

answer_speed_stats <- function(){
    # Funkcja tworzy i zwraca ramkę danych
    # z zestawieniem statystyk dotyczących czasu odpowiedzi
    
    # tworzymy ramkę
    Religions <- data.frame(
        Religion = c("Buddhism", "Christisnity", "Hinduism", "Islam", "Judaism")
    )
    
    # obliczamy czasy odpowiedzi
    BTime <- time_to_answer(BPosts)
    CTime <- time_to_answer(CPosts)
    HTime <- time_to_answer(HPosts)
    ITime <- time_to_answer(IPosts)
    JTime <- time_to_answer(JPosts)
    
    # max
    Religions$MaxTime = c(
        max(BTime$TimeToAnswer),
        max(CTime$TimeToAnswer),
        max(HTime$TimeToAnswer),
        max(ITime$TimeToAnswer),
        max(JTime$TimeToAnswer)
        )
    
    # min
    # w jakiś tajemniczy sposób większość czsów jest 0
    # a nawet jednej jest ujemny
    # wydaje mi się że wina leży po stronie danych a nie metody
    Religions$MinTime = c(
        min(BTime$TimeToAnswer),
        min(CTime$TimeToAnswer),
        min(HTime$TimeToAnswer),
        min(ITime$TimeToAnswer),
        min(JTime$TimeToAnswer)
    )
    
    # mediana
    Religions$MedianTime = c(
        median(BTime$TimeToAnswer),
        median(CTime$TimeToAnswer),
        median(HTime$TimeToAnswer),
        median(ITime$TimeToAnswer),
        median(JTime$TimeToAnswer)
    )
    
    # średnia
    Religions$MeanTime = c(
        mean(BTime$TimeToAnswer),
        mean(CTime$TimeToAnswer),
        mean(HTime$TimeToAnswer),
        mean(ITime$TimeToAnswer),
        mean(JTime$TimeToAnswer)
    )
    
    # zapisanie ramki
    write.csv(Religions, "dane/posrednie/Answer_time.csv")
    
    # zwrócenie ramki
    Religions
}

answer_speed_stats2 <- function(){
    # Podobne do pierwszej wersji
    # W tej ramce wszystkie czasy są w jednej kolumnie
    # jest dodatkowa kolumna Type
    # Type mówi czy to Max/Min/Mean/Median
    
    Religions <- answer_speed_stats()
    
    MaxTimes <- select(Religions, Religion, MaxTime)
    MaxTimes <- rename(MaxTimes, Time=MaxTime)
    MaxTimes$Type <- rep("Max", 5)
    
    MinTimes <- select(Religions, Religion, MinTime)
    MinTimes <- rename(MinTimes, Time=MinTime)
    MinTimes$Type <- rep("Min", 5)
    
    MedianTimes <- select(Religions, Religion, MedianTime)
    MedianTimes <- rename(MedianTimes, Time=MedianTime)
    MedianTimes$Type <- rep("Median", 5)
    
    MeanTimes <- select(Religions, Religion, MeanTime)
    MeanTimes <- rename(MeanTimes, Time=MeanTime)
    MeanTimes$Type <- rep("Mean", 5)
    
    Religions <- bind_rows(MinTimes, MedianTimes, MeanTimes, MaxTimes)
    
    Religions
}

draw_answer_speed <- function(){
    # Funkcja rysuje i zapisuje wykresy zestawiające czasy odpowiedzi
    
    # wczytujemy odpowiedznie dane
    Religions <- answer_speed_stats()
    
    # ustawiamy własne kolory
    religion_colors <- c("#FF8133", "#FFE900", "#0094C6", "#1A6318", "#89043D")
    
    # rysujemy wykres z medianą
    ggplot(data = Religions, aes(x=Religion, y=MedianTime, fill=Religion)) +
        geom_bar(stat="identity", color="Black") +
        scale_fill_manual(values=religion_colors) +
        labs(x = "Religia", angle = 90, y = "Mediana (godz.)",
             title = "Mediana czasu oczekiwania na odpowiedź na odpowiednich forach.")
    ggsave(filename = "prezentacja/wykresy/median_times.png")
    
    # rysujemy wykres ze średnią
    ggplot(data = Religions, aes(x=Religion, y=MeanTime, fill=Religion)) +
        geom_bar(stat="identity", color="Black") +
        scale_fill_manual(values=religion_colors) +
        labs(x = "Religia", angle = 90, y = "Średnia (godz.)",
             title = "Średni czas oczekiwania na odpowiedź na odpowiednich forach.")
    ggsave(filename = "prezentacja/wykresy/mean_times.png")
    
    # rysujemy wykres z maksimum
    ggplot(data = Religions, aes(x=Religion, y=MaxTime, fill=Religion)) +
        geom_bar(stat="identity", color="Black") +
        scale_fill_manual(values=religion_colors) +
        labs(x = "Religia", angle = 90, y = "Maksimum (godz.)",
             title = "Maksymalny czas oczekiwania na odpowiedź na odpowiednich forach.")
    ggsave(filename = "prezentacja/wykresy/max_times.png")
}