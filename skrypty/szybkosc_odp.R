library(dplyr)

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
            units="mins"
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
    
    Religions
}

draw_answer_speed <- function(){
    Religions <- answer_speed_stats()
    
}