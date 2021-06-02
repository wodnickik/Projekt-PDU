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
    
    format <- "%Y-%m-%dT%H:%M:%S"
    QandA <- QandA %>%
        mutate(TimeToAnswer=(difftime(
            as.POSIXlt(ACreationDate, format=format),
            as.POSIXlt(QCreationDate, format=format),
            units="mins"
            )))
}

answer_speed_stats <- function(){
    Religions <- data.frame(
        Religion = c("Buddhism", "Christisnity", "Hinduism", "Islam", "Judaism")
    )
    BTime <- time_to_answer(BPosts)
    CTime <- time_to_answer(CPosts)
    HTime <- time_to_answer(HPosts)
    ITime <- time_to_answer(IPosts)
    JTime <- time_to_answer(JPosts)
    Religions$MaxTime = c(
        max(BTime$TimeToAnswer),
        max(CTime$TimeToAnswer),
        max(HTime$TimeToAnswer),
        max(ITime$TimeToAnswer),
        max(JTime$TimeToAnswer)
        )
}