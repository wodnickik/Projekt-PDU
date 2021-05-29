# Porównanie Podstawowych statystyk aktywności na forach

library(dplyr)

Religions <- data.frame(
    Adherents = c(506*(10**6), 2.382*(10**9), 1.161*(10**9), 1.907*(10**9), 14.7*(10**6))
)
rownames(Religions) <- c("Buddhism", "Christisnity", "Hinduism", "Islam", "Judaism")

Religions$Users <- c(
                     length(pull(filter(BUsers, Id!=-1), Id)),
                     length(pull(filter(CUsers, Id!=-1), Id)),
                     length(pull(filter(HUsers, Id!=-1), Id)),
                     length(pull(filter(IUsers, Id!=-1), Id)),
                     length(pull(filter(JUsers, Id!=-1), Id))
                    )

barplot(Religions$Users, names.arg = rownames(Religions))
barplot(Religions$Adherents, names.arg = rownames(Religions))
