#Plik zawieraj¹cy kod do przekonwertowania wszystkich plików .xml z archiwum na
#ramki danych i zapisania ich jako pliki .csv
#Przyk³ady konwersji w wyk³adzie 8. z PDU z tego roku.

#To plik tylko ¿eby pokazaæ, jak zosta³o to zrobione, dane s¹ ju¿ gotowe do u¿ycia

library('XML')

#Konwertowanie plików z pomoc¹ pakietu XML
x <- xmlToList(xmlParse("Badges.xml"))
x <- as.data.frame(do.call(rbind, x))
rownames(x) <- NULL
write.csv(x,"Badges.csv", row.names = FALSE)

x <- xmlToList(xmlParse("Comments.xml"))
x <- as.data.frame(do.call(rbind, x))
rownames(x) <- NULL
write.csv(x,"Comments.csv", row.names = FALSE)

x <- xmlToList(xmlParse("PostHistory.xml"))
x <- as.data.frame(do.call(rbind, x))
rownames(x) <- NULL
write.csv(x,"PostHistory.csv", row.names = FALSE)

x <- xmlToList(xmlParse("PostLinks.xml"))
x <- as.data.frame(do.call(rbind, x))
rownames(x) <- NULL
write.csv(x,"PostLinks.csv", row.names = FALSE)

x <- xmlToList(xmlParse("Posts.xml"))
x <- as.data.frame(do.call(rbind, x))
rownames(x) <- NULL
write.csv(x,"Posts.csv", row.names = FALSE)

x <- xmlToList(xmlParse("Tags.xml"))
x <- as.data.frame(do.call(rbind, x))
rownames(x) <- NULL
write.csv(x,"Tags.csv", row.names = FALSE)

x <- xmlToList(xmlParse("Users.xml"))
x <- as.data.frame(do.call(rbind, x))
rownames(x) <- NULL
write.csv(x,"Users.csv", row.names = FALSE)

x <- xmlToList(xmlParse("Votes.xml"))
x <- as.data.frame(do.call(rbind, x))
rownames(x) <- NULL
write.csv(x,"Votes.csv", row.names = FALSE)
