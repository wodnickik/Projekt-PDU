#Plik do wczytania wszystkich ramek danych z plików csv.
#Konwencja nazewnictwa - odpowiednie przedrostki odpowiadają danym religiom
#B - Buddyzm, C- Chrześcijaństwo, H - Hinduizm, I - Islam, J - Judaizm

#Trzeba ustawić odpowiednią ścieżkę do folderu Dane w projekcie
setwd("C:/Users/Ja/Desktop/Dane")

#BUDDYZM

setwd("Buddhism")
BBadges <- read.csv("Badges.csv")
BComments <- read.csv("Comments.csv")
BPostHistory <- read.csv("PostHistory.csv")
BPostLinks <- read.csv("PostLinks.csv")
BPosts <- read.csv("Posts.csv")
BTags <- read.csv("Tags.csv")
BUsers <- read.csv("Users.csv")
BVotes <- read.csv("Votes.csv")

#CHRZEŚCIJAŃSTWO

setwd("C:/Users/Ja/Desktop/Dane")
setwd("Christianity")
CBadges <- read.csv("Badges.csv")
CComments <- read.csv("Comments.csv")
CPostHistory <- read.csv("PostHistory.csv")
CPostLinks <- read.csv("PostLinks.csv")
CPosts <- read.csv("Posts.csv")
CTags <- read.csv("Tags.csv")
CUsers <- read.csv("Users.csv")
CVotes <- read.csv("Votes.csv")

#HINDUIZM

setwd("C:/Users/Ja/Desktop/Dane")
setwd("Hinduism")
HBadges <- read.csv("Badges.csv")
HComments <- read.csv("Comments.csv")
HPostHistory <- read.csv("PostHistory.csv")
HPostLinks <- read.csv("PostLinks.csv")
HPosts <- read.csv("Posts.csv")
HTags <- read.csv("Tags.csv")
HUsers <- read.csv("Users.csv")
HVotes <- read.csv("Votes.csv")

#ISLAM

setwd("C:/Users/Ja/Desktop/Dane")
setwd("Islam")
IBadges <- read.csv("Badges.csv")
IComments <- read.csv("Comments.csv")
IPostHistory <- read.csv("PostHistory.csv")
IPostLinks <- read.csv("PostLinks.csv")
IPosts <- read.csv("Posts.csv")
ITags <- read.csv("Tags.csv")
IUsers <- read.csv("Users.csv")
IVotes <- read.csv("Votes.csv")

#JUDAIZM

setwd("C:/Users/Ja/Desktop/Dane")
setwd("Judaism")
JBadges <- read.csv("Badges.csv")
JComments <- read.csv("Comments.csv")
JPostHistory <- read.csv("PostHistory.csv")
JPostLinks <- read.csv("PostLinks.csv")
JPosts <- read.csv("Posts.csv")
JTags <- read.csv("Tags.csv")
JUsers <- read.csv("Users.csv")
JVotes <- read.csv("Votes.csv")
