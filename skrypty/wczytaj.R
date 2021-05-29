# Plik do wczytania wszystkich ramek danych z plików csv.
# Konwencja nazewnictwa - odpowiednie przedrostki odpowiadają danym religiom
# B - Buddyzm
# C- Chrześcijaństwo
# H - Hinduizm
# I - Islam
# J - Judaizm

# Zakładamy, że jesteśmy w głównym katalogu projektu
# Dane zapisane są w folderze "dane/nazwa_religii"

#BUDDYZM

path <- file.path("dane", "buddhism")
BBadges <- read.csv(file.path(path, "Badges.csv"))
BComments <- read.csv(file.path(path, "Comments.csv"))
BPostHistory <- read.csv(file.path(path, "PostHistory.csv"))
BPostLinks <- read.csv(file.path(path, "PostLinks.csv"))
BPosts <- read.csv(file.path(path, "Posts.csv"))
BTags <- read.csv(file.path(path, "Tags.csv"))
BUsers <- read.csv(file.path(path, "Users.csv"))
BVotes <- read.csv(file.path(path, "Votes.csv"))

#CHRZEŚCIJAŃSTWO

path <- file.path("dane", "christianity")
CBadges <- read.csv(file.path(path, "Badges.csv"))
CComments <- read.csv(file.path(path, "Comments.csv"))
CPostHistory <- read.csv(file.path(path, "PostHistory.csv"))
CPostLinks <- read.csv(file.path(path, "PostLinks.csv"))
CPosts <- read.csv(file.path(path, "Posts.csv"))
CTags <- read.csv(file.path(path, "Tags.csv"))
CUsers <- read.csv(file.path(path, "Users.csv"))
CVotes <- read.csv(file.path(path, "Votes.csv"))

#HINDUIZM

path <- file.path("dane", "hinduism")
HBadges <- read.csv(file.path(path, "Badges.csv"))
HComments <- read.csv(file.path(path, "Comments.csv"))
HPostHistory <- read.csv(file.path(path, "PostHistory.csv"))
HPostLinks <- read.csv(file.path(path, "PostLinks.csv"))
HPosts <- read.csv(file.path(path, "Posts.csv"))
HTags <- read.csv(file.path(path, "Tags.csv"))
HUsers <- read.csv(file.path(path, "Users.csv"))
HVotes <- read.csv(file.path(path, "Votes.csv"))

#ISLAM

path <- file.path("dane", "islam")
IBadges <- read.csv(file.path(path, "Badges.csv"))
IComments <- read.csv(file.path(path, "Comments.csv"))
IPostHistory <- read.csv(file.path(path, "PostHistory.csv"))
IPostLinks <- read.csv(file.path(path, "PostLinks.csv"))
IPosts <- read.csv(file.path(path, "Posts.csv"))
ITags <- read.csv(file.path(path, "Tags.csv"))
IUsers <- read.csv(file.path(path, "Users.csv"))
IVotes <- read.csv(file.path(path, "Votes.csv"))

#JUDAIZM

path <- file.path("dane", "judaism")
JBadges <- read.csv(file.path(path, "Badges.csv"))
JComments <- read.csv(file.path(path, "Comments.csv"))
JPostHistory <- read.csv(file.path(path, "PostHistory.csv"))
JPostLinks <- read.csv(file.path(path, "PostLinks.csv"))
JPosts <- read.csv(file.path(path, "Posts.csv"))
JTags <- read.csv(file.path(path, "Tags.csv"))
JUsers <- read.csv(file.path(path, "Users.csv"))
JVotes <- read.csv(file.path(path, "Votes.csv"))