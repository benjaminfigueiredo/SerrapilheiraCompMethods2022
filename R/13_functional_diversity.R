library(vegan)

comm <- read.csv("data/raw/cestes/comm.csv")
traits <- read.csv("data/raw/cestes/traits.csv")

rownames(comm) <- paste0("Site", comm[,1])
comm <- comm[,-1]

rownames(traits) <- traits$Sp
traits <- traits[,-1]

richness <- vegan::specnumber(comm)
richness

# the standard entropy-based diversity metrics

shannon <- vegan::diversity(comm)
shannon
simpson <- vegan::diversity(comm, index = "simpson")
simpson

# now we go to functional traits

gow <- cluster::daisy(traits, metric = "gower")
gow # the gower metric measures the distance in trait space among many different data types
gow2 <- FD::gowdis(traits)
#implementations in R vary and the literature reports extensions and modifications
identical(gow, gow2) #not the same but why?
plot(gow, gow2, asp = 1) # not the same class, but are identical as we can see in plot

# now we can calculate rao's quadratic entropy
library(SYNCSA)
tax <- rao.diversity(comm)
fun <- rao.diversity(comm, traits = traits)
tax
plot(fun$Simpson,fun$FunRao, pch = 19, asp = 1)
abline(a = 0, b = 1) # identity line to situate the points

library(FD)
FuncDiv1 <- dbFD(x = gow, a = comm, messages = F)
names(FuncDiv1)
FuncDiv <- dbFD(x = traits, a = comm, messages = F)
FuncDiv # here we see a whole lot of metrics of functional diversity, inlcude the rao thing above

# after that we did a whole lot of phylogenetic tree construction that I didn't manage to follow
