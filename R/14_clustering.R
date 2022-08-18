# let's learn some clustering!

library(vegan)
data(dune)
data(dune.env)
table(dune.env$Management)

dune

bray_distance <- vegdist(dune)
# Chord distance, euclidean distance normalized to 1.
chord_distance <- dist(decostand(dune, "norm"))

# now we cluster the dune species data according to these two metrics
b_cluster <- hclust(bray_distance, method = "average")
c_cluster <- hclust(chord_distance, method = "average")

par(mfrow = c(1, 2))
plot(b_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)
plot(c_cluster, hang = -1, main = "", axes = F)
axis(2, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1), las = 2)


