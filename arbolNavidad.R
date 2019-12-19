# Hacer es un árbol de navidad

install.packages("EnvStats")
library(EnvStats)
library(ggplot2)
library(dplyr)

n.sample = 10000
min.val = 5
max.val = 10
mode.val = 7.5
crea.esferas <- function(dt.orig, index.process, displacement = 0.02) {
  coord.get <- dt.orig[index.process,]
  coord.get$rtri.n <- coord.get$rtri.n - sample(seq(0.001, 0.05, 0.001), 1)
  coord.get$ptri.n <- sample(seq(0, coord.get$ptri.n, displacement), 1)
  return(coord.get)
}

tri.dist <- data.frame(rtri.n = rtri(n.sample, min = min.val, max = max.val, mode = mode.val)) %>%
  mutate(ptri.n = dtri(rtri.n, min = min.val, max = max.val, mode = mode.val)) %>%
  arrange(ptri.n)


follaje <- sample(1:nrow(tri.dist), 10000, replace = T) %>%
  lapply(., function(x) crea.esferas(tri.dist, x,displacement = 0.01)) %>%
  bind_rows()

esferas.rojas <- sample(1:nrow(tri.dist), 15) %>%
  lapply(., function(x) crea.esferas(tri.dist, x, displacement = 0.0001)) %>%
  bind_rows()

esferas.azules <- sample(1:nrow(tri.dist), 10) %>%
  lapply(., function(x) crea.esferas(tri.dist, x, displacement = 0.0001)) %>%
  bind_rows()

esferas.amarillas <- sample(1:nrow(tri.dist), 12) %>%
  lapply(., function(x) crea.esferas(tri.dist, x, displacement = 0.0001)) %>%
  bind_rows()

estrella <- tri.dist %>%
  filter(ptri.n == max(ptri.n)) %>%
  mutate(rtri.n = mean(tri.dist$rtri.n))




plot(x = follaje$rtri.n, y = follaje$ptri.n, 
     xlim = c(min(tri.dist$rtri.n) -.5, max(tri.dist$rtri.n) + .5),
     ylim = c(-.05, max(tri.dist$ptri.n) + 0.05), 
     type="n",xlab="",ylab="",xaxt="n",yaxt="n")
rect(7,-.05,8,0, col="tan3", border="tan4", lwd=3)
points(x = follaje$rtri.n, y = follaje$ptri.n, col = 'green', pch = 8)

#xleft, ybottom, xright, ytop


points(esferas.rojas$rtri.n, esferas.rojas$ptri.n, col = "red", pch = 19, cex = 3)
points(esferas.azules$rtri.n, esferas.azules$ptri.n, col = "blue", pch = 19, cex = 3)
points(esferas.amarillas$rtri.n, esferas.amarillas$ptri.n, col = "yellow", pch = 19, cex = 3)
points(estrella$rtri.n, estrella$ptri.n, col = "purple", pch = '*', cex = 12)


text(x = 5.7, y = .4, paste("Feliz navidad!"), cex = 1.6, col = "purple", font = 2)
text(x = 9.2, y = .4, paste("R_Ladies"), cex = 1.6, col = "purple", font = 4)
text(x = 9.2, y = .35, paste("CDMX"), cex = 1.6, col = "purple", font = 4)
