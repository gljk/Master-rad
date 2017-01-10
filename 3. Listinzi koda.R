#Listing 1 Autentikacija na Tviter API koristeći R paket twitteR
require(twitteR)
require(igraph)
consumer_key <- ""
consumer_secret <-
  ""
access_token <- ""
access_secret <- ""

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Listing 2 Demonstracija prikupljanja tvitova za jedan nalog pomoću R paketa twitteR	27
tvitoviNaloga <- userTimeline("Nalog", includeRts = TRUE, n = 3200)
#Argumenti n i includeRts označavaju broj tvitova i inkluziju retvitova.
#Konverzija dobijenog objekta u R dataframe:
tvitoviNaloga <- twListToDF(tvitoviNaloga)

#Listing 3 Funkcija za prikupljanje tvitova sa više naloga, tvitovanih u izabranom vremenskom periodu
#Funkcija ekstrakcija() kao ulaz prima spisak naloga, i dva vremenska perioda
#početak i kraj vremenskog intervala za koji želimo tvitove

ekstrakcija <- function(spisak, od = NULL, do = NULL) {
  tvitovi <- data.frame()
  for (i in 1:length(spisak)) {
    a <- userTimeline(spisak [i], n = 3200, includeRts = TRUE)
    a <- twListToDF(a)
    if (!is.null(od)) {
      a <- a[a$created > od,]
    }
    if (!is.null(do)) {
      a <- a[a$created < do,]
    }
    tvitovi <- rbind(tvitovi, a)
    if (as.numeric(getCurRateLimitInfo()[39, 3]) < 5) {
      Sys.sleep(60 * as.numeric(getCurRateLimitInfo()[39, 4] - Sys.time()) + 10)
    }
  }
  return(tvitovi)
}

#Listing 4 Demonstracija upotrebe funkcije za akviziciju podataka iz Listing 3
#Učitavamo unapred pripremljenu datoteku sa podacima
političari <-
  read.csv(
    "političari.csv",
    header = TRUE,
    encoding = "UTF-8",
    stringsAsFactors = F
  )

#Primenjujemo funkciju na kolonu Nalog, CSV datoteke političari.csv
tvitovi <-
  ekstrakcija(spisak = političari$Nalog,
              od = "2016-03-04 00:00:00 CET",
              do = "2016-05-05 00:00:00 CET")

#Listing 5 Funkcija za primenu snowball metode za širenje uzorka

#Funkcija snowball uzima podatke sa Tvitera i kao rezultat daje imena naloga orignalnog uzorka, i sve one naloge sa kojima su originalni nalozi nadposečno često komunicirali

snowball <- function(x) {
  sb <-
    names(sort(table(x$replyToSN)[table(x$replyToSN) > mean(table(x$replyToSN))],
               decreasing = T))
  sb <- sb[!(sb %in% unique(x$screenName))]
  return(c(unique(x$screenName), sb))
}

#Listing 6 Primena funkcije snowball() i prikupljanje podataka za prošireni uzorak
#Primena snowball funkcije i akvizicija podataka za novi uzorak
prošireni_uzorak <- snowball(tvitovi)
tvitovi.p.u <- ekstrakcija(prošireni_uzorak)

#Listing 7 Preuzimanje podataka  o nalozima sa Tviter servera
naloziMeta <- function(spisak) {
  poltab <- matrix(nrow = length(spisak), ncol = 6)
  for (i in 1:length(spisak)) {
    a <- getUser(spisak[i])
    poltab[i, 1] <- a$screenName
    poltab[i, 2] <- a$followersCount
    poltab[i, 3] <- a$friendsCount
    poltab[i, 4] <- a$statusesCount
    poltab[i, 5] <- a$favoritesCount
    poltab[i, 6] <- as.character(a$created)
  }
  poltab <- as.data.frame(poltab)
  colnames(poltab) <-
    c("Nalog",
      "Pratioci",
      "Prijatelji",
      "Tvitovi",
      "Favorisao",
      "DatumOtvaranja")
  poltab$Pratioci <- as.numeric(as.character(poltab$Pratioci))
  poltab$Prijatelji <- as.numeric(as.character(poltab$Prijatelji))
  poltab$Tvitovi <- as.numeric(as.character(poltab$Tvitovi))
  poltab$Favorisao <- as.numeric(as.character(poltab$Favorisao))
  poltab$DatumOtvaranja <- as.Date(poltab$DatumOtvaranja)
  return(poltab)
}


#Listing 8 Funkcija za konstrukciju matrice povezanosti
#Funkcija matrica_povezanosti kao ulaz ima rezultat funkcije ekstrakcija, a kao rezultat daje matricu povezanosti.

matrica_povezanosti <- function(x) {
  sp <- unique(x$screenName)
  lsp <- length(sp)
  mp <-
    matrix(nrow = lsp, ncol = lsp)
  colnames(mp) <- sp
  row.names(mp) <- sp
  for (i in 1:lsp) {
    l <- x[x$screenName == sp[i],]
    for (j in 1:lsp) {
      if (j == i) {
        j <- j + 1
      }
      if (j > lsp) {
        break
      }
      k <- x[x$screenName == sp[j],]
      if (any(grepl(k$screenName[1], l$text))) {
        mp[i, j] <- table(grepl(k$screenName[1], l$text))["TRUE"]
      }
    }
  }
  mp[is.na(mp)] <- 0
  return(mp)
}

#Listing 9 Primena funkcije matrica_povezanosti na osnovni i prošireni uzorak
#Primena funkcije matrica_povezanosti
mat.pol <- matrica_povezanosti(tvitovi)
mat.pol.pu <- matrica_povezanosti(tvitovi.p.u)


#Listing 10 Prebrojavanje broja tvitova i broja njihovih retvitova i favorite za svaki nalog pojedinačno

retfav <- function(tvitovi) {
  sp <- unique(tvitovi$screenName)
  retfav <- matrix(ncol = 4, nrow = length(sp))
  for (i in 1:length(sp)) {
    l <- tvitovi[tvitovi$screenName == sp[i],]
    retfav[i, 1] <- l$screenName[1]
    retfav[i, 2] <- sum(l[l$isRetweet == F, 3])
    retfav[i, 3] <- sum(l[l$isRetweet == F, 12])
    retfav[i, 4] <- length(l[l$isRetweet == F, 12])
  }
  retfav  <- as.data.frame(retfav)
  colnames(retfav) <-
    c("Nalog", "Favorisan", "Retvitovan", "Tvitovi")
  retfav$Favorisan <- as.numeric(as.character(retfav$Favorisan))
  retfav$Retvitovan <- as.numeric(as.character(retfav$Retvitovan))
  retfav$Tvitovi <-  as.numeric(as.character(retfav$Tvitovi))
  retfav$Pros.Ret <- retfav$Retvitovan / retfav$Tvitovi
  return(retfav)
}

#Listing 11 Kreiranje grafova uz pomoć igraph paketa
#Kreiranje grafova pomoću paketa igraph
mreza.pol <- graph.adjacency(mat.pol, mode = "directed", weighted = T)
mreza.pol.pu <-
  graph.adjacency(mat.pol.pu, mode = "directed", weighted = T)

#Listing 12 Dodeljivanje stranačke pripadnosti kao atributa čvorova
#Dodeljivanje atributa
V(mreza.pol)
E(mreza.pol)
V(mreza.pol)$stranka <- as.factor(političari$Stranka)
V(mreza.pol)$prezime <- političari$Prezime
#dodeljivanje atributa se takođe može postići i ugrađenom funkcijom:
set_vertex_attr(mreza.pol, name = "boja", value = političari$Boja)

#Listing 13 Vizualizacija mreže političara bez dodatnih parametara vizualizacije (Slika 4)
plot.igraph(mreza.pol)

#Listing 14 Vizualizacija mreže sa dodatnim parametrima vizualizacije (Slika 5)	36
frl <- layout.fruchterman.reingold(mreza.pol, niter = 100000)

plot.igraph(
  mreza.pol,
  layout = frl,
  edge.width = log(E(mreza.pol)$weight),
  edge.curved = FALSE,
  edge.color = alpha("gray", rescale(
    x = log(E(mreza.pol)$weight), to = c(0.3, 1)
  )),
  edge.arrow.size = 0.1,
  edge.lty = 1,
  vertex.color = V(mreza.pol)$boja,
  vertex.label.color = "black",
  vertex.label.family = "Arial",
  vertex.frame.color = NA,
  vertex.label = političari$Prezime,
  vertex.size = rescale(degree(mreza.pol), to = c(5, 15)),
  vertex.label.cex = 0.7,
  vertex.label.dist = 0.5
)

#Listing 15 Implementacija grafikona košnice (hive plot) u R-u pomoću paketa HiveR (Slika 6)	39

require(HiveR)
hplot <- adj2HPD(mat.pol.pu)
hplot <- mineHPD(hplot, option = "rad <- tot.edge.count")
hplot$nodes$axis[1:40] <- as.integer(političari$Opozicija)
hplot$nodes$axis[41:355] <- as.integer(3)
hplot$nodes$color[1:40] <- političari$Boja
hplot$axis.cols <- c(rep("#bdbdbd", 3))
hplot$edges$color <- "#bdbdbd"
for (i in 1:length(unique(političari$Stranka))) {
  hplot$edges$color[E(mreza.pol.pu)[V(mreza.pol.pu)[V(mreza.pol.pu)$name %in% političari$Nalog[političari$Stranka ==
                                                                                                 unique(političari$Stranka)[i]]] %--% V(mreza.pol.pu)[V(mreza.pol.pu)$opozicija ==
                                                                                                                                                        3]]] <-
    unique(političari$Boja[političari$Stranka == unique(političari$Stranka)[i]])
}
hplot$edges$color <-
  paste0(hplot$edges$color, substring(alpha("gray", rescale(
    x = log(E(mreza.pol.pu)$weight), to = c(0.1, 0.5)
  )), 8, 9))
hplot <- mineHPD(hplot, option = "remove zero edge")
hplot$nodes$size <-
  rescale(x = as.numeric(eigen_centrality(mreza.pol.pu)$vector), to = c(1, 6))
hplot$edges$weight <- rescale(hplot$edges$weight, to = c(1, 10))
plotHive(hplot, bkgnd = "white")

#Listing 16 Izračunavanje metrika celog grafa
met_graf <- function(graf, atribut = NULL) {
  met_graf <- list(
    "Broj čvorova" = vcount(graf),
    "Broj ivica" = ecount(graf),
    "Gustina" = graph.density(graf),
    "Gustina neusmereni" = graph.density(as.undirected(graf, mode = "mutual")),
    "Povezanost" = vertex.connectivity(graf),
    "Povezanost neusmereni" = vertex.connectivity(as.undirected(graf, mode = "collapse")),
    "Dijametar" = diameter(graf, weights = NA),
    "Najudaljeniji" = paste(
      farthest_vertices(graf, weights = NA)$vertices$name[1],
      farthest_vertices(graf, weights = NA)$vertices$name[2],
      sep = " -- "
    ),
    "Prosečna dužina putanje" = average.path.length(graf),
    "Tranzitivnost" = transitivity(graf),
    "Asortativnost stepena" = assortativity.degree(graf),
    "Asortativnost nom" = if (!is.null(atribut)) {
      assortativity.nominal(graf, get.vertex.attribute(graf, atribut))
    },
    "Cetralizacija intermedijanosti" = centralization.betweenness(graf, directed = TRUE, normalized = TRUE)$centralization,
    "Cetralizacija stepena" = centralization.degree(graf, normalized = TRUE, mode = "all")$centralization,
    "Cetralizacija bliskosti" = centralization.closeness(graf, mode = "all")$centralization,
    "Cetralizacija svojstvenog vektora" =  centralization.evcent(graf, directed = TRUE, normalized = TRUE)$centralization
  )
  return(met_graf)
}

#Listing 17 Funkcija za izračunavanje mera centralnosti
met_cent <- function(graf) {
  cent <- as.matrix(
    cbind(
      betweenness(graf),
      degree(graf, mode = "in"),
      degree(graf, mode = "out"),
      closeness(graf, mode = "in"),
      closeness(graf, mode = "out"),
      eigen_centrality(graf)$vector
    )
  )
  colnames(cent) <-
    c(
      "intermedijarnosti",
      "dolaznog stepena",
      "odlaznog stepena",
      "dolazne bliskosti",
      "odlazne bliskosti",
      "svojstvenog vektora"
    )
  p2d <- p2distance::p2distance(cent)
  cent <- as.data.frame(cent)
  cent$P2odstojanje <- as.vector(p2d$p2distance)
  print(paste("Redosled pokazatalja: ", p2d$variables_sort))
  return(cent)
}

#Listing 18 Korelaciona matrica svih dostupnih pokazatelja naloga u mreži političara (Slika 7)
svipokazatelji <-
  cbind(met_cent(mreza.pol),
        naloziMeta(političari$Nalog)[-1],
        retfav(tvitovi)[-1])
svipokazatelji$DatumOtvaranja <-
  as.numeric(abs(
    difftime(svipokazatelji$DatumOtvaranja, Sys.time(), units = "weeks")
  ))
cor(svipokazatelji, method = "spearman")



#Listing 19 Agregacija centralnosti po stranačkoj pripadnosti
#Agregacija
V(mreza.pol)$stranka <- as.character(političari$Stranka)
g1 <-
  aggregate(
    x = met_cent(mreza.pol)$P2odstojanje,
    FUN = sum,
    by = list(
      get.vertex.attribute(mreza.pol, "boja"),
      get.vertex.attribute(mreza.pol, "stranka")
    )
  )
g1$Broj <-
  as.numeric(table(get.vertex.attribute(mreza.pol, "stranka")))
colnames(g1) <- c("Boja", "Stranka", "Centralnost", "Broj")
g1$CentStand <- g1$Centralnost / g1$Broj



#Listing 20 Asortativnost i tranzitivnost vizualizovanih podgrafova pojedinačnih stranaka u osnovnom uzorku (Slika 9)

homofilija <- vector()
tranzitivnost <- vector()
V(mreza.pol)$stranka <- političari$Stranka
for (i in 1:length(unique(V(mreza.pol)$stranka))) {
  x <-
    subgraph.edges(mreza.pol, as.vector(E(mreza.pol)[V(mreza.pol)[V(mreza.pol)$stranka ==
                                                                    unique(V(mreza.pol)$stranka)[i]] %--% 1:length(V(mreza.pol))]))
  x <- as.undirected(x, mode = "mutual")
  x <- delete.vertices(x, v = degree(x) == 0)
  V(x)$stranka <- as.factor(V(x)$stranka)
  homofilija[i] <- assortativity.nominal(x, types = V(x)$stranka)
  tranzitivnost[i] <- transitivity(x)
  intermed <- rep(NA, vcount(x))
  intermed[which(betweenness(x) == max(betweenness(x)))] <-
    "#ff5658"
  svg(paste0(unique(V(mreza.pol)$stranka)[i], ".svg"),
      height = 4.13,
      family = "Arial")
  plot.igraph(
    x,
    edge.curved = F,
    layout = layout.davidson.harel(
      x,
      weight.node.dist = 70,
      weight.node.edge.dist = 70
    ),
    edge.width = log(E(x)$weight),
    vertex.color = ifelse(V(x)$boja == unique(V(mreza.pol)$boja)[i], unique(V(mreza.pol)$boja)[i], "#bdbdbd"),
    vertex.label.color = ifelse(is.na(intermed), "black", "red"),
    vertex.label.dist = 1.3,
    vertex.frame.color = intermed,
    vertex.label = V(x)$prezime,
    vertex.size = rescale(x = degree(x), to = c(10, 30)),
    vertex.label.cex = rescale(x = degree(x), to = c(0.8, 1.2)),
    vertex.label.family = "Arial",
    main = unique(V(mreza.pol)$stranka)[i]
  )
  text(0.7, 1.6, paste("r = ", gsub("*\\.", ",", as.character(
    round(homofilija[i], 2)
  ))), pos = 4)
  text(0.7, 1.4, paste("T = ", gsub("*\\.", ",", as.character(
    round(tranzitivnost[i], 2)
  ))), pos = 4)
  dev.off()
}


#Listing 21 Funkcija za evaluaciju podobnosti različitih metoda klasterovanja	50

klast_eval <- function(graf, atribut, metod) {
  klaster <- eval(parse(text = metod))(graf)
  l <- unique(na.omit(get.vertex.attribute(graf, atribut)))
  y <- vector()
  x <- matrix(ncol = max(klaster$membership), nrow = length(l))
  for (i in 1:max(klaster$membership)) {
    for (j in 1:length(l)) {
      y[j] <-
        table(unlist(communities(klaster)[i]) %in% V(graf)$name[get.vertex.attribute(graf, atribut) ==
                                                                  l[j]])["TRUE"]
    }
    x[, i] <- y
  }
  x[is.na(x)] <- 0
  return(sum(apply(x, 2, sum) - apply(x, 2 , max)))
}

#Listing 22 Primena funkcije klast_eval() na mrežu osnovnog uzorka i dostupne klaster metode

metode <-
  c(
    "cluster_edge_betweenness",
    "cluster_label_prop",
    "cluster_spinglass",
    "cluster_walktrap",
    "cluster_infomap"
  )
for (i in 1:length(metode)) {
  print(paste(metode[i], klast_eval(mreza.pol, "stranka", metode[i])))
}

#Listing 23 Primena Infomap klaster algoritma na mrežu osnovnog i proširenog uzorka

klast.pol <- cluster_infomap(mreza.pol)
klast.pol.pu <- cluster_infomap(mreza.pol.pu)

#Listing 24 Primer koda za generisanje rasporeda na osnovu klastera  Izvor: (Türei, 2013)

layout.modular <- function(graf, klaster) {
  graf$layout <- layout.auto(graf) #inicijalizacija rasporeda
  nm <-
    length(levels(as.factor(klaster$membership))) #broja klastera
  gr <- 2
  while (gr ^ 2 < nm) {
    #u koliko redova će klasteri biti prikazani
    gr <- gr + 1
  }
  i <- j <- 0
  for (cc in levels(as.factor(klaster$membership))) {
    #za svaki klaster
    F <-
      delete.vertices(graf, klaster$membership != cc) #samo klaster
    F$layout <- layout.kamada.kawai(F)
    F$layout <-
      layout.norm(F$layout, i, i + 0.5, j, j + 0.5) #skaliran raspored unutar klastera
    graf$layout[klaster$membership == cc,] <- F$layout
    if (i == gr) {
      #brojači na za položaj čvorova u rasporedu
      i <- 0
      if (j == gr) {
        j <- 0
      } else{
        j <- j + 1
      }
    } else{
      i <- i + 1
    }
  }
  return(graf$layout)
}

#Listing 25 Vizualizacija mreže osnovnog uzorka sa klasterima (Slika 10)
graspored <- layout.modular(mreza.pol, klast.pol)

plot.igraph(
  mreza.pol,
  layout = graspored,
  edge.width = log(E(mreza.pol)$weight),
  edge.curved = FALSE,
  edge.color = alpha("gray", rescale(
    x = log(E(mreza.pol)$weight), to = c(0.3, 1)
  )),
  edge.arrow.size = 0.1,
  edge.lty = 1,
  vertex.color = V(mreza.pol)$boja,
  vertex.label.color = "black",
  vertex.label.family = "Arial",
  vertex.frame.color = NA,
  vertex.label = političari$Prezime,
  vertex.size = rescale(degree(mreza.pol), to = c(5, 15)),
  vertex.label.cex = 0.7,
  vertex.label.dist = 0.5,
  mark.groups = klast.pol
)
