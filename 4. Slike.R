#Slika 2 R paketi za analizu društvenih mreža prema broju preuzimanja sa CRAN servera u periodu od 29. novembra do 29. decembra 2016. god
require(ggplot2)
require(ggthemes)
require(grid)
require(ggrepel)
require(extrafont)
require(cranlogs)

snapak <-
  c(
    "igraph",
    "sna",
    "statnet",
    "network",
    "egonet",
    "NetSim",
    "amen",
    "staTools",
    "SocialNetworks",
    "assortnet",
    "Dominance",
    "DCG",
    "hierformR",
    "strategicplayers",
    "PAFit",
    "multiplex",
    "asnipe",
    "HiveR"
  )
d2 <- cran_downloads(packages = snapak, when = "last-month")[, 2:3]

snag <- aggregate(count ~ ., d2, FUN = sum)

snagplot <-
  ggplot(snag, aes(x = reorder(package, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(trans = "sqrt",
                     breaks = c(200, 20000, 40000, 60000, 80000)) +
  theme_tufte(base_family = "Arial") +
  xlab(NULL) +
  ylab("Broj preuzimanja") +
  geom_hline(
    yintercept = c(200, 20000, 40000, 60000, 80000),
    col = "white",
    lwd = 0.72
  ) +
  theme(axis.title = element_text(color = "grey20"))

ggsave(
  snagplot,
  filename = "snag.svg",
  width = 160,
  height = 100,
  units = "mm"
)


#Slika 3 Softverski alati za analizu društvenih mreža prema: okruženju, težini savladavanja i mogućnostima

tm <- read.csv("tm.csv",
               header = T,
               sep = "\t",
               encoding = "UTF-8")

tm$f <- tm$Okruženje
levels(tm$f) <- c("Arial", "Franklin Gothic Heavy", "Impact")

tmplot <-
  ggplot(tm,
         aes(
           y = Moć,
           x = Teškoća,
           col = Okruženje,
           label = Softver,
           family = f
         )) +
  geom_text_repel() +
  geom_segment(
    aes(
      x = 0,
      xend = 0,
      y = -1,
      yend = 1
    ),
    size = 0.85,
    arrow = arrow(length = unit(0.5, "cm")),
    show.legend = F
  ) +
  geom_segment(
    aes(
      x = -1,
      xend = 1,
      y = 0,
      yend = 0
    ),
    size = 0.85,
    arrow = arrow(length = unit(0.5, "cm")),
    show.legend = F
  ) +
  xlab("Težina savladavanja softvera") +
  ylab("Mogućnosti, fleskibilnost, podrška zajednice") +
  theme_tufte() +
  theme(
    text = element_text(size = 12),
    legend.text = element_blank(),
    legend.title = element_text(face = "bold"),
    axis.line = axis_custom(),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_color_grey(start = 0.01, end = 0.6, "Okruženje:  ")
tmplotGrob <- ggplotGrob(tmplot)
tmplotGrob$grobs[[8]]$grobs[[1]]$grobs[[4]]$label <- "GUI"
tmplotGrob$grobs[[8]]$grobs[[1]]$grobs[[4]]$gp$fontfamily <- "Arial"
tmplotGrob$grobs[[8]]$grobs[[1]]$grobs[[6]]$label <- "Kod"
tmplotGrob$grobs[[8]]$grobs[[1]]$grobs[[6]]$gp$fontfamily <-
  "Franklin Gothic Heavy"
tmplotGrob$grobs[[8]]$grobs[[1]]$grobs[[8]]$label <- "Web"
tmplotGrob$grobs[[8]]$grobs[[1]]$grobs[[8]]$gp$fontfamily <-
  "Impact"
grid.newpage()
grid.draw(tmplotGrob)
ggsave(
  tmplotGrob,
  filename = "tmplot.svg",
  units = "mm",
  height = 120,
  width = 180
)
ggsave(
  tmplotGrob,
  filename = "tmplot.emf",
  units = "mm",
  height = 120,
  width = 180
)

ggsave(tmplot, filename = "tmplot.svg")

#Slika 7 Korelaciona matrica pokazatelja za Tviter naloge u osnovnom uzorku

library("GGally")
colnames(svipokazatelji) <-
  c(
    "Intermed.",
    "Stepen(dol.)",
    "Stepen(odl.)",
    "Bliskost(dol.)",
    "Bliskost(odl.)",
    "Svojst.vektor",
    "Kompozitni",
    "Pratioci",
    "Prijatelji",
    "Tvitovi(ukupno)",
    "Favorisao",
    "Starost(naloga)",
    "Favorisan(kampanja)",
    "Retvitovan(kampanja)",
    "Tvitovi(kampanja)",
    "Retvitovan(pros.kampanja)"
  )
korelacionamatrica <-
  ggcorr(
    cor(svipokazatelji, method = "spearman"),
    geom = "blank",
    label = T,
    hjust = 0.75
  ) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)


#Slika 8 Prosečna vrednost kompozitnog pokazatelja centralnosti čvorova po strankama u mreži osnovnog uzorka
V(mreza.pol)$stranka <- političari$Stranka
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
require(ggthemes)
barplotStranke <-
  ggplot(data = g1, aes(
    y = CentStand,
    x = reorder(Stranka, CentStand),
    fill = Stranka
  )) +
  geom_bar(stat = "identity") + coord_flip() +
  scale_fill_manual(values = g1$Boja, guide = FALSE) + theme_tufte() + xlab(NULL) +
  ylab("Prosečna vrednost kompozitnog pokazatelja centralnosti") +
  theme(text = element_text(size = 12, family = "Arial")) +
  geom_hline(yintercept = c(2, 4, 6, 8),
             col = "white",
             lwd = 0.72)

ggsave(
  barplotStranke,
  filename = "barplotstranke.emf",
  units = "mm",
  width = 160,
  height = 100
)

#Prilog IV Konvencionalni sociogram proširenog uzorka
svg(filename = "hairball.svg", width = 6.3)
E(mreza.pol.pu)$boja <- "#bdbdbd"
for (i in 1:length(unique(političari$Stranka))) {
  E(mreza.pol.pu)$boja[E(mreza.pol.pu)[V(mreza.pol.pu)[V(mreza.pol.pu)$name %in% političari$Nalog[političari$Stranka ==
                                                                                                    unique(političari$Stranka)[i]]] %--% V(mreza.pol.pu)[V(mreza.pol.pu)$opozicija ==
                                                                                                                                                           3]]] <-
    unique(političari$Boja[političari$Stranka == unique(političari$Stranka)[i]])
}
plot.igraph(
  mreza.pol.pu,
  layout = layout.drl(mreza.pol.pu),
  edge.width = rescale(x = log(E(mreza.pol.pu)$weight), to = c(0.4, 0.8)),
  edge.curved = FALSE,
  edge.color = alpha(E(mreza.pol.pu)$boja, rescale(
    x = log(E(mreza.pol.pu)$weight), to = c(0.4, 0.8)
  )),
  edge.arrow.size = 0,
  edge.lty = 1,
  vertex.color = c(V(mreza.pol)$boja, rep("gray", 315)),
  vertex.frame.color = NA,
  vertex.label = NA,
  vertex.size = rescale(degree(mreza.pol.pu), to = c(3, 8))
  
)
dev.off()
