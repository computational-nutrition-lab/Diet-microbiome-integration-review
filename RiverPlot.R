# make river plot for review
#nstall.packages("riverplot")
require(riverplot)
require(dplyr)          # Needed for the count function
require(riverplot)      # Does all the real work
require(RColorBrewer)   # To assign nice colours
require(reshape2)

dat <- read.delim("Google Drive/Shared drives/Diet-microbiome integration review/visualization.txt")
sdOrd <- names(rev(sort(table(dat$Study.design))))

dat$Study.design <- factor(dat$Study.design, levels = sdOrd)

dcOrd <- names(rev(sort(table(dat$Dietary.collection.method..no..of.measurements.))))
dat$Dietary.collection.method..no..of.measurements. <- factor(dat$Dietary.collection.method..no..of.measurements., levels = dcOrd)


dat$Dietary.exposures.studied[dat$Dietary.exposures.studied == "Mixed"] <- "Mixed exposures"
deOrd <- names(rev(sort(table(dat$Dietary.exposures.studied))))
dat$Dietary.exposures.studied <- as.factor(dat$Dietary.exposures.studied)



# we need data to be in the form of nodes, edges, and value for flow between

# get node list
ID = c(rev(levels(dat$Study.design)), 
       rev(levels(dat$Dietary.exposures.studied)),
       rev(levels(dat$Dietary.collection.method..no..of.measurements.)))

x = c(rep(1.5,length(levels(dat$Study.design))),
      rep(2.5,length(levels(dat$Dietary.exposures.studied))),
      rep(2,length(levels(dat$Dietary.collection.method..no..of.measurements.))))

nodes<- data.frame(ID = ID, x = x)
nodes$col <- c("#B8B0AC",
               "#A87C9F",
               "#84B6B2",
               "#4E79A7",
               "#E25757",
               "#F28D2E",
               "#EDC949",
               "#56A24F",
               "#E25757",
               "#F6B379",
               "#FF9DA9",
               "#C5B561",
               "#9EA178",
               "#768D90",
               "#528E7B")

edges <- melt(table(dat$Study.design, dat$Dietary.collection.method..no..of.measurements.))
edges <- edges[edges$value != 0,]
edges <- edges[order(edges$value, decreasing = F),]

edges2 <- melt(table(dat$Dietary.collection.method..no..of.measurements., dat$Dietary.exposures.studied))
edges2 <- edges2[edges2$value != 0,]
edges2 <- edges2[order(edges2$value, decreasing = F),]

colnames(edges) <- c("N1", "N2", "Value")
colnames(edges2) <- c("N1", "N2", "Value")

edges <- rbind(edges, edges2)

rownames(nodes) <- nodes$ID

r <- makeRiver(nodes, edges)   

pdf("Google Drive/Shared drives/Diet-microbiome integration review/river.pdf",
    width = 7,
    height = 5)

op <- par(cex=0.7)
plot(r, 
     srt =0, 
     gravity = "c", 
     plot_area=c(0.8, 0.8), 
     fix.pdf = T)
par(op)


dev.off()

# plot to show sample size
dat$SampleSize <- NULL
dat$SampleSize[dat$no..of.subjects < 50] <- "< 50"
dat$SampleSize[dat$no..of.subjects >= 50 & dat$no..of.subjects < 500] <- "[50,500)"
dat$SampleSize[dat$no..of.subjects >= 500 & dat$no..of.subjects < 1500] <- "[500,1500)"
dat$SampleSize[dat$no..of.subjects >= 1500] <- "> 1500"

# order the factor
dat$SampleSize <- factor(dat$SampleSize, levels = c("< 50", "[50,500)", "[500,1500)", "> 1500"))

require(ggplot2)

#plot
ggplot(dat, aes(x=SampleSize, fill = Study.design)) +
  geom_bar(position = "identity") +
  scale_fill_manual(values = c("#4E79A7", "#84B6B2", "#A87C9F", "#B8B0AC")) +
  theme_bw() +
  ylab("Count") +
  xlab("Sample size") +
  guides(fill=guide_legend(title = "Study design"))+
  theme(legend.position = "bottom", legend.direction = "vertical") 

ggsave("Countdata.pdf", 
       dev = "pdf", 
       path = "Google Drive/Shared drives/Diet-microbiome integration review/",
       height = 4.3,
       width = 2.8)  

#plot by method used
ggplot(dat, aes(x=SampleSize, fill = Dietary.collection.method..no..of.measurements.)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#528E7B","#768D90","#9EA178", "#C5B561", "#FF9DA9", "#F6B379", "#E25757")) +
  theme_bw() +
  ylab("Proportion") +
  xlab("Sample size") +
  guides(fill=guide_legend(title = "Dietary method")) +  
  theme(legend.position = "bottom", legend.direction = "vertical") 
  

ggsave("Countdata_method.pdf", 
       dev = "pdf", 
       path = "Google Drive/Shared drives/Diet-microbiome integration review/",
       height = 5,
       width = 2.8) 


require(ggbeeswarm)
# box plot sample sizes
ggplot(dat, aes(y = no..of.subjects, x = Dietary.collection.method..no..of.measurements., color = Dietary.exposures.studied)) +
  geom_beeswarm(alpha = 0.7, size = 3) +
  scale_color_manual(values = c("#56A24F", "#EDC949", "#F28D2E", "#E25757")) +
  theme_bw() +
  xlab("Study design") +
  ylab("Number of subjects")

ggplot(dat, aes(x=SampleSize, fill = Dietary.exposures.studied)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#56A24F", "#EDC949", "#F28D2E", "#E25757")) +
  theme_bw() +
  ylab("Count") +
  xlab("Sample size") +
  guides(fill=guide_legend(title = "Dietary method")) +  
  theme(legend.position = "bottom", legend.direction = "vertical") 


table(dat$SampleSize)/length(dat$SampleSize) *100
