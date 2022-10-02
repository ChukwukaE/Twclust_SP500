#---------------------x---------------------------------------DESCRIPTIVE ANALYSIS   --------------------------------x-------------------------

#---------------------x-------------------------------------Outliers and anomalieS  --------------------------------x-------------------------
#install.packages('anomalize')
#install.packages('timetk')
#install.packages('tibbletime')
#install.packages('pkg')
library(anomalize)
library(timetk)
library(tibbletime)
#library(pkg)
library(tidyverse)

main <- read.csv('data\\S&P500_stock_index_no_miss.csv')
#View(main)
#dim(main)
#str(main)

#remove index and dob
main <- main[-1][-494]
main$date <- as.Date(main$date,format="%Y-%m-%d")

#convert to tibble
main2 <- main %>% select(date,SP500)
main2 <- as_tibble(main2)
class(main2)

#Detecting anomalies in data
df_anomalized <- main2 %>%
  time_decompose(SP500, merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()
df_anomalized %>% glimpse()


#plot decomposed data
df_anomalized %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)

p1 <- df_anomalized %>%
  plot_anomaly_decomposition() +
  ggtitle("Freq/Trend = 'auto'")
p1


#view timeframe
get_time_scale_template()

#plot anomalies
p2 <- df_anomalized %>%
  plot_anomaly_decomposition() +
  ggtitle("Freq/Trend = '2 weeks'")
p2

#change timeframe
time_scale_template() %>%
  mutate(trend = ifelse(time_scale == "day", "2 weeks", trend)) %>%
  set_time_scale_template()
get_time_scale_template()


#failed to plot new time frame
p3 <- main2 %>%
  time_decompose(SP500) %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Trend = 2 Weeks (Global)")
p3


#Rest time frame
#Let's reset the time scale template defaults back to the original defaults.

time_scale_template() %>%
  set_time_scale_template()
# Verify the change
get_time_scale_template()


#convert data points to check outliers
main2 %>% 
  time_decompose(SP500) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes')

#reduced alpha to zero in on outliers
#p5
p5 <- main2 %>%
  time_decompose(SP500) %>%
  anomalize(remainder, alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.05")
#> frequency = 7 days
#> trend = 91 days
p5

#i dont know what happened here
p6 <- main2 %>%
  time_decompose(SP500) %>%
  anomalize(remainder, alpha = 0.025, max_anoms = 0.2) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.05")
#> frequency = 7 days
#> trend = 91 days
p6


#part 2: Outliers 
#install.packages('forecast')
#install.packages('tsoutliers')
library(forecast)
library(tsoutliers)

#get outliers for entire stocks in sp 500
out_<- tsoutliers(unlist(main), iterate = 2, lambda = NULL)
plot(main$date, out_)

#outliers for only sp500
tsoutliers(main$SP500, iterate = 2, lambda = NULL)
str(main[-1])

#plot outliers for only sp500
autoplot(ts(tsclean(main$SP500)), series="clean", color='red', lwd=0.9) +
  autolayer(ts(main$SP500), series="original", color='gray', lwd=1) +
  geom_point(data = tsoutliers(main$SP500) %>% as.data.frame(), 
             aes(x=index, y=replacements), col='blue') +
  labs(x = "Day", y = "Gold price ($US)")
autoplot(ts(main$SP500))


#---------------------x-------------------------------------INFERENTIAL ANALYSIS--------------------------------x-------------------------
#---------------------x-----------------------------import SP500 Stock data From Wikipedia--------------------------------x-------------------------

sp500_sector <- read.csv('data\\SP500_sector.csv')
sp500_sector
#delete additional data from sector data
sp500_sector_c <- sp500_sector[-c(128, 358,91,359, 132,158,209,208,317,100,475,250),]
#View(sp500_sector_c)

#order then bind the data
sp500_sector_or <-sp500_sector_c[order(sp500_sector_c$Symbol),]
#View(sp500_sector_or)


#---------------------x-------------------------Raw SP500 data for plotting--------------------------------x-------------------------
sp500_s <- read.csv('data\\S&P500_stock_r.csv')
View(sp500_s)
sp500_s$date <- as.Date(sp500_s$date)
dim(sp500_s)
#---------------------x-----------------------------------import stock data--------------------------------x-------------------------

#---------------------x------------------------Import Transformed SP500 Financial Data---------------------------x-------------------------
#Import data
library(PerformanceAnalytics)
library(dplyr)
library(tidyverse)
library(tidyr)
library(dplyr)


sp50 <- read.csv('data\\S&P500_wk_pr.csv')
sp50 <-sp50[-1]
sp50$date <-as.Date(sp50$date)


sp500<- read.zoo(sp50, format = "%Y-%m-%d")
View(sp500)
#---------------------x-----------------------------Normalize--------------------------------x-------------------------
#Time series normalization
library(BETS)
sp500_n <- normalize(sp500, mode= 'maxmin')
sp500_n <- as.data.frame(sp500_n)
sp500_n$date <-row.names(sp500_n)
#---------------------x-----------------------------Transpose--------------------------------x-------------------------
library(dplyr)
library(tidyverse)
sz <- sp500_n %>% gather(stock, price, -date)
View(sz)
ss <- sz %>% spread(date, price)
View(ss)

row.names(ss) = ss$stock
ss <- ss[-1]
View(ss)
#---------------------x-----------------------------PCA --------------------------------x-------------------------
#Retaining 90percent of cumulative propotion of Variance
p1 <- princomp(ss, scores = T, cor=T)
summary(p1)
#p$loadings
sp_pca = as.data.frame(p1$scores[,1:135])
describe(sp_pca)
#---------------------x-----------------------------Xie Beni--------------------------------x-------------------------
#install.packages("fclust")
#install.packages('geocmeans')
#install.packages('e1071')
library(ppclust)
library(factoextra)
library(cluster)
library(fclust)
library(fclust)
library(geocmeans)
library(e1071)


#Xie Beni loop
# finding the best k by using the #Xie Beni index of the classification
# trying for k from 2 to 25
R2s <- sapply(2:25,function(k){
  fclust.heir <- FKM(sp_pca, k= k, m = 1.2, index = 'XB',  seed = 122)
  R2 <- XB(fclust.heir$Xca,fclust.heir$U,fclust.heir$H,fclust.heir$m)
  return(R2)
})

#Create dataframe
Df <- data.frame(K=2:25,
                 R2 = R2s)
print(Df)
View(Df)
#Plot Xie Beni Index
library(ggplot2)
ggplot(Df)+
  geom_line(aes(x=K,y=R2s))+
  geom_point(aes(x=K,y=R2s),color="red")+
  xlab("Number of clusters")+
  ylab("Xie Beni index")

#---------------------x-----------------------------HEirarchial---------------------------------------x-------------------------
#Check for best link
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
library(cluster)
ac <- function(x) {
  agnes(sp_pca, method = x)$ac
}

map_dbl(m, ac)

######Hcut
#Hierarchical clustering using Ward linkage
hc1 = hclust(dist(sp_pca), method = "ward.D2")

  #cutting the tree
plot(hc1, cex = 0.2)
rect.hclust(hc1, k = 11, border = 2:5)

#explore the dendrogram
library(dendextend)
dend <- as.dendrogram(hc1)
dend <- color_branches(dend1, k =11)# Color the branches based on the clusters:
dend1 <- set(dend1, "labels_cex", 0.3)
plot(dend,
     main = "S&P500 Hierarchial Cluster",
     horiz =  T,  nodePar = list(cex = .007))

# #delete plot memory
#dev.off()

#Plot dendrogram 
#install.packages('circlize') 
library(circlize)
circlize_dendrogram(dend, cex = 0.2)

#get clusters
democut<-cutree(hc1,k=11)
#View(democut)
summary(as.factor(democut))

#add clusters to dataframe
fcm1 <- cbind(sp500_sector_or, democut)
View(fcm1)

#View table of data with clusters
table(fcm1$GICSÿSector, fcm1$democut)


#plot another dedrogram
require(cluster)
hc.cut <- hcut(dist(sp_pca), k = 11, hc_method = "ward.D2")
# Visualize dendrogram
fviz_dend(hc.cut, labels_track_height = 0.2, show_labels = FALSE, rect = TRUE, scale = "none")

# Visualize cluster
fviz_cluster(hc.cut, ellipse.type = "convex")

#Plot in 2D and 3D
library(rgl)
fviz_cluster(list(data=sp_pca, cluster = democut )) #2D plot
plot3d(sp_pca, col=democut) #Create a 3D plot


#Validate
# calculate silhouette
library(cluster) 
library(factoextra)

d <- dist(sp_pca)
sil <- silhouette(democut, d)
fviz_silhouette(sil)

#---------------------x-----------------------------Fuzzy KMeans   --------------------------------x-------------------------
fkm.sp <- FKM(X = sp_pca,k=11, m = 1.2, RS = 50, index = "SIL.F")
View(fkm.sp$U)
fkm.clus <- as.data.frame(fkm.sp$clus)
summary(as.factor(fkm.clus$Cluster))

plot(fkm.sp, pca = T)

#membership
fkm.sp$U

#View(democut)
fkm.r <- cbind(sp500_sector_or,'cluster' = fkm.clus$Cluster)
View(fkm.r)


#View table of data
table(fkm.r$GICSÿSector, fkm.r$cluster)
plot3d(sp_pca, col=fkm.r$cluster, main="Clusters") #Create a 3D plot

#---------------------x-----------------------------time series fuzzy cluster--------------------------------x-------------------------
#install.packages('dtwclust')
#install.packages('TSclust')
library(TSclust)
library(tsclust)
library(cluster)
library(dtwclust)
library(dtw)

#Trying different models
clust.tiers <- tsclust(sp_pca, type = "fuzzy", k =11 , distance = "dtw2", centroid = "fcmdd",  seed =5412 )
plot(clust.tiers)
View(clust.tiers@fcluster)

# clust.ts <- tsclust(sp_pca, type = "fuzzy", k =5 , distance = "dtw2", centroid = "fcmdd",  seed =7102 )#2907 5412-for project
# View(clust.tiers@fcluster)
# summary(as.factor(clust.ts@cluster)) 
# table(sp500_sector_or$GICSÿSector,clust.ts@cluster) # Check how they clustered with the industries

#Cluster summary
summary(as.factor(clust.tiers@cluster)) #check the clusters
table(sp500_sector_or$GICSÿSector,clust.tiers@cluster) # Check how they clustered with the industries
View(cbind(sp500_sector_or[-4][-4][-4], 'cluster' = clust.tiers@cluster))


#Plot
library(rgl)
plot(clust.tiers) #time series plot
fviz_cluster(list(data=sp_pca, cluster = clust.tiers@cluster)) #2D plot
plot3d(sp_pca, col=clust.tiers@cluster, main="Clusters") #Create a 3D plot


#---------------------x-----------------------------COrrelation   --------------------------------x-------------------------
#Check Intercluster Correlation
library(corrplot)
jd <-as.data.frame(clust.tiers@fcluster)
corrplot(cor(jd), method = 'number')


##Check within CLuster correlation
clus_sp <- cbind('cluster' = clust.tiers@cluster, ss)
head(clus_sp)

library(dplyr)
library(tidyverse)
clus_sp1 <- clus_sp %>% filter(cluster== 4)
#View(clus_sp1)
clus_sp1$stock <- row.names(clus_sp1)

#Re-Transpose
sz4 <- clus_sp1[-1] %>% gather( date, price, - stock)
View(sz4)
sz41 <- sz4 %>% spread( stock, price)
View(sz41)

library(corrplot)
corrplot(cor(sz41[-1]), method = 'number')


#statically check correlation over period of two stocks
#install.packages('stats')
library(zoo)
library(stats)
str(sp500_s$date)
pep <- ts(sp500_s$PEP, sp500_s$date)
nvda <- ts(sp500_s$NVDA, sp500_s$date)
aee <- ts(sp500_s$AEE, sp500_s$date)
nee <- ts(sp500_s$NEE, sp500_s$date)

acf(ts.intersect(pep, nvda))
acf(ts.intersect(aee, nee))

#---------------------x----------------------------- plot centers  --------------------------------x-------------------------
centroids <- as.data.frame(clust.tiers@centroids, col.names  = c('cluster1','cluster2','cluster3','cluster4','cluster5','cluster6','cluster7',
                                                                 'cluster8','cluster9','cluster10','cluster11'))

View(centroids)
str(centroids)

#melt the data
library(reshape)
centroids$index <- as.numeric(row.names(centroids))
View(centroids)
# str(cbf)
cbf <- melt(centroids, id.vars = 'index', variable.name= 'series')
View(cbf)

#create line plot for each column in data frame
ggplot(cbf, aes(index, value)) +
  geom_line(aes(colour = variable))

#---------------------x-----------------------------Validation   ----------------------------------------------x-------------------------
#plot(cbind(sp500$AAL, sp500$ABBV), legend.loc = "topleft", main = "ETF prices")
r1 <- calcqualityIndexes(sp_pca,fkm.sp$U,m=1.2, indices = c("Silhouette.index", "Partition.entropy", "Partition.coeff",
                                                            "FukuyamaSugeno.index", "Explained.inertia"))
r2 <- calcqualityIndexes(sp_pca,clust.tiers@fcluster,m=1.2, indices = c("Silhouette.index", "Partition.entropy", "Partition.coeff",
                                                                        "FukuyamaSugeno.index", "Explained.inertia"))
r2
df <- cbind(unlist(r1), unlist(r2))

knitr::kable(df,
             digits = 9,col.names = c("FKM", "Dtwclust"))

#---------------------x-----------------------------EWMA Volatility  --------------------------------x-------------------------
library(quarks)


e5 <- sapply(centroids[,1:11],function(k){
  e4<-fhs(k, p = 0.975, model = c("EWMA", "GARCH"), lambda = 0.94, nboot = 1000)
  return(e4$VaR)
})
e3
#Create dataframe
ewma <- data.frame( EWMA_0.80 = e3, EWMA_0.94 = e5)
View(ewma)
#ewma <- NULL
#First Plot

#plot
dim(sp500_s)
plot(sp500_s$date,                              # Draw first time series
     sp500_s$PEP,
     type = "l",
     col = 2,
     ylim = c(- 1, 800),
     xlab = "Year",
     ylab = "Values")
lines(sp500_s$date,                             # Draw second time series
      sp500_s$NVDA,
      type = "l",
      col = 3)
# lines(sp500_s$date,                            # Draw third time series
#       sp500_s$SP500,
#       type = "l",
#       col = 4)
legend("topright",                           # Add legend to plot
       c("PEP(C5", "NVDA(C4)"),
       lty = 1,
       col = 2:4)
#---------------------x-----------------------------end   --------------------------------x-------------------------