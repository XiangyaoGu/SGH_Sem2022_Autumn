# prepare package
install.packages('eurostat')
library(eurostat)
install.packages('countrycode')
library(countrycode)
library(ggplot2)
# prepare the data
d <- get_eurostat(id = 'prc_hicp_manr')
d1 <- d 
#str(d1)
#head(d1,10)
#unique(d1$geo)
d1 <- d1[d1$coicop == 'CP00',]
d1 <- d1[d1$time >= '2000-02-01' & d1$time <= '2022-09-30',]

eu_mb <- c('AT','BE','BG','HR','CY','CZ','DK','EE','FI','FR','DE','EL','HU',
           'IE','IT','LV','LT','LU','MT','NL','PL','PT','RO','SK','SI','ES',
           'SE')

d2 <- d1[d1$geo %in% eu_mb,]
d2$geo_funam <- countrycode(d2$geo,origin ='iso2c',
                            destination = 'country.name',nomatch = NULL)
#unique(d2$geo_funam)
d2$geo_funam[d2$geo_funam=='EL'] <- 'Greece'

# visualization
head(d2,10)
ggplot(d2,aes(x=time,y=values,color=factor(geo_funam)))+
  geom_line()+
  guides(color = guide_legend(title = '27 EU Countries'))+
  ggtitle('HICP time series for EU countries')+
  theme(legend.key.size = unit(5,'mm'),
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        plot.title = element_text(size = 12, face = "bold",color = 'grey'))

#clustering
out <- aggregate(values ~ geo_funam, d2, c)
out2 <- out[,-1]
rownames(out2)<-out[,1]
z <- scale(out2)
rownames(z)<-rownames(out2)

dis <- dist(z, method="minkowski", p=1.5)
hc1 <- hclust(dis, method = "complete" )
plot(hc1, cex = 1, hang = -1,main = "Clustering of EU countries based on HICP"
     ,xlab='Countries')
rect.hclust(hc1, k = 4, border = 2:5)
