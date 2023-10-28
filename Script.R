# https://berkeleyearth.org/
# RefYears = c(1951, 1980)
RefYears = c(1850, 1900) # reference for the 1.5°C threshold
PlotYears = c(1850, as.numeric(strsplit(date(),' ')[[1]][length(strsplit(date(),' ')[[1]])]))

LPI_final0_relative_to_Min = 2.3 # [0; 1] LPI_final0_relative_to_Min > LPI_final1_relative_to_Min
LPI_final1_relative_to_Min = 0.2 # [0; 1] LPI_final0_relative_to_Min > LPI_final1_relative_to_Min

   TitlePosi = 1 # [0; 1] relative to max anomaly
subTitlePosi = 0.88  # [0; 1] 

outFile = "/DISQUE/politique/écologie/Climat/showyourstripes/FromRawData/Mystripes.gif"
duration = 300 # ms

# Lang = 'Fr' # 'Uk' or 'Fr'
Lang = 'Uk' # 'Uk' or 'Fr'

#https://www.nagraj.net/notes/gifs-in-r/
library(magick)
library(ggplot2)
library(circlize) # colorRamp2
library(png)


# europe-TAVG-Trend.txt
# Complete_TAVG_complete.txt
# A = read.table("/media/hmh/DD/DISQUE/politique/écologie/Climat/showyourstripes/FromRawData/Complete_TAVG_complete.txt",comment.char = "%", na.strings = "NaN")
# A = read.table("https://data.berkeleyearth.org/auto/Regional/TAVG/Text/europe-TAVG-Trend.txt",comment.char = "%", na.strings = "NaN")

A = read.table("/media/hmh/DD/DISQUE/politique/écologie/Climat/showyourstripes/FromRawData/Land_and_Ocean_complete.txt",comment.char = "%", na.strings = "NaN")
# A = read.table("https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Land_and_Ocean_complete.txt",comment.char = "%", na.strings = "NaN")

# A = read.table("https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Complete_TAVG_complete.txt",comment.char = "%", na.strings = "NaN")
colnames(A) = c("Year", "Month", "Anomaly", "Unc.", "Anomaly", "Unc.", "Anomaly", "Unc.", "Anomaly", "Unc.", "Anomaly", "Unc.")
A = A[,c(1,3)]
# head(A)

# B = read.table("https://www.livingplanetindex.org/session/22b0b29a3d227f9f8701f5be49c8f416/download/downloadData?w=",sep=",",h=T)[,1:4]
B = read.table("/media/hmh/DD/DISQUE/politique/écologie/Climat/showyourstripes/FromRawData/Living Planet Index - Living Planet Report 2022.csv",sep=",",h=T)[,1:4]

A = as.data.frame(t(sapply(split(A,A$Year),function(x)apply(x,2,mean,na.rm=T))))
# head(A)
RefPeriod = A$Year >= RefYears[1] & A$Year <= RefYears[2]
Ref = mean(A$Anomaly[RefPeriod])
A$Anomaly = A$Anomaly - Ref

A$LPI_final = NA
for(y in B$Year){A$LPI_final[A$Year==y] = B$LPI_final[B$Year==y]}

Aplot = A[ A$Year >= PlotYears[1] & A$Year <= PlotYears[2]  ,]

LPI_final0 = min(Aplot$Anomaly) * LPI_final0_relative_to_Min
LPI_final1 = min(Aplot$Anomaly) * LPI_final1_relative_to_Min

Aplot$LPI_final. = Aplot$LPI_final*  (LPI_final1-LPI_final0) + LPI_final0
# range(A$LPI_final.,na.rm = T)

gradient = list(colours = c("#0005e2","#575bef","#cbd4ff", "white","#ffbaba","#ff0000","#c30000")
                ,values = c(quantile(A$Anomaly[A$Anomaly < 0],probs = c(0 ,0.25 ,0.9   ) )
                           ,0
                           ,quantile(A$Anomaly[A$Anomaly > 0],probs = c(0.25 , 0.75,  1   ) )
                ))

FunColor = colorRamp2(breaks = gradient$values
                     ,colors = gradient$colours)
Colors = data.frame("break" = unique(Aplot$Anomaly),"color" = FunColor(unique(Aplot$Anomaly)))

# plot(Colors$break., col = Colors$color, pch=19)

Aplot$color = FunColor(Aplot$Anomaly)

Aplot$duration = duration
# # Aplot$duration[Aplot$Year <= 1900] = Aplot$duration[Aplot$Year <= 1900]/3
# Aplot$duration[Aplot$Year >= 1968] = Aplot$duration[Aplot$Year >= 1968]*3
# 
# Aplot$duration[Aplot$duration<1] = 1
# Aplot$duration = round(Aplot$duration)
# Aplot$duration = Aplot$duration * 10

Aplot$duration[nrow(Aplot)] = (10 #s
                               )*1000
sum(Aplot$duration)/1000 # s

yaxis = list(main = seq(-2, 2, by = 0.5)
            # ,dec = seq(-2, 2, by = 0.2)
            )
yaxis$label = yaxis$main
yaxis$label[yaxis$label >0] = paste0('+', yaxis$label[yaxis$label>0])
yaxis$label[yaxis$label==0] = paste0('  ', yaxis$label[yaxis$label==0])
yaxis$label = paste0(yaxis$label,'°C')

xaxis = list(main = seq(1850, 2023, by = 50))
xaxis$main = sort(c(xaxis$main, 1970,2018))
xaxis$label = xaxis$main
xaxis$label[is.element(xaxis$main,c(1970,2018))] = ''

# yaxis$label[!(yaxis$label %% 2 == 0 | yaxis$label %% 2 == 1)] = ""
# yaxis$label[yaxis$main==1] = "+1"
# yaxis$label[yaxis$main==2] = "°C"

# yaxis$dec = yaxis$dec[!is.element(yaxis$dec, yaxis$main)]

for(upToYear in Aplot$Year[Aplot$Year > 1855]){
  if((upToYear < 1940 & upToYear %% 5 == 0) | (upToYear >= 1940 & upToYear < 1968 & upToYear %% 2 == 0) | (upToYear >= 1968)  ){
# upToYear = 2023
    w = Aplot$Year <= upToYear
    
Plot = ggplot(Aplot[w,], aes(x = Year, y = Anomaly)) + 
  # geom_bar(aes(fill = color),stat = "identity") +
  # scale_fill_manual(values = Colors$color, breaks = Colors$color) + 
  geom_segment( x=Aplot$Year[w],xend=Aplot$Year[w], y=0, yend=Aplot$Anomaly[w] , color = 'grey20') +
  geom_point( aes(color=color, fill=color), size=2) + 
  scale_color_manual(values = Colors$color, breaks = Colors$color) + 
  # theme(base_size = 25) +
 theme(panel.background = element_rect(fill = "black",linewidth = 0.25, linetype = 8)
       ,panel.grid = element_line(color = "black")
       ,axis.title = element_text(size = 18,colour = 'white')
       ,axis.text = element_text(size = 16,colour = 'white')
       ,axis.line = element_line(colour = 'white')
       ,axis.ticks = element_line(colour = 'white')
       ,legend.position="none"
       ,plot.margin = margin(t = 10, r = 0, b = 8, l = 0, unit = "mm")
       ,plot.background = element_rect(fill = "black")
     ) +
  scale_y_continuous( limits = c(min(Aplot$Anomaly)*2.5, max(Aplot$Anomaly)*1.15) , breaks = yaxis$main, labels = yaxis$label
                      ,sec.axis = dup_axis()) +
  scale_x_continuous( limits = c(PlotYears[1]-.5,PlotYears[2]+.5) , breaks = xaxis$main, labels = xaxis$label) + 
  geom_rect(xmin = min(B$Year)-1, xmax = max(B$Year)+1, ymin = LPI_final0, ymax = LPI_final1, color = 'white',fill="#373737")+ 
  geom_line(   aes(y = LPI_final., x = Year), color = 'white') +
  

  annotate("text", x = PlotYears[1]-diff(range(A$Year))*.001, y = max(Aplot$Anomaly)*1.1*   TitlePosi, label = paste(c('Uk' = "Global temperature change", 'Fr' = "Évolution globale des températures")[Lang], paste(range(Aplot$Year),collapse = '-')), colour="white",size=5.4, hjust=0) + 
  annotate("text", x = PlotYears[1]-diff(range(A$Year))*.001, y = max(Aplot$Anomaly)*1.1*subTitlePosi, label = paste(c('Uk' = "Relative to average of", 'Fr'="Par rapport à la moyenne de")[Lang],paste(RefYears,collapse = '-'), "[°C]"), colour="white", hjust=0,size=4.5) + 
  annotate("text", x = min(B$Year)+diff(range(A$Year))*0.075
           , y = LPI_final1*1.4 , label = c('Uk' = "Biodiversity", 'Fr'="Biodiversité")[Lang], colour="#00b012",size=4.75, hjust=0, vjust=1) + 
  annotate("text", x = min(B$Year) -diff(range(A$Year))*.025
           , y = (LPI_final0+LPI_final1)*0.5, label = c('Uk'="Living\nPlanet\nIndex", 'Fr'="Indice\nPlanète\nVivante")[Lang], colour="white", hjust=1, vjust=0.5, size = 4) + 
  annotate("text", x = min(B$Year)-diff(range(A$Year))*.01, y = LPI_final0, label = "0%", colour="white", hjust=1, vjust = 0.25) + 
  annotate("text", x = min(B$Year)-diff(range(A$Year))*.01, y = LPI_final1, label = "100%", colour="white", hjust=1, vjust = 0.75) + 
  annotate("text", x = min(B$Year), y = LPI_final0*1.085, label = "1970", colour="white", hjust=0.5, vjust=1) + 
  annotate("text", x = max(B$Year), y = LPI_final0*1.085, label = "2018", colour="white", hjust=0.5, vjust=1) + 
  annotate("text", x = (max(B$Year)+max(Aplot$Year))*0.5, y = min(c(max(Aplot$LPI_final.,na.rm = T), Aplot$LPI_final.[Aplot$Year<=upToYear]), na.rm = T), label = paste0(round(min(c(max(Aplot$LPI_final,na.rm = T),Aplot$LPI_final[Aplot$Year<=upToYear]), na.rm = T)*100),"%") , colour="white", hjust=0.1, vjust=0.5, size = 4) +
  annotate("text", x = (min(Aplot$Year)*0.75+max(Aplot$Year)*0.25), y = 1,  label = upToYear , colour=Aplot$color[Aplot$Year==upToYear], size = 12) +
  ylab("") + xlab(c('Uk' = "Year", 'Fr'="Année")[Lang])
  

  # plot with picture as layer
  if(upToYear >= 2005){
    img <- readPNG("/DISQUE/image/images pour diaporama/Smileys/hot.png")
    Plot = Plot +    annotation_raster(img, xmin = 1999, xmax = 2011, ymin = 1.17, ymax = 1.36) 
  }
  if(upToYear >= 2017 & upToYear%%2 == max(Aplot$Year)%%2){
    img <- readPNG("/DISQUE/image/images pour diaporama/Smileys/skull-crossbones.png")
    Plot = Plot +    annotation_raster(img, xmin = 2019, xmax = 2032, ymin = 1.435, ymax = 1.64) 
  }

  # y = 2023
  fp <-  paste0("./temp_to_del_", upToYear, " (",Aplot$duration[Aplot$Year == upToYear], "ms).png")
  suppressWarnings(
  ggsave(plot = Plot, 
         filename = fp,
         device = "png",width = 8.25,height = 6.5
         ,dpi = 115)
  )

}}
  




# => then "open as layers" in gimps and export to gif
# Delay between frames


# file.remove(paste0("./temp_to_del_", Aplot$Year, " (",Aplot$duration, "ms).png"))



