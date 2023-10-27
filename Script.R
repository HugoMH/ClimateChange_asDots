# https://data.berkeleyearth.org/

# anomalies relative to the Jan 1951-Dec 1980 average
RefYears = c(1951, 1980)
PlotYears = c(1850, as.numeric(strsplit(date(),' ')[[1]][length(strsplit(date(),' ')[[1]])]))

LPI_final0_relative_to_Min = 0.8 # [0; 1] LPI_final0_relative_to_Min > LPI_final1_relative_to_Min
LPI_final1_relative_to_Min = 0.3 # [0; 1] LPI_final0_relative_to_Min > LPI_final1_relative_to_Min

   TitlePosi = 1 # [0; 1] relative to max anomaly
subTitlePosi = 0.87  # [0; 1] 

outFile = "/DISQUE/politique/écologie/Climat/showyourstripes/FromRawData/Mystripes.gif"
duration = 10 # s

#https://www.nagraj.net/notes/gifs-in-r/
library(magick)
library(ggplot2)
library(circlize) # colorRamp2

# europe-TAVG-Trend.txt
# Complete_TAVG_complete.txt
A = read.table("/media/hmh/DD/DISQUE/politique/écologie/Climat/showyourstripes/FromRawData/Complete_TAVG_complete.txt",comment.char = "%", na.strings = "NaN")
# A = read.table("https://data.berkeleyearth.org/auto/Regional/TAVG/Text/europe-TAVG-Trend.txt",comment.char = "%", na.strings = "NaN")
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

gradient = list(colours = c("#0004b4","#575bef","#cbd4ff", "white","#ffabab","#ff0000","#b10000")
                ,values = c(quantile(A$Anomaly[A$Anomaly < 0],probs = c(0 ,0.25 ,0.9   ) )
                           ,0
                           ,quantile(A$Anomaly[A$Anomaly > 0],probs = c(0.10 , 0.75,  1   ) )
                ))

FunColor = colorRamp2(breaks = gradient$values
                     ,colors = gradient$colours)
Colors = data.frame("break" = unique(Aplot$Anomaly),"color" = FunColor(unique(Aplot$Anomaly)))

Aplot$color = FunColor(Aplot$Anomaly)

Aplot$duration = ceiling(duration/diff(range(Aplot$Year))*100)
Aplot$duration[Aplot$Year <= RefYears[1]] = Aplot$duration[Aplot$Year <= RefYears[1]]/3
Aplot$duration[Aplot$Year >= RefYears[2]] = Aplot$duration[Aplot$Year >= RefYears[2]]*3

Aplot$duration[Aplot$duration<1] = 1
Aplot$duration = Aplot$duration * 10

Aplot$duration[nrow(Aplot)] = (3 #s
                               )*1000
sum(Aplot$duration)/1000 # s


yaxis = list(main = seq(-2, 2, by = 0.5)
            # ,dec = seq(-2, 2, by = 0.2)
            )
yaxis$label = yaxis$main
yaxis$label[!(yaxis$label %% 2 == 0 | yaxis$label %% 2 == 1)] = ""
yaxis$label[yaxis$main==1] = "+1"
yaxis$label[yaxis$main==1.5] = "°C"

# yaxis$dec = yaxis$dec[!is.element(yaxis$dec, yaxis$main)]

for(upToYear in Aplot$Year){
# upToYear = 2023
Plot = ggplot(Aplot[Aplot$Year <= upToYear,], aes(x = Year, y = Anomaly)) + 
  geom_bar(aes(fill = color),stat = "identity") + 
  scale_fill_manual(values = Colors$color, breaks = Colors$color)  + 
  # theme(base_size = 25) +
 theme(panel.background = element_rect(fill = "black",linewidth = 0.25, linetype = 8)
       ,panel.grid = element_line(color = "black")
       ,axis.title = element_text(size = 20,colour = 'white')
       ,axis.text = element_text(size = 20,colour = 'white')
       ,axis.line = element_line(colour = 'white')
       ,axis.ticks = element_line(colour = 'white')
       ,legend.position="none"
       ,plot.background = element_rect(fill = "black")
     ) +
  scale_y_continuous( limits = range(Aplot$Anomaly), breaks = yaxis$main, labels = yaxis$label
                      ,sec.axis = dup_axis()) +

  annotate("text", x = PlotYears[1], y = max(Aplot$Anomaly)*   TitlePosi, label = paste("Temperature changes at Earth land surface", paste(range(Aplot$Year),collapse = '-')), colour="white",size=5, hjust=0) + 
  annotate("text", x = PlotYears[1], y = max(Aplot$Anomaly)*subTitlePosi, label = paste("Relative to average of",paste(RefYears,collapse = '-'), "[°C]"), colour="white", hjust=0) + 
  annotate("text", x = min(B$Year)-diff(range(A$Year))*.03
           , y = (min(Aplot$Anomaly)+LPI_final0)*0.475 , label = "Biodiversity", colour="white",size=5, hjust=0, vjust=1) + 
  annotate("text", x = mean(B$Year)
           , y = (min(Aplot$Anomaly)+LPI_final0)*0.475, label = "Living Planet Index\n1970-2018", colour="white", hjust=0, vjust=1) + 
  annotate("text", x = min(B$Year)-diff(range(A$Year))*.001, y = LPI_final0, label = "0%", colour="white", hjust=1) + 
  annotate("text", x = min(B$Year)-diff(range(A$Year))*.001, y = LPI_final1, label = "100%", colour="white", hjust=1) + 
  ylab("") + xlim(PlotYears[1]-.5,PlotYears[2]+.5) +
  
  geom_rect(xmin = min(B$Year)-1, xmax = max(B$Year)+1, ymin = LPI_final0, ymax = LPI_final1, color = 'white')+ 
  geom_line(   aes(y = LPI_final., x = Year), color = 'white') 
  
  # y = 2023
  fp <-  paste0("./temp_to_del_", upToYear, " (",Aplot$duration[Aplot$Year == upToYear], "ms).png")
  suppressWarnings(
  ggsave(plot = Plot, 
         filename = fp,
         device = "png",width = 9,height = 5
         ,dpi = 150)
  )
}
  
# => then "open as layers" in gimps and export to gif
# Delay between frames


# file.remove(paste0("./temp_to_del_", Aplot$Year, " (",Aplot$duration, "ms).png"))



