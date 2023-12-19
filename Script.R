# https://berkeleyearth.org/
# RefYears = c(1951, 1980)
setwd("/DISQUE/politique/écologie/Climat/showyourstripes/FromRawData/")

RefYears = c(1850, 1900) # reference for the 1.5°C threshold
# PlotYears = c(1850, as.numeric(strsplit(date(),' ')[[1]][length(strsplit(date(),' ')[[1]])]))
PlotYears = c(1850, 2050)

LPI_final0_relative_to_Min = 2.3 # [0; 1] LPI_final0_relative_to_Min > LPI_final1_relative_to_Min
LPI_final1_relative_to_Min = 0.2 # [0; 1] LPI_final0_relative_to_Min > LPI_final1_relative_to_Min

   TitlePosi = 1 # [0; 1] relative to max anomaly
subTitlePosi = 0.94  # [0; 1] 

duration = 300 # ms

# not working # FileFormat = 'png'
FileFormat = 'pdf'

Lang = 'Fr' # 'Uk' or 'Fr'
# Lang = 'Uk' # 'Uk' or 'Fr'

Rapports = list("Fr" = c(`Rapport Meadows (1972)` = 1972#, 'GIEC' = 1988
                         , '1er rapport du GIEC (1990)' = 1990 )
               ,"Uk" = c(`Meadows report (1972)`  = 1972#, 'IPCC' = 1988
                         , '1st IPCC report (1990)' = 1990)
               )

Events = list("Fr" = c(
  "Lampe électrique\n(1879)" = 1879
  ,"Voitures grand\npublique (1908)" = 1908
  ,"Chute du mur de Berlin (1989)" = 1989#, 'GIEC' = 1988
                         , '11 Septembre (attentat ; 2001)' = 2001 )
                ,"Uk" = c(
                  "First electric lamp\n(1879)" = 1879
  ,"Mass-market cars (1908)" = 1908
  ,'Fall of the Berlin Wall (1989)'  = 1989#, 'IPCC' = 1988
                          , '11 September attack (2001)' = 2001)
)


PlotYears_Biodiv = c(1970, 2018)
BiodivColor = "#00b012" # on grey
BiodivColor2 = "#17d92b" # on black
#https://www.nagraj.net/notes/gifs-in-r/
# library(magick)
library(ggplot2)
library(circlize) # colorRamp2
library(png)
library(xlsx)
# library(BioMathR) # gg_export( png_from_pdf

# europe-TAVG-Trend.txt
# Complete_TAVG_complete.txt
# A = read.table("/media/hmh/DD/DISQUE/politique/écologie/Climat/showyourstripes/FromRawData/data/Complete_TAVG_complete.txt",comment.char = "%", na.strings = "NaN")
# A = read.table("https://data.berkeleyearth.org/auto/Regional/TAVG/Text/europe-TAVG-Trend.txt",comment.char = "%", na.strings = "NaN")

A = read.table("./data/Land_and_Ocean_complete.txt",comment.char = "%", na.strings = "NaN")
# A = read.table("https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Land_and_Ocean_complete.txt",comment.char = "%", na.strings = "NaN")

# A = read.table("https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Complete_TAVG_complete.txt",comment.char = "%", na.strings = "NaN")
colnames(A) = c("Year", "Month", "Anomaly", "Unc.", "Anomaly", "Unc.", "Anomaly", "Unc.", "Anomaly", "Unc.", "Anomaly", "Unc.")
A = A[,c(1,3)]
# head(A)

# https://www.livingplanetindex.org/latest_results
# B = read.table("https://www.livingplanetindex.org/session/22b0b29a3d227f9f8701f5be49c8f416/download/downloadData?w=",sep=",",h=T)[,1:4]
B = read.table("./data/Living Planet Index - Living Planet Report 2022.csv",sep=",",h=T)[,1:4]

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

Aplot$duration[nrow(Aplot)] = (14 #s
                               )*1000
sum(Aplot$duration)/1000 # s

yaxis = list(main = seq(-0.5, 2, by = 0.5)
            # ,dec = seq(-2, 2, by = 0.2)
            )
yaxis$label = yaxis$main
yaxis$label[yaxis$label >0] = paste0('+', yaxis$label[yaxis$label>0])
yaxis$label[yaxis$label==0] = paste0('  ', yaxis$label[yaxis$label==0])
yaxis$label = paste0(yaxis$label,'°C')

Ylim = c(min(Aplot$Anomaly)*2.5, max(Aplot$Anomaly)*1.35)

xaxis = list(main = seq(PlotYears[1], PlotYears[2], by = 50))
xaxis$main = sort(c(xaxis$main, PlotYears_Biodiv[1], PlotYears_Biodiv[2]))
xaxis$label = xaxis$main
xaxis$label[is.element(xaxis$main, PlotYears_Biodiv )] = ''


CO2 = read.xlsx("./data/Global_Carbon_Budget_2023v1.0.xlsx",sheetIndex = 3)
i0 = which(CO2[,1] == 'Year')

colnames(CO2)[1:2] = CO2[i0,1:2]
CO2 = CO2[(i0+1):nrow(CO2),1:2]
CO2 = as.data.frame(apply(CO2, 2, as.numeric))

# 1 billion tonnes C = 1 gigatonne C = 3.664 billion tonnes of CO2
CO2[,2] = CO2[,2]*3.664

CO2$CumSum = cumsum(CO2[,2])
# plot(CO2$Year, CO2$`fossil emissions excluding carbonation`)
# plot(CO2$Year, CO2$CumSum)
CO2scaling = median(Aplot$Anomaly[Aplot$Year>=1970 & Aplot$Year<=max(CO2$Year)]) / median(CO2$CumSum[CO2$Year>=1970])
# CO2scaling = max(A$Anomaly) / max(CO2$CumSum)
CO2$y       = CO2$`fossil emissions excluding carbonation` * CO2scaling
CO2$yCumSum = CO2$CumSum                                   * CO2scaling
CO2$`fossil emissions excluding carbonation`
CO2$CumSum
rownames(CO2) = CO2$Year

CO2ColorAxis = '#909200'
CO2ColorCum  = '#686900'
CO2ColorYear = '#3c3c07'

SizeCO2line =3

Aplot[as.character(CO2$Year[is.element(CO2$Year, Aplot$Year)]), 'CO2']       = CO2[as.character(Aplot$Year[is.element(Aplot$Year , CO2$Year)]),'y']
Aplot[as.character(CO2$Year[is.element(CO2$Year, Aplot$Year)]), 'CO2CumSum'] = CO2[as.character(Aplot$Year[is.element(Aplot$Year , CO2$Year)]),'yCumSum']

for(upToYear in Aplot$Year[Aplot$Year >= 1885]){
  # if((upToYear < 1940 & upToYear %% 5 == 0) | (upToYear >= 1940 & upToYear < 1968 & upToYear %% 2 == 0) | (upToYear >= 1968)  ){
  if(T){
    # upToYear = 1987
    # upToYear = 2023
    w = Aplot$Year <= upToYear
  wi = which(w & !is.na(Aplot$CO2))
  
  colorInnerTicks = rep('white', upToYear - PlotYears[1]+1)
  names(colorInnerTicks) = PlotYears[1]:upToYear
  if(upToYear >= PlotYears_Biodiv[1]){
    colorInnerTicks[as.character(PlotYears_Biodiv[1]:min(c(PlotYears_Biodiv[2],upToYear)))] = "#48ff5b"
  }
  CO2barAddToX = 100
  
  Plot = ggplot(Aplot[w,], aes(x = Year, y = Anomaly)) + 
    # Temperatures
    geom_segment( x=Aplot$Year[w],xend=Aplot$Year[w], y=0, yend=Aplot$Anomaly[w] , color = 'grey20') +
    
    # CO2
    geom_line( aes(y = CO2CumSum), size=SizeCO2line, color=CO2ColorYear, lineend='round') + 
    geom_segment( x=Aplot$Year[max(wi)]+CO2barAddToX,xend=Aplot$Year[max(wi)]+CO2barAddToX, y=0, yend=Aplot$CO2CumSum[max(wi)]+SizeCO2line/2*(CO2scaling*12) , color = CO2ColorYear
                  , linewidth=CO2barAddToX*2+CO2barAddToX/4.5) + # upToYear = 1900
                  # , linewidth=CO2barAddToX*2+CO2barAddToX/4.5) +
    
    # Temperatures
    geom_point( aes(color=color, fill=color), size=2) + 
    scale_color_manual(values = Colors$color, breaks = Colors$color) + 
    # theme(base_size = 25) +
   theme_bw() + 
   theme(panel.background = element_rect(fill = "black",linewidth = 0.25, linetype = 8)
         ,panel.grid = element_line(color = "black")
         ,axis.title = element_text(size = 18,colour = 'white')
         ,axis.text = element_text(size = 16,colour = 'white')
         ,axis.text.y.right =  element_text(color = CO2ColorAxis)
         ,axis.title.y.right =  element_text(color = CO2ColorAxis)
         ,axis.text.y.left = element_text(color = 
                                             FunColor(yaxis$main)
                                            ,face = 'bold' )
         ,axis.line = element_line(colour = 'white')
         ,axis.ticks = element_line(colour = 'white')
         ,legend.position="none"
         ,plot.margin = margin(t = 10, r = 0, b = 7, l = 0, unit = "mm")
         ,plot.background = element_rect(fill = "black")
         ,plot.title = element_text( color = 'white', size = 40)
         ,text=element_text(family="Arial")
       ) + 
    # ggtitle(title) +
    scale_y_continuous( limits = Ylim , breaks = yaxis$main, labels = yaxis$label
                        # ,sec.axis = dup_axis()) +
                        ,sec.axis = sec_axis(~. /CO2scaling, name = c('Fr'=latex2exp::TeX("$CO_2$   (Milliards de tonnes)                   "),'Uk'=latex2exp::TeX("$CO_2$  (Billions of tons)                         "))[Lang], breaks = seq(0,2500,by=500))) +
    scale_x_continuous( limits = c(PlotYears[1]-1,PlotYears[2]+.5) , breaks = xaxis$main, labels = xaxis$label) + 
    
    geom_segment( x=PlotYears[1]:upToYear, xend=PlotYears[1]:upToYear
                  , y=Ylim[1]-diff(Ylim)*0.05, yend=Ylim[1]-diff(Ylim)*0.04 , color = colorInnerTicks, linewidth=0.25) +
    geom_rect(xmin = min(B$Year)-1, xmax = max(B$Year)+1, ymin = LPI_final0, ymax = LPI_final1, color = 'white',fill="#373737") + 
    geom_line(   aes(y = LPI_final., x = Year), color = 'white') +
    
    annotate("text", x = PlotYears[1]-diff(range(A$Year))*.0057803, y = max(Aplot$Anomaly)*1.35*   TitlePosi, label = list('Uk' = latex2exp::TeX(paste("Temperature change on Earth"        ,paste(range(Aplot$Year),collapse = '-')),bold = T)
                                                                                                                         , 'Fr' = latex2exp::TeX(paste("Évolution mondiale des températures",paste(range(Aplot$Year),collapse = '-')),bold = T))[Lang][[1]], colour="white",size=5.5, hjust=0) + 
    annotate("text", x = PlotYears[1]-diff(range(A$Year))*.0057803, y = max(Aplot$Anomaly)*1.35*subTitlePosi, label = paste(c('Uk' = "Relative to average of", 'Fr'="Par rapport à la moyenne de")[Lang],paste(RefYears,collapse = '-'), "[°C]")
    , colour="white", hjust=0,size=4.25) + 
    annotate("text", x = PlotYears[1]-diff(range(A$Year))*.0057803, y = max(Aplot$Anomaly)*1.35*subTitlePosi - max(Aplot$Anomaly)/25, label =  c('Uk' = "\n(Atmospheric temperatures close to land and ocean surfaces)",'Fr' = "\n(Températures de l'atmosphère près de la surface des terres et des océans)")[Lang]
    , colour="white", hjust=0,size=3.75) + 
    
# text CO2
    annotate("text", x = PlotYears[1]-diff(range(A$Year))*.0057803, y = max(Aplot$Anomaly)*1.35*subTitlePosi - max(Aplot$Anomaly)/5
             , label = c('Fr'=latex2exp::TeX("Milliards de tonnes de $CO_2$",bold = T),'Uk'=latex2exp::TeX("Billions of tonnes of $CO_2$",bold = T))[Lang]
             , colour=CO2ColorCum,size=5.5, hjust=0) + 
    annotate("text", x = PlotYears[1]-diff(range(A$Year))*.0057803, y = max(Aplot$Anomaly)*1.35*subTitlePosi - max(Aplot$Anomaly)/3.75
             , label = c('Fr'="produites par l'humain et accumulées d'année en année",'Uk'="produced by humans and accumulated from year to year")[Lang]
             , colour=CO2ColorCum,size=4.5, hjust=0) + 
    annotate("text", x = PlotYears[1]-diff(range(A$Year))*.0057803, y = max(Aplot$Anomaly)*1.35*subTitlePosi - max(Aplot$Anomaly)/2.75
             , label = c('Fr'=latex2exp::TeX("...Il nous en cuira !",italic = T, bold = T) , 'Uk'=latex2exp::TeX("...There will be hell to pay!",italic = T, bold = T))[Lang]
             , colour=CO2ColorCum,size=4.5, hjust=0, ) + 
    
    annotate("text", x = min(B$Year)+diff(range(A$Year))*0.075
             , y = LPI_final1*1.4 , label = c('Uk' = "Biodiversity", 'Fr'="Biodiversité")[Lang], colour=BiodivColor,size=4.75, hjust=0, vjust=1) + 
    annotate("text", x = min(B$Year) -diff(range(A$Year))*.025
             , y = (LPI_final0+LPI_final1)*0.5, label = c('Uk'="Living\nPlanet\nIndex", 'Fr'="Indice\nPlanète\nVivante")[Lang], colour="white", hjust=1, vjust=0.5, size = 4) + 
    annotate("text", x = min(B$Year)-diff(range(A$Year))*.01, y = LPI_final0, label = "0%", colour="white", hjust=1, vjust = 0.25) + 
    annotate("text", x = min(B$Year)-diff(range(A$Year))*.01, y = LPI_final1, label = "100%", colour="white", hjust=1, vjust = 0.75) + 
    annotate("text", x = min(B$Year), y = LPI_final0*1.085, label = PlotYears_Biodiv[1], colour=BiodivColor2, hjust=0.5, vjust=1) + 
    annotate("text", x = max(B$Year), y = LPI_final0*1.085, label = PlotYears_Biodiv[2], colour=BiodivColor2, hjust=0.5, vjust=1) + 
    annotate("text", x = (max(B$Year)+max(Aplot$Year))*0.5, y = min(c(max(Aplot$LPI_final.,na.rm = T), Aplot$LPI_final.[Aplot$Year<=upToYear]), na.rm = T), label = paste0(round(min(c(max(Aplot$LPI_final,na.rm = T),Aplot$LPI_final[Aplot$Year<=upToYear]), na.rm = T)*100),"%") , colour="white", hjust=0.1, vjust=0.5, size = 4) +
    annotate("text", x = (min(Aplot$Year)*0.9+max(Aplot$Year)*0.1), y = 0.85,  label = upToYear , colour=Aplot$color[Aplot$Year==upToYear], size = 12) +
    ylab("") + xlab(c('Uk' = "Year", 'Fr'="Année")[Lang])
    
  # add reports
  for(r in names(Rapports[Lang][[1]])){
    year = Rapports[Lang][[1]][r]
    if(year <= upToYear){
      xtex = year-diff(PlotYears)*0.07
      ytex = Aplot$Anomaly[Aplot$Year==year] + diff(Ylim) * 0.125
      Plot = Plot + 
        annotate("text", x = xtex, y = ytex, label = r, colour='white',hjust=1, vjust=0, size=3.5,fontface =2 ) + 
        geom_segment( x=xtex+1, xend=year, y=ytex, yend=Aplot$Anomaly[Aplot$Year==year] , color = "white", linewidth=0.1)
      }
    }
  
  # add Events
  for(r in names(Events[Lang][[1]])){
    year = Events[Lang][[1]][r]
    if(year <= upToYear){
      Sign = sign(Aplot$Anomaly[Aplot$Year==year])
      xtex = year-diff(PlotYears)*0.07
      ytex = Aplot$Anomaly[Aplot$Year==year] + diff(Ylim) * 0.125 * Sign
      if(r=="Lampe électrique\n(1879)" | r == "First electric lamp\n(1879)"){
        xtex = xtex + 17
        ytex = ytex + 0.15
        Plot = Plot + 
          annotate("text", x = xtex-10, y = ytex-0.05, label = r, colour='grey75',hjust=0.5, vjust=ifelse(Sign==1,0,1), size=3.25, fontface =2 ) + 
          geom_segment( x=xtex, xend=year, y=ytex-0.025, yend=Aplot$Anomaly[Aplot$Year==year] , color = "grey50", linewidth=0.05)
      }else{
        Plot = Plot + 
          annotate("text", x = xtex, y = ytex, label = r, colour='grey75',hjust=1, vjust=ifelse(Sign==1,0,1), size=3.25 , fontface =2) + 
          geom_segment( x=xtex+1, xend=year, y=ytex, yend=Aplot$Anomaly[Aplot$Year==year] , color = "grey50", linewidth=0.05)
      }
    }
  }
    # add CO2 arrow
      img <- readPNG("./fig/Smileys/Arrow_CO2_flèche_.png")
      arrowHigh = 0.22
      Plot = Plot + annotation_raster(img, xmin = PlotYears[2]-25, xmax = PlotYears[2]+10.5, ymin = Aplot$CO2CumSum[max(wi)]+0.0075-arrowHigh/2, ymax = Aplot$CO2CumSum[max(wi)]+0.0075+arrowHigh/2) 
      # annotate("text", x = , y = 
      #          , label = expression(phantom()%->%phantom()) # latex2exp::TeX("\\symbol($\\rightarrow$)",bold=T)
      #          , colour="#bfc200",size=30, hjust=0)  + 
    # add pictures
    if(upToYear >= 2002){
      img <- readPNG("./fig/Smileys/hot.png")
      Plot = Plot +    annotation_raster(img, xmin = 1996, xmax = 2011, ymin = 1.15, ymax = 1.35) 
    }
    if(upToYear >= max(Aplot$Year)-13 & (upToYear%%3 == max(Aplot$Year)%%3 | upToYear%%3 == (max(Aplot$Year)+1)%%3)){
      # img <- readPNG("/DISQUE/image/images pour diaporama/Smileys/skull-crossbones.png")
      img <- readPNG("./fig/Smileys/danger de mort.png")
      
      Ydanger = max(Aplot$Anomaly)+0.1
      Plot = Plot +    annotation_raster(img, xmin = 2005.5, xmax = 2040, ymin = Ydanger+0.05, ymax = Ydanger+diff(Ylim)*0.175)
      # if(upToYear < max(Aplot$Year)){
        Plot = Plot + geom_text(mapping = aes(x = (2005.5 + 2040)/2, y = Ydanger),  label = "DANGER" , colour="red", size=4.5, vjust = 0.5)
      # }
      # annotate("text", x = (min(Aplot$Year)*0.075+max(Aplot$Year)*0.925), y = 1.475,  label = "DANGER" , colour="red", size = 4, hjust=0) +
    }
  
    # text CO2
  if(upToYear > 1987){
    Plot = Plot + 
    annotate("text", x = PlotYears[2]-26+c('Fr'=0, 'Uk'=2)[Lang][[1]], y = Aplot$CO2CumSum[max(wi)]-0.165
             , label = c('Fr'=latex2exp::TeX(r"(Le $CO_2$ restera)",bold = T),'Uk'=latex2exp::TeX(r"($CO_2$ will)",bold = T))[Lang]
             , colour="#bfc200",size=4.25, hjust=0) + 
    annotate("text", x = PlotYears[2]-26+c('Fr'=0, 'Uk'=2)[Lang][[1]], y = Aplot$CO2CumSum[max(wi)]-0.35
             , label = c('Fr'="pendant des\ncentaines\nd'années",'Uk'="remain for\nhundreds\nof years")[Lang]
             , colour="#bfc200",size=4.25, hjust=0,fontface = "bold")
  }else{
    Plot = Plot + 
      annotate("text", x = PlotYears[2]-62+c('Fr'=0, 'Uk'=12)[Lang][[1]], y = Aplot$CO2CumSum[max(wi)]+0.13+c('Fr'=0, 'Uk'=0.02)[Lang][[1]]
               , label = c('Fr'=latex2exp::TeX(r"(Le $CO_2$ restera pendant)",bold = T),'Uk'=latex2exp::TeX(r"($CO_2$ will remain for)",bold = T))[Lang]
               , colour=CO2ColorAxis, size=4.25, hjust=0) + 
      annotate("text", x = PlotYears[2]-62+c('Fr'=0, 'Uk'=12)[Lang][[1]], y = Aplot$CO2CumSum[max(wi)]+0.06+c('Fr'=0, 'Uk'=0.02)[Lang][[1]]
               , label = c('Fr'="des centaines d'années",'Uk'="hundreds of years")[Lang]
               , colour=CO2ColorAxis, size=4.25, hjust=0,fontface = "bold")
  }
  
  
  # FileFormat = 'pdf'
  # FileFormat = 'png'
  fp <-  paste0("./temp_to_del_", upToYear, " (",Aplot$duration[Aplot$Year == upToYear], "ms).",FileFormat)
    suppressWarnings(
      # gg_export(
      #   png_from_pdf
      # )
      
    ggsave(plot = Plot,
           units = 'in',
           filename = fp,
           # device = ""
           device = c('png'='png', 'pdf' = cairo_pdf)[FileFormat][[1]]
           ,width = 9.23,height = 8.25
           # ,dpi = 100
           )
    )
}}
file.copy(fp, paste0("./temp_to_del_0", " (500ms).",FileFormat),overwrite = T)

Sys.sleep(3)

# sudo apt install poppler-utils
# pdftoppm -png test.pdf test

PDFs = R.utils::listDirectory()
PDFs = PDFs[regexpr('.pdf',PDFs)!=-1]
PDFs = sapply(PDFs,function(x)strsplit(x,'.pdf',fixed = T)[[1]][1])
sapply(PDFs,function(x)
  system(paste0('pdftoppm -r 72 -singlefile -png "',x,'.pdf" "',x,'"'))
  )

# => then "open as layers" in gimps and export to gif
# Delay between frames
# +
# https://www.freeconvert.com/gif-compressor

# file.remove(paste0("./temp_to_del_", Aplot$Year, " (",Aplot$duration, "ms).","pdf"))
# file.remove(paste0("./temp_to_del_0", " (500ms).","pdf"))
# file.remove(paste0("./temp_to_del_", Aplot$Year, " (",Aplot$duration, "ms).","png"))
# file.remove(paste0("./temp_to_del_0", " (500ms).","png"))

