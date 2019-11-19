#Exam 3 script
library(dplyr)
library(ggplot2)
library(reshape2)
library(plyr)
library(stringr)
# Spectral colour map from ColorBrewer
spectral <- function(n=6) {
  library("RColorBrewer")
  rev(brewer.pal(name="Spectral", n=n))
}

scale_fill_spectral <- function(...) {
  scale_fill_gradientn(colours=spectral(...))
}
scale_colour_spectral <- function(...) {
  scale_colour_gradientn(colours=spectral(...))
}

#Section 1
load("test3_data.Rdata")

fields = names(d[,c(1:17)])
d1 = d[,c(fields)]
d2 = arrange(d1, transect.id, desc(dateTime))

#Section 2

dir.create("E3_plots")


ddply(.data = d1, .variables = c("transect.id"), function(x){
  
  name <- unique(x$transect.id)
  
  plota <- ggplot(data = x, aes(x = dateTime, y = depth)) +
    geom_point()+
    facet_wrap(~tow)
  # scale_x_datetime(name = "Time", labels = date_format("%H:%M"),
  #                  breaks = date_breaks("15 min"), minor_breaks = "5 min") +
  geom_smooth() #add smoother
  ggtitle(label = name)
  
  ggsave(filename = paste0(name,'.png'),
         plot = plota, width = 4, height = 3, units = 'in',
         dpi = 300)
  
}, .inform = T, .progress = "text")







ggplot(d2, aes(x=dateTime, y=depth)) +
  geom_line(colour="blue") + geom_smooth()
  


unique(d2$tow)
# identify variables of interest to plot
vars <- c("und", "s", "m")
x=d[d$transect.id=="OST14-1W-Und",]

ddply(.data = d, .variables = "transect.id", function(x){
  vars <- c("und", "s", "m")
  x <- na.omit(x)
  
  x$depth_round <- round(x$depth, digits = 1)
  
  dm <- melt(x, id.vars=c("dateTime", "depth_round"), measure.vars=vars)
  
  und_gg <- ggplot(data = d[d$variable == "und",], aes(x=(dateTime), y=-depth_round)) +
    geom_line(aes(colour=value, size = value), na.rm=T, show.legend = F) +
    scale_colour_gradient(high=spectral(), na.value=NA)+
     scale_x_datetime(name = "Time", labels = date_format("%H:%M"),
                     breaks = date_breaks("15 min"), minor_breaks = "5 min") +
    scale_x_datetime(name = "") +
    scale_y_continuous("depth", expand=c(0.01,0.01)) +
    facet_grid(variable~.) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "black"),
          strip.text.y = element_text(face = "bold", size = 12)) +
    theme(axis.text.x = element_blank(),
          axis.title.y = element_text(face = "bold", size = 12)) +
    geom_smooth()+
    ggtitle(label = unique(x$transect.id))

  
  s_gg <- ggplot(d[d$variable == "s",], aes(x=dateTime, y=-depth_round)) +
    geom_line(aes(colour=value, size = value), na.rm=T, show.legend = FALSE) +
    scale_colour_gradient(high=spectral(), na.value=NA) +
    # scale_x_datetime("Time", labels = date_format("%H:%M"), breaks = date_breaks("15 min"), minor_breaks = "5 min") +
    scale_x_datetime(name = "") +
    scale_y_continuous("depth", expand=c(0.01,0.01)) + facet_grid(variable~.) +
    theme(strip.text.y = element_text(size = 10)) +
    theme(axis.text.x = element_blank()) +
    theme(axis.title = element_text(size = 12))+
    geom_smooth()
  
  m_gg <- ggplot(d[d$variable == "m",], aes(x=dateTime, y=-depth_round)) +
    geom_line(aes(colour=value, size = value), na.rm=T, show.legend = FALSE) +
    scale_colour_gradient(high=spectral(), na.value=NA) +
    scale_x_datetime(name = "") +
    #scale_x_datetime("Time", labels = date_format("%H:%M"), breaks = date_breaks("15 min"), minor_breaks = "5 min") +
    scale_y_continuous("depth", expand=c(0.01,0.01)) + facet_grid(variable~.) +
    theme(strip.text.y = element_text(size = 10)) +
    theme(axis.text.x = element_blank()) +
    theme(axis.title = element_text(size = 12))+
    geom_smooth()
  
  
  g <- grid.arrange(und, s, m, ncol=1)
  
  #print image files to a directory
  png(file = paste0("E3_plots/",unique(x$transect.id), ".png"), width = 8.5,
      height = 14, units = "in", res = 300)
  plot(g)
  dev.off()
  
}, .progress = "text")


#Section 3

#create custom function
study.fxn <- function(x){
  
  #t <- stringr::str_split_fixed(string = x[['transect.id']], pattern = "-", n = 3)
  t <- stringr::str_split_fixed(string = x, pattern = "-", n = 3)
  
  s <- t[2]
if(str_detect(string = s, pattern = "Eddy")){
  study = "Eddy"
  
} else {
  study = "Spatial"
}

return(study)

}

d$study=NA
for(i in 1:nrow(d)){
  
  d$study[i] <- study.fxn(x = d$transect.id[i]) 
}
d$study1 = NA
d$study1 <- 
  apply(X = d, 1, FUN = study.fxn)



#Section 4
d.spac = d[d$study=="spatial",]
press = ggplot(d.spac, aes(x = region,y = pressure)) + geom_boxplot() + facet_wrap(.~region_fac)
press

#Section 5
d_s = d.spac[d.spac$tow=="s",]
d_m = d.spac[d.spac$tow=="m",]
plot1 = d_s %>% group_by(region) %>% summarise(mean = mean(temp,na.rm = T), sd = sd(temp,na.rm = T))
plot2 = d_m %>% group_by(region) %>% summarise(mean = mean(temp,na.rm = T), sd = sd(temp,na.rm = T))

plot1$f_deg = NA
plot1$K_deg = NA
plot2$f_deg = NA
plot2$K_deg = NA

for(i in 1:nrow(plot1)){
  plot1[i,]$f_deg = (plot1[i,]$sd + 32)*(5/9)
  plot1[i,]$K_deg = (plot1[i,]$sd + 273.15)
}

for(i in 1:nrow(plot2)){
  plot2[i,]$f_deg = (plot2[i,]$sd + 32)*(5/9)
  plot2[i,]$K_deg = (plot2[i,]$sd + 273.15)
}


library(reshape2)
m.vars=c("temp")
id.vars = c("region","tow")
dm <- melt(d, id.vars=id.vars, measure.vars=m.vars)

Melt1 = melt(plot1, id.vars=id.vars, measure.vars=m.vars)
Melt2 = melt(plot2, id.vars=id.vars, measure.vars=m.vars)
#Section 6

bar.1=ggplot(dm,aes(x=variable, y= value)) +geom_bar(stat = "identity", position = "dodge") + facet_grid(.~region)
bar.1

##plot on log10 scale
bar.2=ggplot(dm,aes(x=variable, y= value)) +geom_bar(stat = "identity", position = "dodge") + facet_grid(.~region)+
scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
              labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) 
