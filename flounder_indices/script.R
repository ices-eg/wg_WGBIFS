## Script for estimating survey indices for flounder in the Baltic.
## Latest version of "surveyIndex" package should be used.
## Author: Casper W. Berg, DTU Aqua.
##remotes::install_github("casperwberg/surveyIndex/surveyIndex")
library(DATRAS)
library(surveyIndex)
library(maps)
library(mapdata)

png(width=1024,height=1024,pointsize=16)

## Extra stuff (disable to make script run much faster)
do.noship = FALSE
do.tweedie = FALSE
do.leaveout = FALSE

## Use multiple cores to speed up things (only works if you have MKL installed)
##try( setMKLthreads(2) )

species = "Platichthys flesus"

if(!file.exists("flounderDATRAS.RData")){
    BITS <- readExchangeDir("~/Documents/DATRAS/exchange/BITS/",strict=FALSE)
    
    BITS <- subset(BITS,Species==species)
    
    geartab <- xtabs(~Gear,data=BITS[[2]])
    
    goodGears = geartab[geartab>100]
    
    BITS <- addSpectrum(BITS,by=1)
    BITS$Ntot <- rowSums(BITS$N)
    aggregate(Ntot ~ Gear,data=BITS[[2]],FUN=median)
    gtab2 <- aggregate(Ntot ~ Gear,data=BITS[[2]],FUN=sum)

    ## only gears that caught at least 1000 flounders in total
    goodGears = gtab2[gtab2$Ntot>1000,1]

    
    
    BITS <- subset(BITS,Gear %in% goodGears, Quarter %in% c("1","4"))
    
    BITS <- subset(BITS,!HaulVal=="I",StdSpecRecCode%in%c(1,3))
    
    ## some "No oxygen" hauls have not been performed (HaulDur=0 or NA)
    ## Set HaulDur to a positive number for these such that they are included
    BITS$HaulDur[ BITS$HaulDur<5 | is.na(BITS$HaulDur) ] = 10
    
    BITS <- subset(BITS,!is.na(Depth))
    
    BITS<-addSpatialData(BITS,"~/Documents/shapefiles/ICES_areas.shp")

    IBTS <- readExchangeDir("~/Documents/DATRAS/exchange/IBTS/",strict=FALSE)
    IBTS <- subset(IBTS,Species==species, Year %in% 1991:2023,Quarter=="1")
    IBTS<-addSpatialData(IBTS,"~/Documents/shapefiles/ICES_areas.shp")
    IBTS <- subset(IBTS,ICES_SUB %in% c("21","22","23"))
    
    d <- c(BITS,IBTS)

    IBTS <- BITS <- NULL;
    gc()
    ## make "continuous" time variable, discretized to quarterly to prevent too much time wiggliness
    d[[2]]$ctime = as.numeric(as.character(d$Year))+(as.numeric(as.character(d$Quarter) )-1)/4   
    
    d[[1]]$ctime = as.numeric(as.character(d[[1]]$Year))+(as.numeric(as.character(d[[1]]$Quarter) )-1/4)
    d = subset(d,!is.na(ICES_SUB))

    
    
    save(d,file="flounderDATRAS.RData",version=2)
} else {
    load("flounderDATRAS.RData")
}


d = subset(d, !is.na(Depth), HaulVal!="I")

## Re-order Gear levels (most common first)
d$Gear = factor(d$Gear, levels = names(sort(summary(d$Gear),decreasing=TRUE)))


summary(d[[1]]$LngtCm)
summary(d[[3]]$LngtCm)
## 400 cm flounders??
d[[1]] = subset(d[[1]],LngtCm<100)
d[[3]] = subset(d[[3]],LngtCm<100)

d = addSpectrum(d,by=1)


nyears = nlevels(d$Year)

d$Ntot = rowSums(d$N)
## Remove ships that never observed any flounder
d = removeZeroClusters(d,factors=c("Ship"))

## Estimate L-W relationship with time-varying params:  W_t = a_t * L ^ b_t 
LWmodel <- gam( log(IndWgt) ~ Quarter + s(ctime,k=nyears) + s( log(LngtCm),by=ctime),data=subset(d[[1]],IndWgt>0))

years = as.numeric(levels(d$Year))

## Make L-W conversion for each year and quarter
nd = data.frame(LngtCm = attr(d,"cm.breaks")[-1]-0.5)
LWsQ1 = matrix(NA,length(years),length(nd$LngtCm))
LWsQ4 = matrix(NA,length(years),length(nd$LngtCm))
d$NW = d$N
for(qq in c("1","4")){
    for(yy in years){
        print(yy)
        nd$ctime = yy + ifelse(qq=="1",0,0.75)
        nd$Quarter = qq
        LW = exp(predict(LWmodel,newdata=nd))
        if(qq=="1")  LWsQ1[which(years==yy),] = LW
        if(qq=="4")  LWsQ4[which(years==yy),] = LW
        sel = which(d[[2]]$Year==yy & d[[2]]$Quarter==qq)
        for(i in sel) d$NW[i,] = d$N[i,]*LW         
    }
}

par(mfrow=c(1,1))
plot(years,LWsQ1[,26]/mean(LWsQ1[,26]),ylim=c(0.8,1.2),main="26 cm flounder weight")
points(years,LWsQ4[,26]/mean(LWsQ4[,26]),col=2)
abline(h=1)
legend("topright",col=1:2,legend=c("Q1","Q4"),pch=1)

plot(d)

par(mfrow=c(2,1))
plot(colSums(d$N),type="h",main="Total numbers by length")
plot(colSums(d$NW),type="h",main="Total biomass by length")

d$biomass = rowSums(d$NW[,20:ncol(d$NW)])/1000

par(mfrow=c(1,1))
zero = d$biomass==0
plot(d$lon,d$lat,pch=16,cex=sqrt(d$biomass)/4,main = "Biomass > 20 cm",col=rgb(0,0,1,0.25))
points(d$lon[zero],d$lat[zero],pch=".",col=2,cex=2)
maps::map("worldHires", fill = TRUE, plot = TRUE,add = TRUE, col = grey(0.8))


grid = getBathyGrid(d,minDepth=5,maxDepth=190,resolution=4,maxDist=0.2,shapefile="~/Documents/shapefiles/ICES_areas.shp",select="ICES_SUB")

grid = subset(grid,ICES_SUB %in% as.character(21:28))

## We need a grid for each year for this model, because of the YearQ factor
gridlist = list()
gridlistQ4 = list()
for(yy in levels(d$Year)){
    gridlist[[yy]] = grid
    gridlist[[yy]]$Year=yy
    gridlist[[yy]]$YearQ=paste(yy,"1")
    gridlist[[yy]]$Quarter="1"
    gridlistQ4[[yy]] = gridlist[[yy]]
    gridlistQ4[[yy]]$YearQ=paste(yy,"4")
    gridlistQ4[[yy]]$Quarter="4"

}

plot(grid$lon,grid$lat)
points(d$lon,d$lat,pch=".",col=2,cex=3)


d$Nage=matrix(d$biomass,ncol=1)
colnames(d$Nage)<-"1"

d$YearQ = factor(paste(d$Year,d$Quarter))

model = "Year*Quarter + s(sqrt(Depth),k=5,bs='ds',m=c(1,0),by=Quarter) + Gear + s(Ship,bs='re') + s(lon,lat,bs='ds',m=c(1,0.5),k=80,by=Quarter) + s(lon,lat,bs='ds',m=c(1,0.5),by=YearQ,k=6,id=1) + offset(log(HaulDur))"

system.time( SI <- getSurveyIdx(d,ages=1,predD=gridlist,fam="LogNormal",modelP=model,modelZ=model,gamma=1,cutOff=0,control=list(trace=TRUE,maxit=20)))

if(do.noship){
    model.noship = "Year*Quarter + s(sqrt(Depth),k=5,bs='ds',m=c(1,0),by=Quarter) + Gear + s(lon,lat,bs='ds',m=c(1,0.5),k=80,by=Quarter) + s(lon,lat,bs='ds',m=c(1,0.5),by=YearQ,k=6,id=1) + offset(log(HaulDur))"
    
    system.time( SI.noship <- getSurveyIdx(d,ages=1,predD=gridlist,fam="LogNormal",modelP=model.noship,modelZ=model.noship,gamma=1,cutOff=0,control=list(trace=TRUE,maxit=20)))
}

if(do.tweedie){
    model.tw = "Year*Quarter + s(sqrt(Depth),k=6,bs='ds',m=c(1,0),by=Quarter) + Gear + s(lon,lat,bs='ds',m=c(1,0.5),k=120,by=Quarter) + s(lon,lat,bs='ds',m=c(1,0.5),by=YearQ,k=7,id=1) + offset(log(HaulDur))"
    
    system.time( SI.tw <- getSurveyIdx(d,ages=1,predD=gridlist,fam="Tweedie",modelP=model.tw,modelZ=model.noship,gamma=1,cutOff=0,control=list(trace=TRUE,maxit=10)))
}

AIC.surveyIdx(SI)
if(do.tweedie) AIC.surveyIdx(SI.tw)
if(do.noship) AIC.surveyIdx(SI.noship)
## SI best

if(do.noship && do.tweedie){
    par(mfrow=c(1,1),mar=c(4,4,4,4))
    surveyIndex:::plot.SIlist(list(Q1=SI,Q1.noship=SI.noship,Q1.tw=SI.tw),rescale=TRUE,main="Biomass>20 cm",allCI=TRUE)
    abline(h=1)
}

if(do.tweedie){
    par(mfrow=c(3,1))
    factorplot(SI.tw$pModels[[1]],"Gear",main="Gear effect - Tweedie")
    abline(h=1)
    factorplot(SI$pModels[[1]],"Gear",main="Gear effect - DLN positive")
    abline(h=1)
    factorplot(SI$zModels[[1]],"Gear",invlink=plogis,main="Gear effect - DLN p/a")
    abline(h=0.5)
}

xtabs(~Gear+Year+Quarter,data=d[[2]])

## Quarter 4 indices
dQ4 = subset(d,Quarter=="4")
library(MASS)
SIQ4 <- redoSurveyIndex(dQ4,SI,predD=gridlistQ4,predfix=list(Quarter="4"))


par(mfrow=c(2,1))
depthDist(SI,gridlist[[1]],by=5,main="Depth distribution Q1")

depthDist(SIQ4,gridlistQ4[[1]],by=5,main="Depth distribution Q4")

## Residuals
resid <- residuals(SI)

## QQ-plot
par(mfrow=c(1,1),mar=c(4,4,4,4))
qqnorm(resid)
abline(0,1,col=2)

plot(d$HaulVal,resid)

################
## Maps
################

mycols = c(rev(heat.colors(9)),"darkred")

dQ1 = subset(d,Quarter=="1")
surveyIdxPlots(SI,dQ1,myids=NULL,predD=gridlist,select="absolutemap",year=years,colors=mycols,par=list(mfrow=n2mfrow(nyears),mar=c(0,0,2,0),cex=0.6),legend=FALSE,map.cex=1,mapBubbles=TRUE)

surveyIdxPlots(SI,d,myids=NULL,predD=gridlist,select="absolutemap",year=years,colors=mycols,par=list(mfrow=n2mfrow(nyears),mar=c(0,0,2,0),oma=c(6,1,2,1),cex=0.6),legend=FALSE,map.cex=1,mapBubbles=FALSE)
mapLegend(SI,years,mycols)
box()
title("Q1 absolute maps",outer=TRUE)

surveyIdxPlots(SIQ4,d,myids=NULL,predD=gridlist,select="absolutemap",year=years,colors=mycols,par=list(mfrow=n2mfrow(nyears),mar=c(0,0,2,0),oma=c(6,1,2,1),cex=0.6),legend=FALSE,map.cex=1,mapBubbles=FALSE)
mapLegend(SI,years,mycols)
box()
title("Q4 absolute maps",outer=TRUE)


dQ4 = subset(d,Quarter=="4")
surveyIdxPlots(SIQ4,dQ4,myids=NULL,predD=gridlist,select="absolutemap",year=unique(dQ4$Year),colors=mycols,par=list(mfrow=n2mfrow(nyears),mar=c(0,0,2,0),oma=c(6,1,1,1),cex=0.6),legend=FALSE,map.cex=1,mapBubbles=TRUE)

## Uncertainty maps

CVcols = colorRampPalette(rev(c("red","yellow","green","blue")))
    surveyIdxPlots(SI,d,myids=NULL,predD=gridlist,select="CVmap",year=years,colors=CVcols(8),par=list(mfrow=n2mfrow(nyears),mar=c(0,0,2,0),cex=0.6),legend=TRUE,legend.signif=2,map.cex=1,cutp=c(0,0.1,0.2,0.3,0.4,0.6,0.8,1.4,Inf))
title("Q1 CV maps",outer=TRUE)

surveyIdxPlots(SIQ4,dQ4,myids=NULL,predD=gridlist,select="CVmap",year=unique(dQ4$Year),colors=CVcols(8),par=list(mfrow=n2mfrow(nyears),mar=c(0,0,2,0),cex=0.6),legend=TRUE,legend.signif=2,map.cex=1,cutp=c(0,0.1,0.2,0.3,0.4,0.6,0.8,1.4,Inf))
title("Q4 CV maps",outer=TRUE)



par(mfrow=c(2,1),mar=c(4,4,4,4))
factorplot(SI$pModels[[1]],"Ship",levs=levels(d$Ship),main="Ship effects - positive")
abline(h=1)
factorplot(SI$zModels[[1]],"Ship",invlink=plogis,levs=levels(d$Ship),main="Ship effects - presence/absence")
abline(h=0.5)

par(mfrow=c(2,1),mar=c(4,4,4,4))
factorplot(SI$pModels[[1]],"Gear",main="Gear effects - positive")
abline(h=1)
factorplot(SI$zModels[[1]],"Gear",invlink=plogis,main="Gear effects - p/a")
abline(h=0.5)

##ge = getEffect(SI,d,"Gear",cutOff=0)[[1]]
##gesd=(log(ge[,2])-log(ge[,3]))/2
##concurvity(SI$pModels[[1]])
##concurvity(SI$pModels[[1]],full=FALSE)


## Plot maps with gears
d$GearNum = as.numeric(d$Gear)
d.yq = split(d,d$YearQ)
latlims=range(d$lat)
lonlims=range(d$lon)

par(mfrow=n2mfrow(length(d.yq[1:22])),mar=c(0,0,2,0))
lapply(d.yq[1:22],function(x) {
    plot(x,xlim=lonlims,ylim=latlims,col=x$GearNum,pch=x$GearNum,plot.response=FALSE,axes=FALSE,plot.squares=FALSE)
    title(x$YearQ[1])
    })
plot(1:nlevels(d$Gear),rep(1,nlevels(d$Gear)),pch=1:nlevels(d$Gear),col=1:nlevels(d$Gear),axes=FALSE)
axis(1,labels=levels(d$Gear),at=1:nlevels(d$Gear))

par(mfrow=c(1,1),mar=c(4,4,4,4))
surveyIndex:::plot.SIlist(list(Q1=SI,Q4=SIQ4),rescale=TRUE,main="Biomass>20 cm",allCI=TRUE)
abline(h=1)

###################################
## Calculate indices by subarea
###################################

grid2223 = subset(grid,ICES_SUB %in% as.character(22:23))

gridlist2223 = list()
gridlist2223Q4 = list()
for(yy in levels(d$Year)){
    gridlist2223[[yy]] = grid2223
    gridlist2223[[yy]]$Year=yy
    gridlist2223[[yy]]$YearQ=paste(yy,"1")
    gridlist2223[[yy]]$Quarter="1"
    gridlist2223Q4[[yy]] = gridlist2223[[yy]]
    gridlist2223Q4[[yy]]$YearQ=paste(yy,"4")
    gridlist2223Q4[[yy]]$Quarter="4"
}

grid2123 = subset(grid,ICES_SUB %in% as.character(21:23))

gridlist2123 = list()
gridlist2123Q4 = list()
for(yy in levels(d$Year)){
    gridlist2123[[yy]] = grid2123
    gridlist2123[[yy]]$Year=yy
    gridlist2123[[yy]]$YearQ=paste(yy,"1")
    gridlist2123[[yy]]$Quarter="1"
    gridlist2123Q4[[yy]] = gridlist2123[[yy]]
    gridlist2123Q4[[yy]]$YearQ=paste(yy,"4")
    gridlist2123Q4[[yy]]$Quarter="4"
}


grid2628 = subset(grid,ICES_SUB %in% as.character(26:28))

gridlist2628 = list()
gridlist2628Q4 = list()
for(yy in levels(d$Year)){
    gridlist2628[[yy]] = grid2628
    gridlist2628[[yy]]$Year=yy
    gridlist2628[[yy]]$YearQ=paste(yy,"1")
    gridlist2628[[yy]]$Quarter="1"
    gridlist2628Q4[[yy]] = gridlist2628[[yy]]
    gridlist2628Q4[[yy]]$YearQ=paste(yy,"4")
    gridlist2628Q4[[yy]]$Quarter="4"

}

SI.2223 <- redoSurveyIndex(d,SI,gridlist2223,predfix=list(Quarter="1"))
SI.2223.Q4 <- redoSurveyIndex(dQ4,SI,gridlist2223Q4,predfix=list(Quarter="4"))

par(mfrow=c(1,1),mar=c(4,4,4,4))
surveyIndex:::plot.SIlist(list(Q1=SI.2223,Q4=SI.2223.Q4),rescale=TRUE,main="22-23 Biomass>20 cm",allCI=TRUE)
abline(h=1)

SI.2123 <- redoSurveyIndex(d,SI,gridlist2123,predfix=list(Quarter="1"))
SI.2123.Q4 <- redoSurveyIndex(dQ4,SI,gridlist2123Q4,predfix=list(Quarter="4"))

par(mfrow=c(1,1),mar=c(4,4,4,4))
surveyIndex:::plot.SIlist(list(Q1=SI.2123,Q4=SI.2123.Q4),rescale=TRUE,main="21-23 Biomass>20 cm",allCI=TRUE)
abline(h=1)


SI.2628 <- redoSurveyIndex(d,SI,gridlist2628,predfix=list(Quarter="1"))
SI.2628.Q4 <- redoSurveyIndex(dQ4,SI,gridlist2628Q4,predfix=list(Quarter="4"))
surveyIndex:::plot.SIlist(list(Q1=SI.2628,Q4=SI.2628.Q4),rescale=TRUE,main="26-28 Biomass>20 cm",allCI=TRUE)
abline(h=1)

surveyIndex:::plot.SIlist(list(Q1.2628=SI.2628,Q4.2628=SI.2628.Q4,Q1.2223=SI.2223,Q4.2223=SI.2223.Q4),rescale=TRUE,main="Biomass>20 cm",allCI=TRUE)
abline(h=1)


xtabs(~ICES_SUB+Year,data=dQ4[[2]])
xtabs(~ICES_SUB+Year,data=subset(d,Quarter=="1")[[2]])

## 22-23 Q1: small number of hauls until 1996
##       Q4: nothing in 1991, ok from 1992   
## 26-28 Q1: ok all years
##       Q4: bad coverage 1991-1996. ok until 2002, good from 2003.

######################
## Write CSV files
######################

write.csv(data.frame(Year=rownames(SI.2223$idx),Index2223Q1=SI.2223$idx[,1],Index2223Q1.CV=SI.2223$idx.CV[,1]),file="index2223Q1.csv",row.names=FALSE)

write.csv(data.frame(Year=rownames(SI.2223.Q4$idx),Index2223Q4=SI.2223.Q4$idx[,1],Index2223Q4.CV=SI.2223.Q4$idx.CV[,1]),file="index2223Q4.csv",row.names=FALSE)

write.csv(data.frame(Year=rownames(SI.2123$idx),Index2123Q1=SI.2123$idx[,1],Index2123Q1.CV=SI.2123$idx.CV[,1]),file="index2123Q1.csv",row.names=FALSE)

write.csv(data.frame(Year=rownames(SI.2123.Q4$idx),Index2123Q4=SI.2123.Q4$idx[,1],Index2123Q4.CV=SI.2123.Q4$idx.CV[,1]),file="index2123Q4.csv",row.names=FALSE)

write.csv(data.frame(Year=rownames(SI.2628$idx),Index2628Q1=SI.2628$idx[,1],Index2628Q1.CV=SI.2628$idx.CV[,1]),file="index2628Q1.csv",row.names=FALSE)

write.csv(data.frame(Year=rownames(SI.2628.Q4$idx),Index2628Q4=SI.2628.Q4$idx[,1],Index2628Q4.CV=SI.2628.Q4$idx.CV[,1]),file="index2628Q4.csv",row.names=FALSE)


dev.off()

## Leave one gear out runs 
if(do.leaveout){
    oldgears = levels(d$Gear)[-c(1:2)]
    LO <- list()
    LO.d <- list()
    for(gg in oldgears){
        cat("Leaving out ",gg,"\n")
        LO.d[[gg]] <- subset(d,Gear!=gg)
        LO[[gg]] <- getSurveyIdx(LO.d[[gg]],ages=1,predD=gridlist,fam="LogNormal",modelP=model,modelZ=model,gamma=1,cutOff=0,control=list(trace=TRUE,maxit=20))
        
    }
    
    LO$base = SI
    
    png("leaveout.png",width=1024,height=800,pointsize=14)
    surveyIndex:::plot.SIlist(rev(LO),rescale=TRUE,main="Leave one gear out")
    dev.off()
}
