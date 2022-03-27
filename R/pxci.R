pxciarFun <-
  function(#*#
    x=NULL
    ,#*#
    nn='pxzozoned'
    ) {#*#
  # - [ ] define area
  getgd(nn)
  if(is.null(x)) {x=pxzozoned[1,irregpcode(substr(rc,1,3))]} #do not like this 20180531 but satisfies scripting
  pxciard <<- regpcode(x)
  putt(pxciard)
}#*#

#' @export
pxcisu2Fun <-
  function(#*#
    nn='pxciard'
    ) {#*#
  # - [ ] stub for the old collar
  getgd(nn)
  pxcisu2d <<- pxciard
  putt(pxcisu2d)
}#*#


#' @export
pxcicpxFun <-
  function(#*#
    nn=c('pxjocpd','pxciard')
    ) {#*#
  # - [ ] subset cpd to area
  getgd(nn)
  pxcicpxd <<- pxjocpd[grep(grepstring(pxciard),rcunique),]
  putt(pxcicpxd)
}#*#


#' @export
pxcirdo4Fun <-
  function(#*#
    nn=c('pxg2rdo4d','pxcisu2d')
    ) {#*#
  # - [ ] pxcirdo4 from rdo4 (os) #not so big anyway
  getgd(nn)
  cirdo4 <- function(rdo4d,cisu2d) {
    x1 <- as.list(seq_along(rdo4d))
    for(i in seq_along(rdo4d)) {
      x1[[i]] <- rdo4d[[i]][grep(
        grepstring(unique(substr(cisu2d,1,3*i))),
        rdo4d[[i]]@data[,'rc']
      ),]
    }
    x1
  }
  pxcirdo4d <<- cirdo4(rdo4d=pxg2rdo4d,cisu2d=pxcisu2d)
  putt(pxcirdo4d)
}#*#


#' @export
pxcifadeFun <-
  function(#*#
  nn=c('pxcisu2d','pxsofaded')
  ) {#*#
  # - [ ] pxcifade (err)
  getgd(nn)
  cifade <- function(cisu2d,faded) {
    x1 <- faded[grep(grepstring(cisu2d),idseg)]
    x1[,rc12:=substr(id,1,12)]
  }
  pxcifaded <<- cifade(cisu2d=pxcisu2d,faded=pxsofaded)
  putt(pxcifaded)
}#*#

#' @export
pxcippm1Fun <-
  function(#*#
    nn=c('pxpvppm1d','pxcicpxd','pxcisu2d')
    ) {#*#
  # - [ ] pxcippm1 (ppm)
  getgd(nn)
  cippm1 <- function(ppm1d,cicpxd,cisu2d) {
    cicpxd[ppm1d,on=c(rc='rc12'),nomatch=0][grep(grepstring(cisu2d),rc)]
  }
  pxcippm1d <<- cippm1(ppm1d=pxpvppm1d,cicpxd=pxcicpxd,cisu2d=pxcisu2d)
  putt(pxcippm1d)
}#*#


#' @export
pxcierrFun <-
  function(#*#
    nn=c('pxcifaded','pxcisu2d','pxcicpxd')
    ,#*#
    yearsmax=22
    ,#*#
    daysmin=90
    ,#*#
    errmax=1
    ,#*#
    errmin=-1
    ) {#*#
  # - [ ] screen err
  getgd(nn)
  cierr <- function(cifaded,cisu2d,cicpxd,yearsmax,daysmin,errmax,errmin) {
    #browser()
    d1 <- paste0(as.character(2017-yearsmax),'-12-31')
    cifaded[,days:=as.integer(deed_date-startdate)]
    x1 <- setkey(cifaded[(d1<startdate)][daysmin<=days][err<errmax][errmin<err],id,deed_date)
    x1a <- x1[,.(err=sum(err)*365/days,r=sum(r)*365/days,fit=fit*365/days,rc12=max(rc12),days=max(days)),'id,startdate'] #added startdate into by
    x1b <- x1a[grep(grepstring(cisu2d),id)][,inlier:=T][cicpxd[,.(rc,rcunique,zo)],on=c(rc12='rc'),nomatch=0]
    x1b
  }
  pxcierrd <<- cierr(cifaded=pxcifaded,cisu2d=pxcisu2d,cicpxd=pxcicpxd,yearsmax=yearsmax,daysmin=daysmin,errmax=errmax,errmin=errmin)
  putt(pxcierrd)
}#*#

#' @export
pxcippmFun <-
  function(#*#
    nn=c('pxcippm1d')
    ,#*#
    pvmax=50e6
    ,#*#
    pvmin=1e4
    ,#*#
    m2max=500
    ,#*#
    m2min=30
    ,#*#
    ppm2max=2e4
    ,#*#
    ppm2min =2e2
    ) {#*#
  # - [ ]  screen ppm
  getgd(nn)
  cippm <- function(cippm1d,pvmax=50e6,pvmin=1e4,m2max=500,m2min=30,ppm2max=2e4,ppm2min =2e2) {
    x2 <- copy(cippm1d)[,ppm2:=pv/median]
    x3 <- setkey(x2[pv<pvmax][pvmin<pv][median<m2max][m2min<median][ppm2<ppm2max][ppm2min<ppm2],id)
    x3
  }
  pxcippmd <<- cippm(cippm1d=pxcippm1d,pvmax=pvmax,pvmin=pvmin,m2max=m2max,m2min=m2min,ppm2max=ppm2max,ppm2min=ppm2min)
  putt(pxcippmd)
}#*#

#' @export
pxcicercFun <-
  function(#*#
    nn=c('pxcippmd','pxcierrd')
    ,#*#
    k1=1
    ) {#*#
  # - [ ] pxcicerc: join and screen to minimum k1 records on both inputs
  getgd(nn)
  cicerc <- function(cippmd,cierrd,k1=1) {
    x1 <- cippmd[,.(rcode,rcunique,id,m2=median,pv,ppm2=pv/median,estate,newbuild,type,lastsale)][cierrd[,.(id,r,err)],on=c(id='id'),nomatch=0]
    x2 <- x1[,.N,rcunique][k1<=N,.(rcunique,N)]
    x2
  }
  pxcicercd <<- cicerc(cippmd=pxcippmd,cierrd=pxcierrd,k1=k1)
  putt(pxcicercd)
}#*#


#' @export
pxcicenFun <-
  function(#*#
    nn=c('pxcicercd','pxcippmd','pxcierrd')
    ) {#*#
  # - [ ] pxcicen: number of records by type in each cell
  getgd(nn)
  cicen <- function(cicercd,cippmd,cierrd) {
    x1 <- cippmd[cicercd,on=c(rcunique='rcunique')][,.(nppm=.N),rcunique]
    x2 <- cierrd[cicercd,on=c(rcunique='rcunique')][,.(nerr=.N),rcunique]
    x1[x2,on=c(rcunique='rcunique')]
  }
  pxcicend <<- cicen(cicercd=pxcicercd,cippmd=pxcippmd,cierrd=pxcierrd)
  putt(pxcicend)
}#*#

#' @export
pxcinbrFun <-
  function(#*#
    ty=c('pxcippmd','pxcierrd')
    ,#*#
    nn='pxcicend'
    ,#*#
    k1=getpv('cinbr','k1br')
    #*#,...
    ) {#*#
  # - [ ] neighbour: for each (cell,type) the first m units
  getgd(c(ty,nn))
  cinbr <- function(ty,nn) {
    x1 <- rbindlist(lapply(ty,cinbr1,k1=k1))
    setkey(x1,rcunique)[pxcicend[,.(rcunique)],mult='all',nomatch=0]
  }
  pxcinbrd <<- cinbr(ty=ty,nn=nn)
  putt(pxcinbrd)
}#*#

#' @export
pxcinoupFun <-
  function(#*#
    nn=c('pxcippmd','pxcinbrd')
    ,#*#
    k1=getpv('cinoup','k1p')
    ,#*#
    p1=getpv('cinoup','p1p')
    #*#,...
    ) {#*#
  # - [ ] the sublime function: rnk, p, weight, inlier
  getgd(nn)
  cinoup <- function(k=100,p=.05){#},...) {
    #pxcinoupd <<- cinou(x='cippmd',nn='pxcinbrd',jrank='ppm2',k=k,p=p,...)
    cinou(x='pxcippmd',cinbrd=pxcinbrd,jrank='ppm2',k=k,p=p)#,...)
  }
  pxcinoupd <<- cinoup(k=k1,p=p1)
  putt(pxcinoupd)
}#*#

#' @export
pxcinoueFun <-
  function(#*#
    nn=c('pxcippmd','pxcierrd','pxcinbrd')
    ,#*#
    k1=getpv('cinoue','k1e')
    ,#*#
    p1=getpv('cinoue','p1e')
    #*#,...#*#
    ) {#*#
  # - [ ] rnk, p, weight, inlier
  getgd(nn)
  cinoue <- function(k,p){#...) {
    cinou(x='pxcierrd',cinbrd=pxcinbrd,jrank='err',k=k,p=p) #},...)
  }
  pxcinoued <<- cinoue(k=k1,p=p1)
  putt(pxcinoued)
}#*#
pxcinouechk <-#*#
  function(#*#
    nn=c('pxcinoued','pxcinoupd','pxcicend','pxcinbrd')#*#
    ) {#*#
  x1 <- pxcinoued[,.(err=weighted.mean(err,weight),Ne=as.numeric(.N)),rcunique]#*#
  x2 <- pxcinoupd[,.(m2=weighted.mean(median,weight),pv=weighted.mean(pv,weight),Np=as.numeric(.N)),rcunique][,ppm2:=pv/m2][]#*#
  x3 <- melt(x1,measure.vars=c('err','Ne'))#*#
  x4 <- melt(x2,measure.vars=c('Np','m2','pv','ppm2'))#*#
  all.equal(x3[,sort(unique(rcunique))],x4[,sort(unique(rcunique))]) &#*#
    all.equal(x3[,sort(unique(rcunique))],pxcinbrd[,sort(unique(rcunique))])&#*#
    all.equal(pxcicend[,sort(unique(rcunique))],x3[,sort(unique(rcunique))])&#*#
    pxcicend[x3[variable=='Ne'],on=c(rcunique='rcunique')][,all(nerr<=value)]#*#
}#*#

#' @export
pxcinouaFun <-
  function(#*#
    yrs=2^(0:4)
    ,#*#
    nn=c('pxcinoued','pxlrsetdad')
    ) {#*#
  # - [ ] aggregate err on key of rcunique, yrs
  getgd(nn)
  x0 <- pxcinoued[weight>0,.(startdate=startdate,err,zo,rcunique,days)] #days is segment length, weight is 0 for outliers
  da1 <- as.Date(pxlrsetdad[length(pxlrsetdad)-yrs*4])
  ll <- list(NULL)
  for(i in seq_along(yrs)) ll[[i]] <- x0[da1[i]-365<startdate][,yrs:=yrs[i]][,.(err=mean(err)),'rcunique,zo,yrs']
  x1 <- rbindlist(ll)
  pxcinouad <<- x1
  putt(pxcinouad)
}#*#

#' @export
pxcivoFun <-
  function(#*#
    nn=c('pxcicpxd','pxcicend','pxciard','pxosrdo1d')
    ,#*#
      uproj1 = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'
    ,#*#
      uproj2 = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'  #uproj2 = "+proj=longlat +datum=WGS84"
    ,#*#
      eps=getpv('civo','epsvo')
) {#*#
  # - [ ] voronoi
  getgd(nn)
  x3 <- civo(cicpxd=pxcicpxd,cicend=pxcicend,uproj1=uproj1,uproj2=uproj2,eps=eps)
  x4 <- raster::intersect(x3,pxosrdo1d$a[match(pxciard,regpcode(pxosrdo1d$a@data[,1])),])
  x4@data <- x4@data[,setdiff(names(x4@data),c('EE','NN')),with=F]
  pxcivod <<- x4
  putt(pxcivod)
}#*#

#' @export
pxciagFun <-
  function(#*#
    nn=c('pxcinoupd','pxcinoued','pxcicntnd')
    ,#*#
    startyear=1995
    ) {#*#
  # - [ ] aggregate abnormal return and ppm2
  getgd(nn)
  x1 <- ciag(cinoupd=pxcinoupd,cinoued=pxcinoued,startyear=startyear)
  pxciagd <<- x1[pxcicntnd,on=c(rcunique='rcunique')]
  putt(pxciagd)
}#*#

#' @export
pxcijoFun <-
  function(#*#
    nn=c('pxciagd','pxcivod')
    ) {#*#
  # - [ ] join aggregate to spdf
  getgd(nn)
  pxcijod <<- cijo(ciagd=pxciagd,civod=pxcivod)
  putt(pxcijod)
}#*#

#decode step
#' @export
pxcidecnFun <-
  function(#*#
    typex=c('D','S','T','F')
    ,#*#
    nn='pxcicntnd'
    ) {#*#
  # - [ ] decodes count(brk,typex) - should not be a step
  getgd(nn)
  x1 <- cidecn(typex=typex,cicntnd=pxcicntnd)
  pxcidecnd <<- cbind(pxcicntnd[,.(rcunique=rcunique)],x1)
  #browser()
  putt(pxcidecnd)
}#*#

#encode step
#' @export
pxcicntnFun <-
  function(#*#
    nn='pxcinoupd'
    ,#*#
    brkx=brk()
    ,#*#
    countthresh=c(0,3)
    ,#*#
    typex=c('F','D','S','T')
    ) {#*#
  # - [ ] encodes count(brk,typex)
  getgd(nn)
  pxcicntnd <<- cicntn(pxcinoupd,brkx,countthresh,typex)
  putt(pxcicntnd)
}#*#
#this is not exported to avoid autorun
pxcicntnchk <- #*#
  function(#*#
    nn=c('pxcinoupd','pxcicntnd','pxcidecnd','pxciagd')#*#
    ,#*#
    jx=NULL#*#
    ) {#*# #incomplete, just check on random sample that something is in the bin when flagged
  getgd(nn)#*#
  stopifnot(identical(pxcicntnd[,sort(unique(rcunique))],pxciagd[,sort(unique(rcunique))]))#*#
  if(is.null(jx)) {#*#
    jx <- pxcinoupd[sample(1:nrow(pxcinoupd),10),rcunique]#*#
  }#*#
  jxx <- pxcinoupd[,match(jx,rcunique)]#*#
  for(j in jxx) {#*#
    x1 <- pxcinoupd[j,rcunique]#*#
    x2 <- setkeyv(copy(pxcinoupd)[type!='O'],c('rcunique','rcneigh'))[list(x1,x1)]#*#
    x3 <- setkey(pxcidecnd,rcunique)[x1]#*# #note it is checking the complete round trip code, decode
    x4 <- x3[,-1,with=F]#*#
    i <- which(as.numeric(unlist(x4))>0)#*#
    brkx <- brk()*1000#*#
    x5<-NULL#*#
    for(ii in seq_along(i)) {#*#
      x5[ii] <- sum(brkx[i[ii]]<x2[,pv]&x2[,pv]<brkx[i[ii]+1])#*#
    }#*#
    stopifnot(all(x5>=x4[,i,with=F]))#*# #x4 is a lower bound
    x6 <- cbind(c('actual','LB.stored'),rbind(as.data.table(as.list(x5)),x4[,i,with=F],use.names=F))#*# #LB meaning 'lower bound' because on store bits for '>1' and '>3'
    stopifnot(all(x6[1,-1,with=F]>=x6[2,-1,with=F]))#*#
    print(x6)#*#
    print(paste0(x1,' passed'))#*#
  }#*#
  TRUE#*#
}#*#


