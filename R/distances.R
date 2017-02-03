#' Hidden functions for EM algorithm.
#'
#' These functions are used by the EM() function and were contained in the file 'distances.R'.
#'
#' @import fields
#'
#' @keywords internal
#'
#' @export

distances<-function(dati){ #dati should have (at least) 3 columns; column 2 should be lattitude and 3 longitude; one column should be called "year"
  years<- unique(sort(dati$year))
  distance.pastYrs<-rep(NA, dim(dati)[[1]])
  distance.currentYr<-rep(NA, dim(dati)[[1]])
  for( i in 2:length(years)){
    if(sum(dati$year==years[i])>0 & sum(dati$year< years[i])>0){
      past <- subset(dati, dati$year<years[i])
      pres<-  subset(dati, dati$year==years[i])
      dist <- 0.001*rdist(pres[,3:2],past[,3:2])
      dist.min<-apply(dist,1,min)
      distance.pastYrs[dati$year==years[i]]<-dist.min
      if(sum(dati$year==years[i])>1){
        dist<-0.001*rdist(pres[,3:2], pres[,3:2])
        diag(dist)<-NA
        dist.min<-apply(dist, 1, min, na.rm=T)
        distance.currentYr[dati$year==years[i]]<-dist.min}
    }}
  return(data.frame(distance.pastYrs, distance.currentYr, year=dati$year))
}

distance.by.year<-function( obs.dist) {  #obs.dist should be the output of distances
  distances<-apply(obs.dist[,1:2],1,min, na.rm=T)
  output<-split(distances,obs.dist$year)
  past.dist<-split(obs.dist$distance.pastYrs, obs.dist$year)
  for(i in 2:length(output)){
    if(sum(past.dist[[i]]==output[[i]])==0){
      index=which.min(past.dist[[i]])
      output[[i]][index]<-past.dist[[i]][index]
    }
  }
  weights<-list()
  for(i in 1:length(output)){
    weights[[i]]<-rep(0.5, length(output[[i]]))}
  output[[1]]<-rep(NA, length(output[[1]]))
  return(list(output,weights))
}

random.distances<-function(dati, rdm10000){
  min.dist <- list()
  kern<-list()
  years<-unique(sort(dati$year))
  for (i in 1:length(years)){
    pres <- subset(dati,dati$year< years[i]+1)
    dist <- 0.001*rdist(rdm10000[,3:2],pres[,3:2])
    out.min <- apply(dist,1,min)
    min.dist[[i]]<-out.min
    kern[[i]]<-density(min.dist[[i]],from=0.001, to=max(min.dist[[i]])+5, n=2^13)
    bar.width<-kern[[i]]$x[2]-kern[[i]]$x[1]
    nc<-sum(kern[[i]]$y*bar.width)
    kern[[i]]$y<-kern[[i]]$y/nc
  }

  return(kern)
}

g.x<-function(x,Kern){    #Kern should be the kernel pertaining to just that year.

  a<-rep(NA, length(x))
  for(i in 1:length(x)){
    a[i]<-which.min(abs(Kern$x-x[i]))}
  b<-Kern$y[a]
  return(b)
}

ddispersq<-function(x,alpha,c){

  answer<-c/(alpha*gamma(1/c))*exp(-1*(x/alpha)^c)
  answer[x<=0]<-0
  return(answer);
}

f2.x<-function(x, alpha, year.index,c, xrange){  #xrange should be the xvalues used in the kernel for that year
  Kern<-ddispersq(xrange, alpha,c)
  bar.width<-xrange[2]-xrange[1]
  nc<-sum(Kern*bar.width)
  Kern<-Kern/nc
  a<-rep(NA, length(x))
  for( i in 1:length(x)){
    a[i]<-which.min(abs(xrange-x[i]))}
  b<-Kern[a]
  return(b)
}

EM.update<-function(output,Weights,Sigma2,Pi,c, Kerns){
  new.weights<-Weights
  new.sigma2<-0
  n<-sapply(output,length)
  for(i in 2:length(n)){
    if(n[i]>0){
      for(j in 1:n[i]){
        new.weights[[i]][j]<-Pi*f2.x(output[[i]][j],Sigma2,i,c, Kerns[[i]]$x)/(Pi*f2.x(output[[i]][j],Sigma2,i,c,Kerns[[i]]$x)+(1-Pi)*g.x(output[[i]][j],Kerns[[i]]))
      }
      print(c(n[i],length(new.weights[[i]]), length(output[[i]])))
      new.sigma2<-new.sigma2+sum(new.weights[[i]]*output[[i]]^c)
    }}
  new.sigma2<-c*new.sigma2/sum(sapply(new.weights,sum))
  new.sigma2<-new.sigma2^(1/c)

  new.pi<-sum(sapply(new.weights, sum))/sum(n)
  return(list(new.weights, new.sigma2, new.pi))
}

EM.max<-function(output, Weights, Sigma2, Pi, c, Kerns){
  EM.update(output, Weights,Sigma2,Pi,c,Kerns)->updateA
  EM.update(output, updateA[[1]], updateA[[2]],updateA[[3]],c,Kerns)->updateB

  while(abs(updateA[[3]]-updateB[[3]]) >0.00001){
    updateA=updateB
    updateB=EM.update(output, updateA[[1]], updateA[[2]], updateA[[3]],c, Kerns)
    print(c(updateB[[3]], updateB[[2]]))
  }
  return(updateB)
}

update.anchor<-function(output, Distances,dati, threshold){

  past.dist<-split(Distances$distance.pastYrs,Distances$year)
  #print(length(past.dist))
  anchor.ind<-list()
  nyears<-length(output)
  for( i in 1:nyears){
    anchor.ind[[i]]<- past.dist[[i]]==output[[i]]
  }
  distance3<-output
  anchor.dist<-vector(length=nyears, mode="list")

  year<-sort(unique(Distances$year))
  for(i in 2:nyears){ #second year does not have link back to first
    if(sum(anchor.ind[[i]]) < length(anchor.ind[[i]]) & length(anchor.ind[[i]])>0 ){
      a<-dati[Distances$year==year[i],2:3]
      a.false<-a[!anchor.ind[[i]],]
      a.true<-a[anchor.ind[[i]],]
      #    print("a.false")
      #    print(a.false)

      #print("a.true")
      # print(a.true)
      anchor.dist[[i]]<- 0.001*rdist(a.false,a.true)

      mindist<-apply(anchor.dist[[i]],1,min)
      #print(mindist)
      #print(c(year[i], sum(mindist>threshold), sum(anchor.ind[[i]])))
      while(sum(mindist>threshold) >0 & sum(anchor.ind[[i]]) < length(anchor.ind[[i]])){

        index<-which.min(past.dist[[i]][!anchor.ind[[i]]][mindist >threshold])
        #print("new anchor point")

        distance3[[i]][!anchor.ind[[i]]][mindist>threshold][index]<-past.dist[[i]][!anchor.ind[[i]]][mindist>threshold][index]
        anchor.ind[[i]][!anchor.ind[[i]]][mindist>threshold][index]<-TRUE

        if(sum(anchor.ind[[i]]) < length(anchor.ind[[i]])){
          a.false<-a[!anchor.ind[[i]],]
          a.true<-a[anchor.ind[[i]],]

          anchor.dist[[i]]<- 0.001*rdist(a.false,a.true)

          mindist<-apply(anchor.dist[[i]],1,min)
          print(mindist)
        }

      }}}

  return(distance3)

}

loglikelihood<-function(output, Pi, Sigma2,c, Kerns){
  nyears=length(output)
  n<-sapply(output, length)
  answer<-0
  for(i in 2:nyears){
    for(j in 1:n[i]){answer<-answer+log((Pi*f2.x(output[[i]][j],Sigma2,i,c,Kerns[[i]]$x)+(1-Pi)*g.x(output[[i]][j],Kerns[[i]])))
    if(is.na(log(Pi*f2.x(output[[i]][j],Sigma2,i,c,Kerns[[i]]$x)+(1-Pi)*g.x(output[[i]][j],Kerns[[i]])) )){
      print(c(output[[i]][j], f2.x(output[[i]][j],Sigma2,i,c,Kerns[[i]]$x),g.x(output[[i]][j],Kerns[[i]]) ))}


    }


  }
  return(answer)
}

selectC<-function(output, start.vals, crange, Kerns){  #crange should be a vector of the values for c you want to try
  loglikelihoods<-rep(NA, length(crange))
  maxsol<-list()
  for(i in 1:length(crange)){
    maxsol[[i]]<-EM.max(output, start.vals[[1]], start.vals[[2]], start.vals[[3]], crange[i], Kerns)
    loglikelihoods[i]<-loglikelihood(output,maxsol[[i]][[3]], maxsol[[i]][[2]], crange[i], Kerns)
  }

  return(list(maxsol, loglikelihoods))
}


OS<-function(Output,ctest){
  # -> Output Sequencer. Summarizes the output of EM().

  lista<-list()
  lista[[1]]<-unlist(Output[[2]][[1]])    #dist
  lista[[2]]<-Output[[1]][[1]][[which.max(unlist(Output[[1]][[2]]))]][[1]]    #individual prob. on natural dispersal (list of 1 element per year)

  #  lista[[3]]<-Output[[1]][[1]][[which.max(unlist(Output[[1]][[2]]))]][[2]]    #alpha
  #  lista[[4]]<-ctest[[which.max(unlist(Output[[1]][[2]]))]]    #c
  #  lista[[5]]<-Output[[1]][[1]][[which.max(unlist(Output[[1]][[2]]))]][[3]]    #Pi
  #  lista[[6]]<-Output[[3]]    #real Pi value
  #  lista[[7]]<-unlist(Output[[1]][[2]])    #log-likelihoods

  names(lista)<- c('dist','Pnat')
  return(lista)
}
