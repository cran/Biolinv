#' Runs the EM algorithm.
#'
#' Computes user-defined one dimensional dispersal kernels.
#'
#' @param dataset the data frame to be analised (WGS84, colums order should be "year","lat","long","origin")
#' @param cval number of numeric vector of values of C to be tested for.
#' @param randompoints data frame of 'y' and 'x' coordinates of random points (projected coordinate system)
#'
#' @return dataset argument with two additional colums. 'dist': distance from nearest point of natural origin or nearest anchor point (see Details); 'Pnat': probability of being of natural origin.
#'
#' @export
#'
#' @examples
#' data('nzp')
#' data('frogs')
#' randp<- RPG(rpopn=1000, boundary=nzp, SP= 'random_frog')
#' \dontrun{
#' frogsEM<- EM(dataset= frogs, randompoints= randp)
#'  }


EM<-function(dataset, cval=2, randompoints){

  dati<-dataset
  realp<-nrow(subset(dati,dati$Pnat==1))/nrow(dati)

  input.dist<-distances(dati)
  output.weights<-distance.by.year(input.dist)
  Kerns<-random.distances(dati,randompoints)
  update<-EM.max(output.weights[[1]], output.weights[[2]],6,.5,2,Kerns)
  update.anchor(output.weights[[1]], input.dist,dati, threshold=update[[2]]*sqrt(2))
  bestsols<-selectC(output.weights[[1]], update, cval, Kerns)
  tempOut<- (list(bestsols,output.weights,realp))

  tempOut2<- (OS(Output= tempOut)) #(dist, Pnat)
  dataset$Pnat<- unlist(tempOut2[[2]])
  dataset$Dist<- unlist(tempOut2[[1]])

  return(dataset)
}
