SimulateCellCountsTrueDiff <- function(props,nsamp,size=20,depth,a,b.grp1,b.grp2){

  # Generate total cell counts for each sample
  numcells <- rnbinom(nsamp,size=size,mu=depth)

  # Generate sample proportions for each cell type
  true.p <- matrix(NA, nrow=length(props), ncol=nsamp)
  for(k in 1:length(props)){
    true.p[k,1:(nsamp/2)] <- rbeta(nsamp/2,a[k],b.grp1[k])
    true.p[k,((nsamp/2)+1):nsamp] <- rbeta(nsamp/2,a[k],b.grp2[k])
  }

  # Generate counts for nsamp samples
  counts <- matrix(NA,ncol=nsamp, nrow=nrow(true.p))
  rownames(counts) <- paste("c",0:(nrow(true.p)-1), sep="")
  for(i in 1:length(props)){
    counts[i,] <- rbinom(nsamp, size=numcells, prob=true.p[i,])

  }
  counts
}
