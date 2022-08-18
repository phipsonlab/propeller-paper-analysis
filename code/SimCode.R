SimulateCellCounts <- function(props,nsamp,size=20,depth,a,b){

    # Generate total cell counts for each sample
    numcells <- rnbinom(nsamp,size=size,mu=depth)

    # Generate sample proportions for each cell type
    true.p <- matrix(NA, nrow=length(props), ncol=nsamp)
    if(length(a)==1){
      for(k in 1:length(props)) true.p[k,] <- rbeta(nsamp,a,b[k])
    }
    else{
      for(k in 1:length(props)) true.p[k,] <- rbeta(nsamp,a[k],b[k])
    }

    # Generate counts for nsamp samples
    counts <- matrix(NA,ncol=nsamp, nrow=nrow(true.p))
    rownames(counts) <- paste("c",0:(nrow(true.p)-1), sep="")
    for(i in 1:length(props)){
        counts[i,] <- rbinom(nsamp, size=numcells, prob=true.p[i,])

    }
    counts
}
