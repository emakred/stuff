
# path to the Dropbox directory
path_to_Dropbox <- '/Users/EMACrowdpac/Dropbox (Crowdpac Data Sci)/'

# csv file with just bonica_rid's
id.file <- '/Users/EMACrowdpac/Desktop/rids.csv'

if(!exists('cands')){
	# path to the datafile
	source_data <- paste(path_to_Dropbox,'contributions/finished_runs/cands_79_14_current.rda', sep='')
	# loads the variable 'cands'
	print(load(source_data))
}


# function to find a subset of incumbents who are used in 'generate.rescaling.params' 
# to find the rescaling factors for CP scores
get.current.mcs <- function(cands){
  # calibrating the range of cf-scores between given quantile for a given subset 
  # of candidates: those that served in congress between 2004 and 2014. The rational 
  # for this period rather than current members of the 113th Congress is that the 
  # scores are identified based on a distributional assumpation. Given Republican 
  # majority control of the 113th, the median and quantiles skew to the
  # right. The 2004 to 2014 period gives a more balanced partisan distribution.
  current.mcs <- ((cands[,'election'] %in% seq(2004,2014,2)) &
                  (!is.na(as.numeric(cands[,'ICPSR2']))) &
                  # grepl('fed',cands[,'seat']) &
                  grepl('federal:senate|federal:house',cands[,'seat']) &
                  cands[,'Party'] %in% c(100,328,200))
  current.mcs[is.na(current.mcs)] <- FALSE
  # pick off the rows for candidates in the above defined subset
  c.cands <- cands[current.mcs,]
  c.cands <- c.cands[!duplicated(c.cands[,'bonica_rid']),]
  return(c.cands)
}

# finds the conversion factors
generate.rescaling.params <- function(cands) {
  # calculating boundaries for 0.005 - 0.99 quantile cf-scores
  c.cands <- get.current.mcs(cands)
  # pick column 'oc1' for the *un*scaled cf-score
  z <- as.numeric(as.character(c.cands[,'oc1']))
  # cut-offs for raw cf-scores  (na.rm=T to ignore NA's) 
  qq1 <- quantile(z,pr=c(.005,.995),na.rm=T)
  # save p(0.005) and p(0.99) for cand[,'oc1']
  to_file <- paste(path_to_Dropbox,'crowdpac/data/rescaling_params.rda',sep='')
  save(qq1,file=to_file)
  return(qq1)
}

# executes the rescaling based on the population selected in get.current.mcs 
# and the facotrs found in generate.rescaling.params
rescale.cf <- function(z,qq1){
    z[z > qq1[2]] <- qq1[2]
    z[z < qq1[1]] <- qq1[1]
    z <- z - mean(qq1)
    z <- ((z / (max(abs(qq1-mean(qq1)),na.rm=T)))) * 10
    return(z)
}



# load in the file of just bonica_rids whose scores are needed

temp<-read.csv(id.file,as.is=T)

# find the unscaled version of the scores
foo<-cbind(temp$id,cands[match(temp$id,cands[,'bonica_rid']),'oc1'])

# rescale the scores 
# variables to save in csv:  [1] bonica_id2, [2] cp.score && [3] cf.score
idscores <- cbind(foo[,1],rescale.cf(as.numeric(foo[,2]),generate.rescaling.params(cands)),foo[,2])

# save a file called idscores.csv located in Dropbox/crowdpac/output/idscores.csv
write.to.file <- paste(path_to_Dropbox,'crowdpac/output/ridscores.csv',sep='')
write.csv(idscores,write.to.file,row.names=F)



