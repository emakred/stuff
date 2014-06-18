# path_to_Dropbox <- '/Users/EMACrowdpac/Dropbox (Crowdpac Data Sci)/'

source_data <- paste(path_to_Dropbox,'contributions/data/cand_all_1979_2014.rda', sep='')
# source_data <- paste(path_to_Dropbox,'contributions/finished_runs/cands_79_14_current.rda', sep='')

get.current.mcs <- function(){
  # calibrating the range of cf-scores between given quantile for a given subset 
  # of candidates: those that served in congress between 2004 and 2014. The rational 
  # for this period rather than current members of the 113th Congress is that the 
  # scores are identified based on a distributional assumpation. Given Republican 
  # majority control of the 113th, the median and quantiles skew to the
  # right. The 2004 to 2014 period gives a more balanced partisan distribution.
  print(load(source_data))
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


generate.rescaling.params <- function() {
  # calculating boundaries for 0.005 - 0.99 quantile cf-scores
  c.cands <- get.current.mcs()
  # pick column 'oc1' for the *un*scaled cf-score
  z <- as.numeric(as.character(c.cands[,'oc1']))
  # cut-offs for raw cf-scores  (na.rm=T to ignore NA's) 
  qq1 <- quantile(z,pr=c(.005,.995),na.rm=T)
  return(qq1)
}



rescale.cf <- function(z,qq1){
    z[z > qq1[2]] <- qq1[2]
    z[z < qq1[1]] <- qq1[1]
    z <- z - mean(qq1)
    z <- ((z / (max(abs(qq1-mean(qq1)),na.rm=T)))) * 10
    return(z)
}


convert.districts <- function(locations) {
  ##### check district, year format problems
  for (x in 1:length(locations)) {
    v <- locations[x]
    if (!is.na(v)) {
      if (grepl('^[0-9]+$',v)) {
        v <- paste(cc[x,'State'],v,sep='')
      } else if (grepl('^[A-Z|a-z]{2}[-]?[0-9]',v)) {   
        v <- gsub('-','',v)                               
      } else if (grepl('^[A-Z|a-z]{3}[-]?[0-9]',v)) {
        v <- gsub('-','',v)
        v <- paste(substr(v,1,2),substr(v,4,nchar(v)),sep='')
      }
    } else {
      v <- ''
    }
    locations[x] <- v
  }
  return(locations)
}



get.cands <- function(year,state,path_to_Dropbox) {

  print(load(source_data))
  cand_list <- cands[cands[,'fecyear']==as.character(year)  
                     # & grepl('federal:house|federal:senate',cands[,'seat']) 
                     & cands[,'State']==state 
                     # & cands[,'Party'] %in% c(100,200)
                     ,]
  cand_list <- cand_list[grepl('cand',cand_list[,'bonica_rid']),]
  cands <- cand_list

  # have to order these because in the next step (the match), only the first match is taken
  cands[,'election'] <- substr(cands[,'election'],nchar(cands[,'election']) -3, nchar(cands[,'election']))
  cands <- cands[rev(order(cands[,'election'])),]

  cc2 <- cands

  # want to keep only the most recent record for each candidate, so:
  # since the data is already sorted, just remove all but first occurrence of each bonica_rid
  cc2 <- cc2[!duplicated(cc2[,'bonica_rid']),]

  party.label <- ifelse(cc2[,'Party'] == 100,'DEM',
                        ifelse(cc2[,'Party'] == 200,'REP','IND'))#,
                              # ifelse(cc[,'third.party'] == 'LIB','LIB',
                              #        ifelse(cc[,'third.party'] == 'GRN','LIB','IND'))))
  cc2 <- cbind(cc2,cfscore=as.numeric(cc2[,'oc1']),
              party.label=party.label)

  qq1 <- generate.rescaling.params()
  z <- rescale.cf(as.numeric(cc2[,'oc1']),qq1)

  cc2[,'cfscore'] <- z
  
  # zero out district where race isn't district-based
  cc2[cc2[,'seat']=='federal:senate' ,'District'] <- ''
  locations <- as.vector(cc2[,'District'])
  # locations <- convert.districts(locations)

  ccnew <- cbind(cc2[,c('election','bonica_rid','Name','State')],District=locations,cc2[,c('Incum_Chall','Party','party.label','seat','candStatus','fecyear','Cand.ID')],cp.score=cc2[,'cfscore']) 

  cand.master <- ccnew[!is.na(ccnew[,'bonica_rid']),]
  
  return(cand.master)
}


