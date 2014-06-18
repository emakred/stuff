###############################################################################
##CANDIDATE MASTER 
###############################################################################
if (!exists('cands')) {
  # print(load('/scratch1/Dropbox/contributions/finished_runs/cands_79_14_current.rda'))
  print(load('/scratch1/Dropbox/contributions/finished_runs/aresults_79_14_current.rda'))
  cands <- aresults$cands

  # ------------ ADAM's addition start ----------------------
  ##loading in roll call estimates for candidates that have voting records and merging with the
  ##CFscores. This is a one time hack around. This combining across scores will usually be performed
  ##in prelim_contribs.R
  print(load('/scratch1/Dropbox/crowdpac/data/cp_rc_out.rda'))
  rc.ip <- senout$leginfo$static.idealPoints
  m <- match(as.numeric(as.character(cands[,'ICPSR2'])),as.numeric(rownames(rc.ip)))
  dw <- rc.ip[m,1]

  cf <- as.numeric(cands[,'oc1'])

  ##combining scales with principal components analysis. 
  oo <- cbind(cf,dw)
  rownames(oo) <- cands[,'bonica_rid']
  oo <- oo[complete.cases(oo),]
  pr.out <- predict(prcomp(oo,scale=TRUE))
  cor(data.frame(oo,pr.out[,1]))
  plot(data.frame(oo,pr.out[,1]))
  pred.out <- pr.out[,1]
  library(leiv)
  leiv.out <- leiv(oo[,1]~pred.out,abs.tol = 1e-10)
  pred.out1 <- attributes(leiv.out)$intercept + attributes(leiv.out)$slope * pred.out
  m <- match(cands[,'bonica_rid'],rownames(oo))
  pred.out2 <- pred.out1[m]

  cands[!is.na(pred.out2),'oc1'] <- pred.out2[!is.na(pred.out2)]
  save(cands,file='/scratch1/Dropbox/contributions/finished_runs/cands_79_14_current.rda')
  # ------------ ADAM's addition end ----------------------
}


rescale.cf <- function(z){
    z[z > qq1[2]] <- qq1[2]
    z[z < qq1[1]] <- qq1[1]
    z <- z - mean(qq1)
    z <- ((z / (max(abs(qq1-mean(qq1)),na.rm=T)))) * 10
    return(z)
}

# initialize data frame loaded 
# add in another column to capture friendly seat name for UI
cands <- cbind(cands,friendly_seat=character(length(cands[,'election'])))


cands[,'election'] <- substr(cands[,'election'],nchar(cands[,'election']) -3, nchar(cands[,'election']))
cands <- cands[rev(order(cands[,'election'])),]

# hardcoded listing of candidates
fant.cands <- c('cand1572',
              'cand1610',
              'cand23468',
              'cand303',
              'cand3331',
              'cand3333',
              'cand3334',
              'cand3335',
              'cand98927',
              'cand99123',
              'cand99764') #    
cc2 <- cands[match(fant.cands,cands[,'bonica_rid']),]

# want to keep only the most recent record for each candidate, so:
# - sort in reverse chronological order  <-- NEED THIS STILL?
# - then remove any remaining records don't have election =2014  
# - then remove all but first occurrence of each bonica_rid
cc2 <- cc2[rev(order(cc2[,'election'])),]
cc2 <- cc2[!duplicated(cc2[,'bonica_rid']),]


cc2 <- cbind(cc2,cfscore=as.numeric(cc2[,'oc1']),##num.donors=num.donors,##as.numeric(cands[,'num.givers']),
            party.label=party.label)


load('/scratch1/Dropbox/crowdpac/data/rescaling_params.rda' )
z <- rescale.cf(as.numeric(cc2[,'oc1']))

cc2[,'cfscore'] <- z
write.csv(cc2, file='/scratch1/Dropbox/crowdpac/output/fantasy_candidates.csv')

