candidate_list <- read.csv('/scratch1/Dropbox/crowdpac/states/2014/CA/temp_CA.csv',as.is=T)

# print(load('/scratch1/Dropbox/contributions/data/cand_all_1979_2014.rda'))

# print(load('/scratch1/Dropbox/contributions/finished_runs/cands_79_14_current.rda'))
# m <- match(cands[,'bonica_rid'],candidate_list[,'bonica_rid'])
# candidate_list[na.omit(m),'FEC.ID'] <- cands[which(!is.na(m)),'FEC.ID']


print(load('/scratch1/Dropbox/contributions/finished_runs/aresults_79_14_current.rda'))
m <- match(aresults$cands[,'bonica_rid'],candidate_list[,'bonica_rid'])
candidate_list[na.omit(m),'FEC.ID'] <- aresults$cands[which(!is.na(m)),'FEC.ID']

write.csv(candidate_list,file='/scratch1/Dropbox/crowdpac/states/2014/CA/temp_CA_out.csv',row.names=FALSE) 
  