rm(list=ls(all=TRUE))
gc()
library(foreign)
library(MASS)
# ADAM commented out next line
##library(ggplot2);
library(Matrix)
library(foreach)
setwd('~/workspace/contributions')

source('src/utils.r')
reload()

library(RMySQL)

con <- dbConnect(MySQL(), user="crowdpacdata", password="crowdpacdata2014", dbname='cfdb',
                 host='cp-data-analysis3.cbtto5axwp1k.us-east-1.rds.amazonaws.com', port=5841)

# ERIN modified e.index, evect.all, evect to add
# 113 (2013), 113 (2014)
e.index <- c(96,96,97,97,98,98,99,99,100,100,
             101,101,102,102,103,103,104,104,
             105,105,106,106,107,107,108,108,
             109,109,110,110,111,111,112,112,
             113,113)

names(e.index) <- as.character(1979:2014)
evect.all <- c(1979:2014)
evect <- evect.all[(c(1979:2014)-1978)]
evect1 <- substr(evect,3,4)

cctypes <- c(
             '24K',  ##CONTRIBUTION MADE TO NON-AFFILIATED
             '24Z',  ##IN-KIND CONTRIBUTION MADE TO REGISTERED FILER
             '24E',  ##INDEPENDENT EXPENDITURE FOR
             '24C',  ##COORDINATED EXPENDITURE
             '24F'   ##COMMUNICATION COST FOR CANDIDATE (C7)
             )

###############################################################################
##SPECIFYING ARGUMENTS THAT WILL BE PASSED TO prep_scale()
###############################################################################
##DATA ARGUMENTS
ndups <- 1 
ndups.use <- 1
num.cont <-  1
individuals <- TRUE
pacs <- TRUE
include.states <- TRUE
include.527s <- FALSE
cands.only <- FALSE
direct.only <- TRUE
ncores <- 0
exclude.unitem <- FALSE
exclude.gen.elect <- FALSE
out.of.state <- FALSE
convert.to.count <- TRUE
cultural.only <- TRUE

##OTHER ARGUMENTS
ps.cont.ids <- FALSE ##period specific contributor ids
remove.candidate.pacs <- FALSE
remove.party.cmtes <- TRUE
remove.presidential.candidates <- FALSE
combine.prim.general <- TRUE
remove.corporate.pacs <- FALSE
match.leadership.pacs <- FALSE
summarize.contrib.sources <- TRUE
use.only.linked.observations <- TRUE
save.progress <- TRUE

##RECIPIENT ARGUMENTS
pacs.as.recipients <- TRUE
include.pacs.as.nrc <- FALSE
ideological.subset <- NULL ##'Abortion'
only.party.cmtes <- FALSE
constrain.cc.ips <- FALSE

##AWM CONSTRAINT
normalize <- TRUE
rank.order <- FALSE

##CHANGE THIS TO A CATCH-ALL VARIABLE OF RESTRICTIONS
subset.cands <- NULL
subset.contribs <- NULL

## OTHERS
# if(individuals | include.states){organize.cmdat <- FALSE}
# split.inc.chall <- organize.cmdat <- ((length(evect) > 2))
organize.cmdat <- FALSE

dynamic.cands <- ((length(evect) > ifelse(combine.prim.general,2,1)))

if(include.states){
  state.subset <- c("DC", "AK", "AL", "AR", "AZ", "CA", "CO",
                    "CT", "DE", "FL", "GA","HI", "IA",
                    "ID", "IL", "IN", "KS", "KY", "LA",
                    "MA", "MD", "ME", "MI", "MN", "MO",
                    "MS", "MT", "NC", "ND", "NE", "NH",
                    "NJ", "NM", "NV", "NY", "OH", "OK",
                    "OR", "PA", "RI", "SC", "SD", "TN",
                    "TX", "UT", "VA", "VT", "WA", "WI",
                    "WV", "WY")
  # split.inc.chall <- FALSE
  dynamic.cands <- TRUE
  combine.prim.general <- TRUE
  organize.cmdat <- FALSE
}else{
  state.subset=NULL
}

split.inc.chall <- FALSE

##COMBINING ARGUMENTS INTO A LIST THAT WILL BE PASSED TO prep_scaling()
awm.options <- get.awm.list()
attach(awm.options)



###############################################################################
##CALLING prep.scaling()x
###############################################################################
load('~/workspace/contributions/data/cand_all_1979_2014.rda')
load('~/workspace/contributions/data/cmdat_all_1979_2014.rda')

if(!include.states){
  cands <- cands[cands[,'election'] %in% evect,]
  print(table(cands[,'election']))
}

# ADAM commented out next 14 lines
## isector <- cands[,'Sector.Long']
## isector[is.na(isector)] <- ''
## use.sector <- (is.na(isector) | (isector %in% c('Joint Candidate Cmtes','Human Rights',
##                                                 'Ideological/Single-Issue',
##                                                 'Party Cmtes','Unknown')))
## cands <- cands[(!(cands[,'igcat'] %in% c('C','L','T','V')) |  use.sector) |
##                cands[,'cand_or_committee'] =='CAND' |
##                cands[,'seat'] %in% c('federal:house','federal:senate',
##                                      'federal:president','federal:527',
##                                      'state:governor','state:committee',
##                                      'state:judicial','state:judicial:lower',
##                                      'state:lower','state:office',
##                                      'state:office:da','state:office:sheriff',
##                                      'state:upper',''),]

# ADAM commented out next line
##cands <- cands[!grep(("emily's list",cands[,'Name'],ignore.case=T),]
cands <- cands[!grepl('actblue',cands[,'Name'],ignore.case=T),]
cands <- cands[!grepl('america coming',cands[,'Name'],ignore.case=T),]
cands <- cands[!grepl('america coming',cands[,'Name'],ignore.case=T),]
cands <- cands[!(grepl('moveon',cands[,'Name'],ignore.case=T) &
                 cands[,'seat'] == 'federal:committee'),]

if(split.inc.chall & length(evect) > 2){
  cands <- get.enter.exit(cands,e.index,evect)
  cands <- split.ic(cands,e.index,evect)
  save(cands,file='~/workspace/tmp/cands_split.rda')
  load('~/workspace/tmp/cands_split.rda')
}

if(cands.only){
  cands <- cands[cands[,'cand_or_committee'] =='CAND',]
}

setwd('~/workspace/contributions/')
source('src/utils.r')
source('analysis/polarization/polarization_utils.r')
source('/workspace/contributions/analysis/db_scripts.R')
reload()


get.name.org <- function(orgname,rel.vars){

    rval <- NULL
    for( x in orgname){
        org <- dbGetQuery(con,
                          paste("select ",rel.vars," from newconts where",
                                ' match(organization_name,parent_organization_name) against (',
                                "+'" , '"',x,'"' ,"'", "in boolean mode)",
                                sep=''))
        org2 <- dbGetQuery(con,
                           paste("select ",rel.vars," from newconts where",
                                 ' match(contributor_occupation, contributor_employer) against (',
                                 "+'" , '"',x,'"' ,"'", "in boolean mode)",
                                 sep=''))

        org3 <- dbGetQuery(con,
                           paste("select ",rel.vars," from newconts where",
                                 ' match(contributor_name) against (',
                                 "+'" , '"',x,'"' ,"'", "in boolean mode)",
                                 sep=''))
        rval <- c(rval,org,org2,org3)
        # ADAM commented out next line
        ##if(x == 'conservation'){browser()}
    }
    return(rval)
}


env.pacs <- read.csv('~/workspace/contributions/data/issue_specific/environment.csv',as.is=T)
prooilpacs <- env.pacs[env.pacs[,'Issue'] == 'Oil & Gas',]
proenvpacs <- env.pacs[env.pacs[,'Issue'] == 'Environment',]

# ADAM commented out next 5 lines
## rel.vars <- paste("cycle,transaction_id,bonica_id2,contributor_name,",
##                   " contributor_type, amount, recipient_name,",
##                   " seat, contributor_employer, contributor_occupation, date,",
##                   " organization_name, parent_organization_name, recipient_type,",
##                   " recipient_ext_id, recipient_party",sep='')
rel.vars <- 'bonica_id2'
rel.vars1 <- paste('bonica_id2, ','contributor_name, ','contributor_occupation, ',
              'contributor_employer, ','organization_name, ',
              'contributor_category',sep='')

###############################################################################
##PRO-ENVIRONMENT
###############################################################################

cme <- cmdat[grepl('JE',cmdat$indcode),]
plp <- unique(c(as.character(cme[,'comname']),
                as.character(cme[,'conorgname']),
                as.character(cme[,'name_short']),
                as.character(proenvpacs[,'Name']),
                as.character(proenvpacs[,'affiliate']),
                as.character(proenvpacs[,'ultorg'])))
plp <- plp[nchar(plp) > 3]
plp <- plp[!grepl('none',plp,ignore.case=T)]
plp <- plp[!grepl('blank',plp,ignore.case=T)]
plp <- plp[plp != 'SERVICES']
plp <- plp[!(plp == 'FUND')]
plp <- plp[plp != 'FEDERAL FUND']
plp <- gsub('"','',plp)
plp <- gsub("'",'',plp)
plp <- gsub("\\.",'',plp)
plp <- gsub("\\-",' ',plp)
plp <- gsub("\\/",' ',plp)
plp <- gsub('\\(','',plp)
plp <- gsub('\\)','',plp)
opc <- unique(c('friends of nature',
                'friends of earth',
                'global witness',
                'world wildlife','wwf',
                'treepeople','sierra club',
                'environmentalist','green peace',
                'environmental protection','environmental advocacy',
                'greenpeace',##'environment','evironmental',
                'clean energy','alternative energy','clean water',
                'clean air','center for biological diversity',
                'audobon','environmental professionals',
                'earth liberation','ela','elf','environmental law',
                'keep america beautiful','wilderness','epa',
                'green technology','green tech','climatologist',
                'climate scientist','environmental protection',
                'rising tide','preserve our land','nature conservancy',
                'sustainable silicon valley','union of concerned scientists',
                'of conservation','lcv','conservationist','earthjustice','environmental defense',
                'tree hugger',plp))


o.proenv <- NULL
for(x in opc){
    o.proenv <- unique(c(o.proenv,unlist(get.name.org(x,rel.vars))))
    print(x)
    # ERIN commented out next line
    # if("4020282587" %in% o.proenv){browser()}
    print(length(o.proenv))
}
opc = unique(o.proenv)

# org3 query took about 4 hours for abortion, started at 4:52pm, may 10
org3 <- dbGetQuery(con,
                   paste("select ",rel.vars1," from newconts where",
                         ' contributor_category = "JE300"',
                         ' or recipient_category = "JE300"',
                         sep=''))

opc <- unique(c(opc,org3[,'bonica_id2']))
save(opc,file='~/workspace/tmp/proenv_bids.rda')


###############################################################################
##OIL AND GAS
###############################################################################
cmc <- cmdat[grepl('E10',cmdat$indcode) |
             grepl('E11',cmdat$indcode) |
             grepl('E12',cmdat$indcode),]
plc <- unique(c(as.character(cmc[,'comname']),
                as.character(cmc[,'conorgname']),
                as.character(cmc[,'name_short']),
                as.character(prooilpacs[,'Name']),
                as.character(prooilpacs[,'affiliate']),
                as.character(prooilpacs[,'ultorg'])))
plc <- plc[nchar(plc) > 3]
plc <- plc[!grepl('none',plc,ignore.case=T)]
plc <- plc[!grepl('blank',plc,ignore.case=T)]
plc <- plc[!grepl('none',plc,ignore.case=T)]
plc <- plc[!grepl('blank',plc,ignore.case=T)]
plc <- plc[plc != 'SERVICES']
plc <- plc[!(plc == 'FUND')]
plc <- plc[!(plc == 'GOVT')]
plc <- plc[!(plc == 'CENTER')]
plc <- plc[plc != 'FEDERAL FUND']
plc <- gsub('"','',plc)
plc <- gsub("'",'',plc)
plc <- gsub("\\.",'',plc)
plc <- gsub("\\-",' ',plc)
plc <- gsub("/",' ',plc)
plc <- gsub('\\(','',plc)
plc <- gsub('\\)','',plc)
opl <- unique(c('oil','gas','coal','petroleum','fuel','gasoline','koch industries',
                'consol energy','peabody energy','headwaters inc','chevrontexaco',
                'royal dutch','bp','conoco','conocophillips','suncor','cloud peak',
                'alpha natural','pacifiCorp','murray energy','booth energy group',
                'BHP Billiton','Cline Group','teco energy','nacco','fuels','black hills',
                'enron','exxon','mobil','texaco','chevron','shell','massey',plc))
o.prooil <- NULL
for(x in opl){
    o.prooil <- unique(c(o.prooil,as.character(unlist(get.name.org(x,rel.vars)))))
    print(c(x,length(o.prooil)))
}
opl = unique(o.prooil)
org4 <- dbGetQuery(con,
                   paste("select ",rel.vars1," from newconts where",
                         ' contributor_category like "E1%"',
                         ' or recipient_category like "E1%"',
                         sep=''))
opl <- unique(c(opl,org4[,'bonica_id2']))
save(opl,file='~/workspace/tmp/prooil_bids.rda')



###############################################################################
##ALL ENERGY -- ERIN: all energy not used in rest of model
###############################################################################
# cma <- cmdat[grepl('E',cmdat$indcode),]
# cca2 <- cands[grepl('E',cands[,'indcode'],ignore.case=T),]
# opp <- unique(c(cma[,'comname'],
#                 cma[,'conorgname'],
#                 cma[,'name_short'],
#                 cca2[,'Name'],
#                 'energy','environment'))
# opp <- opp[nchar(opp) > 3]
# opp <- opp[!grepl('none',opp,ignore.case=T)]
# opp <- opp[!grepl('blank',opp,ignore.case=T)]
# opp <- gsub('"','',opp)
# opp <- gsub("'",'',opp)
# opp <- gsub("\\.",'',opp)
# opp <- gsub("\\-",' ',opp)
# opp <- gsub("\\/",' ',opp)
# opp <- gsub('\\(','',opp)
# opp <- gsub('\\)','',opp)

# o.other <- NULL
# for(x in opp){
#     o.other <- unique(c(o.other,as.character(unlist(get.name.org(x,rel.vars)))))
#     print(x)
#     print(length(o.other))
# }
# opo = unique(o.other)
# org5 <- dbGetQuery(con,
#                    paste("select ",rel.vars1," from newconts where",
#                          ' contributor_category like "E%"',
#                          ' or recipient_category like "E%"',sep=''))
# opo <- unique(c(opo,org5[,'bonica_id2']))
# save(opo,file='~/workspace/tmp/other_environment_bids.rda')





###############################################################################
##STEP TWO
###############################################################################


##get from org in db:
print(load('~/workspace/tmp/proenv_bids.rda'))
print(load('~/workspace/tmp/prooil_bids.rda'))

##load('~/workspace/tmp/other_abortion_bids.rda')
env.pacs <- read.csv('~/workspace/contributions/data/issue_specific/environment.csv',as.is=T)
prooilpacs <- env.pacs[env.pacs[,'Issue'] == 'Oil & Gas',]
proenvpacs <- env.pacs[env.pacs[,'Issue'] == 'Environment',]


###############################################################################
##PRO ENVIRONMENT
###############################################################################
newid <- cme[!(cme[,'comid'] %in% proenvpacs[,'ICPSR2']),'comid']
newey <- cme[!(cme[,'comid'] %in% proenvpacs[,'ICPSR2']),'eyear']
nm <- matrix('',length(newid),ncol(proenvpacs))
colnames(nm) <- colnames(proenvpacs)
nm[,'ICPSR'] <- paste(newid,newey,sep='')
nm[,'ICPSR2'] <- newid
proenvpacs <- rbind(proenvpacs,nm)

qq <- opc
for(xx in 1:nrow(proenvpacs)){
    p <- as.character(proenvpacs[xx,'ICPSR'])
    p1 <- as.character(proenvpacs[xx,'ICPSR2'])
    p2 <- as.character(proenvpacs[xx,'bonica_rid'])
    cts <- dbGetQuery(con,
                      paste("select bonica_id2",
                            " from newconts",
                            " where recipient_ext_id = '",p,"'",sep=''))
    cts2 <- dbGetQuery(con,
                       paste("select bonica_id2",
                             " from newconts",
                             " where recipient_ext_id = '",p1,"'",sep=''))
    cts3 <- dbGetQuery(con,
                       paste("select bonica_id2",
                             " from newconts",
                             " where recipient_ext_id like '",p1,"%'",
                             " and seat like 'fed%'",
                            sep=''))
    if (!is.na(p2)) {
        cts4 <- dbGetQuery(con,
                           paste("select bonica_id2",
                                 " from newconts",
                                 " where bonica_rid = '",p2,"'",
                                 sep=''))

        qq <- unique(c(qq,unlist(cts),unlist(cts2),unlist(cts3),unlist(cts4)))
    } else {
        qq <- unique(c(qq,unlist(cts),unlist(cts2),unlist(cts3)))
        cts4 <- NULL
    }
    print(c(xx,p,length(unique(qq))))
}
opc2 <- qq
save(opc2,file='~/workspace/tmp/proenv_bids2.rda')

###############################################################################
##PRO-OIL
###############################################################################
newid <- cmc[!(cmc[,'comid'] %in% prooilpacs[,'ICPSR2']),'comid']
newey <- cmc[!(cmc[,'comid'] %in% prooilpacs[,'ICPSR2']),'eyear']
nm <- matrix('',length(newid),ncol(prooilpacs))
colnames(nm) <- colnames(prooilpacs)
nm[,'ICPSR'] <- paste(newid,newey,sep='')
nm[,'ICPSR2'] <- newid
prooilpacs <- rbind(prooilpacs,nm)
qq2 <- opl
for(xx in 1:nrow(prooilpacs)){
    p <- as.character(prooilpacs[xx,'ICPSR'])
    p1 <- as.character(prooilpacs[xx,'ICPSR2'])
    p2 <- as.character(prooilpacs[xx,'bonica_rid'])

    cts <- dbGetQuery(con,
                      paste("select bonica_id2",
                            " from newconts",
                            " where recipient_ext_id = '",p,"'",sep=''))
    cts2 <- dbGetQuery(con,
                       paste("select bonica_id2",
                             " from newconts",
                             " where recipient_ext_id = '",p1,"'",sep=''))

    cts3 <- dbGetQuery(con,
                      paste("select bonica_id2",
                            " from newconts",
                            " where recipient_ext_id like '",p1,"%'",
                            " and seat like 'fed%'",
                            sep=''))
    if (!is.na(p2)) {
        cts4 <- dbGetQuery(con,
                           paste("select bonica_id2",
                                 " from newconts",
                                 " where bonica_rid = '",p2,"'",
                                 sep=''))
        qq2 <- unique(c(qq2,unlist(cts),unlist(cts2),unlist(cts3),unlist(cts4)))

    } else {
        qq2 <- unique(c(qq2,unlist(cts),unlist(cts2),unlist(cts3)))
        cts4 <- NULL
    }
    print(c(xx,p,p1,length(unique(qq2))))
}
opl2 <- qq2
save(opl2,file='~/workspace/tmp/prooil_bids2.rda')


###############################################################################
##ALL ENERGY
###############################################################################
# newid <- cma[!(cma[,'comid'] %in% env.pacs[,'ICPSR2']),'comid']
# newey <- cma[!(cma[,'comid'] %in% env.pacs[,'ICPSR2']),'eyear']
# nm <- matrix('',length(newid),ncol(env.pacs))
# colnames(nm) <- colnames(env.pacs)
# nm[,'ICPSR'] <- paste(newid,newey,sep='')
# nm[,'ICPSR2'] <- newid
# env.pacs <- rbind(env.pacs,nm)
# qq2 <- opo
# for(xx in 1:nrow(env.pacs)){
#     p <- as.character(env.pacs[xx,'ICPSR'])
#     p1 <- as.character(env.pacs[xx,'ICPSR2'])
#     cts <- dbGetQuery(con,
#                       paste("select bonica_id2",
#                             " from newconts",
#                             " where recipient_ext_id = '",p,"'",sep=''))
#     cts2 <- dbGetQuery(con,
#                        paste("select bonica_id2",
#                              " from newconts",
#                              " where recipient_ext_id = '",p1,"'",sep=''))

#     cts3 <- dbGetQuery(con,
#                       paste("select bonica_id2",
#                             " from newconts",
#                             " where recipient_ext_id like '",p1,"%'",
#                             " and seat like 'fed%'",
#                             sep=''))

#     qq2 <- unique(c(qq2,unlist(cts),unlist(cts2),unlist(cts3)))
#     print(c(xx,p,p1,length(unique(qq2))))
# }
# opo2 <- unique(c(opo,opc2,opl2))
# save(opo2,file='~/workspace/contributions/data/issue_specific/environment.rda')



