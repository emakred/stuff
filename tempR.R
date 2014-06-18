

# --------- for DE ----------------------------------------------------------------------------------------

library(yaml)

legs <- yaml.load_file('~/workspace/crowdpac/congress-legislators-master/congress-legislators-master/legislators-current.yaml')
lmat <- matrix(NA,length(legs),28)

for(xx in 1:length(legs)){
    oo <- legs[[xx]]$id
    oo1 <- legs[[xx]]$bio
    oo2 <- legs[[xx]]$name
    oo3 <- legs[[xx]]$terms
    rr <- c(lname=oo2$last,
            fname=oo2$first,
            state=oo3[[length(oo3)]]$state,
            office=oo3[[length(oo3)]]$type,
            party=oo3[[length(oo3)]]$party,
            firstcong = substr(oo3[[1]]$end,1,4),
            lastcong = substr(oo3[[length(oo3)]]$end,1,4),
            bonica_rid = NA,
            icpsr=oo$icpsr,
            bioguide=oo$bioguide,
            thomas=ifelse(is.null(oo$thomas),NA,oo$thomas),
            lis=ifelse(is.null(oo$lis),NA,oo$lis),
            govtrack=oo$govtrack,
            opensecrets=ifelse(is.null(oo$opensecrets),NA,oo$opensecrets),
            votesmart=ifelse(is.null(oo$votesmart),NA,oo$votesmart),
            fec1 =ifelse(is.null(oo$fec[1]),NA,oo$fec[1]),
            fec2 =ifelse(is.null(oo$fec[2]),NA,oo$fec[2]),
            cspan=ifelse(is.null(oo$cspan),NA,oo$cspan),
            maplight =ifelse(is.null(oo$maplight),NA,oo$maplight),
            washington_post=ifelse(is.null(oo$washington_post),NA,oo$washington_post),
            wikipedia = ifelse(is.null(oo$wikipedia),NA,oo$wikipedia),
            url=ifelse(is.null(oo3[[length(oo3)]]$url),NA,oo3[[length(oo3)]]$url),
            address=ifelse(is.null(oo3[[length(oo3)]]$address),NA,oo3[[length(oo3)]]$address),
            phone=ifelse(is.null(oo3[[length(oo3)]]$phone),NA,oo3[[length(oo3)]]$phone),
            rss_url=ifelse(is.null(oo3[[length(oo3)]]$rss_url),NA,oo3[[length(oo3)]]$rss_url),
            birthday=oo1$birthday,
            gender = oo1$gender,
            religion =ifelse(is.null(oo1$lutheran),NA,oo1$lutheran))
    print(length(rr))
    if(xx == 1){
        colnames(lmat) <- names(rr)
        lmat[xx,] <- rr
    }else{
        m <- match(colnames(lmat),names(rr))
        lmat[xx,(!is.na(m))] <- rr[m[!is.na(m)]]
    }
    print(xx)
}

load('~/workspace/contributions/data/cand_all_1979_2014.rda')
cands <- cands[cands[,'cand_or_committee'] == 'cand',]
m <- match(as.numeric(as.character(lmat[,'icpsr'])),as.numeric(as.character(cands[,'ICPSR2'])),incomparables=c(NA,''))
m1 <- match(lmat[,'fec1'],cands[,'ICPSR2'],incomparables=c(NA,''))
m[is.na(m)] <- m1[is.na(m)]
m1 <- match(lmat[,'icpsr'],cands[,'after_switch_ICPSR'],incomparables=c(NA,''))
m[is.na(m)] <- m1[is.na(m)]
m1 <- match(lmat[,'icpsr'],cands[,'before_switch_ICPSR'],incomparables=c(NA,''))
m[is.na(m)] <- m1[is.na(m)]

for(xx in which(is.na(m))){
    if(!is.na(lmat[xx,'fec1'])){
        oo <- grep(lmat[xx,'fec1'],cands[,'unq_candids'])
        oo1 <- unique(cands[oo,'ICPSR2'])
        if(length(oo1) == 1){
            m[xx] <- oo[1]
        }
    }
}
lmat[,'bonica_rid'] <- as.character(cands[m,'bonica_rid'])

ca <- read.csv('~/workspace/crowdpac/states/2014/CA/candidate_master_to_import.csv')

m <- match(ca[,'bonica_rid'],lmat[,'bonica_rid'])

# new <- cbind(ca[which(!is.na(m)),],lmat[m[!is.na(m)],])
new <- lmat[m[!is.na(m)],]

m <- match(new[,'bonica_rid'],cands[,'bonica_rid'])

newer <- cbind(new,cands[m,'FEC.ID'])

write.csv(newer,file='~/workspace/crowdpac/states/2014/CA/DE.csv',row.names=FALSE)

# -------- find contrib IDs from bonica_rids ---------------------------------------------------------------

library(RMySQL)

username = 'crowdpacdata'
passwd = 'crowdpacdata2014'
dbname = 'cfdb'
port = 5841
host = 'cp-data-analysis3.cbtto5axwp1k.us-east-1.rds.amazonaws.com'

con <- dbConnect(MySQL(), user=username, password=passwd, dbname=dbname, port=port, host=host)

temp <- read.csv('~/workspace/crowdpac/states/2014/CA/rids.csv')

cong <- dbGetQuery(con,paste("select distinct bonica_rid, ContribID from cand_cont_db where bonica_rid in ('",paste(temp[,1],collapse="','"),"');",sep=''))

write.csv(cong,file='~/workspace/crowdpac/states/2014/CA/rid_id2.csv')



# -----------------------------------------------------------------------

# get 7680 records from cmdat, but no scores from aresults

print(load('~/workspace/contributions/finished_runs/aresults_79_14_current.rda'))

biz <- cmdat[grepl('G1',cmdat$indcode) | grepl('F1',cmdat$indcode) | grepl('X4',cmdat$indcode) | grepl('F2',cmdat$indcode),]

biz.only <- biz[!grepl('LG',biz[,'indcode']),]

m <- match(biz.only[,'comid'],aresults$contributors[,'comid'])

biz.only.w.score <- cbind(biz.only,aresults$contributors[m,'oc1'])


# -----------------------------------------------------------------------



# !! cands had industry code and score, see if that will work <-  only get 2 records

print(load('~/workspace/contributions/finished_runs/cands_79_14_current.rda'))

biz <- cands[grepl('G1',cands[,'indcode']) | grepl('F1',cands[,'indcode']) | grepl('X4',cands[,'indcode']) | grepl('F2',cands[,'indcode']),]

biz.only <- biz[!grepl('LG',biz[,'indcode']),]


# -----------------------------------------------------------------------


# temp<-matrix(nrow=0,ncol=6)
# colnames(temp) <- c('bonica_rid','X.1','X.2','X.3','X.4','X.5')

temp2 <- read.csv('~/workspace/crowdpac/states/2014/CA/candidate_issue_positions_to_import.csv')

ids <- as.character(temp2[,'bonica_rid'])

issue_order <- cbind(bonica_rid=ids,X.1='abortion',X.2='marriage',X.3='energy_and_environment')

write.csv(issue_order,'~/workspace/crowdpac/output/candidate_issue_order_to_import.csv')