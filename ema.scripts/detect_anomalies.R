
cand.master <- read.csv('~/Dropbox/crowdpac/states/2014/CA/candidate_master_to_import.csv')
cand.issue.scores <- read.csv('~/Dropbox/crowdpac/states/2014/CA/candidate_issue_positions_to_import.csv')

master.ids <- as.matrix(cand.master[,'bonica_rid'])
names <- as.matrix(cand.master[,'Name'])
district <- as.matrix(cand.master[,'District'])
party <- as.matrix(cand.master[,'party.label'])
cpscore <- as.matrix(round(as.numeric(cand.master[,'cfscore']),digits=2))

issue.ids <- cand.issue.scores[,'bonica_rid']
m <- match(master.ids,issue.ids)

energy.environment <- as.matrix(cand.issue.scores[m,'energy_and_environment'])
abortion <- as.matrix(cand.issue.scores[m,'abortion'])
marriage <- as.matrix(cand.issue.scores[m,'marriage'])
# healthcare <- as.matrix(cand.issue.scores[m,'healthcare'])
# immigration <- as.matrix(cand.issue.scores[m,'immigration'])
# social.security <- as.matrix(cand.issue.scores[m,'social_security_and_medicare'])
# guns <- as.matrix(cand.issue.scores[m,'guns'])
# education <- as.matrix(cand.issue.scores[m,'education'])
# unions.labor <- as.matrix(cand.issue.scores[m,'labor'])
# jobs.economy <- as.matrix(cand.issue.scores[m,'jobs_and_the_economy'])
# banking.finance <- as.matrix(cand.issue.scores[m,'banking_and_finance'])
# foreign.policy <- as.matrix(cand.issue.scores[m,'foreign_policy'])
# defense <- as.matrix(cand.issue.scores[m,'defense_and_national_security'])
# budget.taxes <- as.matrix(cand.issue.scores[m,'budget_and_taxes'])
# economic.inequality <- as.matrix(cand.issue.scores[m,'welfare_and_poverty'])
# civil.liberties <- as.matrix(cand.issue.scores[m,'privacy'])

# score.matrix <- cbind(master.ids,names,district,party,cpscore,
# 	                  healthcare,immigration,social.security,
# 	                  guns,education,unions.labor,jobs.economy,
# 	                  energy.environment,banking.finance,abortion,
# 	                  foreign.policy,defense,budget.taxes,
# 	                  economic.inequality,civil.liberties)
# colnames(score.matrix) <- c('bonica_rid','Name','District','party','cpscore',
# 	                        'healthcare','immigration','social.security',
# 	                        'guns','education','unions.labor','jobs.economy',
# 	                        'energy.environment','banking.finance','abortion',
# 	                        'foreign.policy','defense','budget.taxes',
# 	                        'economic.inequality','civil.liberties')
score.matrix <- cbind(master.ids,names,district,party,cpscore,
	                  energy.environment,abortion,marriage)
colnames(score.matrix) <- c('bonica_rid','Name','District','party','cpscore',
	                        'energy.environment','abortion','same.sex.marriage')

add.to.list <- matrix(nrow=0,ncol=7)
for (i in 1:nrow(score.matrix)) {
# for (i in 7:7) {

	if (!is.na(score.matrix[i,'cpscore'])) {
		print(i)
		## GENERAL SCORES

		# if general score does not match party
		cond1 <- ((sign(as.numeric(score.matrix[i,'cpscore'])) == -1 && score.matrix[i,'party'] == 'REP') ||
		          (sign(as.numeric(score.matrix[i,'cpscore'])) == 1 && score.matrix[i,'party'] == 'DEM')) 
		if (is.na(cond1)) { cond1<-FALSE }
		if (cond1==TRUE) {
			anomaly <- 'general score does not match party'	
			add.to.list <- rbind(add.to.list,c(score.matrix[i,c('bonica_rid','Name','District','party')],anomaly=anomaly,score.matrix[i,'cpscore'],details=''))
		}

		# if abs(general score) > 9
		cond2 <- abs(as.numeric(score.matrix[i,'cpscore'])) > 9
		if (cond2==TRUE) {
			anomaly <- 'general score greater than 9'
			add.to.list <- rbind(add.to.list,c(score.matrix[i,c('bonica_rid','Name','District','party')],anomaly=anomaly,score.matrix[i,'cpscore'],details=''))
		}

		# if abs(general score) < 1
		cond3 <- abs(as.numeric(score.matrix[i,'cpscore'])) < 1
		if (cond3==TRUE) {
			anomaly <- 'general score less than 1'
			add.to.list <- rbind(add.to.list,c(score.matrix[i,c('bonica_rid','Name','District','party')],anomaly=anomaly,score.matrix[i,'cpscore'],details=''))
		}

		## ISSUE SPECIFIC

		# if average of issue scores > 4 points from general score
		# cond4 <- abs(as.numeric(score.matrix[i,'cpscore']) - mean(as.numeric(score.matrix[i,c('healthcare','immigration','social.security','guns',
		# 	                                                         'education','unions.labor','jobs.economy','energy.environment',
		# 	                                                         'banking.finance','abortion','foreign.policy','defense',
		# 	                                                         'budget.taxes','economic.inequality','civil.liberties')]),na.rm=TRUE)) > 4
		cond4 <- abs(as.numeric(score.matrix[i,'cpscore']) - mean(as.numeric(score.matrix[i,c('energy.environment','abortion','same.sex.marriage')]),na.rm=TRUE)) > 4
		if (is.na(cond4)) { cond4<-FALSE }
		if (cond4==TRUE) {
			anomaly <- 'average issue score > 4 points from general score'
			add.to.list <- rbind(add.to.list,c(score.matrix[i,c('bonica_rid','Name','District','party')],anomaly=anomaly,score.matrix[i,'cpscore'],details=''))
		}

		# issue.vec <- score.matrix[i,c('healthcare','immigration','social.security','guns','education','unions.labor',
		# 	    					  'jobs.economy','energy.environment','banking.finance','abortion','foreign.policy',
		# 	    					  'defense','budget.taxes','economic.inequality','civil.liberties')]
		issue.vec <- score.matrix[i,c('energy.environment','abortion','same.sex.marriage')]
		have.scores <- which(!is.na(issue.vec))
		
		if (length(have.scores) > 0) {
			# print(paste(score.matrix[i,'bonica_rid'],'has issue scores',sep=' '))
			# if an issue score is > 6 points from general score
			cond5 <- FALSE
			issues5 <- ''
			for (j in have.scores) {
				if (eval(parse(text=paste("abs(as.numeric(score.matrix[i,'cpscore']) - as.numeric(issue.vec['",names(issue.vec)[j],"'])) > 6",sep='')))) {
					cond5<-TRUE; 
					# break
					this.issue <- paste(names(issue.vec)[j],"-->",round(as.numeric(issue.vec[names(issue.vec)[j]]),digits=2),sep=' ')
					issues5 <- paste(issues5,this.issue,sep=' |')
				}
			}
			if (cond5==TRUE) {
				anomaly <- paste('an issue score is > 6 points from general score',sep='')
				add.to.list <- rbind(add.to.list,c(score.matrix[i,c('bonica_rid','Name','District','party')],anomaly=anomaly,score.matrix[i,'cpscore'],details=issues5))
			}
			
			# if C and L issue scores
			issues6 <- ''
			cond6 <- length(unique(c( unique(sign(as.numeric(issue.vec[have.scores]))) , sign(as.numeric(score.matrix[i,'cpscore'])) )))!=1
			# print(paste(cond6,score.matrix[i,'bonica_rid'],sep=' '))
			if (cond6==TRUE) {
				# if (score.matrix[i,'party']=='REP') {
				# 	indices <- which(sign(as.numeric(issue.vec)) == -1) 
				# }
				# else if (score.matrix[i,'party']=='DEM') {
				# 	indices <- which(sign(as.numeric(issue.vec)) == 1)
				# }
				indices <- which(sign(as.numeric(issue.vec[have.scores])) - as.numeric((vector('numeric',length(have.scores))+1)*sign(as.numeric(score.matrix[i,'cpscore']))) !=0)
				scores <- issue.vec[indices]
				iss <- names(issue.vec[indices])
				for (k in 1:length(indices)) {
					this.issue <- paste(iss[k],"-->",round(as.numeric(scores[k]),digits=2),sep=' ')
					issues6 <- paste(issues6,this.issue,sep=' |')
				}
				anomaly <- 'issue scores on opposide side'
				add.to.list <- rbind(add.to.list,c(score.matrix[i,c('bonica_rid','Name','District','party')],anomaly=anomaly,score.matrix[i,'cpscore'],details=issues6))
			}

			# if issue score > 9
			indices <- which(abs(as.numeric(issue.vec)) > 9)
			issues7 <- ''
			cond7 <- length(indices) > 0
			if (cond7 == TRUE) {
				scores <- issue.vec[indices]
				iss <- names(issue.vec[indices])
				for (n in 1:length(indices)) {
					this.issue <- paste(iss[n],"-->",round(as.numeric(scores[n]),digits=2),sep=' ')
					issues7 <- paste(issues7,this.issue,sep=' |')
				}
				anomaly <- 'issue score greater than 9'
				add.to.list <- rbind(add.to.list,c(score.matrix[i,c('bonica_rid','Name','District','party')],anomaly=anomaly,score.matrix[i,'cpscore'],details=issues7))
			}
		}
	}	
}

currorder <- order(add.to.list[,'District'])
anomalies <- add.to.list[currorder,]


