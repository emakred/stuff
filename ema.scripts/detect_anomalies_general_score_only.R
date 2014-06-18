
cand.master <- read.csv('~/Desktop/calist.csv')

master.ids <- as.matrix(cand.master[,'bonica_rid'])
names <- as.matrix(cand.master[,'name'])
seat <- as.matrix(cand.master[,'friendly_seat'])
district <- as.matrix(cand.master[,'district'])
party <- as.matrix(cand.master[,'party'])
cpscore <- as.matrix(round(as.numeric(cand.master[,'cp.score']),digits=2))

score.matrix <- cbind(master.ids,names,seat,district,party,cpscore)
colnames(score.matrix) <- c('bonica_rid','name','seat','district','party','cpscore')

add.to.list <- matrix(nrow=0,ncol=7)
for (i in 1:nrow(score.matrix)) {
# for (i in 7:7) {
	print(i)
	if (!is.na(score.matrix[i,'cpscore'])) {
		## GENERAL SCORES

		# if general score does not match party
		cond1 <- ((sign(as.numeric(score.matrix[i,'cpscore'])) == -1 && score.matrix[i,'party'] %in% c('R','L','AE')) ||
		          (sign(as.numeric(score.matrix[i,'cpscore'])) == 1 && score.matrix[i,'party'] %in% c('D','G','P&F'))) 
		if (is.na(cond1)) { cond1<-FALSE }
		if (cond1==TRUE) {
			anomaly <- 'general score does not match party'	
			add.to.list <- rbind(add.to.list,c(score.matrix[i,c('bonica_rid','name','seat','district','party')],anomaly=anomaly,score.matrix[i,'cpscore']))
		}

		# if abs(general score) > 9
		cond2 <- abs(as.numeric(score.matrix[i,'cpscore'])) > 9
		if (cond2==TRUE) {
			anomaly <- 'general score greater than 9'
			add.to.list <- rbind(add.to.list,c(score.matrix[i,c('bonica_rid','name','seat','district','party')],anomaly=anomaly,score.matrix[i,'cpscore']))
		}

		# if abs(general score) < 1
		cond3 <- abs(as.numeric(score.matrix[i,'cpscore'])) < 1
		if (cond3==TRUE) {
			anomaly <- 'general score less than 1'
			add.to.list <- rbind(add.to.list,c(score.matrix[i,c('bonica_rid','name','seat','district','party')],anomaly=anomaly,score.matrix[i,'cpscore']))
		}

	}
}


currorder <- order(add.to.list[,'seat'],add.to.list[,'district'])
anomalies <- add.to.list[currorder,]


