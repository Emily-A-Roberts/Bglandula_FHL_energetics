# Sensitivity analysis graph code 


# mat.air.water <- air.water.resp.M1
# diff.mat.air.water <- air.water.resp.M1 - air.water.resp.M1[1,1]
# 
# mat.air.water <- air.water.resp.M2
# diff.mat.air.water <- air.water.resp.M2 - air.water.resp.M2[1,1]
# (diff.vec.water.air.comb <- c(mat.air.water[1,1], diff.mat.air.water[c(3),1],diff.mat.air.water[c(1,3),3]))

# 
# if(sens.analysis = 0){
#   diff.air.water.no_param <- diff.vec.water.air.comb
# }
# 
# # I need to find my old code with the nice IPP output
# # It is in the BE folder on my harddrive. 
# 
# barplot(c(diff.air.water.no_param[c(3),1],diff.air.water.no_param[c(1,3),3])~c("+2C seawater","+2C low tide","+2C seawater + low tide"),
#         ylab = "Change in SFG (J)", xlab = "Simulated change in temp")


par(mfrow = c(1,1))
par(mar = c(4, 7, 3, 4))

#https://stackoverflow.com/questions/37071001/barplot-greek-letters-on-y-axis-in-r


x <- seq(-4,4, length=10)
color.names = new.pal[c(1,2,3)]



# 1. Time 1, SFG####
data <- matrix(c(1,.0005,-.33,-.12,0.66,-.07,-.19,-.79,-.57,-.74,-.58,3.14,2.64,1,.0005,-.33,-.12,0.66,-.07,-.19,-.79,-.57,-.74,-.58,3.14,2.64), ncol = 13)
data[1,] <- c(0.44085,.0005,-.33,-.12,0.66,-.07,-.19,-.79,-.57,-.74,-.58,3.14,2.64) #retyping here because was a bit mismatched. 
data[2,] <- c(-.44085,-.0005,.36,.11,-.46,.07,.16,1.12,0.58,0.96,0.44,-1.57,-1.17)
# Adding negative 1 to center around 0 rather than 1. 

head(data)
rownames(data) <- c('+ uncertainty','- uncertainty')
# colnames(data) <- c('SE','C.F.', 
#                     'c_aq_recovery',
#                     'e_aer_recovery',
#                     'c_aq_exposure',
#                     'e_aer_exposure',
#                     'c_aq_resp',
#                     'e_aq_resp',
#                     'c_feed',
#                     'e_feed')
names <- c('SE','ED', 
           'size aer_resp',
           'size aq_resp',
           'size feed',
           'aer_recov c',
           'aer_recov e',
           'aer_exp c',
           'aer_exp e',
           'aq_resp c',
           'aq_resp e',
           'feed c',
           'feed e')


# names <- c('SE','ED', 
#            'size AER_R',
#            'size AQ_R',
#            'size FR',
#            'AER_rec c',
#            'AER_rec e',
#            'AER_exp c',
#            'AER_exp e',
#            'AQ_R c',
#            'AQ_R e',
#            'FR c',
#            'FR e')

colnames(data) <- names

par(mfrow = c(1,1))
barplot(data[1,], horiz = T, las=1, xlim = c(-4,4), xaxt='n', ylab='',
        beside=T, col=c('grey'))
barplot(data[2,], horiz = T, las=1, xlim = c(-4,4), xaxt='n',
        yaxt='n',                 #To prevent double printing of y-labels.
        beside=T, col=c('white'), add = TRUE)
axis(1, at=pretty(x),  lab=paste0(pretty(x)+1), las=TRUE)
title(main = expression("Scope for Growth (J)"))

