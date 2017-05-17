# jonashaslbeck@gmail.com

figDir <- '/Users/jmb/Dropbox/MyData/_PhD/__projects/mgm_JSS/4_code/jss_code/'


# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------
# ------------------- Code and Examples: Installation ------------------------------------------------
# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

# install.pacakges('mgm') # from CRAN
library(devtools)
install_github('jmbh/mgm') # for developmental version

library(mgm)

library(qgraph)

# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------
# ------------------- Code and Examples: "Stationary Mixed Graphical Models' -------------------------
# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------


# -------------------- Estimating Mixed Graphical Models --------------------

set.seed(1)
fit_mgm <- mgm(data = mgm_data$data,
               type = c("g", "c", "c", "g"),
               levels = c(1, 2, 4, 1), 
               k = 2, 
               lambdaSel = "CV",
               lambdaFolds = 10,
               ruleReg = "AND")

round(fit_mgm$pairwise$wadj, 2)


# -------------------- Making Predictions from Mixed Graphical Models --------------------

pred_mgm <- predict(object = fit_mgm, 
                    data = mgm_data$data,
                    errorCon = c("RMSE", "R2"),
                    errorCat = c("CC", "nCC"))

pred_mgm$errors


# -------------------- Visualizing Mixed Graphical Models --------------------

# install.packages('qgraph')
library(qgraph)
errors <- c(pred_mgm$errors[1, 3], pred_mgm$errors[2:3, 4], pred_mgm$errors[4, 3])

pdf(paste0(figDir,'Fig_mgm_p4_example.pdf'), width = 7, height = 4)
set.seed(1)
qgraph(fit_mgm$pairwise$wadj, 
       edge.color = fit_mgm$pairwise$edgecolor, 
       pie = errors, 
       pieColor = c("lightblue", "tomato", "tomato", "lightblue"),
       nodeNames = c("Gaussian", "Categorical; m = 2", "Categorical; m = 4", "Gaussian"),
       legend = TRUE,
       vsize = 15, 
       esize = 20, mar=c(7,7,7,7))
dev.off()


# -------------------- Sampling from Mixed Graphical Model --------------------

# a) General Graph Info
type <- c("g", "c", "c", "g")
level <- c(1, 2, 4, 1)

# b) Define Thresholds
thresholds <- list()
thresholds[[1]] <- 0
thresholds[[2]] <- rep(0, level[2])
thresholds[[3]] <- rep(0, level[3])
thresholds[[4]] <- 0

# c) Define Standard deviation of Gaussians
sds <- rep(1, 4)

# d) Define Interaction
factors <- list()
factors[[1]] <- matrix(c(1,4,
                         2,3,
                         1,2), ncol=2, byrow = T)
interactions <- list()
interactions[[1]] <- vector("list", length = 3)
# 2-way interaction: 1-4
interactions[[1]][[1]] <- array(.5, dim = c(level[1], level[4]))
# 2-way interaction: 2-3
int_2 <- matrix(0, nrow = level[2], ncol = level[3])
int_2[1, 1:2] <- 1
interactions[[1]][[2]] <- int_2
# 2-way interaction: 1-2
int_1 <- matrix(0, nrow = level[1], ncol = level[2])
int_1[1, 1] <- 1
interactions[[1]][[3]] <- int_1


# e) Sampling
set.seed(1)
mgm_data <- mgmsampler(factors = factors,
                       interactions = interactions,
                       thresholds = thresholds,
                       sds = sds,
                       type = type,
                       level = level,
                       N = 500)

head(mgm_data$data)


# -------------------- Application: Autism and Quality of Life --------------------

data("autism_data_large")

# Fit MGM Model
set.seed(1)
fit_ADS <- mgm(data = autism_data_large$data, 
               type = autism_data_large$type,
               level = autism_data_large$level,
               k = 2, 
               lambdaSel = 'EBIC', 
               lambdaGam = 0.25)

# Make plot
width <- 9
pdf(paste0(figDir,'Fig_mgm_application_Autism.pdf'), width = width, height = width*.7)

qgraph(fit_ADS$pairwise$wadj, 
       layout = 'spring', repulsion = 1.3,
       edge.color = fit_ADS$pairwise$edgecolor, 
       nodeNames = autism_data_large$colnames,
       color = autism_data_large$groups_color, 
       groups = autism_data_large$groups_list,
       legend.mode="style2", legend.cex=.4, 
       vsize = 3.5, esize = 15)

dev.off()


# For paper text
range(autism_data_large$level)
autism_data_large$level[c(6,7,27)]



# -------------------- Application HOI: PTSD --------------------

head(PTSD_data$data)


# ----- Fit Model -----

apply(PTSD_data$data, 2, function(x) length(unique(x)))

set.seed(1)
fit_mgmk <- mgm(data = PTSD_data$data, 
                type = PTSD_data$type, 
                level = PTSD_data$level,
                k = 3, 
                lambdaSel = "CV", 
                overparameterize = TRUE)


fit_mgmk$factorgraph


# ----- Factor Graph Visualization -----


# List of estimated interactions
fit_mgmk$rawfactor$indicator

# Compute Label vector
labels <- c(PTSD_data$names, rep(2, 3), rep(3, 5))

# Visualize factor graph using qgraph()
library(RColorBrewer)
cols <- brewer.pal(5, 'Set1')
node.color <- c('white', cols[1:3], rep('white', 2+8))

width <- 8
pdf(paste0(figDir,'Fig_mgm_HOI.pdf'), width = width, height = width*.5)

par(mfrow=c(1,2))

set.seed(1)
qgraph(fit_mgmk$factorgraph$graph,
       color = node.color,
       edge.color = fit_mgmk$factorgraph$edgecolor,
       layout = 'spring',
       labels =  labels,
       shape = c('circle', 'square')[fit_mgmk$factorgraph$nodetype+1],
       vsize = c(8, 4)[fit_mgmk$factorgraph$nodetype+1], 
       mar = c(5,5,5,5))

mtext('(a)', side = 1, line=3)

plot.new()
par(mar=c(2,2,2,2))
mtext('(b)', side = 1)


dev.off()




# ----- Create Three (conditional) 2-way tables (add with inkscape to above factor graph viz) -----

# conditional 2-way table
i <- c(4,2,3)
PTSD_data$names[c(i[1],i[2],i[3])]
tb <- table(PTSD_data$data[,i[1]],
            PTSD_data$data[,i[2]],
            PTSD_data$data[,i[3]])
tb[,,1] <- tb[,,1] / sum(tb[,,1])
tb[,,2] <- tb[,,2] / sum(tb[,,2])
tb <- round(tb, 2)

# margingal 2-way table
tbm <- table(PTSD_data$data[,4], PTSD_data$data[,2])
tbm <- round(tbm / sum(tbm), 2)
tbm


# ----- 1) Marginal (upset, dreams) -----

library(scales)
cex_text <- 1.7
cex_text_yn <- 1.5
nC3 <- cols

pdf(paste(figDir, 'Table1_Marg.pdf'), width = 8, height = 8)

plot.new()
par(mar=c(2,2,2,2))
plot.window(xlim=c(-.5, 1.7), ylim=c(-.5, 1.7))

# Probability Cells
rect(0, 0, .5, .5, 
     col = alpha('black', tbm[2,1])) # bottom left
rect(.5, 1, 0, .5,
     col = alpha('black', tbm[1,1])) # top left
rect(.5, 0, 1, .5,
     col = alpha('black', tbm[2,2])) # bottom right
rect(.5, .5, 1, 1,
     col = alpha('black', tbm[1,2])) # top right
rect(0, 0, 1, 1, lwd = 2)

# Text
text(0.25, 0.25, tbm[2,1], cex = cex_text) # bottom left
text(0.25, 0.75, tbm[1,1], cex = cex_text) # bottom left
text(0.75, 0.25, tbm[2,2], cex = cex_text) # bottom right
text(0.75, 0.75, tbm[1,2], cex = cex_text) # top right

## Description: shown variables
# Upset
rect(-.1, 0, -.05, 1, border=F, col = nC3[3])
text(-.4, .5, 'Upset', col = nC3[3], cex = cex_text)
text(-.2, .25, 'Yes', col = nC3[3], cex = cex_text_yn)
text(-.2, .75, 'No', col = nC3[3], cex = cex_text_yn)


rect(0, 1.1, 1, 1.05, border=F, col = nC3[1])
text(.5, 1.38, 'Dreams', col = nC3[1], cex = cex_text)
text(0.25, 1.2, 'No', col = nC3[1], cex = cex_text_yn)
text(0.75, 1.2, 'Yes', col = nC3[1], cex = cex_text_yn)

dev.off()



# ----- 2.4.2) Conditional on: flashbacks == 0, 1  -----

for(i in 1:2) {
  
  pdf(paste(figDir, 'Table1_Cond',i-1,'.pdf'), width = 8, height = 8)
  
  plot.new()
  par(mar=c(2,2,2,2))
  plot.window(xlim=c(-.5, 1.7), ylim=c(-.5, 1.7))
  
  
  # conidtional 2-way table
  tbm <- tb[,,i]
  
  # Probability Cells
  rect(0, 0, .5, .5, 
       col = alpha('black', tbm[2,1])) # bottom left
  rect(.5, 1, 0, .5,
       col = alpha('black', tbm[1,1])) # top left
  rect(.5, 0, 1, .5,
       col = alpha('black', tbm[2,2])) # bottom right
  rect(.5, .5, 1, 1,
       col = alpha('black', tbm[1,2])) # top right
  rect(0, 0, 1, 1, lwd = 2)
  
  # Text
  text(0.25, 0.25, tbm[2,1], cex = cex_text) # bottom left
  text(0.25, 0.75, tbm[1,1], cex = cex_text) # bottom left
  text(0.75, 0.25, tbm[2,2], cex = cex_text) # bottom right
  text(0.75, 0.75, tbm[1,2], cex = cex_text) # top right
  
  ## Description: shown variables
  # Upset
  rect(-.1, 0, -.05, 1, border=F, col = nC3[3])
  text(-.4, .5, 'Upset', col = nC3[3], cex = cex_text)
  text(-.2, .25, 'Yes', col = nC3[3], cex = cex_text_yn)
  text(-.2, .75, 'No', col = nC3[3], cex = cex_text_yn)
  
  rect(0, 1.1, 1, 1.05, border=F, col = nC3[1])
  text(.5, 1.4, 'Dreams', col = nC3[1], cex = cex_text)
  text(0.25, 1.2, 'No', col = nC3[1], cex = cex_text_yn)
  text(0.75, 1.2, 'Yes', col = nC3[1], cex = cex_text_yn)
  
  
  if(i==1) answer = 'No' else answer = 'Yes'
  
  ## Description: shown variables
  rect(1.1, 0, 1.05, 1, border=F, col = nC3[2])
  text(1.5, .5, 'Flashbacks', col = nC3[2], cex = cex_text)
  text(1.2, .25, answer, col = nC3[2], cex = cex_text_yn)
  text(1.2, .75, answer, col = nC3[2], cex = cex_text_yn)
  
  dev.off()
  
}

