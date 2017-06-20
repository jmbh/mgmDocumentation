# jonashaslbeck@gmail.com

# !!! Fill in Directory where figures should be saved !!!
figDir <- getwd()

# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------
# ------------------- Code and Examples: Installation ------------------------------------------------
# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

install.packages('mgm') # from CRAN
# library(devtools)
# install_github('jmbh/mgm') # for developmental version
library(mgm)
library(qgraph)

# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------
# ------------------- Code and Examples: "Stationary mixed Autoregressive Models (mVARs)' ------------
# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------


# -------------------- Estimating mVAR --------------------

set.seed(1)
fit_mvar <- mvar(data = mvar_data$data, 
                 type = c("c", "c", "c", "c", "g", "g"),
                 level = c(2, 2, 4, 4, 1, 1),
                 lambdaSel = "CV",
                 lambdaFolds = 10, 
                 lags = 1)

round(fit_mvar$wadj[,,1], 2)


# -------------------- Making Predictions from mVAR --------------------

pred_mgm <- predict(object = fit_mvar, 
                    data = mvar_data$data,
                    errorCon = c("RMSE", "R2"),
                    errorCat = c("CC", "nCC"))

pred_mgm$errors


# -------------------- Visualizing mVAR --------------------


errors <- c(pred_mgm$errors[1:4, 5], pred_mgm$errors[5:6, 3])

pdf(paste0(figDir, "Fig_mvar_p6_example.pdf"), width = 7, height = 4)
set.seed(1)
qgraph(t(fit_mvar$wadj[,,1]), 
       edge.color = t(fit_mvar$edgecolor[,,1]), 
       pie = errors, 
       pieColor = c(rep("tomato", 4), rep("lightblue", 2)),
       nodeNames = c(paste0("Categorical; m = ", c(2,2,4,4)), rep("Gaussian", 2)),
       vsize = 8,  esize = 13, asize = 7, mar=c(7,7,7,7)
)
dev.off()


# -------------------- Sampling from mVAR --------------------

# a) Basic specification
p <- 6
type <- c("c", "c", "c", "c", "g", "g") 
level <- c(2, 2, 4, 4, 1, 1) 
max_level <- max(level)
lags <- 1
n_lags <- length(lags)

# c) Specify thresholds
thresholds <- list()
for(i in 1:p) thresholds[[i]] <- rep(0, level[i])

# d) Specify standard deviations for the Gaussians
sds <- rep(1, p)

# e) Create coefficient array
coefarray <- array(0, dim=c(p, p, max_level, max_level, n_lags))

# Lagged effect 5 <- 6
coefarray[5, 6, 1, 1, 1] <- .4
# Lagged effect 1 <- 5
coefarray[1, 5, 1:level[1], 1:level[5], 1] <- c(0, 1)
# Lagged effect 2 <- 4
m1 <- matrix(0, nrow=level[2], ncol=level[4])
m1[1,1:2] <- 1
m1[2,3:4] <- 1
coefarray[1, 3, 1:level[2], 1:level[4], 1] <- m1


# 2) Sample
set.seed(1)
mvar_data <- mvarsampler(coefarray = coefarray,
                         lags = lags,
                         thresholds = thresholds,
                         sds = sds,
                         type = type,
                         level = level,
                         N = 200,
                         pbar = TRUE)


# -------------------- Application: Resting State fMRI Data --------------------

p <- ncol(restingstate_data$data)

# ---------- Fit Model ----------

set.seed(1)
rs_mvar <- mvar(data = restingstate_data$data, 
                type = rep("g", p), 
                level = rep(1, p), 
                lambdaSel = "CV",
                lambdaFolds = 10,
                lags = c(1,2,3))


# ---------- Plotting ----------

# Download & read brain image
library(httr)
url='https://raw.githubusercontent.com/jmbh/mgmDocumentation/master/files/brainpic.jpg'
GET(url, write_disk(paste0(figDir,"brainpic.jpg"), overwrite=TRUE))
library(jpeg) 
img <- readJPEG(source=paste0(figDir,"brainpic.jpg"), native=FALSE)


# Plotting
width <- 6
pdf(paste0(figDir, 'Fig_mvar_application.pdf'), width = width, height = width/3)

par(mfrow=c(1, 3), mar = c(1, 1, 2, 1))
for(i in c(1, 2, 3)) {
  plot(-1:1, -1:1, type = "n", axes = FALSE, xlab = "", ylab = "")
  rasterImage(img, -1.1, -1.1, 1.1, 1.1)   # Plot brain image
  qgraph(t(rs_mvar$wadj[,,i]), # Plot VAR network on top
         edge.color = t(rs_mvar$edgecolor[,,i]),
         layout = cbind(restingstate_data$layout_x, restingstate_data$layout_y), 
         plot = FALSE, labels = F,
         maximum = max(abs(rs_mvar$wadj)))
  mtext(paste0("(", letters[i], ") Lag = ", i), 3, cex = .7, line = .7)
}

dev.off()


