

# simulate generations of people, with SNP data, assortative mating and vertical transmission

library(MASS)

rm(list=ls())

n        <- 10000              # people each generation
num.gen  <- 10                 # generations
num.cvs  <- 200                # causal variants
num.ncvs <- 100                # non-causal variants
num.vs   <- num.cvs + num.ncvs # variants total
min.maf  <- .2                 # minor allele frequencies
max.maf  <- .5 

# VG
vg1 <- .60 # number that genotypic value is multiplied by

# AM - correlation between male & female phenotypes
am11 <-  .30 

# VT multiplier
f11 <- .0  

# create template for a "population registry", to be filled in later
reg <- data.frame(ID =         1:(n*(num.gen+2)),                # ID
                  mumID = rep(NA, n*(num.gen+2)),                # mum's ID
                  dadID = rep(NA, n*(num.gen+2)),                # dad's ID
                  P =     rep(NA, n*(num.gen+2)),                # phenotype
                  A =     rep(NA, n*(num.gen+2)),                # breeding value
                  E =     rep(NA, n*(num.gen+2)),                # environmental value
                  mumP =  rep(NA, n*(num.gen+2)),                # mum's phenotype
                  dadP =  rep(NA, n*(num.gen+2)),                # dad's phenotype
                  gen =   rep(0:(num.gen+1), rep(n, num.gen+2))) # generation a person belongs to

#CVs and their effect sizes
maf.vector <- runif(num.vs,min.maf,max.maf)
gentp.var <- maf.vector*(1-maf.vector)*2

alpha.pre <- c(rnorm(num.cvs, 0, 1), rep(0, num.ncvs))
alpha <- alpha.pre*sqrt(1/((num.cvs-1)*gentp.var))

cv.info <- data.frame(maf=maf.vector,alpha1=alpha) 

num.gentps <- num.vs*n
XO <- matrix(rbinom(num.gentps,size=2,prob=cv.info$maf),nrow=n,ncol=num.vs,byrow=TRUE)
AO <- XO %*% as.matrix(cv.info[,"alpha1"])  

# 
dt <- data.frame("AO" =  AO, 
                 "sex" = rep(c(0,1), c(n/2, n/2)), 
                 "id" =  1:n)

template.AM.dist <- mvrnorm(n/2, c(0,0), Sigma=matrix(c(1,am11,am11,1),nrow=2),empirical=T)

rank.template.males   <- rank(template.AM.dist[,1])
rank.template.females <- rank(template.AM.dist[,2])

females <- dt[dt$sex == 0, c(1,3)]
males   <- dt[dt$sex == 1, c(1,3)]
names(females) <- c("mumA", "mumID")
names(males)   <- c("dadA", "dadID")

# create phenotypes
gen <- 0

females$mumE <- rnorm(n/2)
females$mumP <- females$mumA*sqrt(vg1) + females$mumE*(sqrt(1-vg1))
var(females$mumP)

males$dadE <- rnorm(n/2)
males$dadP <- males$dadA*sqrt(vg1) + males$dadE*(sqrt(1-vg1))
var(males$dadP)

females$f.rank <- rank(females$mumP)
males$m.rank <- rank(males$dadP)

pairs1 <- data.frame(cbind(females[match(rank.template.females, females$f.rank),],
                           males[match(rank.template.males,     males$m.rank),]))

XOf <- XO[pairs1$mumID,]
XOm <- XO[pairs1$dadID,]

XOf_fh <- XOf*.5
XOf_fh[XOf_fh == .5] <- matrix(sample(0:1, (n/2)*num.vs, replace = T), n/2, num.vs)[XOf_fh == .5]
XOm_fh <- XOm*.5
XOm_fh[XOm_fh == .5] <- matrix(sample(0:1, (n/2)*num.vs, replace = T), n/2, num.vs)[XOm_fh == .5]

XOf_mh <- XOf*.5
XOf_mh[XOf_mh == .5] <- matrix(sample(0:1, (n/2)*num.vs, replace = T), n/2, num.vs)[XOf_mh == .5]
XOm_mh <- XOm*.5
XOm_mh[XOm_mh == .5] <- matrix(sample(0:1, (n/2)*num.vs, replace = T), n/2, num.vs)[XOm_mh == .5]

XOf_c <- XOf_fh + XOm_fh
XOm_c <- XOf_mh + XOm_mh

AOf_c <- XOf_c %*% as.matrix(cv.info[,"alpha1"])
AOm_c <- XOm_c %*% as.matrix(cv.info[,"alpha1"])

pairs1$c0A <- AOf_c[pairs1$mumID - n*gen,] 
pairs1$c0E <- rnorm(n/2) 
pairs1$c1A <- AOm_c[pairs1$mumID - n*gen,] 
pairs1$c1E <- rnorm(n/2)

pairs1$c0P <- pairs1$c0A*sqrt(vg1) + pairs1$c0E*sqrt(1-vg1) + (pairs1$mumP + pairs1$dadP)*f11
pairs1$c1P <- pairs1$c1A*sqrt(vg1) + pairs1$c1E*sqrt(1-vg1) + (pairs1$mumP + pairs1$dadP)*f11

nn <- (gen+1)*n
pairs1$c0ID <- (nn+1):(nn+nn/2)
pairs1$c1ID <- ((nn+nn/2)+1):(2*nn)

# fill in information into the population registry
reg[pairs1$mumID, 4:6] <- pairs1[, c("mumP", "mumA", "mumE")]
reg[pairs1$dadID, 4:6] <- pairs1[, c("dadP", "dadA", "dadE")]

reg[pairs1$c0ID, 2:8] <- pairs1[, c("mumID", "dadID", "c0P", "c0A", "c0E", "mumP", "dadP")]
reg[pairs1$c1ID, 2:8] <- pairs1[, c("mumID", "dadID", "c1P", "c1A", "c1E", "mumP", "dadP")]

# add to XO
XO <- rbind(XO, XOf_c, XOm_c)

for(gen in 1:num.gen){
  females <- pairs1[, c("c0A", "c0E", "c0P", "c0ID", "mumID", "dadID")]
  names(females) <- c("mumA", "mumE", "mumP", "mumID", "mormor", "morfar")
  
  males <- pairs1[, c("c1A", "c1E", "c1P", "c1ID", "mumID", "dadID")]
  names(males) <- c("dadA", "dadE", "dadP", "dadID", "farmor", "farfar")
  
  template.AM.dist <- mvrnorm(n/2, c(0,0), Sigma=matrix(c(1,am11,am11,1),nrow=2),empirical=F)
  
  rank.template.males <- rank(template.AM.dist[,1])
  rank.template.females <- rank(template.AM.dist[,2])
  
  females$f.rank <- rank(females$mumP)
  males$m.rank <- rank(males$dadP)
  
  pairs1 <- data.frame(cbind(females[match(rank.template.females, females$f.rank),],
                             males[match(rank.template.males,     males$m.rank),]))
  
  XOf <- XOf_c[pairs1$mumID -  n*gen,]
  XOm <- XOm_c[pairs1$dadID - (n*gen + n/2),]
  
  XOf_fh <- XOf*.5
  XOf_fh[XOf_fh == .5] <- matrix(sample(0:1, (n/2)*num.vs, replace = T), n/2, num.vs)[XOf_fh == .5]
  XOm_fh <- XOm*.5
  XOm_fh[XOm_fh == .5] <- matrix(sample(0:1, (n/2)*num.vs, replace = T), n/2, num.vs)[XOm_fh == .5]
  
  XOf_mh <- XOf*.5
  XOf_mh[XOf_mh == .5] <- matrix(sample(0:1, (n/2)*num.vs, replace = T), n/2, num.vs)[XOf_mh == .5]
  XOm_mh <- XOm*.5
  XOm_mh[XOm_mh == .5] <- matrix(sample(0:1, (n/2)*num.vs, replace = T), n/2, num.vs)[XOm_mh == .5]
  
  XOf_c <- XOf_fh + XOm_fh 
  XOm_c <- XOf_mh + XOm_mh
  
  AOf_c <- XOf_c %*% as.matrix(cv.info[,"alpha1"])
  AOm_c <- XOm_c %*% as.matrix(cv.info[,"alpha1"])
  
  pairs1$c0A <- AOf_c
  pairs1$c0E <- rnorm(n/2) 
  pairs1$c1A <- AOm_c 
  pairs1$c1E <- rnorm(n/2)
  
  pairs1$c0P <- pairs1$c0A*sqrt(vg1) + pairs1$c0E*sqrt(1-vg1) + (pairs1$mumP + pairs1$dadP)*f11
  pairs1$c1P <- pairs1$c1A*sqrt(vg1) + pairs1$c1E*sqrt(1-vg1) + (pairs1$mumP + pairs1$dadP)*f11
  
  nn <- (gen+1)*n
  pairs1$c0ID <- (nn+1):(nn+n/2)
  pairs1$c1ID <- ((nn+n/2)+1):(nn+n)
  
  reg[pairs1$c0ID, 2:8] <- pairs1[, c("mumID", "dadID", "c0P", "c0A", "c0E", "mumP", "dadP")]
  reg[pairs1$c1ID, 2:8] <- pairs1[, c("mumID", "dadID", "c1P", "c1A", "c1E", "mumP", "dadP")]
  
  XO <- rbind(XO, XOf_c, XOm_c)
  
  df_name <- paste0("pairs_", gen)
  assign(df_name, pairs1)
}

# get the last, second to last, and third to last sets of pairs
assign("pairs_last",    get(paste0("pairs_", num.gen)))
assign("pairs_2ndlast", get(paste0("pairs_", num.gen-1)))
assign("pairs_3rdlast", get(paste0("pairs_", num.gen-2)))

# find relatives
pairs_last$mumbro <- pairs_2ndlast[match(pairs_last$mumID, pairs_2ndlast$c0ID), "c1ID"]
pairs_last$dadsis <- pairs_2ndlast[match(pairs_last$dadID, pairs_2ndlast$c1ID), "c0ID"]

pairs_last$mumbroc0 <- pairs_last[match(pairs_last$mumbro, pairs_last$dadID), "c0ID"]
pairs_last$dadsisc0 <- pairs_last[match(pairs_last$dadsis, pairs_last$mumID), "c0ID"]

pairs_last$mumbroc1 <- pairs_last[match(pairs_last$mumbro, pairs_last$dadID), "c1ID"]
pairs_last$dadsisc1 <- pairs_last[match(pairs_last$dadsis, pairs_last$mumID), "c1ID"]

pairs_last$mumbrospo <- pairs_last[match(pairs_last$mumbro, pairs_last$dadID), "mumID"]
pairs_last$dadsisspo <- pairs_last[match(pairs_last$dadsis, pairs_last$mumID), "dadID"]

pairs_last$mumbrospobro <- pairs_2ndlast[match(pairs_last$mumbrospo, pairs_2ndlast$c0ID), "c1ID"]
pairs_last$dadsissposis <- pairs_2ndlast[match(pairs_last$dadsisspo, pairs_2ndlast$c1ID), "c0ID"]

pairs_last$mumbrospobrospo <- pairs_last[match(pairs_last$mumbrospobro, pairs_last$dadID), "mumID"]
pairs_last$dadsissposisspo <- pairs_last[match(pairs_last$dadsissposis, pairs_last$mumID), "dadID"]

pairs_last$mumbrospobroc0 <- pairs_last[match(pairs_last$mumbrospobro, pairs_last$dadID), "c0ID"]
pairs_last$dadsissposisc0 <- pairs_last[match(pairs_last$mumbrospobro, pairs_last$dadID), "c0ID"]

pairs_last$mumbrospobroc1 <- pairs_last[match(pairs_last$mumbrospobro, pairs_last$dadID), "c1ID"]
pairs_last$dadsissposisc1 <- pairs_last[match(pairs_last$mumbrospobro, pairs_last$dadID), "c1ID"]

pairs_last$mormor <- pairs_2ndlast[match(pairs_last$mumID, pairs_2ndlast$c0ID), "mumID"]
pairs_last$morfar <- pairs_2ndlast[match(pairs_last$mumID, pairs_2ndlast$c0ID), "dadID"]
pairs_last$farmor <- pairs_2ndlast[match(pairs_last$dadID, pairs_2ndlast$c1ID), "mumID"]
pairs_last$farfar <- pairs_2ndlast[match(pairs_last$dadID, pairs_2ndlast$c1ID), "dadID"]

pairs_last$mormorbro <- pairs_3rdlast[match(pairs_last$mormor, pairs_3rdlast$c0ID), "c1ID"]
pairs_last$morfarsis <- pairs_3rdlast[match(pairs_last$morfar, pairs_3rdlast$c1ID), "c0ID"]
pairs_last$farmorbro <- pairs_3rdlast[match(pairs_last$farmor, pairs_3rdlast$c0ID), "c1ID"]
pairs_last$farfarsis <- pairs_3rdlast[match(pairs_last$farfar, pairs_3rdlast$c1ID), "c0ID"]

pairs_last$mormorbroc0 <- pairs_2ndlast[match(pairs_last$mormorbro, pairs_2ndlast$dadID), "c0ID"]
pairs_last$morfarsisc0 <- pairs_2ndlast[match(pairs_last$morfarsis, pairs_2ndlast$mumID), "c0ID"]
pairs_last$farmorbroc0 <- pairs_2ndlast[match(pairs_last$farmorbro, pairs_2ndlast$dadID), "c0ID"]
pairs_last$farfarsisc0 <- pairs_2ndlast[match(pairs_last$farfarsis, pairs_2ndlast$mumID), "c0ID"]

pairs_last$mormorbroc1 <- pairs_2ndlast[match(pairs_last$mormorbro, pairs_2ndlast$dadID), "c1ID"]
pairs_last$morfarsisc1 <- pairs_2ndlast[match(pairs_last$morfarsis, pairs_2ndlast$mumID), "c1ID"]
pairs_last$farmorbroc1 <- pairs_2ndlast[match(pairs_last$farmorbro, pairs_2ndlast$dadID), "c1ID"]
pairs_last$farfarsisc1 <- pairs_2ndlast[match(pairs_last$farfarsis, pairs_2ndlast$mumID), "c1ID"]

pairs_last$mormorbroc0c0 <- pairs_last[match(pairs_last$mormorbroc0, pairs_last$mumID), "c0ID"]
pairs_last$morfarsisc0c0 <- pairs_last[match(pairs_last$morfarsisc0, pairs_last$mumID), "c0ID"]
pairs_last$farmorbroc0c0 <- pairs_last[match(pairs_last$farmorbroc0, pairs_last$mumID), "c0ID"]
pairs_last$farfarsisc0c0 <- pairs_last[match(pairs_last$farfarsisc0, pairs_last$mumID), "c0ID"]

pairs_last$mormorbroc0c1 <- pairs_last[match(pairs_last$mormorbroc0, pairs_last$mumID), "c1ID"]
pairs_last$morfarsisc0c1 <- pairs_last[match(pairs_last$morfarsisc0, pairs_last$mumID), "c1ID"]
pairs_last$farmorbroc0c1 <- pairs_last[match(pairs_last$farmorbroc0, pairs_last$mumID), "c1ID"]
pairs_last$farfarsisc0c1 <- pairs_last[match(pairs_last$farfarsisc0, pairs_last$mumID), "c1ID"]

pairs_last$mormorbroc1c0 <- pairs_last[match(pairs_last$mormorbroc1, pairs_last$mumID), "c0ID"]
pairs_last$morfarsisc1c0 <- pairs_last[match(pairs_last$morfarsisc1, pairs_last$mumID), "c0ID"]
pairs_last$farmorbroc1c0 <- pairs_last[match(pairs_last$farmorbroc1, pairs_last$mumID), "c0ID"]
pairs_last$farfarsisc1c0 <- pairs_last[match(pairs_last$farfarsisc1, pairs_last$mumID), "c0ID"]

pairs_last$mormorbroc1c1 <- pairs_last[match(pairs_last$mormorbroc1, pairs_last$mumID), "c1ID"]
pairs_last$morfarsisc1c1 <- pairs_last[match(pairs_last$morfarsisc1, pairs_last$mumID), "c1ID"]
pairs_last$farmorbroc1c1 <- pairs_last[match(pairs_last$farmorbroc1, pairs_last$mumID), "c1ID"]
pairs_last$farfarsisc1c1 <- pairs_last[match(pairs_last$farfarsisc1, pairs_last$mumID), "c1ID"]

# sibs
sibs <- data.frame(cbind(i_id = pairs_last$c0ID,
                         j_id = pairs_last$c1ID,
                         rel = "sibs",
                         x = .5))

sibs$pair_id <- paste(pmax(sibs$i_id, sibs$j_id),
                      pmin(sibs$i_id, sibs$j_id), sep = "_")
sibs <- sibs[!duplicated(sibs$pair_id),]

# spouses
spouses <- data.frame(cbind(i_id = pairs_last$mumID,
                            j_id = pairs_last$dadID,
                            rel = "spouses",
                            x = .0))
spouses$pair_id <- paste(pmax(spouses$i_id, spouses$j_id),
                         pmin(spouses$i_id, spouses$j_id), sep = "_")
spouses <- spouses[!duplicated(spouses$pair_id),]

# 1st cousins
first_c <- data.frame(cbind(i_id = c(pairs_last$mumbroc0, pairs_last$mumbroc0, pairs_last$mumbroc1, pairs_last$mumbroc1, 
                                     pairs_last$dadsisc0, pairs_last$dadsisc0, pairs_last$dadsisc1, pairs_last$dadsisc1),
                            j_id = c(pairs_last$c0ID,     pairs_last$c1ID,     pairs_last$c0ID,     pairs_last$c1ID, 
                                     pairs_last$c0ID,     pairs_last$c1ID,     pairs_last$c0ID,     pairs_last$c1ID),
                            rel = "1st cousins",
                            x = .125))
first_c$pair_id <- paste(pmax(first_c$i_id, first_c$j_id),
                         pmin(first_c$i_id, first_c$j_id), sep = "_")
first_c <- first_c[!duplicated(first_c$pair_id),]

# 2nd cousins
second_c <- data.frame(cbind(i_id = c(pairs_last$mormorbroc0c0, pairs_last$mormorbroc0c0, pairs_last$mormorbroc0c1, pairs_last$mormorbroc0c1, 
                                      pairs_last$morfarsisc0c0, pairs_last$morfarsisc0c0, pairs_last$morfarsisc0c1, pairs_last$morfarsisc0c1, 
                                      pairs_last$farmorbroc0c0, pairs_last$farmorbroc0c0, pairs_last$farmorbroc0c1, pairs_last$farmorbroc0c1, 
                                      pairs_last$farfarsisc0c0, pairs_last$farfarsisc0c0, pairs_last$farfarsisc0c1, pairs_last$farfarsisc0c1,
                                      pairs_last$mormorbroc1c0, pairs_last$mormorbroc1c0, pairs_last$mormorbroc1c1, pairs_last$mormorbroc1c1, 
                                      pairs_last$morfarsisc1c0, pairs_last$morfarsisc1c0, pairs_last$morfarsisc1c1, pairs_last$morfarsisc1c1, 
                                      pairs_last$farmorbroc1c0, pairs_last$farmorbroc1c0, pairs_last$farmorbroc1c1, pairs_last$farmorbroc1c1, 
                                      pairs_last$farfarsisc1c0, pairs_last$farfarsisc1c0, pairs_last$farfarsisc1c1, pairs_last$farfarsisc1c1),
                             j_id = c(pairs_last$c0ID,          pairs_last$c1ID,          pairs_last$c0ID,          pairs_last$c1ID, 
                                      pairs_last$c0ID,          pairs_last$c1ID,          pairs_last$c0ID,          pairs_last$c1ID,
                                      pairs_last$c0ID,          pairs_last$c1ID,          pairs_last$c0ID,          pairs_last$c1ID, 
                                      pairs_last$c0ID,          pairs_last$c1ID,          pairs_last$c0ID,          pairs_last$c1ID,
                                      pairs_last$c0ID,          pairs_last$c1ID,          pairs_last$c0ID,          pairs_last$c1ID, 
                                      pairs_last$c0ID,          pairs_last$c1ID,          pairs_last$c0ID,          pairs_last$c1ID,
                                      pairs_last$c0ID,          pairs_last$c1ID,          pairs_last$c0ID,          pairs_last$c1ID, 
                                      pairs_last$c0ID,          pairs_last$c1ID,          pairs_last$c0ID,          pairs_last$c1ID),
                             rel = "2nd cousins",
                             x = .03125))
second_c$pair_id <- paste(pmax(second_c$i_id, second_c$j_id),
                          pmin(second_c$i_id, second_c$j_id), sep = "_")
second_c <- second_c[!duplicated(second_c$pair_id),]

# co-cousins
co_c <- data.frame(cbind(i_id = c(pairs_last$mumbroc0, pairs_last$mumbroc0, pairs_last$mumbroc1, pairs_last$mumbroc1),
                         j_id = c(pairs_last$dadsisc0, pairs_last$dadsisc0, pairs_last$dadsisc1, pairs_last$dadsisc1),
                         rel = "co cousins",
                         x = .0))
co_c$pair_id <- paste(pmax(co_c$i_id, co_c$j_id),
                      pmin(co_c$i_id, co_c$j_id), sep = "_")
co_c <- co_c[!duplicated(co_c$pair_id),]


# create big data frame with all the pairs of relatives

rel <- rbind(sibs, spouses, first_c, second_c, co_c)
rel$dup <- 0
rel[rel$pair_id %in% rel$pair_id[duplicated(rel$pair_id)], "dup"] <- 1
# table(rel$dup)
# rel[rel$dup == 1,]
rel <- rel[rel$dup == 0,]
rel$i_id <- as.numeric(rel$i_id)
rel$j_id <- as.numeric(rel$j_id)
rel$x    <- as.numeric(rel$x)

# get pi-hat for everyone

rel_cal <- function(person1, person2, mafs){
  return(mean((person1 - 2 * mafs) / sqrt(2 * mafs * (1 - mafs))*
                (person2 - 2 * mafs) / sqrt(2 * mafs * (1 - mafs))))
}

# pi hat based on all SNPs
rel$pi_hat <- NA
for(i in 1:nrow(rel)){
  rel[i, "pi_hat"] <- rel_cal(XO[as.numeric(rel[i, "i_id"]), ], 
                              XO[as.numeric(rel[i, "j_id"]), ], 
                              maf.vector)
}

# only causal SNPs
rel$pi_hat_c <- NA
for(i in 1:nrow(rel)){
  rel[i, "pi_hat_c"] <- rel_cal(XO[as.numeric(rel[i, "i_id"]), 1:num.cvs],
                                XO[as.numeric(rel[i, "j_id"]), 1:num.cvs], 
                                maf.vector[1:num.cvs])
}

# only non-causal SNPs
rel$pi_hat_nc <- NA
for(i in 1:nrow(rel)){
  rel[i, "pi_hat_nc"] <- rel_cal(XO[as.numeric(rel[i, "i_id"]), (num.cvs+1):num.vs],
                                 XO[as.numeric(rel[i, "j_id"]), (num.cvs+1):num.vs],
                                 maf.vector[(num.cvs+1):num.vs])
}

# pi hats expressed as deviations from expectations ("x")
rel$pi_hat_d    <- rel$pi_hat    - rel$x 
rel$pi_hat_c_d  <- rel$pi_hat_c  - rel$x
rel$pi_hat_nc_d <- rel$pi_hat_nc - rel$x

# standardized scores on phenotype
reg$zp <- scale(reg$P)
rel$i_zp <- reg[match(rel$i_id, reg$ID), "A"]
rel$j_zp <- reg[match(rel$j_id, reg$ID), "A"]

# genotypic values
rel$i_A <- reg[match(rel$i_id, reg$ID), "A"] 
rel$j_A <- reg[match(rel$j_id, reg$ID), "A"]

# product of standardized scores
rel$z1z2 <- rel$i_zp * rel$j_zp 
rel$a1a2 <- rel$i_zp * rel$j_zp 

# "traditional" correlations
cor.test(rel[rel$x == .5, "i_zp"], 
         rel[rel$x == .5, "j_zp"])

cor.test(rel[rel$x == .125, "i_zp"], 
         rel[rel$x == .125, "j_zp"])

cor.test(rel[rel$x == .03125, "i_zp"], 
         rel[rel$x == .03125, "j_zp"])

cor.test(rel[rel$rel == "co cousins", "i_zp"], 
         rel[rel$rel == "co cousins", "j_zp"])


# predicting z1z2 from expected pi hat ("x") and deviations from expected pi hat ("pi_hat_d")
summary(lm(z1z2 ~ x + pi_hat_d, rel))
summary(lm(z1z2 ~ x + pi_hat_c_d, rel))
summary(lm(z1z2 ~ x + pi_hat_nc_d, rel))

# relatedness type as a categorical predictor
summary(lm(z1z2 ~ rel + pi_hat_d, rel))
summary(lm(z1z2 ~ rel + pi_hat_c_d, rel))
summary(lm(z1z2 ~ rel + pi_hat_nc_d, rel))

## run analyses separately for different kinds of relatives
table(rel$rel)
type <- "sibs"
type <- "1st cousins"
type <- "2nd cousins"
type <- "co cousins"
type <- "spouses"

# running analyses for only relatives of the type given by "type"
summary(lm(z1z2 ~ pi_hat_d,        rel[rel$rel == type,]))
summary(lm(z1z2 ~ pi_hat_c_d,      rel[rel$rel == type,]))
summary(lm(z1z2 ~ pi_hat_nc_d,     rel[rel$rel == type,]))

# running analyses for everyone except relatives of the type "type"
summary(lm(z1z2 ~ x + pi_hat_d,    rel[rel$rel != type,]))
summary(lm(z1z2 ~ x + pi_hat_c_d,  rel[rel$rel != type,]))
summary(lm(z1z2 ~ x + pi_hat_nc_d, rel[rel$rel != type,]))

# phenotypic variance over generations
for(i in 0:num.gen){
  print(var(reg[reg$gen == i, "P"]))
}

# genotypic variance over generations
for(i in 0:num.gen){
  print(var(reg[reg$gen == i, "A"]))
}



