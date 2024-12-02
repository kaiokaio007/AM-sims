
# island sim II

# what happens when using pgs instead of direct values of A? 

rm(list = ls())
n <- 10000      # people per island
i <- 10        # nr of islands
move_p <- .05  # proportion of people moving

a_t0 <- .5
e_t0 <- .5

gen0 <- as.data.frame(cbind(rnorm(n*i), rnorm(n*i)))
names(gen0) <- c("a", "e")
gen0$p <- gen0$a*sqrt(a_t0) + gen0$e*sqrt(e_t0)
gen0$sex <- rep(0:1, (n*i)/2)
gen0$ID <- 1:(n*i)
gen0$i <- rep(1:i, rep(n, i))

females <- gen0[gen0$sex == 0, c(1,2,3,5,6)]
names(females) <- c("mumA", "mumE", "mumP", "mumID", "mumi")

males <- gen0[gen0$sex == 1, c(1,2,3,5,6)]
names(males) <- c("dadA", "dadE", "dadP", "dadID", "dadi")

# randomly select a specified percentage of the people on each island
# to put into a group that will move, and distribute this group on islands according to their rank

# n*move_p people per i

movers_f <- vector()
for(x in 1:i){
  movers_f[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
females$mover_f <- 0
females[movers_f, "mover_f"] <- 1
females$mumi2 <- females$mumi
females[movers_f[order(females[movers_f, "mumP"])], "mumi2"] <- rep(1:i, rep((n/2)*move_p, i))
females[females$mover_f == 1, ] <- females[movers_f[order(females[movers_f, "mumP"])], ][]

movers_m <- vector()
for(x in 1:i){
  movers_m[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
males$mover_m <- 0
males[movers_m, "mover_m"] <- 1
males$dadi2 <- males$dadi
males[movers_m[order(males[movers_m, "dadP"])], "dadi2"] <- rep(1:i, rep((n/2)*move_p, i))
males[males$mover_m == 1, ] <- males[movers_m[order(males[movers_m, "dadP"])], ][]


# make pairs

s_v <- vector()

nn <- n/2

# for(x in 1:i){
#   s_v[(nn*(x-1)+1):(nn*x)] <- c(nn*x, (nn*(x-1)+1):(nn*x-1))
# }

for(x in 1:i){
  s_v[(nn*(x-1)+1):(nn*x)] <- sample((nn*(x-1)+1):(nn*x))
}

pairs1 <- data.frame(cbind(females,
                           males[s_v,]))

# produce two children per pair, one boy one girl
pairs1$c0A <- (pairs1$mumA + pairs1$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs1$c0E <- rnorm(n/2) 
pairs1$c1A <- (pairs1$mumA + pairs1$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs1$c1E <- rnorm(n/2)

pairs1$c0P <- pairs1$c0A*sqrt(.5) + pairs1$c0E*sqrt(.5)
pairs1$c1P <- pairs1$c1A*sqrt(.5) + pairs1$c1E*sqrt(.5)

nt <- n*i

pairs1$c0ID <- (nt+1):(nt+nt/2)
pairs1$c1ID <- ((nt+nt/2)+1):(2*nt)


# gen2
females <- pairs1[, c("c0A", "c0E", "c0P", "c0ID", "mumi2")]
names(females) <- c("mumA", "mumE", "mumP", "mumID", "mumi")

males <- pairs1[, c("c1A", "c1E", "c1P", "c1ID", "dadi2")]
names(males) <- c("dadA", "dadE", "dadP", "dadID", "dadi")

movers_f <- vector()
for(x in 1:i){
  movers_f[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
females$mover_f <- 0
females[movers_f, "mover_f"] <- 1
females$mumi2 <- females$mumi
females[movers_f[order(females[movers_f, "mumP"])], "mumi2"] <- rep(1:i, rep((n/2)*move_p, i))
females[females$mover_f == 1, ] <- females[movers_f[order(females[movers_f, "mumP"])], ][]

movers_m <- vector()
for(x in 1:i){
  movers_m[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
males$mover_m <- 0
males[movers_m, "mover_m"] <- 1
males$dadi2 <- males$dadi
males[movers_m[order(males[movers_m, "dadP"])], "dadi2"] <- rep(1:i, rep((n/2)*move_p, i))
males[males$mover_m == 1, ] <- males[movers_m[order(males[movers_m, "dadP"])], ][]

for(x in 1:i){
  s_v[(nn*(x-1)+1):(nn*x)] <- sample((nn*(x-1)+1):(nn*x))
}

pairs2 <- data.frame(cbind(females, 
                           males[s_v,]))

# produce two children per pair, one boy one girl
pairs2$c0A <- (pairs2$mumA + pairs2$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs2$c0E <- rnorm(n/2) 
pairs2$c1A <- (pairs2$mumA + pairs2$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs2$c1E <- rnorm(n/2)

pairs2$c0P <- pairs2$c0A*sqrt(.5) + pairs2$c0E*sqrt(.5)
pairs2$c1P <- pairs2$c1A*sqrt(.5) + pairs2$c1E*sqrt(.5)

pairs2$c0ID <- ((2*nt)+1):((2*nt)+nt/2)
pairs2$c1ID <- (((2*nt)+nt/2)+1):(3*nt)

# gen3
females <- pairs2[, c("c0A", "c0E", "c0P", "c0ID", "mumi2")]
names(females) <- c("mumA", "mumE", "mumP", "mumID", "mumi")

males <- pairs2[, c("c1A", "c1E", "c1P", "c1ID", "dadi2")]
names(males) <- c("dadA", "dadE", "dadP", "dadID", "dadi")

movers_f <- vector()
for(x in 1:i){
  movers_f[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
females$mover_f <- 0
females[movers_f, "mover_f"] <- 1
females$mumi2 <- females$mumi
females[movers_f[order(females[movers_f, "mumP"])], "mumi2"] <- rep(1:i, rep((n/2)*move_p, i))
females[females$mover_f == 1, ] <- females[movers_f[order(females[movers_f, "mumP"])], ][]

movers_m <- vector()
for(x in 1:i){
  movers_m[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
males$mover_m <- 0
males[movers_m, "mover_m"] <- 1
males$dadi2 <- males$dadi
males[movers_m[order(males[movers_m, "dadP"])], "dadi2"] <- rep(1:i, rep((n/2)*move_p, i))
males[males$mover_m == 1, ] <- males[movers_m[order(males[movers_m, "dadP"])], ][]

for(x in 1:i){
  s_v[(nn*(x-1)+1):(nn*x)] <- sample((nn*(x-1)+1):(nn*x))
}

pairs3 <- data.frame(cbind(females, 
                           males[s_v,]))

# produce two children per pair, one boy one girl
pairs3$c0A <- (pairs3$mumA + pairs3$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs3$c0E <- rnorm(n/2) 
pairs3$c1A <- (pairs3$mumA + pairs3$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs3$c1E <- rnorm(n/2)

pairs3$c0P <- pairs3$c0A*sqrt(.5) + pairs3$c0E*sqrt(.5)
pairs3$c1P <- pairs3$c1A*sqrt(.5) + pairs3$c1E*sqrt(.5)

pairs3$c0ID <- ((3*nt)+1):((3*nt)+nt/2)
pairs3$c1ID <- (((3*nt)+nt/2)+1):(4*nt)

# gen4
females <- pairs3[, c("c0A", "c0E", "c0P", "c0ID", "mumi2")]
names(females) <- c("mumA", "mumE", "mumP", "mumID", "mumi")

males <- pairs3[, c("c1A", "c1E", "c1P", "c1ID", "dadi2")]
names(males) <- c("dadA", "dadE", "dadP", "dadID", "dadi")

movers_f <- vector()
for(x in 1:i){
  movers_f[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
females$mover_f <- 0
females[movers_f, "mover_f"] <- 1
females$mumi2 <- females$mumi
females[movers_f[order(females[movers_f, "mumP"])], "mumi2"] <- rep(1:i, rep((n/2)*move_p, i))
females[females$mover_f == 1, ] <- females[movers_f[order(females[movers_f, "mumP"])], ][]

movers_m <- vector()
for(x in 1:i){
  movers_m[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
males$mover_m <- 0
males[movers_m, "mover_m"] <- 1
males$dadi2 <- males$dadi
males[movers_m[order(males[movers_m, "dadP"])], "dadi2"] <- rep(1:i, rep((n/2)*move_p, i))
males[males$mover_m == 1, ] <- males[movers_m[order(males[movers_m, "dadP"])], ][]

for(x in 1:i){
  s_v[(nn*(x-1)+1):(nn*x)] <- sample((nn*(x-1)+1):(nn*x))
}

pairs4 <- data.frame(cbind(females, 
                           males[s_v,]))

# produce two children per pair, one boy one girl
pairs4$c0A <- (pairs4$mumA + pairs4$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs4$c0E <- rnorm(n/2) 
pairs4$c1A <- (pairs4$mumA + pairs4$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs4$c1E <- rnorm(n/2)

pairs4$c0P <- pairs4$c0A*sqrt(.5) + pairs4$c0E*sqrt(.5)
pairs4$c1P <- pairs4$c1A*sqrt(.5) + pairs4$c1E*sqrt(.5)

pairs4$c0ID <- ((4*nt)+1):((4*nt)+nt/2)
pairs4$c1ID <- (((4*nt)+nt/2)+1):(5*nt)

# gen5
females <- pairs4[, c("c0A", "c0E", "c0P", "c0ID", "mumi2")]
names(females) <- c("mumA", "mumE", "mumP", "mumID", "mumi")

males <- pairs4[, c("c1A", "c1E", "c1P", "c1ID", "dadi2")]
names(males) <- c("dadA", "dadE", "dadP", "dadID", "dadi")

movers_f <- vector()
for(x in 1:i){
  movers_f[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
females$mover_f <- 0
females[movers_f, "mover_f"] <- 1
females$mumi2 <- females$mumi
females[movers_f[order(females[movers_f, "mumP"])], "mumi2"] <- rep(1:i, rep((n/2)*move_p, i))
females[females$mover_f == 1, ] <- females[movers_f[order(females[movers_f, "mumP"])], ][]

movers_m <- vector()
for(x in 1:i){
  movers_m[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
males$mover_m <- 0
males[movers_m, "mover_m"] <- 1
males$dadi2 <- males$dadi
males[movers_m[order(males[movers_m, "dadP"])], "dadi2"] <- rep(1:i, rep((n/2)*move_p, i))
males[males$mover_m == 1, ] <- males[movers_m[order(males[movers_m, "dadP"])], ][]

for(x in 1:i){
  s_v[(nn*(x-1)+1):(nn*x)] <- sample((nn*(x-1)+1):(nn*x))
}

pairs5 <- data.frame(cbind(females, 
                           males[s_v,]))

# produce two children per pair, one boy one girl
pairs5$c0A <- (pairs5$mumA + pairs5$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs5$c0E <- rnorm(n/2) 
pairs5$c1A <- (pairs5$mumA + pairs5$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs5$c1E <- rnorm(n/2)

pairs5$c0P <- pairs5$c0A*sqrt(.5) + pairs5$c0E*sqrt(.5)
pairs5$c1P <- pairs5$c1A*sqrt(.5) + pairs5$c1E*sqrt(.5)

pairs5$c0ID <- ((5*nt)+1):((5*nt)+nt/2)
pairs5$c1ID <- (((5*nt)+nt/2)+1):(6*nt)

# gen6
females <- pairs5[, c("c0A", "c0E", "c0P", "c0ID", "mumi2")]
names(females) <- c("mumA", "mumE", "mumP", "mumID", "mumi")

males <- pairs5[, c("c1A", "c1E", "c1P", "c1ID", "dadi2")]
names(males) <- c("dadA", "dadE", "dadP", "dadID", "dadi")

movers_f <- vector()
for(x in 1:i){
  movers_f[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
females$mover_f <- 0
females[movers_f, "mover_f"] <- 1
females$mumi2 <- females$mumi
females[movers_f[order(females[movers_f, "mumP"])], "mumi2"] <- rep(1:i, rep((n/2)*move_p, i))
females[females$mover_f == 1, ] <- females[movers_f[order(females[movers_f, "mumP"])], ][]

movers_m <- vector()
for(x in 1:i){
  movers_m[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
males$mover_m <- 0
males[movers_m, "mover_m"] <- 1
males$dadi2 <- males$dadi
males[movers_m[order(males[movers_m, "dadP"])], "dadi2"] <- rep(1:i, rep((n/2)*move_p, i))
males[males$mover_m == 1, ] <- males[movers_m[order(males[movers_m, "dadP"])], ][]

for(x in 1:i){
  s_v[(nn*(x-1)+1):(nn*x)] <- sample((nn*(x-1)+1):(nn*x))
}

pairs6 <- data.frame(cbind(females, 
                           males[s_v,]))

# produce two children per pair, one boy one girl
pairs6$c0A <- (pairs6$mumA + pairs6$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs6$c0E <- rnorm(n/2) 
pairs6$c1A <- (pairs6$mumA + pairs6$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs6$c1E <- rnorm(n/2)

pairs6$c0P <- pairs6$c0A*sqrt(.5) + pairs6$c0E*sqrt(.5)
pairs6$c1P <- pairs6$c1A*sqrt(.5) + pairs6$c1E*sqrt(.5)

pairs6$c0ID <- ((6*nt)+1):((6*nt)+nt/2)
pairs6$c1ID <- (((6*nt)+nt/2)+1):(7*nt)

# gen7
females <- pairs6[, c("c0A", "c0E", "c0P", "c0ID", "mumi2")]
names(females) <- c("mumA", "mumE", "mumP", "mumID", "mumi")

males <- pairs6[, c("c1A", "c1E", "c1P", "c1ID", "dadi2")]
names(males) <- c("dadA", "dadE", "dadP", "dadID", "dadi")

movers_f <- vector()
for(x in 1:i){
  movers_f[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
females$mover_f <- 0
females[movers_f, "mover_f"] <- 1
females$mumi2 <- females$mumi
females[movers_f[order(females[movers_f, "mumP"])], "mumi2"] <- rep(1:i, rep((n/2)*move_p, i))
females[females$mover_f == 1, ] <- females[movers_f[order(females[movers_f, "mumP"])], ][]

movers_m <- vector()
for(x in 1:i){
  movers_m[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
males$mover_m <- 0
males[movers_m, "mover_m"] <- 1
males$dadi2 <- males$dadi
males[movers_m[order(males[movers_m, "dadP"])], "dadi2"] <- rep(1:i, rep((n/2)*move_p, i))
males[males$mover_m == 1, ] <- males[movers_m[order(males[movers_m, "dadP"])], ][]

for(x in 1:i){
  s_v[(nn*(x-1)+1):(nn*x)] <- sample((nn*(x-1)+1):(nn*x))
}

pairs7 <- data.frame(cbind(females, 
                           males[s_v,]))

# produce two children per pair, one boy one girl
pairs7$c0A <- (pairs7$mumA + pairs7$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs7$c0E <- rnorm(n/2) 
pairs7$c1A <- (pairs7$mumA + pairs7$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs7$c1E <- rnorm(n/2)

pairs7$c0P <- pairs7$c0A*sqrt(.5) + pairs7$c0E*sqrt(.5)
pairs7$c1P <- pairs7$c1A*sqrt(.5) + pairs7$c1E*sqrt(.5)

pairs7$c0ID <- ((7*nt)+1):((7*nt)+nt/2)
pairs7$c1ID <- (((7*nt)+nt/2)+1):(8*nt)

# gen8
females <- pairs7[, c("c0A", "c0E", "c0P", "c0ID", "mumi2")]
names(females) <- c("mumA", "mumE", "mumP", "mumID", "mumi")

males <- pairs7[, c("c1A", "c1E", "c1P", "c1ID", "dadi2")]
names(males) <- c("dadA", "dadE", "dadP", "dadID", "dadi")

movers_f <- vector()
for(x in 1:i){
  movers_f[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
females$mover_f <- 0
females[movers_f, "mover_f"] <- 1
females$mumi2 <- females$mumi
females[movers_f[order(females[movers_f, "mumP"])], "mumi2"] <- rep(1:i, rep((n/2)*move_p, i))
females[females$mover_f == 1, ] <- females[movers_f[order(females[movers_f, "mumP"])], ][]

movers_m <- vector()
for(x in 1:i){
  movers_m[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
males$mover_m <- 0
males[movers_m, "mover_m"] <- 1
males$dadi2 <- males$dadi
males[movers_m[order(males[movers_m, "dadP"])], "dadi2"] <- rep(1:i, rep((n/2)*move_p, i))
males[males$mover_m == 1, ] <- males[movers_m[order(males[movers_m, "dadP"])], ][]

for(x in 1:i){
  s_v[(nn*(x-1)+1):(nn*x)] <- sample((nn*(x-1)+1):(nn*x))
}

pairs8 <- data.frame(cbind(females, 
                           males[s_v,]))

# produce two children per pair, one boy one girl
pairs8$c0A <- (pairs8$mumA + pairs8$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs8$c0E <- rnorm(n/2) 
pairs8$c1A <- (pairs8$mumA + pairs8$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs8$c1E <- rnorm(n/2)

pairs8$c0P <- pairs8$c0A*sqrt(.5) + pairs8$c0E*sqrt(.5)
pairs8$c1P <- pairs8$c1A*sqrt(.5) + pairs8$c1E*sqrt(.5)

pairs8$c0ID <- ((8*nt)+1):((8*nt)+nt/2)
pairs8$c1ID <- (((8*nt)+nt/2)+1):(9*nt)

# gen9
females <- pairs8[, c("c0A", "c0E", "c0P", "c0ID", "mumi2")]
names(females) <- c("mumA", "mumE", "mumP", "mumID", "mumi")

males <- pairs8[, c("c1A", "c1E", "c1P", "c1ID", "dadi2")]
names(males) <- c("dadA", "dadE", "dadP", "dadID", "dadi")

movers_f <- vector()
for(x in 1:i){
  movers_f[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
females$mover_f <- 0
females[movers_f, "mover_f"] <- 1
females$mumi2 <- females$mumi
females[movers_f[order(females[movers_f, "mumP"])], "mumi2"] <- rep(1:i, rep((n/2)*move_p, i))
females[females$mover_f == 1, ] <- females[movers_f[order(females[movers_f, "mumP"])], ][]

movers_m <- vector()
for(x in 1:i){
  movers_m[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
males$mover_m <- 0
males[movers_m, "mover_m"] <- 1
males$dadi2 <- males$dadi
males[movers_m[order(males[movers_m, "dadP"])], "dadi2"] <- rep(1:i, rep((n/2)*move_p, i))
males[males$mover_m == 1, ] <- males[movers_m[order(males[movers_m, "dadP"])], ][]

for(x in 1:i){
  s_v[(nn*(x-1)+1):(nn*x)] <- sample((nn*(x-1)+1):(nn*x))
}

pairs9 <- data.frame(cbind(females, 
                           males[s_v,]))

# produce two children per pair, one boy one girl
pairs9$c0A <- (pairs9$mumA + pairs9$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs9$c0E <- rnorm(n/2) 
pairs9$c1A <- (pairs9$mumA + pairs9$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs9$c1E <- rnorm(n/2)

pairs9$c0P <- pairs9$c0A*sqrt(.5) + pairs9$c0E*sqrt(.5)
pairs9$c1P <- pairs9$c1A*sqrt(.5) + pairs9$c1E*sqrt(.5)

pairs9$c0ID <- ((9*nt)+1):((9*nt)+nt/2)
pairs9$c1ID <- (((9*nt)+nt/2)+1):(10*nt)

# gen10
females <- pairs9[, c("c0A", "c0E", "c0P", "c0ID", "mumi2")]
names(females) <- c("mumA", "mumE", "mumP", "mumID", "mumi")

males <- pairs9[, c("c1A", "c1E", "c1P", "c1ID", "dadi2")]
names(males) <- c("dadA", "dadE", "dadP", "dadID", "dadi")

movers_f <- vector()
for(x in 1:i){
  movers_f[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
females$mover_f <- 0
females[movers_f, "mover_f"] <- 1
females$mumi2 <- females$mumi
females[movers_f[order(females[movers_f, "mumP"])], "mumi2"] <- rep(1:i, rep((n/2)*move_p, i))
females[females$mover_f == 1, ] <- females[movers_f[order(females[movers_f, "mumP"])], ][]

movers_m <- vector()
for(x in 1:i){
  movers_m[((n/2)*(x-1)*move_p+1):((n/2)*x*move_p)] <- sample(((n/2)*(x-1)+1):((n/2)*(x)), (n/2)*move_p)
}
males$mover_m <- 0
males[movers_m, "mover_m"] <- 1
males$dadi2 <- males$dadi
males[movers_m[order(males[movers_m, "dadP"])], "dadi2"] <- rep(1:i, rep((n/2)*move_p, i))
males[males$mover_m == 1, ] <- males[movers_m[order(males[movers_m, "dadP"])], ][]

for(x in 1:i){
  s_v[(nn*(x-1)+1):(nn*x)] <- sample((nn*(x-1)+1):(nn*x))
}

pairs10 <- data.frame(cbind(females, 
                           males[s_v,]))

# produce two children per pair, one boy one girl
pairs10$c0A <- (pairs10$mumA + pairs10$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs10$c0E <- rnorm(n/2) 
pairs10$c1A <- (pairs10$mumA + pairs10$dadA)*.5 + rnorm(n/2,0,sqrt(.5)) 
pairs10$c1E <- rnorm(n/2)

pairs10$c0P <- pairs10$c0A*sqrt(.5) + pairs10$c0E*sqrt(.5)
pairs10$c1P <- pairs10$c1A*sqrt(.5) + pairs10$c1E*sqrt(.5)

pairs10$c0ID <- ((10*nt)+1):((10*nt)+nt/2)
pairs10$c1ID <- (((10*nt)+nt/2)+1):(11*nt)



# I'll put lots of relatives into pairs10
pairs10$mumbro <- pairs9[match(pairs10$mumID, pairs9$c0ID), "c1ID"]
pairs10$dadsis <- pairs9[match(pairs10$dadID, pairs9$c1ID), "c0ID"]

pairs10$mumbroc0 <- pairs10[match(pairs10$mumbro, pairs10$dadID), "c0ID"]
pairs10$dadsisc0 <- pairs10[match(pairs10$dadsis, pairs10$mumID), "c0ID"]

pairs10$mumbroc1 <- pairs10[match(pairs10$mumbro, pairs10$dadID), "c1ID"]
pairs10$dadsisc1 <- pairs10[match(pairs10$dadsis, pairs10$mumID), "c1ID"]

pairs10$mumbrospo <- pairs10[match(pairs10$mumbro, pairs10$dadID), "mumID"]
pairs10$dadsisspo <- pairs10[match(pairs10$dadsis, pairs10$mumID), "dadID"]

pairs10$mumbrospobro <- pairs9[match(pairs10$mumbrospo, pairs9$c0ID), "c1ID"]
pairs10$dadsissposis <- pairs9[match(pairs10$dadsisspo, pairs9$c1ID), "c0ID"]

pairs10$mumbrospobrospo <- pairs10[match(pairs10$mumbrospobro, pairs10$dadID), "mumID"]
pairs10$dadsissposisspo <- pairs10[match(pairs10$dadsissposis, pairs10$mumID), "dadID"]

pairs10$mumbrospobroc0 <- pairs10[match(pairs10$mumbrospobro, pairs10$dadID), "c0ID"]
pairs10$dadsissposisc0 <- pairs10[match(pairs10$mumbrospobro, pairs10$dadID), "c0ID"]

pairs10$mumbrospobroc1 <- pairs10[match(pairs10$mumbrospobro, pairs10$dadID), "c1ID"]
pairs10$dadsissposisc1 <- pairs10[match(pairs10$mumbrospobro, pairs10$dadID), "c1ID"]

pairs10$mormor <- pairs9[match(pairs10$mumID, pairs9$c0ID), "mumID"]
pairs10$morfar <- pairs9[match(pairs10$mumID, pairs9$c0ID), "dadID"]
pairs10$farmor <- pairs9[match(pairs10$dadID, pairs9$c1ID), "mumID"]
pairs10$farfar <- pairs9[match(pairs10$dadID, pairs9$c1ID), "dadID"]

pairs10$mormorbro <- pairs8[match(pairs10$mormor, pairs8$c0ID), "c1ID"]
pairs10$morfarsis <- pairs8[match(pairs10$morfar, pairs8$c1ID), "c0ID"]
pairs10$farmorbro <- pairs8[match(pairs10$farmor, pairs8$c0ID), "c1ID"]
pairs10$farfarsis <- pairs8[match(pairs10$farfar, pairs8$c1ID), "c0ID"]

pairs10$mormorbroc0 <- pairs9[match(pairs10$mormorbro, pairs9$dadID), "c0ID"]
pairs10$morfarsisc0 <- pairs9[match(pairs10$morfarsis, pairs9$mumID), "c0ID"]
pairs10$farmorbroc0 <- pairs9[match(pairs10$farmorbro, pairs9$dadID), "c0ID"]
pairs10$farfarsisc0 <- pairs9[match(pairs10$farfarsis, pairs9$mumID), "c0ID"]

pairs10$mormorbroc1 <- pairs9[match(pairs10$mormorbro, pairs9$dadID), "c1ID"]
pairs10$morfarsisc1 <- pairs9[match(pairs10$morfarsis, pairs9$mumID), "c1ID"]
pairs10$farmorbroc1 <- pairs9[match(pairs10$farmorbro, pairs9$dadID), "c1ID"]
pairs10$farfarsisc1 <- pairs9[match(pairs10$farfarsis, pairs9$mumID), "c1ID"]

pairs10$mormorbroc0c0 <- pairs10[match(pairs10$mormorbroc0, pairs10$mumID), "c0ID"]
pairs10$morfarsisc0c0 <- pairs10[match(pairs10$morfarsisc0, pairs10$mumID), "c0ID"]
pairs10$farmorbroc0c0 <- pairs10[match(pairs10$farmorbroc0, pairs10$mumID), "c0ID"]
pairs10$farfarsisc0c0 <- pairs10[match(pairs10$farfarsisc0, pairs10$mumID), "c0ID"]

pairs10$mormorbroc0c1 <- pairs10[match(pairs10$mormorbroc0, pairs10$mumID), "c1ID"]
pairs10$morfarsisc0c1 <- pairs10[match(pairs10$morfarsisc0, pairs10$mumID), "c1ID"]
pairs10$farmorbroc0c1 <- pairs10[match(pairs10$farmorbroc0, pairs10$mumID), "c1ID"]
pairs10$farfarsisc0c1 <- pairs10[match(pairs10$farfarsisc0, pairs10$mumID), "c1ID"]

pairs10$mormorbroc1c0 <- pairs10[match(pairs10$mormorbroc1, pairs10$mumID), "c0ID"]
pairs10$morfarsisc1c0 <- pairs10[match(pairs10$morfarsisc1, pairs10$mumID), "c0ID"]
pairs10$farmorbroc1c0 <- pairs10[match(pairs10$farmorbroc1, pairs10$mumID), "c0ID"]
pairs10$farfarsisc1c0 <- pairs10[match(pairs10$farfarsisc1, pairs10$mumID), "c0ID"]

pairs10$mormorbroc1c1 <- pairs10[match(pairs10$mormorbroc1, pairs10$mumID), "c1ID"]
pairs10$morfarsisc1c1 <- pairs10[match(pairs10$morfarsisc1, pairs10$mumID), "c1ID"]
pairs10$farmorbroc1c1 <- pairs10[match(pairs10$farmorbroc1, pairs10$mumID), "c1ID"]
pairs10$farfarsisc1c1 <- pairs10[match(pairs10$farfarsisc1, pairs10$mumID), "c1ID"]

#vector with all phenotypes, ordered by ID

phenos <- c(gen0$p, 
            pairs1$c0P, pairs1$c1P, 
            pairs2$c0P, pairs2$c1P, 
            pairs3$c0P, pairs3$c1P, 
            pairs4$c0P, pairs4$c1P, 
            pairs5$c0P, pairs5$c1P, 
            pairs6$c0P, pairs6$c1P, 
            pairs7$c0P, pairs7$c1P, 
            pairs8$c0P, pairs8$c1P, 
            pairs9$c0P, pairs9$c1P, 
            pairs10$c0P, pairs10$c1P)


genos <- c(gen0$a, 
           pairs1$c0A, pairs1$c1A, 
           pairs2$c0A, pairs2$c1A, 
           pairs3$c0A, pairs3$c1A, 
           pairs4$c0A, pairs4$c1A, 
           pairs5$c0A, pairs5$c1A, 
           pairs6$c0A, pairs6$c1A, 
           pairs7$c0A, pairs7$c1A, 
           pairs8$c0A, pairs8$c1A, 
           pairs9$c0A, pairs9$c1A, 
           pairs10$c0A, pairs10$c1A)

test <- c(gen0$ID, 
          pairs1$c0ID, pairs1$c1ID, 
          pairs2$c0ID, pairs2$c1ID, 
          pairs3$c0ID, pairs3$c1ID, 
          pairs4$c0ID, pairs4$c1ID, 
          pairs5$c0ID, pairs5$c1ID, 
          pairs6$c0ID, pairs6$c1ID, 
          pairs7$c0ID, pairs7$c1ID, 
          pairs8$c0ID, pairs8$c1ID, 
          pairs9$c0ID, pairs9$c1ID, 
          pairs10$c0ID, pairs10$c1ID)


test[9443]


# correlations

#spouses
cor.test(phenos[pairs10$mumID], phenos[pairs10$dadID])

#sibs
cor.test(phenos[pairs10$c0ID], phenos[pairs10$c1ID])

#1st cousins
cor.test(phenos[pairs10$mumbroc0], phenos[pairs10$c0ID])
cor.test(phenos[pairs10$mumbroc0], phenos[pairs10$c1ID])
cor.test(phenos[pairs10$mumbroc1], phenos[pairs10$c0ID])
cor.test(phenos[pairs10$mumbroc1], phenos[pairs10$c1ID])
cor.test(phenos[pairs10$dadsisc0], phenos[pairs10$c0ID])
cor.test(phenos[pairs10$dadsisc0], phenos[pairs10$c1ID])
cor.test(phenos[pairs10$dadsisc1], phenos[pairs10$c0ID])
cor.test(phenos[pairs10$dadsisc1], phenos[pairs10$c1ID])

#2nd cousins
cor.test(phenos[pairs10$mormorbroc0c0], phenos[pairs10$c0ID])
cor.test(phenos[pairs10$mormorbroc0c0], phenos[pairs10$c1ID])
cor.test(phenos[pairs10$mormorbroc0c1], phenos[pairs10$c0ID])
cor.test(phenos[pairs10$mormorbroc0c1], phenos[pairs10$c1ID])

#cocousins
cor.test(phenos[pairs10$mumbroc0], phenos[pairs10$dadsisc0])
cor.test(phenos[pairs10$mumbroc0], phenos[pairs10$dadsisc1])
cor.test(phenos[pairs10$mumbroc1], phenos[pairs10$dadsisc0])
cor.test(phenos[pairs10$mumbroc1], phenos[pairs10$dadsisc1])

#spouses
cor.test(phenos[pairs10$mumID], phenos[pairs10$dadID])
cor.test(genos[pairs10$mumID], genos[pairs10$dadID])

#sibs
cor.test(phenos[pairs10$c0ID], phenos[pairs10$c1ID])
cor.test(genos[pairs10$c0ID], genos[pairs10$c1ID])

#1st cousins
cor.test(genos[pairs10$mumbroc0], genos[pairs10$c0ID])
cor.test(genos[pairs10$mumbroc0], genos[pairs10$c1ID])
cor.test(genos[pairs10$mumbroc1], genos[pairs10$c0ID])
cor.test(genos[pairs10$mumbroc1], genos[pairs10$c1ID])
cor.test(genos[pairs10$dadsisc0], genos[pairs10$c0ID])
cor.test(genos[pairs10$dadsisc0], genos[pairs10$c1ID])
cor.test(genos[pairs10$dadsisc1], genos[pairs10$c0ID])
cor.test(genos[pairs10$dadsisc1], genos[pairs10$c1ID])

#2nd cousins
cor.test(genos[pairs10$mormorbroc0c0], genos[pairs10$c0ID])
cor.test(genos[pairs10$mormorbroc0c0], genos[pairs10$c1ID])
cor.test(genos[pairs10$mormorbroc0c1], genos[pairs10$c0ID])
cor.test(genos[pairs10$mormorbroc0c1], genos[pairs10$c1ID])

#cocousins
cor.test(genos[pairs10$mumbroc0], genos[pairs10$dadsisc0])
cor.test(genos[pairs10$mumbroc0], genos[pairs10$dadsisc1])
cor.test(genos[pairs10$mumbroc1], genos[pairs10$dadsisc0])
cor.test(genos[pairs10$mumbroc1], genos[pairs10$dadsisc1])




library(plyr)
ddply(pairs10, c("mumi2"), summarize, mean(mumP), mean(mumA))

summary(lm(c1P ~ c1A, pairs10))
summary(lm(c1P ~ c1E, pairs10))


mean(pairs1 [ pairs1$mumi2 == 9 , "mumA"])
mean(pairs2 [ pairs2$mumi2 == 9 , "mumA"])
mean(pairs3 [ pairs3$mumi2 == 9 , "mumA"])
mean(pairs4 [ pairs4$mumi2 == 9 , "mumA"])
mean(pairs5 [ pairs5$mumi2 == 9 , "mumA"])
mean(pairs6 [ pairs6$mumi2 == 9 , "mumA"])
mean(pairs7 [ pairs7$mumi2 == 9 , "mumA"])
mean(pairs8 [ pairs8$mumi2 == 9 , "mumA"])
mean(pairs9 [ pairs9$mumi2 == 9 , "mumA"])
mean(pairs10[pairs10$mumi2 == 9 , "mumA"])





# make some twins

pairs10$c02A <- pairs10$c0A
pairs10$c02E <- rnorm(n/2) 
pairs10$c02P <- pairs10$c02A*sqrt(.5) + pairs10$c02E*sqrt(.5)

cor.test(pairs10$c0P, pairs10$c02P)
cor.test(pairs10$c0P, pairs10$c1P)

cor.test(pairs10$c0P, pairs10$c1P)$estimate/cor.test(pairs10$c0P, pairs10$c02P)$estimate

(cor.test(pairs10$c0P, pairs10$c02P)$estimate - cor.test(pairs10$c0P, pairs10$c1P)$estimate)*2   # A
(cor.test(pairs10$c0P, pairs10$c1P)$estimate - cor.test(pairs10$c0P, pairs10$c02P)$estimate/2)*2 # C
1-cor.test(pairs10$c0P, pairs10$c02P)$estimate                                                   # E


# what does the pgs-correlation imply/suggest about the true genotypic correlation between mates?



















