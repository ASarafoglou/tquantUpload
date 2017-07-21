library('pks')
data(probability)
dat <- probability
#Q in App, Item in probability
# A1  3
# A2  1
# A3  4
# A4  2
# B1  7
# B2  9
# B3  5
# B4  10
# C1  12
# C2  11
#data of students before taking statistics class w/o items 6 & 8
dat1 <- cbind(dat$b102, dat$b101, dat$b104, dat$b102, dat$b107, 
              dat$b109, dat$b105, dat$b110, dat$b112, dat$b111)
colnames(dat1)[1:10] <-c('a1','a2','a3','a4','b1','b2','b3','b4','c1','c2')
#data of students after taking statistics class w/o items 6 & 8
dat2 <- cbind(dat$b202, dat$b201, dat$b204, dat$b202, dat$b207, dat$b209, dat$b205, dat$b210, dat$b212, dat$b211)
colnames(dat3)[1:10] <-c('a1','a2','a3','a4','b1','b2','b3','b4','c1','c2')

barplot(colSums(dat2, na.rm = T))
barplot(colSums(dat3, na.rm = T ))
barplot((colSums(dat3, na.rm = T))-(colSums(dat2, na.rm = T)))

dat1 <- as.pattern(dat1)
state         <- rep(c(TRUE), 10
state.pattern <- paste(as.character(as.numeric(state)), collapse = "")

# most frequent pattern
"1111111111"
max(table(dat1))
# 504 observations
sum(table(dat1))
# find your knowledge state
index <- which(names(table(dat1)) %in% state.pattern)
freq  <- round(table(dat1)[index]/504 * 100, 3)
paste("Compared to students that completed a statistics
      course the occurence of your knowledge state has
      a probability of", freq, "percent", sep=" ")

              

















