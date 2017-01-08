#generate matrix
data <- matrix(data = FALSE, ncol = 10, nrow = 10)
# 1-5-9, 1-6-10, 1-7-10
data[2, 5] = data[5, 9] = TRUE
data[2, 6] = data[6, 9] = TRUE
data[2, 7] = data[7, 10] = TRUE
# 2-5-9, 2-8-9
data[4, 7] = data[7, 10] = TRUE
data[4, 8] = data[8, 10] = TRUE
#3-6-10
data[1, 5] = data[5, 9] = TRUE
# 4-8-9, 4-7-10
data[3, 8] = data[8, 10] = TRUE
data[3, 6] = data[6, 9] = TRUE

hasse_Diag = function(vec) {

vec[which(vec == TRUE)] = "darkseagreen3"
vec[which(vec == FALSE)] = "white"

v = c("A1", "A2", "A3", "A4", "B1", "B2", "B3", "B4", "C1", "C2")
hasse(data, v,
        ebg      = "black",
        vcol     = "navy",
        vbg      = vec,
        vsize    = .75,
        main     = " ",
        compress = TRUE)
      }

