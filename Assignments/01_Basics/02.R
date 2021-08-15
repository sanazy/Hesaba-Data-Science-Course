#install.packages('gtools')
library(gtools)

# Vector of L: Lier and T: Truth-teller
x <- c('L', 'T')

############         Part a         ############

mat1 <- permutations(n=2, r=16, v=x, repeats.allowed=T)
out1 <- matrix(, nrow=0, ncol=ncol(mat1))

for (row in 1:nrow(mat1)) {
  for (col in 2:ncol(mat1)-1) {
    if (mat1[row,col] != mat1[row,col+1]) {
      if (col == ncol(mat1)-1){
        out1 <- rbind(out1, mat1[row,])
      }
      next
    }
    else {
      break
    }
  }
}

for (row in 1:nrow(out1)){
  a <- table(out1[row,])
  print(a['L'])
}

############         Part b         ############

mat2 <- permutations(n=2, r=12, v=x, repeats.allowed=T)
out2 <- matrix(, nrow=0, ncol=ncol(mat2))

for (row in 1:nrow(mat2)) {
  for (col in 3:ncol(mat2)) {
    if (col == ncol(mat2)){
      if (xor(mat2[row,col] != mat2[row,1], mat2[row,col] != mat2[row,col-1])) {
        out2 <- rbind(out2, mat2[row,])
      }
    }
    else {
      if (xor(mat2[row,col] != mat2[row,col+1], mat2[row,col] != mat2[row,col-1])) {
        next
      }
      else {
        break
      }
    }
  }
}


for (row in 1:nrow(out2)){
  a <- table(out2[row,])
  print(a['L'])
}

############         Save Outputs         ############

# Number of liers for one case of each output
ans_1 <- table(out1[1,])['L']
ans_2 <- table(out2[1,])['L']

save(ans_1, ans_2, file = "2740430299_02.RData")
load("2740430299_02.RData")

print(ans_1)
print(ans_2)
