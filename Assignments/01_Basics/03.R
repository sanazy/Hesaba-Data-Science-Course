num_exp <- c(1:100000)

total_cnt <- 0 
cnt_girl <- 0
cnt_boy  <- 0

for (val in num_exp) {
  
  girl_1  <- floor(runif(1, min=1, max=7))
  boy_1   <- floor(runif(1, min=1, max=7))
  
  if (girl_1 == 6 & boy_1 == 6) {
    
    while (TRUE){
      total_cnt <- total_cnt + 1
      girl_2  <- floor(runif(1, min=1, max=7))
      boy_2   <- floor(runif(1, min=1, max=7))
      
      if (girl_2 == 6){
        cnt_girl <- cnt_girl + 1
        break
      } 
      
      if (boy_2 == 1){
        cnt_boy <- cnt_boy + 1
        break
      } 
    }
  }
}

mean_girl <- cnt_girl/total_cnt
mean_boy  <- cnt_boy/total_cnt

if (mean_girl < mean_boy){
  less_mean <- 'Goli'
} else {
  less_mean <- 'Gholi'
}

ans_1 <- paste('Faster time mean:  ', less_mean)
ans_2 <- paste('Mean of winning Goli:  ', mean_girl)

save(ans_1, ans_2, file = "2740430299_03.RData")
load("2740430299_03.RData")

print(ans_1)
print(ans_2)
