num_exp <- c(1:1000000)

tie  <- 0
cnt_boy  <- 0
cnt_girl  <- 0

for (val in num_exp) {
  
  girl  <- floor(runif(4, min=0, max=2))
  boy   <- floor(runif(4, min=0, max=2))
  
  table_girl <- table(girl)
  table_boy  <- table(boy)
  
  # Conditions
  b0 = table_boy[names(table_boy)==1] == 0
  b1 = table_boy[names(table_boy)==1] == 1
  b2 = table_boy[names(table_boy)==1] == 2
  g0 = table_girl[names(table_girl)==1] == 0
  g1 = table_girl[names(table_girl)==1] == 1
  g2 = table_girl[names(table_girl)==1] == 2
  g3 = table_girl[names(table_girl)==1] == 3
  g4 = table_girl[names(table_girl)==1] == 4
  
  if (b2 & (g0 | g1 | g2 | g3)) {
    cnt_boy <- cnt_boy + 1
  } else if (b2 == g4) {
    tie <- tie + 1
  } else if (g4 & (b0 | b1)){
    cnt_girl <- cnt_girl + 1
  }
}

sum <- cnt_boy + tie + cnt_girl
boy_share = (cnt_boy / sum) * 100000
girl_share = 100000 - boy_share

ans_1 <- paste('Money share for Gholi: ', boy_share)
ans_2 <- paste('Money share for Goli: ', girl_share)

save(ans_1, ans_2, file = "2740430299_04.RData")
load("2740430299_04.RData")

print(ans_1)
print(ans_2)
