#install.packages('magic')
library(magic)

ans_1 <- magic(4)
ans_2 <- magic(5)
ans_3 <- magic(6)

print(ans_1)
print(ans_2)
print(ans_3)

save(ans_1, ans_2, ans_3, file = "2740430299_01.RData")
load("2740430299_01.RData")