
# packages ------------------------------------------------------
require(data.table)
require(tidyverse)
require(plyr)

# directory -----------------------------------------------------
setwd("/Users/Yuki/Dropbox/sokouo1/全魚種csv")

# load the data -------------------------------------------------
test = read.csv("全魚種1999a.csv", fileEncoding = "CP932")

path = "/Users/Yuki/Dropbox/sokouo1/全魚種csv/"
file_list = list.files(path, pattern="csv")
# 
# 2002a〜: 種の始まりがイレギュラー
# 2004a〜: また別の形式に
# 1:8, 9:12, 13:24で条件分岐が必要
# 
file = file_list[1]
is_blank = function(x) {is.na(x) | x == ""}

# df = read.table(file, sep=",", na.strings=c(' '), fileEncoding = "CP932")
df1 = read.csv(file, na.strings = NULL, fileEncoding = "CP932", skip = 1)
df2 = read.csv(file, na.strings = NULL, fileEncoding = "CP932", skip = 2)
df3 = read.csv(file, na.strings = NULL, fileEncoding = "CP932", skip = 3)

# x = df %>%
#   # 空行の除去
#   filter(rowSums(is_blank(.)) != ncol(.)) %>%
#   # 空列の除去
#   select_if(colSums(is_blank(.)) != nrow(.))
# summary(x)

df1 = df1[-c(1:3), ] #number
df2 = df2[-c(1:2), ] #station
df3 = df3[-1, ] #depth

x1 = df1 %>% tidyr::gather(key = ymd, value = number, 3:ncol(df1)) %>% mutate(n_number = as.numeric(number)) %>% dplyr::rename(species = 年月日, net = X)
summary(x1)
x1[is.na(x1)] = 0

x2 = df2 %>% tidyr::gather(key = station, value = number, 3:ncol(df2)) %>% mutate(n_number = as.numeric(number)) %>% dplyr::rename(species = STATIONコード, net = X)
summary(x2)
x1[is.na(x2)] = 0

x3 = df3 %>% tidyr::gather(key = depth, value = number, 3:ncol(df1)) %>% mutate(n_number = as.numeric(number)) %>% dplyr::rename(species = 水深, net = X)
summary(x3)
x3[is.na(x3)] = 0

X = left_join(x1 %>% select(-number), x2 %>% select(-number), x3 %>% select(-number), by = c("species", "net", "n_number"))
