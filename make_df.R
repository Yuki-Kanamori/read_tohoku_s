
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
file = file_list[8]
# is_blank = function(x) {is.na(x) | x == ""}

# df = read.table(file, sep=",", na.strings=c(' '), fileEncoding = "CP932")
df = read.csv(file, na.strings = NULL, fileEncoding = "CP932")
for(j in 1:3){
  assign(paste0('df', j),
         read.csv(file, na.strings = NULL, fileEncoding = "CP932", skip = j))
}
 
colnames(df2)[1] = 'X'
colnames(df2)[2] = 'X'
colnames(df2)[3] = 'X'

# df1 = read.csv(file, na.strings = NULL, fileEncoding = "CP932", skip = 1)
# df2 = read.csv(file, na.strings = NULL, fileEncoding = "CP932", skip = 2)
# df3 = read.csv(file, na.strings = NULL, fileEncoding = "CP932", skip = 3)

for(j in 1:3){
  name = paste0('df', j)
  data = get(name)
  assign(paste0('df', j),
         data %>% filter(rowSums(is_blank(.)) != ncol(.)) %>% 
           select_if(colSums(is_blank(.)) != nrow(.)))
}

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

if(nrow(x1)*3 - (nrow(x1)+nrow(x2)+nrow(x3)) == 0){
  x = cbind(x1, x2 %>% select(station), x3 %>% select(depth)) %>% mutate(file_name = file)
}else{
    warning(paste('please check the objects'))
  }




# using loop function -----------------------------------------------------
setwd("/Users/Yuki/Dropbox/sokouo1/全魚種csv")
path = "/Users/Yuki/Dropbox/sokouo1/全魚種csv/"
file_list = list.files(path, pattern="csv")
test = c()
# (length(file_list)-1)

for(i in 1:6){
  # i = 3
  file = file_list[i]
  
  for(j in 1:3){
    assign(paste0('df', j),
           read.csv(file, na.strings = NULL, fileEncoding = "CP932", skip = j))
  }
  
  # for(j in 1:3){
  #   name = paste0('df', j)
  #   data = get(name)
  #   assign(paste0('df', j),
  #          colnames(data)[2] = 'X')
  # }
  
  for(j in 1:3){
    name = paste0('df', j)
    data = get(name)
    assign(paste0('df', j),
           data %>% filter(rowSums(is_blank(.)) != ncol(.)) %>% 
             select_if(colSums(is_blank(.)) != nrow(.)))
  }
  
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
  
  if(nrow(x1)*3 - (nrow(x1)+nrow(x2)+nrow(x3)) == 0){
    x = cbind(x1, x2 %>% select(station), x3 %>% select(depth)) %>% mutate(file_name = file)
  }else{
    warning(paste('please check the objects'))
  }
  
  test = rbind(test, x)
}
unique(test$file_name)





















