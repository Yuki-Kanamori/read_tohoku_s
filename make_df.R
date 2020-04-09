
# 概要 ----------------------------------------------------------------------
# データの形式は３パターン
# (1) 1998a~:
# (2) 2002a~: 種の始まる行が(1)とは異なる
# (3) 2004a~: (1)(2)では尾数(###a.csv)と重量(###b.csv)のデータが別々のファイルになっていたが，(3)は両データが含まれている．また，新たに網次データも含まれている
# さらに，(1)でも2001aと2001bファイルは，ざっとファイルを見る限り分からないような小さな違い(空欄やデータの型が悪さをしている感じ)があり，~2000bと2001a&2001bの読み込むコードは別のものにする必要がある



# packages ------------------------------------------------------
require(data.table)
require(tidyverse)
require(plyr)

# directory -----------------------------------------------------
setwd("/Users/Yuki/Dropbox/sokouo1/全魚種csv")

# load the data -------------------------------------------------
test = read.csv("全魚種1999a.csv", fileEncoding = "CP932")
test2 = c()

path = "/Users/Yuki/Dropbox/sokouo1/全魚種csv/"
file_list = list.files(path, pattern="csv")
# 
# 2002a〜: 種の始まりがイレギュラー
# 2004a〜: また別の形式に
# 1:8, 9:12, 13:24で条件分岐が必要
# 
file = file_list[24]
# is_blank = function(x) {is.na(x) | x == ""}

# df = read.table(file, sep=",", na.strings=c(' '), fileEncoding = "CP932")
df = read.csv(file, na.strings = NULL, fileEncoding = "CP932")
for(j in 1:7){
  assign(paste0('df', j),
         read.csv(file, na.strings = NULL, fileEncoding = "CP932", skip = j))
}
for(j in 1:7){
  name = paste0('df', j)
  data = get(name)
  assign(paste0('df', j),
         data %>% filter(rowSums(is_blank(.)) != ncol(.)) %>% 
           select_if(colSums(is_blank(.)) != nrow(.)))
}
df1 = df1[-c(1:7), ] #ymd
df2 = df2[-c(1:6), ] #station
df3 = df3[-c(1:5), ] #depth
df4 = df4[-c(1:4), ] #station_code
df5 = df5[-c(1:3), ] #net_no
df6 = df6[-c(1:2), ] #net
df7 = df7[-c(1:1), ] #data_type

x1 = df1 %>% tidyr::gather(key = ymd, value = abundance, 2:ncol(df1)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 年月日)
summary(x1)
x1[is.na(x1)] = 0

x2 = df2 %>% tidyr::gather(key = station, value = abundance, 2:ncol(df2)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 調査ライン)
summary(x2)
x2[is.na(x2)] = 0

x3 = df3 %>% tidyr::gather(key = depth, value = abundance, 2:ncol(df3)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 水深)
summary(x3)
x3[is.na(x3)] = 0

colnames(df4)
x4 = df4 %>% tidyr::gather(key = station_code, value = abundance, 2:ncol(df4)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = STATIONコード)
summary(x4)
x4[is.na(x4)] = 0

colnames(df5)
x5 = df5 %>% tidyr::gather(key = net_no, value = abundance, 2:ncol(df5)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 網次)
summary(x5)
x5[is.na(x5)] = 0

colnames(df6)
x6 = df6 %>% tidyr::gather(key = net, value = abundance, 2:ncol(df6)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 網)
summary(x5)
x5[is.na(x5)] = 0

colnames(df7)
x7 = df7 %>% tidyr::gather(key = data_type, value = abundance, 2:ncol(df7)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = データ種別)
summary(x7)
x7[is.na(x7)] = 0

if(nrow(x1)*7 - (nrow(x1)+nrow(x2)+nrow(x3)+nrow(x4)+nrow(x5)+nrow(x6)+nrow(x7)) == 0){
  x = cbind(x1, x2 %>% select(station), x3 %>% select(depth), x4 %>% select(station_code), x5 %>% select(net_no), x6 %>% select(net), x7 %>% select(data_type)) %>% mutate(file_name = file)
}else{
    warning(paste('please check the objects'))
  }
test2 = rbind(test2, x)



# using loop function -----------------------------------------------------
setwd("/Users/Yuki/Dropbox/sokouo1/全魚種csv")
path = "/Users/Yuki/Dropbox/sokouo1/全魚種csv/"
file_list = list.files(path, pattern="csv")
dat = c()
dat2 = c()
# for(i in 1:(length(file_list)-1)){ #やけに重たくなるので，loopを分けた
for(i in 1:8){  
  file = file_list[i]
  
  if(i < 7){
    for(j in 1:3){
      assign(paste0('df', j),
             read.csv(file, na.strings = NULL, fileEncoding = "CP932", skip = j))
    }
    
    for(j in 1:3){
      name = paste0('df', j)
      data = get(name)
      assign(paste0('df', j),
             data %>% filter(rowSums(is_blank(.)) != ncol(.)) %>% 
               select_if(colSums(is_blank(.)) != nrow(.)))
    }
    
    df1 = df1[-c(1:3), ] #abundance
    df2 = df2[-c(1:2), ] #station
    df3 = df3[-1, ] #depth
    
    x1 = df1 %>% tidyr::gather(key = ymd, value = abundance, 3:ncol(df1)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 年月日, net = X)
    summary(x1)
    x1[is.na(x1)] = 0
    
    x2 = df2 %>% tidyr::gather(key = station, value = abundance, 3:ncol(df2)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = STATIONコード, net = X)
    summary(x2)
    x2[is.na(x2)] = 0
    
    x3 = df3 %>% tidyr::gather(key = depth, value = abundance, 3:ncol(df3)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 水深, net = X)
    summary(x3)
    x3[is.na(x3)] = 0
    
    if(nrow(x1)*3 - (nrow(x1)+nrow(x2)+nrow(x3)) == 0){
      x = cbind(x1, x2 %>% select(station), x3 %>% select(depth)) %>% mutate(file_name = file)
    }else{
      warning(paste('please check the objects'))
    }
    
    dat = rbind(dat, x)
  }
  
  if(i > 6 && i < 9){
    for(j in 1:3){
      assign(paste0('df', j),
             read.csv(file, na.strings = NULL, fileEncoding = "CP932", skip = j))
    }
    
    colnames(df2)[2] = 'X'
    
    df1 = df1[-c(30:31), ] 
    df2 = df2[-c(29:30), ] 
    df3 = df3[-c(28:29), ] 
    
    df1 = df1[-c(1:3), ] #abundance
    df2 = df2[-c(1:2), ] #station
    df3 = df3[-1, ] #depth
    
    x1 = df1 %>% tidyr::gather(key = ymd, value = abundance, 3:ncol(df1)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 年月日, net = X)
    summary(x1)
    x1[is.na(x1)] = 0
    
    x2 = df2 %>% mutate(X2 = as.character(as.factor(df2$X))) %>% tidyr::gather(key = station, value = abundance, 3:ncol(df2)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = STATIONコード, net = X2) %>% select(-X)
    summary(x2)
    x2[is.na(x2)] = 0
    
    x3 = df3 %>% tidyr::gather(key = depth, value = abundance, 3:ncol(df1)) %>% mutate(n_abundance = as.numeric(abundance )) %>% dplyr::rename(species = 水深, net = X)
    summary(x3)
    x3[is.na(x3)] = 0
    
    if(nrow(x1)*3 - (nrow(x1)+nrow(x2)+nrow(x3)) == 0){
      x = cbind(x1, x2 %>% select(station), x3 %>% select(depth)) %>% mutate(file_name = file)
    }else{
      warning(paste('please check the objects'))
    }
    dat = rbind(dat, x)
  }
  
  
  if(i > 8 && i < 13){
    for(j in 1:3){
      assign(paste0('df', j),
             read.csv(file, na.strings = NULL, fileEncoding = "CP932", skip = j))
    }
    
    for(j in 1:3){
      name = paste0('df', j)
      data = get(name)
      assign(paste0('df', j),
             data %>% filter(rowSums(is_blank(.)) != ncol(.)) %>% 
               select_if(colSums(is_blank(.)) != nrow(.)))
    }

    df1 = df1[-c(1:2), ] #number
    df2 = df2[-c(1), ] #station
    df3 = df3 #depth
    colnames(df3)[2] = "X"
    
    x1 = df1 %>% tidyr::gather(key = ymd, value = abundance, 3:ncol(df1)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 年月日, net = X)
    summary(x1)
    x1[is.na(x1)] = 0
    
    x2 = df2 %>% mutate(X2 = as.character(as.factor(df2$X))) %>% tidyr::gather(key = station, value = abundance, 3:ncol(df2)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = STATIONコード, net = X2) %>% select(-X)
    summary(x2)
    x2[is.na(x2)] = 0
    
    x3 = df3 %>% tidyr::gather(key = depth, value = abundance, 3:ncol(df3)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 水深, net = X)
    summary(x3)
    x3[is.na(x3)] = 0
    
    if(nrow(x1)*3 - (nrow(x1)+nrow(x2)+nrow(x3)) == 0){
      x = cbind(x1, x2 %>% select(station), x3 %>% select(depth)) %>% mutate(file_name = file)
    }else{
      warning(paste('please check the objects'))
    }
    dat = rbind(dat, x)
  }
}


for(i in 13:24){
  file = file_list[i]
  
  for(j in 1:7){
    assign(paste0('df', j),
           read.csv(file, na.strings = NULL, fileEncoding = "CP932", skip = j))
  }
  for(j in 1:7){
    name = paste0('df', j)
    data = get(name)
    assign(paste0('df', j),
           data %>% filter(rowSums(is_blank(.)) != ncol(.)) %>% 
             select_if(colSums(is_blank(.)) != nrow(.)))
  }
  df1 = df1[-c(1:7), ] #ymd
  df2 = df2[-c(1:6), ] #station
  df3 = df3[-c(1:5), ] #depth
  df4 = df4[-c(1:4), ] #station_code
  df5 = df5[-c(1:3), ] #net_no
  df6 = df6[-c(1:2), ] #net
  df7 = df7[-c(1:1), ] #data_type
  
  x1 = df1 %>% tidyr::gather(key = ymd, value = abundance, 2:ncol(df1)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 年月日)
  summary(x1)
  x1[is.na(x1)] = 0
  
  x2 = df2 %>% tidyr::gather(key = station, value = abundance, 2:ncol(df2)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 調査ライン)
  summary(x2)
  x2[is.na(x2)] = 0
  
  x3 = df3 %>% tidyr::gather(key = depth, value = abundance, 2:ncol(df3)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 水深)
  summary(x3)
  x3[is.na(x3)] = 0
  
  colnames(df4)
  x4 = df4 %>% tidyr::gather(key = station_code, value = abundance, 2:ncol(df4)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = STATIONコード)
  summary(x4)
  x4[is.na(x4)] = 0
  
  colnames(df5)
  x5 = df5 %>% tidyr::gather(key = net_no, value = abundance, 2:ncol(df5)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 網次)
  summary(x5)
  x5[is.na(x5)] = 0
  
  colnames(df6)
  x6 = df6 %>% tidyr::gather(key = net, value = abundance, 2:ncol(df6)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = 網)
  summary(x5)
  x5[is.na(x5)] = 0
  
  colnames(df7)
  x7 = df7 %>% tidyr::gather(key = data_type, value = abundance, 2:ncol(df7)) %>% mutate(n_abundance = as.numeric(abundance)) %>% dplyr::rename(species = データ種別)
  summary(x7)
  x7[is.na(x7)] = 0
  
  if(nrow(x1)*7 - (nrow(x1)+nrow(x2)+nrow(x3)+nrow(x4)+nrow(x5)+nrow(x6)+nrow(x7)) == 0){
    x = cbind(x1, x2 %>% select(station), x3 %>% select(depth), x4 %>% select(station_code), x5 %>% select(net_no), x6 %>% select(net), x7 %>% select(data_type)) %>% mutate(file_name = file)
  }else{
    warning(paste('please check the objects'))
  }
  dat2 = rbind(dat2, x)
}


# combine the three objects; dat, dat1, and dat3 --------
file = file_list[25]
dat3 = read.csv(file, fileEncoding = "CP932") %>% mutate(file_name = file)

head(dat, 3)
head(dat2, 3)
head(dat3, 3)
summary(dat3)

d = dat
d2 = dat2

d = d %>% mutate(レグ = NA, 網次 = NA, 魚種NO = NA, 調査種類 = NA, 実曳 = NA, 年 = as.numeric(str_sub(file_name, 4, 7)), data_type = str_sub(file_name, 8, 8)) %>% dplyr::rename(STATIONコード = station, 和名 = species, 網 = net)
d = d %>% mutate(data_type2 = ifelse(d$data_type == "a", '漁獲尾数', '漁獲量'))
summary(d)
d_ymd = d %>% distinct(ymd)
d = d %>% mutate(月 = NA, 日 = NA) %>% select(-ymd, -abundance)
d_dep = d %>% distinct(depth) %>% mutate(水深 = as.numeric(str_sub(depth, 2, 4)))
d_dep[75,2] = 1900
d_dep[76,2] = 2000
d_dep[77:nrow(d_dep), 2] = NA
d = left_join(d, d_dep, by = "depth") %>% select(-depth)


d2 = d2 %>% mutate(レグ = NA, 魚種NO = NA, 調査種類 = NA, 年 = as.numeric(str_sub(file_name, 4, 7)), data_type = str_sub(file_name, 8, 8)) %>% dplyr::rename(STATIONコード = station, 水深 = depth, 和名 = species, 網 = net, 実曳 = , 網次 = net_no)



dat = dat %>% mutate(レグ = NA, net_no = NA, 魚種NO = NA, 調査種類 = NA) %>% dplyr::rename(STATIONコード = station, 水深 = depth, 和名 = species, 網 = net)

実曳 = station_code, 


























