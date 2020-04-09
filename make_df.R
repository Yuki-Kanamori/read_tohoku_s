
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

# make the dataframe -------------------------------------------------
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

d2 = d2 %>% mutate(レグ = NA, 魚種NO = NA, 調査種類 = NA, 年 = as.numeric(str_sub(file_name, 4, 7))) %>% dplyr::rename(STATIONコード = station, 和名 = species, 網 = net, 実曳 = station_code)
d2_ymd = d2 %>% distinct(ymd)
d2 = d2 %>% mutate(月 = NA, 日 = NA) %>% select(-ymd, -abundance)
d2_dep = d2 %>% distinct(depth) %>% mutate(水深 = as.numeric(str_sub(depth, 2, 5)))
d2 = left_join(d2, d2_dep, by = "depth") %>% select(-depth)
d2_netno = d2 %>% distinct(net_no) %>% mutate(網次 = as.numeric(str_sub(net_no, 2, 2)))
d2 = left_join(d2, d2_netno, by = "net_no") %>% select(-net_no)

head(d)
head(d2)
d = d %>% select(-data_type) %>% dplyr::rename(data_type = data_type2)
old1998_2007 = rbind(d, d2) 


# save the data -----------------------------------------------------------
new_dir = paste0("/Users/Yuki/Dropbox/sokouo1/全魚種csv", '/output')
dir.create(new_dir)
setwd(new_dir)
write.csv(old1998_2007, "全魚種1998-2007.csv", fileEncoding = "CP932")

sp_list = old1998_2007 %>% distinct(和名)
sp_list = sp_list[-c(679:684), ] %>% as.data.frame()
colnames(sp_list)[1] = "和名"
sp_list = sp_list %>% filter(和名 != "あ行", 和名 != "か行", 和名 != "さ行", 和名 != "た行", 和名 != "な行", 和名 != "は行", 和名 != "ま行", 和名 != "や行", 和名 != "ら行", 和名 != "わ行")
# write.csv(sp_list, "species_list_old.csv", fileEncoding = "CP932")

sp_list2 = dat3 %>% distinct(和名)
sp_all = rbind(sp_list, sp_list2) %>% distinct(和名) %>% dplyr::arrange(和名) %>% filter(rowSums(is_blank(.)) != ncol(.)) %>% 
  select_if(colSums(is_blank(.)) != nrow(.))
write.csv(sp_all, "splist_all.csv", fileEncoding = "CP932")

# 2008年以降のデータに合わせる --------------------------------------------------------
# 修正中
# メモ --------------------------------------------------------
# 尾数と重量が別ファイルで保存されている期間のデータでは，尾数列と重量列を横に並べるのは難しい可能性がある -------------------------------------------------------
# d_n = d %>% filter(data_type == 'a') %>% dplyr::rename(漁獲尾数 = n_abundance)
d_n = d %>% filter(data_type == 'a')
colnames(d_n)[3] = "漁獲尾数"
summary(d_n)
# d_w = d %>% filter(data_type == 'b') %>% dplyr::rename(漁獲量 = n_abundance)
d_w = d %>% filter(data_type == 'b')
colnames(d_w)[3] = "漁獲量"
summary(d_w)

# colnames(d_n)
# d = left_join(d_n, d_w, by = c("和名", "網", "STATIONコード", "file_name", "レグ", "網次", "魚種NO", "調査種類", "実曳", "年", "月", "日", "水深"))
# 
# d = left_join(d_n, d_w, by = c("レグ", "年", "月", "日", "実曳", "調査種類", "STATIONコード", "水深", "網次", "網", "魚種NO", "和名", "漁獲尾数", "漁獲量", "file_name"))






















