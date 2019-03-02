# Case 5: Web Scraping (Naver Apartment) -----------------------------------------
install.packages("dplyr")
install.packages("stringr")
install.packages("httr")
install.packages("rvest")

library(dplyr)
library(stringr)
library(httr)
library(rvest)

url <- 'http://land.naver.com/article/articleList.nhn?rletTypeCd=A01&tradeTypeCd=A1&rletNo=25827&cortarNo=1129013500&hscpTypeCd=A01%3AA03%3AA04&mapX=&mapY=&mapLevel=&page=1&articlePage=&ptpNo=&rltrId=&mnex=&bildNo=&articleOrderCode=&cpId=&period=&prodTab=&atclNo=&atclRletTypeCd=&location=1476&bbs_tp_cd=&sort=&siteOrderCode=&schlCd=&tradYy=&exclsSpc=&splySpcR=&cmplYn=#_content_list_target'
start <- proc.time()
Building <- NULL
Top_Floor <- NULL
Floor <- NULL
Area1 <- NULL
Area2 <- NULL
Price <- NULL
Premium <- NULL

for( i in c(1:3)){
  
  cat(i, '/ 3 page \n')
  
  tmp_url <- modify_url(url, query = list(page = i))
  tmp_list <- read_html(tmp_url) %>% html_nodes('a[href^="/article"]') %>% html_attr('href')
  tmp_list <- paste0('http://land.naver.com',tmp_list)
  
  for(j in 2:length(tmp_list)){
    
    tryCatch({
      tmp_paragraph <- read_html(tmp_list[j])
      
      # Building
      tryCatch({
        tmp_building <- repair_encoding(tmp_paragraph %>% html_nodes('div.info_area.info_area_v2.first') %>% 
                                          html_nodes('div.inner') %>% html_text(T))
        tmp_building <- as.numeric(substr(tmp_building[2], 1, 3))
      }, error = function(e){tmp_title <- NA})
      
      Building <- c(Building, tmp_building)
      
      # Floor
      tryCatch({
        tmp_info <- repair_encoding(tmp_paragraph %>% html_nodes('div.detail_view.detail_view_v2') %>% 
                                      html_nodes('div.inner') %>% html_text(T))
        tmp_floor_info <- strsplit(tmp_info[2], "/")
        
        tmp_floor <- tmp_floor_info[[1]][1]
      }, error = function(e){tmp_floor <- NA})
      
      Floor <- c(Floor, tmp_floor)
      
      # Top_Floor
      tryCatch({
        tmp_info <- repair_encoding(tmp_paragraph %>% html_nodes('div.detail_view.detail_view_v2') %>% 
                                      html_nodes('div.inner') %>% html_text(T))
        tmp_floor_info <- strsplit(tmp_info[2], "/")
        
        tmp_top_floor <- as.numeric(substr(tmp_floor_info[[1]][2], 1, nchar(tmp_floor_info[[1]][2])-1))
      }, error = function(e){tmp_top_floor <- NA})
      
      Top_Floor <- c(Top_Floor, tmp_top_floor)
      
      # Price
      tryCatch({
        tmp_price <- tmp_paragraph %>% html_nodes(xpath = "//script[@language='JavaScript']") %>% html_text(T)
        price_start_idx <- gregexpr(pattern ='atclPrice', tmp_price)[[1]][1]
        tmp_price <- substr(tmp_price, price_start_idx+13, price_start_idx+17)
      }, error = function(e){tmp_price <- NA})
      
      Price <- c(Price, tmp_price)
      
      # Areas
      tryCatch({
        tmp_area <- tmp_paragraph %>% html_nodes('div.ar_area') %>% html_nodes('div.ly_tbl') %>% html_text(T)
        tmp_area <- gsub("\t", "", tmp_area)
        tmp_area <- gsub("\n", " ", tmp_area)
        tmp_area <- strsplit(tmp_area, " ")[[1]]
        del_idx <- which(nchar(tmp_area) == 0)
        tmp_area <- tmp_area[-del_idx]
        
        tmp_Area1 <- as.numeric(tmp_area[6])
      }, error = function(e){tmp_Area1 <- NA})
      
      Area1 <- c(Area1, tmp_Area1)
   
      tryCatch({
        tmp_area <- tmp_paragraph %>% html_nodes('div.ar_area') %>% html_nodes('div.ly_tbl') %>% html_text(T)
        tmp_area <- gsub("\t", "", tmp_area)
        tmp_area <- gsub("\n", " ", tmp_area)
        tmp_area <- strsplit(tmp_area, " ")[[1]]
        del_idx <- which(nchar(tmp_area) == 0)
        tmp_area <- tmp_area[-del_idx]
        
        tmp_Area2 <- as.numeric(tmp_area[7])
      }, error = function(e){tmp_Area2 <- NA})
      
      Area2 <- c(Area2, tmp_Area2)
      
    }, error = function(e){print("Invalid conversion, skip the post")})
    
    Sys.sleep(3)
    
  }
}

APT <- data.frame(Building, Top_Floor, Floor, Area1, Area2, Price)
end <- proc.time()
end - start # Total Elapsed Time

# Export the result
write.csv(APT, file = "APT_Price.csv")
