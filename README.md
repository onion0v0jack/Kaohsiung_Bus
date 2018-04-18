### 高雄市公共運輸改善計畫-高雄市公車路線分析
#### 資料來源
高雄市數家客運公司
#### 使用語言-R
使用套件：xlsx、plyr、dplyr、data.table、stringdist、gplots、ggmap、RColorBrewer、XML、methods、httr
#### 應用方法
爬蟲、文字處理、時間處理、Page Rank...等等
#### 想法
1. 因為公司內部的站點資料(名稱、位置等)在記錄上有一些問題，所以需要針對這些進行統一、資料與文字轉換的處理。
2. 對於欲分析的路線，整理其單周內每站的上下人數，以長條圖呈現。
3. 利用Page Rank，索取其特徵值大小，可以看出各站的重要性
4. 利用熱圖，呈現各站上下車人數，尋找其關聯。
5. 考慮去程與回程，會有站點名稱、站點數量與順序的問題。
6. 對單一司機的資料，摘取當天各站乘客的上下車時間，以時間為縱軸，人次為橫軸繪圖，可以看到該司機的工作情況，並且也可以尋找資料中的異常值。
7. 索取去回程的表定時間，並且與實際公車資料比較，計算延遲時間。
8. 透過散佈圖的呈現，可以看延遲時間是否出現異常。
9. 同樣可以利用熱圖，呈現各站去回程的延遲情況，可以針對覺異常的站點進行改善。
10. 將上下車人數呈現在google map上，能夠更直覺的理解。
