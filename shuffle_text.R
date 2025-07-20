## shuffle question
shuffleQuestion <- function(q, a){
    qa_mod <- list(q = q, a = a)
    
    shuffle_text <- function(keywords, qa){
      q <- qa$q
      a <- qa$a
      
      bs <- runif(length(keywords)) < 1/2
      kw2 <- keywords[bs]
      for (i in seq_along(kw2)){
         item <- kw2[[i]]
         if (grepl(item[1], a)){
           q <- gsub(item[3], item[4], q)
           a <- gsub(item[1], item[2], a)
         } else if (grepl(item[2], a)){
           q <- gsub(item[4], item[3], q)
           a <- gsub(item[2], item[1], a)
         }
      }
      return (list(q = q, a=a))
    }
    
    keywords1 <- list(
      c(" every day ", " every week ", "毎日", "毎週"),
      c(" every morning ", " every evening ", "毎朝", "毎晩"),
      c(" every month ", " every year ", "毎月", "毎年"),
      c(" lunch ", " dinner ", "昼食", "夕食"),
      c(" two hours ", " three hours ", "２時間", "３時間"),
      c(" summer ", " winter ", "夏", "冬"),
      
      c("golf", "tennis", "ゴルフ", "テニス"),
      c(" piano ", " violin ", "ピアノ", "バイオリン"),
      c(" guitar ", " piano ", "ギター", "ピアノ"),
      c(" green tea ", " black tea ", "緑茶", "紅茶"),
      c(" coffee ", " tea ", "コーヒー", "お茶"),
      c(" taxi ", " bus ", "タクシー", "バス"),
      c(" orange juice ", " apple juice ", "オレンジジュース", "アップルジュース"),
      
      c(" Bill ", " Steve ", "ビル", "スティーブ"),
      c("Waseda", "Keio", "早稲田", "慶応"),
      c("Fuji Bank", "Mizuho Bank", "富士銀行", "みずほ銀行"),
      c(" economics ", " physics ", "経済学", "物理学"),
      
      c("Greek", "Turkish", "ギリシャ", "トルコ"),
      c("Sweden", "Finland", "スウェーデン", "フィンランド"),
      c("Dutch", "French", "オランダ", "フランス"),
      c("San Fransisco", "New York", "サンフランシスコ", "ニューヨーク"),
      c("Yokohama", "Kobe", "横浜", "神戸"),
      c("London", "Paris", "ロンドン", "パリ"),
      c("Russian", "Ukrainian", "ロシア", "ウクライナ"),
      c("Germany", "Italy", "ドイツ", "イタリア"),
      c("Portuguese", "Spanish", "ポルトガル", "スペイン")
    )
    qa_mod <- shuffle_text(keywords1, qa_mod)
    
    return(list(q = qa_mod$q, a = qa_mod$a))
}
