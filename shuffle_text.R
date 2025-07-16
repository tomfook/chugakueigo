## shuffle question
shuffleQuestion <- function(q, a){
    qa_mod <- list(q = q, a = a)
    
    shuffle_text <- function(keywords, qa){
      if (b == sample(0:1, 1)){
        for (item in keywords){
          if (grepl(item[1], qa$a)){
            q <- gsub(item[3], item[4], qa$q)
            a <- gsub(item[1], item[2], qa$a)
          }
        }
      } else {
        q <- qa$q
        a <- qa$a
      }
      return (list(q = q, a=a))
    }
    
    keywords1 <- list(
      c(" he ", " she ", "彼", "彼女"),
      c("He ", "She ", "彼", "彼女"),
      c(" his ", " her ", "彼", "彼女"),
      c(" him\\b", " her ", "彼", "彼女"),
      
      c(" every day ", " every week ", "毎日", "毎週"),
      c("lunch", "dinner", "昼食", "夕食"),
      c("two hours", "three hours", "２時間", "３時間"),
      
      c("\\bgolf\\b", "tennis", "ゴルフ", "テニス"),
      c("\\bpiano\\b", "violin", "ピアノ", "バイオリン"),
      c(" guitar ", " piano ", "ギター", "ピアノ"),
      c(" green tea ", " black tea ", "緑茶", "紅茶"),
      c(" coffee ", " tea ", "コーヒー", "お茶"),
      c(" taxi ", " bus ", "タクシー", "バス"),
      
      c("\\bBill\\b", "John", "ビル", "ジョン"),
      c("Waseda", "Keio", "早稲田", "慶応"),
      c("Fuji Bank", "Mizuho Bank", "富士銀行", "みずほ銀行"),
      
      c("Greek", "Turkish", "ギリシャ", "トルコ"),
      c("Sweden", "Finland", "スウェーデン", "フィンランド"),
      c("Dutch", "French", "オランダ", "フランス"),
      c("San Fransisco", "New York", "サンフランシスコ", "ニューヨーク"),
      c("Yokohama", "Kobe", "横浜", "神戸"),
      c("London", "Paris", "ロンドン", "パリ"),
      c("Russian", "Ukrainian", "ロシア", "ウクライナ"),
      c("Germany", "Italy", "ドイツ", "イタリア")
    )
    qa_mod <- shuffle_text(keywords1, qa_mod)
    
    # keyword1と同時にできない
    keywords2 <- list(
      c(" she ", " he ", "彼女", "彼"),
      c("She ", "He ", "彼女", "彼")
    )
    qa_mod <- shuffle_text(keywords2, qa_mod)
    
    return(list(q = qa_mod$q, a = qa_mod$a))
}
