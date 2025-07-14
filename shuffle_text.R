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
      c("He ", "She ", "彼女", "彼"),
      c(" his ", " her ", "彼", "彼女"),
      c(" him\\b", " her ", "彼", "彼女"),
      c(" every day ", " every week ", "毎日", "毎週"),
      c("\\bgolf\\b", "tennis", "ゴルフ", "テニス"),
      c("\\bpiano\\b", "violin", "ピアノ", "バイオリン"),
      c("\\bBill\\b", "John", "ビル", "ジョン"),
      c("Waseda", "Keio", "早稲田", "慶応"),
      c("Greek", "Turkish", "ギリシャ", "トルコ")
    )
    qa_mod <- shuffle_text(keywords1, qa_mod)
    
    # keyword1と同時にできない
    keywords2 <- list(
      c(" she ", " he ", "彼女", "彼"),
      c("She ", "He ", "彼女", "彼")
    )
    qa_mod <- shuffle_text(keywords2, qa_mod)
    
    # 必ずペアで実行する
    keywords3 <- list(
      c("\\bfather\\b", "TEMP_FATHER", "父", "TEMP_父"),
      c("\\bmother\\b", "father", "母", "父"),
      c("TEMP_FATHER", "mother", "TEMP_父", "母")
    )
    qa_mod <- shuffle_text(keywords3, qa_mod)
    
    return(list(q = qa_mod$q, a = qa_mod$a))
}
