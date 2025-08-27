source("constants.R")

learning_shuffle_question <- function(q, a){
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

learning_select_next_question <- function(main, qa_state){
    question_index <- sample(
      qa_state$range.max - qa_state$range.min + 1L,
      1,
      prob = qa_state$prob
    ) + (qa_state$range.min - 1L)

    shuffled_qa <- learning_shuffle_question(main$question[question_index], main$answer[question_index])

    list(
      index = question_index,
      question = shuffled_qa$q,
      answer = shuffled_qa$a
    )
}

learning_new_question <- function(main, qa_state) {
  next_q <- learning_select_next_question(main, qa_state)
  qa_state$index <- next_q$index
  qa_state$question <- next_q$question
  qa_state$answer.remember <- next_q$answer
  qa_state$answer <- ""
  qa_state$trial <- qa_state$trial + 1L
  return(qa_state)
}

learning_update_range_and_probability <- function(qa, slider_range, prob_base, zero_limit, main_data) {
  if(is.null(slider_range[1])) {
    qa$range.min <- 1L
  } else {
    qa$range.min <- slider_range[1]
  }

  if(is.null(slider_range[2])) {
    qa$range.max <- nrow(main_data)
  } else {
    qa$range.max <- slider_range[2]
  }

  score_range <- qa$score[seq(qa$range.min, qa$range.max)]
  qa$prob <- (abs(prob_base - qa$ok * SCORING$MULTIPLIER - 1) + 1)^(-score_range) * (cumsum(score_range == 0L) <= zero_limit)

  return(qa)
}

learning_start_session <- function(qa, main_data) {
  qa$trial <- 0L
  qa$ok <- 0L
  qa$start <- TRUE
  qa <- learning_new_question(main_data, qa)
  return(qa)
}

learning_handle_ok_feedback <- function(qa, main_data) {
  if (!qa$start) {
    return(list(success = FALSE, updated_qa = qa, message = "Learning not started"))
  }

  if (qa$answer == "") {
    return(list(success = FALSE, updated_qa = qa, message = "Confirm Answer!"))
  }

  qa$score[qa$index] <- qa$score[qa$index] + 1L
  qa$ok <- qa$ok + 1L
  qa <- learning_new_question(main_data, qa)

  return(list(success = TRUE, updated_qa = qa, message = ""))
}

learning_handle_ng_feedback <- function(qa, main_data) {
  if (!qa$start) {
    return(list(success = FALSE, updated_qa = qa, message = "Learning not started"))
  }

  if (qa$answer == "") {
    return(list(success = FALSE, updated_qa = qa, message = "Confirm Answer!"))
  }

  qa <- learning_new_question(main_data, qa)

  return(list(success = TRUE, updated_qa = qa, message = ""))
}
