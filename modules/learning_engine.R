# ================================
# LEARNING ENGINE MODULE
# Handles question selection, text shuffling, and learning session logic
# ================================

source("constants.R")
source("shuffle_config.R")

# =================
# QUESTION SELECTION FUNCTIONS
# =================
learning_select_next_question <- function(learning_state, qa_data) {
  question_index <- sample(
    length(learning_state$probabilities),
    1,
    prob = learning_state$probabilities
  )

    shuffled_qa <- learning_shuffle_question(qa_data$question[question_index], qa_data$answer[question_index])

    list(
      question_id = qa_data$question_id[question_index],
      question = shuffled_qa$q,
      answer = shuffled_qa$a
    )
}

# =================
# LEARNING SESSION FUNCTIONS
# =================
learning_new_question <- function(learning_state, qa_data, config_state) {
  next_q <- learning_select_next_question(learning_state, qa_data)
  learning_state$question_id <- next_q$question_id
  learning_state$question <- next_q$question
  learning_state$correct_answer <- next_q$answer
  learning_state$answer <- ""
  learning_state$question_count <- learning_state$question_count + 1L
  return(learning_state)
}

learning_update_probability <- function(learning_state, config_state, effective_score, grammar_meta = NULL) {
  # 問題単位スコアに文法単位の弱点をブレンドした「実効弱点」を指数に用いる。
  # alpha=1（既定）では blended == effective_score となり従来の式に完全一致する。
  alpha <- if (!is.null(config_state$grammar_alpha)) config_state$grammar_alpha else LEARNING$DEFAULTS$GRAMMAR_ALPHA
  weakness <- learning_blended_weakness(effective_score, grammar_meta, alpha)

  base_probabilities <- (abs(config_state$prob_base - learning_state$correct_count * LEARNING$PROBABILITY$MULTIPLIER -1) + 1)^(-weakness) * (cumsum(effective_score == 0L) <= config_state$zero_limit)

  #masking
  learning_state$probabilities <- base_probabilities
  question_ids <- as.integer(names(effective_score))
  out_of_range_ids <- question_ids[(question_ids < config_state$range_min) | (question_ids > config_state$range_max)]
  learning_state$probabilities[as.character(out_of_range_ids)] <- 0

  return(learning_state)
}

learning_start_session <- function(learning_state, qa_data, config_state) {
  learning_state$start <- TRUE
  learning_state$question_count <- 0L
  learning_state$correct_count <- 0L
  learning_state <- learning_new_question(learning_state, qa_data, config_state)
  return(learning_state)
}

learning_handle_feedback <- function(learning_state, qa_data, config_state, is_correct) {
  if (!learning_state$start) {
    return(list(success = FALSE, updated_learning_state = learning_state, message = "Learning not started"))
  }

  if (learning_state$answer == "") {
    return(list(success = FALSE, updated_learning_state = learning_state, message = "Confirm Answer!"))
  }

  if (is_correct) {
    learning_state$current_score[learning_state$question_id] <- learning_state$current_score[learning_state$question_id] + 1L
    learning_state$correct_count <- learning_state$correct_count + 1L
  }

  learning_state <- learning_new_question(learning_state, qa_data, config_state)

  return(list(success = TRUE, updated_learning_state = learning_state, message = ""))
}

# =================
# GRAMMAR ADAPTATION FUNCTIONS
# 問題単位スコアを流用して文法単位の弱点を導出し、選択確率にブレンドする。
# 新しいスコアは保存しない（既存の per-question スコアの集約のみ）。
# =================

# qa_data$grammar 列（"tag1|tag2" 形式）から文法メタ情報を組み立てる純関数。
#   ids        : データ中に出現する文法idベクトル
#   members    : 文法id -> それを含む question_id(character) ベクトル
#   q_grammars : question_id(character) -> 文法idベクトル
learning_build_grammar_meta <- function(qa_data) {
  sep <- LEARNING$GRAMMAR$TAG_SEPARATOR
  qids <- as.character(qa_data$question_id)
  grammar_col <- if ("grammar" %in% names(qa_data)) qa_data$grammar else rep("", nrow(qa_data))
  grammar_col[is.na(grammar_col)] <- ""

  q_grammars <- setNames(
    lapply(grammar_col, function(g) {
      if (is.null(g) || g == "") return(character(0))
      tags <- trimws(strsplit(g, sep, fixed = TRUE)[[1]])
      tags[tags != ""]
    }),
    qids
  )

  members <- list()
  for (qid in qids) {
    for (g in q_grammars[[qid]]) {
      members[[g]] <- c(members[[g]], qid)
    }
  }

  list(ids = names(members), members = members, q_grammars = q_grammars)
}

# 文法単位の弱点スコア ḡ_g = その文法を含む問題の実効スコア平均。
# 未出題(score 0)も平均に含めるため、未着手の文法ほど弱点として浮上する。
learning_grammar_scores <- function(effective_score, grammar_meta) {
  if (is.null(grammar_meta) || length(grammar_meta$members) == 0) {
    return(numeric(0))
  }
  vapply(grammar_meta$members, function(qids) {
    vals <- effective_score[qids]
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NA_real_)
    mean(vals)
  }, numeric(1))
}

# 問題単位スコアと文法単位弱点をブレンドした実効弱点を返す。
#   s̃_i = alpha * s_i + (1 - alpha) * min_{g in grammars(i)} ḡ_g
# 複数文法を持つ問題は最も弱い文法（min）に引っ張られる（弱点最優先）。
# alpha>=1 / grammar_meta が無い場合は s_i そのまま（従来と完全一致の高速パス）。
learning_blended_weakness <- function(effective_score, grammar_meta, alpha) {
  if (is.null(grammar_meta) || is.null(alpha) || alpha >= 1 ||
      length(grammar_meta$members) == 0) {
    return(effective_score)
  }

  gmean <- learning_grammar_scores(effective_score, grammar_meta)
  qids <- names(effective_score)

  grammar_component <- vapply(qids, function(qid) {
    gs <- grammar_meta$q_grammars[[qid]]
    if (is.null(gs) || length(gs) == 0) {
      return(as.numeric(effective_score[[qid]]))  # 文法未タグ→自身のスコアで中立
    }
    vals <- gmean[gs]
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(as.numeric(effective_score[[qid]]))
    min(vals)
  }, numeric(1))

  blended <- alpha * as.numeric(effective_score) + (1 - alpha) * grammar_component
  setNames(blended, qids)
}

# =================
# SHUFFLE FUNCTIONS
# Text randomization to prevent memorization
# =================

learning_apply_shuffle_keywords <- function(qa_pair, keywords, selection_rate) {
  selected_keywords <- keywords[runif(length(keywords)) < selection_rate]

  # 一意のプレースホルダ。english/japanese いずれの語にも含まれない制御文字列を使う。
  sentinel <- "SHUFFLE_SWAP"

  for (keyword in selected_keywords) {
    pattern1 <- paste0("\\b", keyword$english[1], "\\b")
    pattern2 <- paste0("\\b", keyword$english[2], "\\b")

    if (grepl(pattern1, qa_pair$a, ignore.case = FALSE) ||
        grepl(pattern2, qa_pair$a, ignore.case = FALSE)) {
      # 真の入れ替え（A<->B）。片方向の gsub だと両語が同居する文で
      # "Saturday and Sunday" -> "Sunday and Sunday" のように崩れるため、
      # プレースホルダ経由で双方向にスワップする。
      a <- gsub(pattern1, sentinel, qa_pair$a, ignore.case = FALSE)
      a <- gsub(pattern2, keyword$english[1], a, ignore.case = FALSE)
      a <- gsub(sentinel, keyword$english[2], a, fixed = TRUE)
      qa_pair$a <- a

      q <- gsub(keyword$japanese[1], sentinel, qa_pair$q, fixed = TRUE)
      q <- gsub(keyword$japanese[2], keyword$japanese[1], q, fixed = TRUE)
      q <- gsub(sentinel, keyword$japanese[2], q, fixed = TRUE)
      qa_pair$q <- q
    }
  }

  return(qa_pair)
}
learning_shuffle_question <- function(q, a){
    qa_pair <- list(q = q, a = a)
    qa_pair <- learning_apply_shuffle_keywords(qa_pair, SHUFFLE_KEYWORDS, LEARNING$SHUFFLE$SELECTION_RATE)
    return(list(q = qa_pair$q, a = qa_pair$a))
}
