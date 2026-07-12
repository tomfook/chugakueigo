#!/usr/bin/env python3
"""
tag_grammar.py — 中学英語 文法タグ付け データ準備ツール (one-off / 非アプリ実行時)

data/qlist.csv の各問題を、英語 answer のパターンから中学英語の文法カテゴリに
自動分類し、`grammar` 列（複数タグは "|" 区切り）を付与して書き戻す。

- これはアプリ実行時コードではなく、データ準備用のオフラインツール。
- 中学英語は文法集合が小さく閉じているため、answer 表層パターンで高カバレッジが出る。
- 出力はレビュー前提の「下書き」。GRAMMAR_LABELS が正準の文法カテゴリ定義。

使い方:
    python3 scripts/tag_grammar.py            # qlist.csv を上書き
    python3 scripts/tag_grammar.py --report   # 上書きせず分布とサンプルのみ表示
"""
import csv
import re
import sys
import collections

QLIST_PATH = "data/qlist.csv"

# ------------------------------------------------------------------
# 文法カテゴリの正準定義 (id -> 日本語ラベル)。data/grammar_list.csv と一致させる。
# ------------------------------------------------------------------
GRAMMAR_LABELS = [
    ("be_present",          "be動詞(現在)"),
    ("be_past",             "be動詞(過去)"),
    ("general_present",     "一般動詞(現在)"),
    ("general_past",        "一般動詞(過去)"),
    ("present_progressive", "現在進行形"),
    ("past_progressive",    "過去進行形"),
    ("present_perfect",     "現在完了"),
    ("future_will",         "未来(will)"),
    ("future_going_to",     "未来(be going to)"),
    ("can",                 "助動詞 can/could"),
    ("must_have_to",        "助動詞 must/have to"),
    ("should",              "助動詞 should"),
    ("may_might",           "助動詞 may/might"),
    ("would_like",          "would like / would"),
    ("imperative",          "命令文"),
    ("there_is",            "There is/are 構文"),
    ("wh_question",         "疑問詞疑問文"),
    ("how_many_much",       "How many / How much"),
    ("yesno_question",      "Yes/No 疑問文"),
    ("negative",            "否定文"),
    ("comparative",         "比較級"),
    ("superlative",         "最上級"),
    ("infinitive",          "to不定詞"),
    ("gerund",              "動名詞"),
    ("passive",             "受動態"),
    ("relative",            "関係代名詞"),
    ("conjunction",         "接続詞(when/because/if/that等)"),
    ("basic_svo",           "基本文(その他)"),
]
VALID_IDS = {gid for gid, _ in GRAMMAR_LABELS}

# 過去形・過去分詞の不規則動詞（中学英語で頻出のもの）
IRREGULAR_PAST = {
    "was", "were", "went", "came", "got", "made", "said", "saw", "took", "gave",
    "found", "told", "became", "left", "felt", "brought", "bought", "thought",
    "kept", "held", "wrote", "read", "met", "paid", "sold", "sent", "built",
    "began", "ran", "sat", "spoke", "stood", "understood", "won", "lost", "put",
    "cut", "let", "set", "cost", "hit", "heard", "knew", "grew", "drew", "flew",
    "drove", "rode", "rose", "chose", "broke", "spent", "lent", "meant", "slept",
    "ate", "drank", "fell", "gave", "made", "shook", "threw",
}
# be動詞・助動詞は「一般動詞の過去」判定から除外する（was/were は be_past、
# had/did は助動詞相当）。これらが混じると be動詞文が general_past に誤計上される。
# -ed で終わるが分詞形容詞として使われ過去動詞ではない語（誤検出の常連）
ED_ADJECTIVES = {
    "united", "interested", "tired", "excited", "surprised", "pleased",
    "married", "crowded", "bored", "worried", "scared", "advanced", "limited",
    "complicated", "dedicated", "educated", "retired", "located", "related",
    "supposed", "needed", "detailed", "talented", "gifted", "aged",
}
IRREGULAR_PP = {
    "been", "gone", "come", "got", "gotten", "made", "said", "seen", "taken",
    "given", "found", "told", "become", "left", "felt", "brought", "bought",
    "thought", "kept", "held", "written", "read", "met", "paid", "sold", "sent",
    "built", "begun", "run", "sat", "spoken", "stood", "understood", "won",
    "lost", "put", "cut", "let", "set", "cost", "hit", "heard", "known", "grown",
    "drawn", "flown", "driven", "ridden", "risen", "chosen", "broken", "spent",
    "eaten", "drunk", "fallen", "had", "done", "shaken", "thrown", "worn",
}

AUX_YESNO = {"do", "does", "did", "is", "are", "am", "was", "were", "have", "has",
             "can", "could", "will", "would", "should", "may", "might", "must", "shall"}
WH_WORDS = {"what", "who", "whom", "whose", "when", "where", "why", "how", "which"}


def norm(ans):
    """answer を分類用に正規化。先頭空白除去・小文字化。"""
    return ans.strip()


def tokens(text):
    return re.findall(r"[a-z']+", text.lower())


def classify(answer):
    """英語 answer から文法タグ集合を返す（複数可、下書き）。"""
    a = norm(answer)
    if not a:
        return []
    low = a.lower()
    toks = tokens(low)
    if not toks:
        return []
    tags = set()
    first = toks[0]
    is_question = a.rstrip().endswith("?")

    # 短縮形を展開語として扱うためのフラグ
    has = lambda w: (" " + w + " ") in (" " + low + " ")
    contr = low  # 短縮形はそのまま検出に使う

    # --- 文の種類 ---
    if is_question:
        if first in WH_WORDS:
            tags.add("wh_question")
            if re.search(r"\bhow (many|much)\b", low):
                tags.add("how_many_much")
        elif first in AUX_YESNO:
            tags.add("yesno_question")

    # 命令文: 主語なしで動詞原形始まり / Don't / Please / Let's
    if first in {"please", "let's", "let"} or (
        not is_question and first in {"don't", "do"} and first == "don't"
    ):
        tags.add("imperative")
    if not is_question and first not in (
        {"i", "you", "he", "she", "it", "we", "they", "this", "that", "these",
         "those", "there", "my", "his", "her", "our", "your", "their", "the",
         "a", "an"} | AUX_YESNO | WH_WORDS
    ) and re.match(r"^[a-z]+", low):
        # 動詞原形始まりの命令文らしきもの（名詞主語は拾いすぎるので代表動詞に限定）
        if first in {"stop", "come", "go", "look", "listen", "wait", "take",
                     "give", "put", "open", "close", "turn", "be", "have",
                     "tell", "call", "try", "keep", "make", "let", "sit",
                     "stand", "read", "write", "eat", "drink", "help", "show",
                     "bring", "get", "watch", "remember", "forget", "use"}:
            tags.add("imperative")

    # 否定
    if re.search(r"n't\b|\bnot\b|\bnever\b|\bno one\b|\bnobody\b|\bnothing\b", low):
        tags.add("negative")

    # There is/are
    if re.search(r"\bthere('s|'re| is| are| was| were)\b", low):
        tags.add("there_is")

    # --- 時制・相 ---
    # 現在進行形 / 過去進行形
    prog = re.search(r"\b(am|is|are|was|were|'m|'s|'re)\s+(\w+ing)\b", low)
    if prog:
        if prog.group(1) in {"was", "were"}:
            tags.add("past_progressive")
        else:
            tags.add("present_progressive")
    # be going to は進行形と別扱い
    if re.search(r"\b(am|is|are|'m|'s|'re)\s+going\s+to\b", low):
        tags.discard("present_progressive")
        tags.add("future_going_to")

    # 現在完了: have/has + pp
    m = re.search(r"\b(have|has|'ve|'s)\s+(?:\w+\s+)?(\w+)\b", low)
    if re.search(r"\b(have|has|'ve|'ve)\s+(\w+ed|been|gone|done|got|gotten|seen|made|had|met|read|written|taken|given|found|told|bought|thought|eaten|drunk|come|become)\b", low):
        tags.add("present_perfect")
    else:
        # has/have + pp(不規則) を語単位で確認
        for i, t in enumerate(toks[:-1]):
            if t in {"have", "has"} and i + 1 < len(toks):
                nxt = toks[i + 1]
                if nxt.endswith("ed") or nxt in IRREGULAR_PP:
                    tags.add("present_perfect")
                    break

    # 未来 will
    if re.search(r"\bwill\b|'ll\b|\bwon't\b", low):
        tags.add("future_will")

    # --- 助動詞 ---
    if re.search(r"\bcan('t)?\b|\bcould(n't)?\b", low):
        tags.add("can")
    if re.search(r"\bmust(n't)?\b|\bhave to\b|\bhas to\b|\bhad to\b", low):
        tags.add("must_have_to")
    if re.search(r"\bshould(n't)?\b", low):
        tags.add("should")
    if re.search(r"\bmay\b|\bmight\b", low):
        tags.add("may_might")
    if re.search(r"\bwould like\b|\bwould you\b|\bi'd like\b|\bwould\b", low):
        tags.add("would_like")

    # --- 構文 ---
    # 比較級
    if re.search(r"\bmore\s+\w+\s+than\b", low) or re.search(r"\b\w+er than\b", low) \
            or re.search(r"\bbetter than\b|\bworse than\b", low):
        tags.add("comparative")
    # 最上級
    if re.search(r"\bthe\s+(most\s+\w+|\w+est)\b", low) or re.search(r"\bthe best\b|\bthe worst\b", low):
        tags.add("superlative")
    # to不定詞
    if re.search(r"\bto\s+(be|have|go|get|do|make|see|play|study|read|write|eat|"
                 r"drink|buy|take|give|tell|call|help|use|meet|learn|start|"
                 r"finish|visit|watch|find|know|say|work|live|come|leave|stay|"
                 r"talk|speak|ask|answer|understand|remember|forget|practice)\b", low):
        tags.add("infinitive")
    # 動名詞: 文頭 Ving / like/enjoy/finish/stop/start + Ving
    if re.match(r"^\w+ing\b", low) and not is_question:
        tags.add("gerund")
    if re.search(r"\b(enjoy|enjoys|enjoyed|like|likes|liked|finish|finished|"
                 r"stop|stopped|start|started|begin|began|love|loves|hate|"
                 r"hates|keep|keeps|practice|practices|mind|minds)\s+\w+ing\b", low):
        tags.add("gerund")
    # 受動態: be + pp (+by)
    if re.search(r"\b(am|is|are|was|were|been|be)\s+(\w+ed|made|done|written|"
                 r"taken|given|found|sold|built|known|spoken|broken|seen|"
                 r"eaten|born)\b", low):
        # 進行形(-ing)や完了と競合しないよう、-ing 直後は除外済み
        if not prog:
            tags.add("passive")
    # 関係代名詞: 名詞 + who/which/that + 動詞
    if re.search(r"\b(who|which|that)\s+(is|are|was|were|has|have|can|will|"
                 r"\w+s|\w+ed)\b", low) and not is_question:
        tags.add("relative")
    # 接続詞
    if re.search(r"\b(because|when|while|if|before|after|although|though|"
                 r"since|until|so that|that)\b", low):
        # that は関係代名詞/接続詞両義だが接続詞側にも寄せる
        if re.search(r"\b(because|when|while|if|before|after|although|though|"
                     r"since|until)\b", low):
            tags.add("conjunction")

    # --- 時制の素朴分類（進行/完了/未来/助動詞がまだ無い場合） ---
    aspectual = tags & {"present_progressive", "past_progressive", "present_perfect",
                        "future_will", "future_going_to", "can", "must_have_to",
                        "should", "may_might", "would_like", "passive"}
    if not aspectual:
        # be動詞 主動詞
        if re.match(r"^(i'm|he's|she's|it's|you're|we're|they're|there's|there're)\b", low) \
                or first in {"am", "is", "are"} \
                or re.search(r"\b(i am|you are|he is|she is|it is|we are|they are|this is|"
                             r"that is|these are|those are)\b", low):
            tags.add("be_present")
        elif first in {"was", "were"} or re.search(r"\b(i|he|she|it|you|we|they|this|that)\s+(was|were)\b", low):
            tags.add("be_past")
        # 一般動詞 過去
        if first == "did" or re.search(r"\bdidn't\b", low):
            tags.add("general_past")
        else:
            for t in toks:
                if t.endswith("ed") and len(t) > 3 and t not in ED_ADJECTIVES:
                    tags.add("general_past")
                    break
                if t in IRREGULAR_PAST:
                    tags.add("general_past")
                    break
        # 一般動詞 現在
        if first in {"do", "does"} or re.search(r"\bdon't\b|\bdoesn't\b", low):
            tags.add("general_present")

    # be動詞現在/過去（相があっても be主動詞なら付ける弱シグナルは省略。素朴分類のみ）

    # フォールバック
    if not tags or tags == {"negative"} or tags <= {"negative", "wh_question",
                                                    "yesno_question", "how_many_much",
                                                    "imperative"}:
        # 時制/構文が全く取れなかった → 基本文
        if not (tags & (set(g for g, _ in GRAMMAR_LABELS) - {"negative", "wh_question",
                        "yesno_question", "how_many_much", "imperative", "basic_svo"})):
            tags.add("basic_svo")

    return [g for g, _ in GRAMMAR_LABELS if g in tags]


def main():
    report_only = "--report" in sys.argv
    with open(QLIST_PATH, encoding="utf-8", newline="") as f:
        raw = f.read()
    # コメント行(#)を保持しつつ読む
    lines = raw.splitlines(keepends=True)
    comment_lines = [ln for ln in lines if ln.startswith("#")]
    data_lines = [ln for ln in lines if not ln.startswith("#")]
    reader = csv.DictReader(data_lines)
    fieldnames = list(reader.fieldnames)
    rows = list(reader)

    dist = collections.Counter()
    untagged = 0
    per_q = {}
    for row in rows:
        tags = classify(row["answer"])
        per_q[row["question_id"]] = tags
        if not tags:
            if row["answer"].strip():
                untagged += 1
        for t in tags:
            dist[t] += 1

    active = [r for r in rows if r["answer"].strip()]
    print(f"active(non-empty) questions: {len(active)}")
    print(f"untagged active questions:   {untagged}")
    print(f"avg tags/question:           {sum(len(per_q[r['question_id']]) for r in active)/max(1,len(active)):.2f}")
    print("\n-- grammar distribution --")
    for gid, label in GRAMMAR_LABELS:
        print(f"  {gid:20s} {label:22s} {dist.get(gid,0):5d}")

    print("\n-- samples --")
    import itertools
    for r in itertools.islice(active, 0, len(active), max(1, len(active)//25)):
        print(f"  [{'|'.join(per_q[r['question_id']]) or 'NONE':45s}] {r['answer'].strip()[:60]}")

    if report_only:
        return

    # grammar 列を追加/更新して書き戻し
    if "grammar" not in fieldnames:
        # question, answer の後ろ、created_at の前に挿入
        idx = fieldnames.index("answer") + 1
        fieldnames = fieldnames[:idx] + ["grammar"] + fieldnames[idx:]
    for row in rows:
        row["grammar"] = "|".join(per_q[row["question_id"]])

    # R の write.csv 流儀に合わせる: ヘッダは全て引用、データ行は文字列列のみ引用し
    # question_id(整数) と is_active(論理) は裸で出す。既存の差分を最小化するため。
    bare_cols = {"question_id", "is_active"}

    def fmt(col, val):
        if col in bare_cols:
            return val  # 引用符なし
        esc = val.replace('"', '""')
        return f'"{esc}"'

    with open(QLIST_PATH, "w", encoding="utf-8", newline="") as f:
        for cl in comment_lines:
            f.write(cl)
        f.write(",".join(f'"{c}"' for c in fieldnames) + "\n")
        for row in rows:
            f.write(",".join(fmt(c, row[c]) for c in fieldnames) + "\n")
    print(f"\nwrote grammar column to {QLIST_PATH}")


if __name__ == "__main__":
    main()
