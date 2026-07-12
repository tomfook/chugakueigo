APP <- list(
  DEFAULTS = list(
    USER = "guest"
  ),
  USERNAME = list(
    MAX_LENGTH = 50L,
    PATTERN = "^[a-zA-Z0-9_-]+$",
    ALLOWED_CHARS_DESC = "letters, numbers, underscore, and hyphen"
  )
)

LEARNING <- list(
  DEFAULTS = list(
    PROBABILITY_BASE = 1.6,
    MAX_ZERO_SCORE_QUESTIONS = 5L,
    # 文法般化の重み alpha。問題単位スコアに alpha、文法単位スコアに (1-alpha) を配分。
    # 1.0 = 文法般化オフ（従来と完全一致）。小さいほど文法単位の弱点が選択に効く。
    GRAMMAR_ALPHA = 1.0
  ),
  PROBABILITY = list(
    STEP = 0.1,
    MIN = 1.0,
    MULTIPLIER = 0.005
  ),
  # 問題単位スコアから導出する文法単位の適応設定
  GRAMMAR = list(
    ALPHA_STEP = 0.1,
    ALPHA_MIN = 0.0,
    ALPHA_MAX = 1.0,
    TAG_SEPARATOR = "|"   # qlist.csv の grammar 列で複数タグを区切る文字
  ),
  SHUFFLE = list(
    SELECTION_RATE = 0.5,
    MIN_SELECTION = 1
  )
)

DATA <- list(
  PATHS = list(
    QUESTIONS = "data/qlist.csv",
    GRAMMAR_LIST = "data/grammar_list.csv",
    SERVICE_ACCOUNT_KEY = "credentials/service-account-key.json"
  ),
  SHEETS = list(
    SCORES = "1p0PyFuH9mCm2A-UtXCZ2FvpXbGPEJoueXG-JA0blz14",
    USERS_META = "1p0PyFuH9mCm2A-UtXCZ2FvpXbGPEJoueXG-JA0blz14"
  )
)

ADMIN <- list(
  PASSWORD_HASH = "8c8f9f17f61d0a0fca3a0fe34dcdc555bd71cd9d08ebff5590baa4338792a9be",
  SALT = "3246807a6f37be90",
  HASH_ALGORITHM = "sha256"
)

CACHE <- list(
  ENABLED = TRUE,
  SHEET_NAMES = list(
    TTL = 300
  ),
  USERS_META = list(
    TTL = 300
  )
)
