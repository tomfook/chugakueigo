# UI Styles
UI <- list(
  STYLES = list(
    ACTIVE = "color:black;",
    INACTIVE = "color:gray;",
    DANGER = "background-color: #d9534f; color: white;",
    WARNING = "font-weight: bold; color: #d9534f;"
  ),
  DEFAULTS = list(
    USER = "guest",
    ZERO_LIMIT = 5L,
    PROBABILITY_BASE = 1.6
  )
)

LEARNING <- list(
  PROBABILITY = list(
    BASE = 1.6,
    STEP = 0.1,
    MIN = 1.0,
    MULTIPLIER = 0.005
  ),
  SHUFFLE = list(
    SELECTION_RATE = 0.5,
    MIN_SELECTION = 1
  )
)

DATA <- list(
  PATHS = list(
    QUESTIONS = "data/qlist.csv",
    SERVICE_ACCOUNT_KEY = "credentials/service-account-key.json"
  ),
  SHEETS = list(
    SCORES = "1p0PyFuH9mCm2A-UtXCZ2FvpXbGPEJoueXG-JA0blz14"
  )
)

ADMIN <- list(
  PASSWORD_HASH = "8c8f9f17f61d0a0fca3a0fe34dcdc555bd71cd9d08ebff5590baa4338792a9be",
  SALT = "3246807a6f37be90",
  HASH_ALGORITHM = "sha256"
)
