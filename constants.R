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
  DELETE_PASSWORD = "delete2024"
)
