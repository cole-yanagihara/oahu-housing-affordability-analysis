# up317
O'ahu Gentrification Project

## Setup
This project uses the U.S. Census Bureau API.

1. Request a free API key:
https://api.census.gov/data/key_signup.html

2. In R:

```r
library(tidycensus)

census_api_key(
  "YOUR_KEY",
  install = TRUE
)
```

3. Restart R.

The analysis can now be run normally.
