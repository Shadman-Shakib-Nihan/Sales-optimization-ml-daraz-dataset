# ══════════════════════════════════════════════════════════════════════════════
# DARAZ SCRAPER v4 — FINAL (confirmed field names from live response)
# Endpoint: /category/?ajax=true&isFirstRequest=true&page=N
# Key path:  json$mods$listItems
# ══════════════════════════════════════════════════════════════════════════════
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

# ══════════════════════════════════════════════════════════════════════════════
# CONFIG
# ══════════════════════════════════════════════════════════════════════════════
MAX_PAGES <- 15

CATEGORIES <- list(
  "Womens Fashion" = "womens-fashion",
  "Mens Fashion"   = "mens-fashion",
  "Mobiles"        = "phones-tablets",
  "Electronics"    = "smartphones",
  "Health Beauty"  = "health-beauty",
  "Home Lifestyle" = "Refrigerators",
  "Watches"        = "watches-accessories",
  "Sports"         = "Citizen-Sports",
  "Babies Toys"    = "babies-toys",
  "Groceries"      = "groceries-pets",
  "TV Appliances"  = "tv-home-appliances",
  "Automotive"     = "automotive-motorbike"
)

USER_AGENTS <- c(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:125.0) Gecko/20100101 Firefox/125.0"
)

# ══════════════════════════════════════════════════════════════════════════════
# SESSION — warm up with homepage visit to get cookies
# ══════════════════════════════════════════════════════════════════════════════
message("🌐 Starting session...")
sess <- httr::handle("https://www.daraz.com.bd")
httr::GET("https://www.daraz.com.bd/",
  handle = sess,
  httr::add_headers(
    `User-Agent`      = sample(USER_AGENTS, 1),
    `Accept`          = "text/html,application/xhtml+xml,*/*",
    `Accept-Language` = "en-US,en;q=0.9"
  ), httr::timeout(20)
)
Sys.sleep(3)
message("✅ Session ready\n")

# ══════════════════════════════════════════════════════════════════════════════
# FETCH ONE PAGE → returns parsed JSON or NULL
# ══════════════════════════════════════════════════════════════════════════════
fetch_page <- function(slug, page = 1) {
  first <- if (page == 1) "&isFirstRequest=true" else ""
  url <- sprintf(
    "https://www.daraz.com.bd/%s/?ajax=true%s&page=%d",
    slug, first, page
  )
  ref <- sprintf("https://www.daraz.com.bd/%s/?page=%d", slug, max(1, page - 1))

  resp <- tryCatch(
    httr::GET(url,
      handle = sess,
      httr::add_headers(
        `User-Agent`       = sample(USER_AGENTS, 1),
        `Accept`           = "application/json, text/plain, */*",
        `Accept-Language`  = "en-US,en;q=0.9",
        `Referer`          = ref,
        `X-Requested-With` = "XMLHttpRequest"
      ), httr::timeout(20)
    ),
    error = function(e) {
      message("  ❌ Connection: ", e$message)
      NULL
    }
  )

  if (is.null(resp)) {
    return(NULL)
  }

  sc <- httr::status_code(resp)
  if (sc != 200) {
    message(sprintf("  ❌ HTTP %d", sc))
    return(NULL)
  }

  body <- httr::content(resp, "text", encoding = "UTF-8")

  # Reject HTML responses (anti-bot redirect)
  if (grepl("^\\s*<!DOCTYPE|^\\s*<html", body, ignore.case = TRUE)) {
    message("  ❌ Got HTML instead of JSON (bot detection) — sleeping 10s")
    Sys.sleep(10)
    return(NULL)
  }

  tryCatch(
    jsonlite::fromJSON(body, flatten = TRUE),
    error = function(e) {
      message("  ❌ JSON parse: ", e$message)
      NULL
    }
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# PARSE listItems — exact column names confirmed from live response
# ══════════════════════════════════════════════════════════════════════════════
parse_items <- function(json, category_name) {
  items <- tryCatch(json$mods$listItems, error = function(e) NULL)

  if (is.null(items) || !is.data.frame(items) || nrow(items) == 0) {
    return(NULL)
  }

  # Helper: safely get a column, return NA vector if missing
  col <- function(df, ...) {
    for (nm in c(...)) {
      if (nm %in% names(df)) {
        return(as.character(df[[nm]]))
      }
    }
    rep(NA_character_, nrow(df))
  }

  # Build clean product URLs
  item_url <- col(items, "itemUrl")
  prod_url <- dplyr::case_when(
    is.na(item_url) ~ NA_character_,
    startsWith(item_url, "http") ~ item_url,
    startsWith(item_url, "//") ~ paste0("https:", item_url),
    TRUE ~ paste0("https://www.daraz.com.bd", item_url)
  )

  tibble::tibble(
    Category      = category_name,
    Name          = col(items, "name", "title"),
    Brand         = col(items, "brandName", "brand"),
    Price_Current = col(items, "price"), # numeric in BDT e.g. 30
    Price_MRP     = col(items, "originalPrice"), # numeric e.g. 409
    Price_Show    = col(items, "priceShow"), # formatted e.g. "৳ 30"
    Discount      = col(items, "discount"), # e.g. "93% Off"
    Rating        = col(items, "ratingScore"), # e.g. 4.44
    Reviews       = col(items, "review"), # e.g. 667
    Units_Sold    = col(items, "itemSoldCntShow"), # e.g. "10.0K sold"
    Seller        = col(items, "sellerName"),
    Seller_ID     = col(items, "sellerId"),
    Product_ID    = col(items, "itemId", "nid"),
    Product_URL   = prod_url,
    Scraped_At    = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# MAIN LOOP
# ══════════════════════════════════════════════════════════════════════════════
all_data <- list()

for (cat_name in names(CATEGORIES)) {
  slug <- CATEGORIES[[cat_name]]
  message(sprintf("📁 %s", cat_name))
  cat_rows <- list()
  empty_count <- 0

  for (p in seq_len(MAX_PAGES)) {
    message(sprintf("  Page %d...", p))

    json <- fetch_page(slug, p)
    rows <- parse_items(json, cat_name)

    if (is.null(rows) || nrow(rows) == 0) {
      empty_count <- empty_count + 1
      message("  ⚠️  No products")
      if (empty_count >= 2) {
        message("  ⏹ Stopping early")
        break
      }
    } else {
      empty_count <- 0
      cat_rows[[p]] <- rows
      message(sprintf("  ✅ %d products", nrow(rows)))
    }

    Sys.sleep(runif(1, 1.5, 2.5))
  }

  if (length(cat_rows) > 0) {
    merged <- dplyr::bind_rows(cat_rows)
    all_data[[cat_name]] <- merged
    message(sprintf("  → %s total: %d\n", cat_name, nrow(merged)))
  } else {
    message(sprintf("  ❌ No data for %s\n", cat_name))
  }

  Sys.sleep(runif(1, 2, 4))
}

# ══════════════════════════════════════════════════════════════════════════════
# CLEAN & SAVE
# ══════════════════════════════════════════════════════════════════════════════
if (length(all_data) == 0) stop("❌ No data collected.")

final_df <- dplyr::bind_rows(all_data) %>%
  dplyr::mutate(
    Price_Current = suppressWarnings(as.numeric(Price_Current)),
    Price_MRP     = suppressWarnings(as.numeric(Price_MRP)),
    Discount_Pct  = suppressWarnings(as.numeric(stringr::str_extract(Discount, "[0-9]+"))),
    Reviews       = suppressWarnings(as.integer(Reviews)),
    Rating        = suppressWarnings(as.numeric(Rating)),
    In_Stock      = In_Stock == "TRUE"
  ) %>%
  dplyr::filter(!is.na(Name), nchar(trimws(Name)) > 0) %>%
  dplyr::distinct(Product_ID, .keep_all = TRUE)

write.csv(final_df, "daraz_final_2.csv", row.names = FALSE)

# ── Report ─────────────────────────────────────────────────────────────────
pct <- function(x) sprintf("%d (%.0f%%)", sum(!is.na(x)), 100 * mean(!is.na(x)))

message("\n📊 Summary:")
message(sprintf("  Total products : %d", nrow(final_df)))
message(sprintf("  Price          : %s", pct(final_df$Price_Current)))
message(sprintf("  Rating         : %s", pct(final_df$Rating)))
message(sprintf("  Reviews        : %s", pct(final_df$Reviews)))
message(sprintf("  Brand          : %s", pct(final_df$Brand)))
message(sprintf("  Units Sold     : %s", pct(final_df$Units_Sold)))
message(sprintf("  Seller         : %s", pct(final_df$Seller)))

cat("\nProducts per category:\n")
print(final_df %>% dplyr::count(Category, sort = TRUE))

message("\n✅ Saved → daraz_final_2.csv")
print(head(final_df[, c(
  "Category", "Name", "Price_Current", "Price_MRP",
  "Discount", "Rating", "Reviews", "Units_Sold", "Seller"
)], 5))

final_df <- read.csv("daraz_final_2.csv", stringsAsFactors = FALSE)
View(final_df)
