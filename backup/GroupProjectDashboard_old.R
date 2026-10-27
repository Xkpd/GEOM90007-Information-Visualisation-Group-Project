# MELBOURNE EXPLORER - Person B
# Shiny structure, six area-experience levels, check-ins and saved progress.
#
# RUN from the group project folder (R console):
#   shiny::runApp("GroupProjectDashboard.R", port = 3839)
# Use the same browser and address/port to reopen a saved journal. Required R
# packages: shiny, leaflet, sf, jsonlite, htmltools. Install separately; the app
# never installs packages or downloads source datasets. Internet is needed for
# basemap tiles and the eventual Tableau workbook, not the progress engine.
# Keep R/_disable_autoload.R: it stops Shiny auto-running the offline prep script.
#
# BASELINE AND SCOPE
# The folder's Handover.md supplies the tourism purpose and Explore / When to go /
# Discover tabs. The user confirmed Person B's level/check-in/persistence tasks.
# Prepared geographic assets come from the team's existing local Git reference
# origin/style/noirj0e at ff4a0ed. The layout follows the group's compact map idea.
# The folder's app.R delegates to this dashboard for the standard Shiny launch.
# data/person_b_reference is an unchanged snapshot of that branch's prepared
# places.csv, areas.geojson and approximate ftz.geojson, plus preparation source.
# The geographic units are CLUE statistical areas, NOT official metro suburbs.
#
# A -> B: enriched data/processed/places.csv (name,category,sub,lat,lon,area,
# in_ftz,rare) + areas.geojson (area), optional ftz.geojson. When those complete
# files are absent, the bundled team reference is used. Source sequential IDs
# are replaced with content-based stable IDs before saving user progress.
# Ped sensor rows are context, never achievement targets. Their `sub` contains
# location_id, not necessarily the physical sensor_id. Do not silently rename.
#
# A -> C: optional data/processed/ped_agg.csv or ped_agg.rds columns:
# location_id, weekday (1=Monday ... 7=Sunday), hour (0..23), mean_count;
# optional n_observations. A must verify IDs, sampling dates and coverage.
# C -> B: set these before running the app, or in the project's .Renviron:
#   MELBOURNE_TABLEAU_URL=https://public.tableau.com/views/WORKBOOK/VIEW
#   MELBOURNE_TABLEAU_LOCATION_FIELD=location_id
# C must provide the actual published workbook. No placeholder satisfies the
# assignment's Tableau requirement. Local progress CSVs are snapshots; they do
# not update Tableau Public automatically. Embed API reference:
# https://help.tableau.com/current/api/embedding_api/en-us/docs/embedding_api_filter.html
#
# B -> D shared reactive hooks (inside ml_server): selected_suburb(),
# selected_place(), selected_sensor(), filtered_places(), shared_progress(),
# shared_visits(), shared_stats(), achievements(). D can replace presentation
# and milestone rules while keeping the same validated state.
#
# DESIGN / REVIEW NOTES
# Overview -> filters -> selection -> explicit check-in reduces accidental edits.
# Sequential blues encode six ordinal experience levels; category colours and
# hollow/filled markers encode different variables. Text repeats colour meaning.
# Clusters reduce overplotting; map updates preserve the viewport. Keyboard
# selectors provide alternatives to map clicks. Familiarity is self-reported
# and independent of visits. All denominators refer to the loaded study areas.
# Undo, staged imports, reset confirmation and visible save errors support recovery.
# Browser saves are scoped to dataset + browser origin, not a shared server file.
# A second tab cannot silently overwrite a newer save. No location/GPS is requested.
# The app starts with no fabricated visits, ratings or crowd measurements.

required_packages <- c("shiny", "leaflet", "sf", "jsonlite", "htmltools")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Missing R packages: ", paste(missing_packages, collapse = ", "), ". Install these separately before running the app.", call. = FALSE)
library(shiny)
library(leaflet)

ml_script_dir <- function() {
  override <- getOption("melbourne.project_dir", "")
  if (nzchar(override)) return(normalizePath(override, mustWork = TRUE))
  files <- lapply(sys.frames(), function(f) f$ofile)
  files <- Filter(function(x) is.character(x) && length(x) == 1L, files)
  if (length(files)) return(dirname(normalizePath(tail(files, 1)[[1]], mustWork = FALSE)))
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg)) return(dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", arg[1]), fixed = TRUE), mustWork = FALSE)))
  getwd()
}
ML_PROJECT_DIR <- ml_script_dir()
source(file.path(ML_PROJECT_DIR, "R", "discovery.R"), local = TRUE)
source(file.path(ML_PROJECT_DIR, "R", "discovery_views.R"), local = TRUE)


# ---- Validated progress model ----
ml_levels <- data.frame(
  level = 0:5,
  label = c("Never been", "Passed through", "Stopped by", "Explored", "Stayed the night", "Lived there"),
  description = c(
    "I have never visited this area.",
    "I have travelled through this area without making a visit.",
    "I have stopped at a place in this area.",
    "I have spent time exploring several places in this area.",
    "I have stayed overnight in this area.",
    "I live here now, or have lived in this area before."
  ),
  color = c("#E8EEF5", "#C8DDF0", "#9AC4E3", "#66A5D1", "#377DB2", "#145789"),
  stringsAsFactors = FALSE
)

ml_today <- function() as.Date(format(Sys.time(), tz = "Australia/Melbourne", format = "%Y-%m-%d"))
ml_now <- function() format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
ml_stop <- function(message) stop(message, call. = FALSE)
ml_is_scalar_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
ml_is_integer <- function(x) is.numeric(x) && !anyNA(x) && all(is.finite(x)) && all(x == floor(x))
ml_require_columns <- function(x, columns, context) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) ml_stop(paste0(context, " is missing: ", paste(missing, collapse = ", "), "."))
  if (anyDuplicated(names(x))) ml_stop(paste0(context, " contains duplicate column names."))
}
ml_validate_ids <- function(x, context, unique = TRUE) {
  if (!is.character(x) || anyNA(x) || any(!grepl("^[A-Za-z0-9][A-Za-z0-9_.:-]{0,79}$", x))) {
    ml_stop(paste0(context, " must be stable non-empty IDs (up to 80 letters, numbers, _, -, ., or :)."))
  }
  if (unique && anyDuplicated(x)) ml_stop(paste0(context, " contains duplicate IDs."))
}
ml_validate_text <- function(x, context, max_length = 200L) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(trimws(x))) || any(nchar(x, type = "chars") > max_length)) {
    ml_stop(paste0(context, " must contain non-empty text of at most ", max_length, " characters."))
  }
  if (any(grepl("[[:cntrl:]]", x))) ml_stop(paste0(context, " contains unsupported control characters."))
}
ml_coordinates <- function(x, context) {
  for (field in c("lat", "lon")) {
    value <- suppressWarnings(as.numeric(x[[field]]))
    bound <- if (field == "lat") 90 else 180
    if (anyNA(value) || any(!is.finite(value)) || any(abs(value) > bound)) {
      ml_stop(paste0(context, " has invalid WGS84 ", field, " coordinates."))
    }
    x[[field]] <- value
  }
  x
}
ml_dataset_id <- function(suburbs, places, mode) {
  # A deterministic identifier, not a cryptographic signature. Reordering source
  # rows is safe; adding/removing IDs or changing a place/suburb link is not.
  links <- paste(places$place_id, places$suburb_id, sep = "=")
  value <- paste(c(mode, sort(suburbs$suburb_id), "|", sort(links)), collapse = "\n")
  bytes <- utf8ToInt(enc2utf8(value)); first <- 17; second <- 29
  for (byte in bytes) {
    first <- (first * 131 + byte) %% 2147483647
    second <- (second * 137 + byte) %% 2147483629
  }
  paste0("melbourne-level-up-", mode, "-", format(first, scientific = FALSE, trim = TRUE), "-", format(second, scientific = FALSE, trim = TRUE))
}
ml_empty_visits <- function() data.frame(visit_id = character(), suburb_id = character(), place_id = character(), visited_on = character(), stringsAsFactors = FALSE)
ml_empty_state <- function(data) {
  list(schema_version = 1L, dataset_id = data$dataset_id,
       progress = data.frame(suburb_id = data$suburbs$suburb_id, familiarity = rep(0L, nrow(data$suburbs)), stringsAsFactors = FALSE),
       visits = ml_empty_visits(), goal = as.integer(min(3L, nrow(data$suburbs))), updated_at = ml_now())
}
ml_state_frame <- function(value, columns, context) {
  if (is.list(value) && !length(value) && is.null(names(value))) {
    result <- as.data.frame(setNames(rep(list(character()), length(columns)), columns), stringsAsFactors = FALSE)
  } else if (is.data.frame(value)) result <- value else ml_stop(paste0(context, " must be a JSON array of records."))
  if (!setequal(names(result), columns) || anyDuplicated(names(result))) ml_stop(paste0(context, " has missing or unexpected fields."))
  if (nrow(result) > 20000L) ml_stop(paste0(context, " exceeds the 20,000-record limit."))
  result <- result[, columns, drop = FALSE]
  rownames(result) <- NULL
  result
}
ml_validate_date <- function(x) {
  if (!is.character(x) || anyNA(x) || any(!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x))) ml_stop("Visit dates must use YYYY-MM-DD.")
  parsed <- tryCatch(as.Date(x, format = "%Y-%m-%d"), error = function(e) rep(as.Date(NA), length(x)))
  if (anyNA(parsed) || any(format(parsed, "%Y-%m-%d") != x)) ml_stop("One or more visit dates are not real calendar dates.")
  if (any(parsed > ml_today())) ml_stop("A check-in cannot be dated in the future (Melbourne time).")
  if (any(parsed < as.Date("1900-01-01"))) ml_stop("Visit dates must be on or after 1900-01-01.")
  x
}
ml_validate_state <- function(state, data) {
  expected <- c("schema_version", "dataset_id", "progress", "visits", "goal", "updated_at")
  if (!is.list(state) || is.data.frame(state) || !setequal(names(state), expected) || anyDuplicated(names(state))) ml_stop("Backup has missing or unexpected top-level fields.")
  if (!ml_is_integer(state$schema_version) || length(state$schema_version) != 1L || state$schema_version != 1L) ml_stop("This backup uses an unsupported schema version.")
  if (!ml_is_scalar_string(state$dataset_id) || !identical(state$dataset_id, data$dataset_id)) ml_stop("This backup belongs to a different dataset. Use the backup tools to review a transfer before restoring it.")
  progress <- ml_state_frame(state$progress, c("suburb_id", "familiarity"), "Progress")
  ml_validate_ids(progress$suburb_id, "Progress suburb_id")
  if (!setequal(progress$suburb_id, data$suburbs$suburb_id)) ml_stop("Progress must contain exactly one row for each area in this dataset.")
  if (!ml_is_integer(progress$familiarity) || any(!progress$familiarity %in% 0:5)) ml_stop("Familiarity must be an integer from 0 to 5.")
  progress <- progress[match(data$suburbs$suburb_id, progress$suburb_id), , drop = FALSE]
  progress$familiarity <- as.integer(progress$familiarity)
  rownames(progress) <- NULL
  visits <- ml_state_frame(state$visits, c("visit_id", "suburb_id", "place_id", "visited_on"), "Visits")
  ml_validate_ids(visits$visit_id, "Visit IDs")
  ml_validate_ids(visits$suburb_id, "Visit suburb_id", unique = FALSE)
  if (!is.character(visits$place_id) || anyNA(visits$place_id)) ml_stop("Visit place_id must be a string; use an empty string for a area-only check-in.")
  ml_validate_ids(visits$place_id[nzchar(visits$place_id)], "Visit place_id", unique = FALSE)
  if (any(!visits$suburb_id %in% data$suburbs$suburb_id)) ml_stop("A visit refers to an unknown area.")
  is_place <- nzchar(visits$place_id)
  if (any(!visits$place_id[is_place] %in% data$places$place_id)) ml_stop("A visit refers to an unknown place.")
  place_rows <- match(visits$place_id[is_place], data$places$place_id)
  if (any(visits$suburb_id[is_place] != data$places$suburb_id[place_rows])) ml_stop("A visited place does not belong to the selected area.")
  ml_validate_date(visits$visited_on)
  if (anyDuplicated(visits[, c("suburb_id", "place_id", "visited_on"), drop = FALSE])) ml_stop("This backup contains duplicate check-ins for the same place (or area-only visit) on the same day.")
  if (!ml_is_integer(state$goal) || length(state$goal) != 1L || state$goal < 1L || state$goal > nrow(data$suburbs)) ml_stop(paste0("Your goal must be a whole number between 1 and ", nrow(data$suburbs), " areas."))
  timestamp <- state$updated_at
  if (!ml_is_scalar_string(timestamp) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", timestamp)) ml_stop("The backup's updated_at field must be a UTC timestamp.")
  parsed_time <- as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  if (is.na(parsed_time) || format(parsed_time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") != timestamp) ml_stop("The backup's updated_at timestamp is invalid.")
  result <- list(schema_version = 1L, dataset_id = data$dataset_id, progress = progress, visits = visits, goal = as.integer(state$goal), updated_at = timestamp)
  # Every accepted state must fit the same limit used when reopening a backup.
  if (nchar(ml_state_json(result), type = "bytes") > 2 * 1024 * 1024) ml_stop("Your progress exceeds the 2 MB backup limit.")
  result
}
ml_state_json <- function(state) {
  as.character(jsonlite::toJSON(state, auto_unbox = TRUE, dataframe = "rows", null = "null", na = "null", pretty = FALSE, digits = NA))
}
ml_decode_state <- function(json) {
  if (!ml_is_scalar_string(json) || nchar(json, type = "bytes") > 2 * 1024 * 1024 || !nzchar(json)) ml_stop("Choose a non-empty JSON backup smaller than 2 MB.")
  # Preserve JSON scalar types before constructing data frames: automatic
  # simplification can otherwise silently convert a mixed boolean/number column.
  parsed <- tryCatch(jsonlite::fromJSON(json, simplifyVector = FALSE), error = function(e) ml_stop("The backup is not valid JSON."))
  records <- function(rows, columns, numeric_columns = character()) {
    if (!is.list(rows) || !is.null(names(rows))) ml_stop("Backup tables must be arrays of records.")
    if (length(rows) > 20000L) ml_stop("A backup table exceeds the 20,000-record limit.")
    if (!length(rows)) return(rows)
    for (row in rows) {
      if (!is.list(row) || !setequal(names(row), columns) || anyDuplicated(names(row))) ml_stop("A backup record has missing or unexpected fields.")
      for (column in columns) {
        value <- row[[column]]
        if (column %in% numeric_columns) {
          if (!ml_is_integer(value) || length(value) != 1L) ml_stop("Familiarity must be an integer from 0 to 5.")
        } else if (!ml_is_scalar_string(value)) ml_stop(paste0("Backup field ", column, " must be a text string."))
      }
    }
    values <- lapply(columns, function(column) {
      template <- if (column %in% numeric_columns) numeric(1) else character(1)
      vapply(rows, function(row) row[[column]], template)
    })
    as.data.frame(setNames(values, columns), stringsAsFactors = FALSE)
  }
  if (is.list(parsed) && !is.null(names(parsed)) && !anyDuplicated(names(parsed))) {
    if ("progress" %in% names(parsed)) parsed$progress <- records(parsed$progress, c("suburb_id", "familiarity"), "familiarity")
    if ("visits" %in% names(parsed)) parsed$visits <- records(parsed$visits, c("visit_id", "suburb_id", "place_id", "visited_on"))
  }
  parsed
}
ml_parse_state <- function(json, data) ml_validate_state(ml_decode_state(json), data)

# Explicit import preparation only: this helper never saves or replaces a journal.
# Older schema-1 backups have no source-name registry, so changed IDs cannot be
# guessed safely. Retain exact identifiers and report every unmatched record.
ml_prepare_import <- function(json, data) {
  parsed <- ml_decode_state(json)
  expected <- c("schema_version", "dataset_id", "progress", "visits", "goal", "updated_at")
  if (!is.list(parsed) || is.data.frame(parsed) || !setequal(names(parsed), expected) || anyDuplicated(names(parsed))) ml_stop("Backup has missing or unexpected top-level fields.")
  if (!ml_is_scalar_string(parsed$dataset_id)) ml_stop("Backup dataset_id must be a text string.")
  if (identical(parsed$dataset_id, data$dataset_id)) {
    return(list(state = ml_validate_state(parsed, data), migrated = FALSE,
                source_json = json, review = NULL, dropped_visits = ml_empty_visits()))
  }
  # Accept older real-data fingerprints as well as the current two-number hash;
  # the data namespace separates these journals from artificial test records.
  real_id <- "^melbourne-level-up-real-[A-Za-z0-9._-]{1,80}$"
  if (!grepl(real_id, parsed$dataset_id) || !grepl(real_id, data$dataset_id)) {
    ml_stop("Only journals from real Melbourne datasets can be reviewed for transfer. Mock and unrelated datasets stay separate.")
  }
  progress <- ml_state_frame(parsed$progress, c("suburb_id", "familiarity"), "Progress")
  visits <- ml_state_frame(parsed$visits, c("visit_id", "suburb_id", "place_id", "visited_on"), "Visits")
  # Reconstruct only the source relationships recorded in the backup, then apply
  # the same type, date, uniqueness, limits and internal-consistency validation.
  # This cannot attest to the source data's accuracy; the user reviews a transfer.
  if (!is.character(visits$place_id) || anyNA(visits$place_id)) ml_stop("Visit place_id must be a text string.")
  links <- unique(visits[nzchar(visits$place_id), c("place_id", "suburb_id"), drop = FALSE])
  if (anyDuplicated(links$place_id)) ml_stop("The source backup assigns one place to multiple areas.")
  source_data <- list(dataset_id = parsed$dataset_id,
    suburbs = data.frame(suburb_id = progress$suburb_id, stringsAsFactors = FALSE), places = links)
  original <- ml_validate_state(parsed, source_data)
  common_areas <- intersect(original$progress$suburb_id, data$suburbs$suburb_id)
  if (!length(common_areas)) ml_stop("No area identifiers match this dataset. Keep the original backup; an explicit source-ID crosswalk is needed.")

  candidate <- ml_empty_state(data)
  matching <- match(candidate$progress$suburb_id, original$progress$suburb_id)
  candidate$progress$familiarity[!is.na(matching)] <- original$progress$familiarity[matching[!is.na(matching)]]
  area_exists <- original$visits$suburb_id %in% data$suburbs$suburb_id
  is_place <- nzchar(original$visits$place_id)
  place_row <- match(original$visits$place_id, data$places$place_id)
  place_exists <- !is.na(place_row)
  same_area <- rep(FALSE, nrow(original$visits))
  same_area[place_exists] <- original$visits$suburb_id[place_exists] == data$places$suburb_id[place_row[place_exists]]
  keep <- area_exists & (!is_place | (place_exists & same_area))
  candidate$visits <- original$visits[keep, , drop = FALSE]
  rownames(candidate$visits) <- NULL
  candidate$goal <- as.integer(min(original$goal, nrow(data$suburbs)))
  candidate <- ml_validate_state(candidate, data)
  missing_areas <- !original$progress$suburb_id %in% data$suburbs$suburb_id
  review <- list(source_dataset_id = original$dataset_id, source_updated_at = original$updated_at,
    source_visits = nrow(original$visits), retained_visits = sum(keep), dropped_visits = sum(!keep),
    visits_missing_area = sum(!area_exists),
    visits_missing_place = sum(area_exists & is_place & !place_exists),
    visits_changed_area = sum(area_exists & is_place & place_exists & !same_area),
    retained_areas = length(common_areas), added_areas = sum(is.na(matching)),
    dropped_areas = sum(missing_areas), dropped_rated_areas = sum(missing_areas & original$progress$familiarity > 0L),
    goal_before = original$goal, goal_after = candidate$goal)
  list(state = candidate, migrated = TRUE, source_json = json, review = review,
       dropped_visits = original$visits[!keep, , drop = FALSE])
}
ml_import_review_lines <- function(prepared) {
  if (!isTRUE(prepared$migrated)) return(paste(nrow(prepared$state$visits), "check-ins will replace your current journal."))
  r <- prepared$review
  c(paste("Keep", r$retained_visits, "of", r$source_visits, "check-ins and ratings for", r$retained_areas, "matching areas."),
    paste(r$added_areas, "new areas start at familiarity 0."),
    paste("Exclude", r$dropped_visits, "unmatched check-ins:", r$visits_missing_area, "with missing areas,", r$visits_missing_place,
          "with missing or changed place IDs, and", r$visits_changed_area, "whose place now belongs to a different area."),
    paste("Exclude", r$dropped_areas, "missing areas, including", r$dropped_rated_areas, "with a nonzero rating."),
    if (r$goal_before != r$goal_after) paste("Adjust the area goal from", r$goal_before, "to", r$goal_after, "to fit this dataset."),
    "Matching uses exact identifiers only. Renamed places are not guessed or converted to area-only visits.",
    "Confirming replaces this dataset's current journal. Your original backup or earlier browser journal remains unchanged; Undo can recover the current journal during this session.")
}
ml_set_level <- function(state, id, level, data) {
  state <- ml_validate_state(state, data)
  if (!ml_is_scalar_string(id) || !id %in% data$suburbs$suburb_id) ml_stop("Choose a valid area before setting familiarity.")
  if (!ml_is_integer(level) || length(level) != 1L || !level %in% 0:5) ml_stop("Choose a familiarity level from 0 to 5.")
  state$progress$familiarity[match(id, state$progress$suburb_id)] <- as.integer(level)
  state$updated_at <- ml_now()
  state
}
ml_checkin <- function(state, id, place_id = "", date = as.character(ml_today()), data) {
  state <- ml_validate_state(state, data)
  if (!ml_is_scalar_string(id) || !id %in% data$suburbs$suburb_id) ml_stop("Choose a valid area before checking in.")
  if (!ml_is_scalar_string(place_id) || (nzchar(place_id) && !place_id %in% data$places$place_id)) ml_stop("Choose a valid place, or a area-only check-in.")
  if (nzchar(place_id) && data$places$suburb_id[match(place_id, data$places$place_id)] != id) ml_stop("The selected place belongs to a different area. Select a place in this area.")
  if (inherits(date, "Date")) date <- as.character(date)
  if (!ml_is_scalar_string(date)) ml_stop("Choose one visit date.")
  ml_validate_date(date)
  if (nrow(state$visits) >= 20000L) ml_stop("The 20,000-check-in limit has been reached. Export a backup before starting a fresh record.")
  duplicate <- state$visits$suburb_id == id & state$visits$place_id == place_id & state$visits$visited_on == date
  if (any(duplicate)) ml_stop("Already checked in here on this date. Choose another date or place.")
  # IDs are local record identifiers, not user identities. Retry on the unlikely
  # event of a collision; no dependency on a database, account or remote service.
  repeat {
    visit_id <- paste0("visit-", format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"), "-", paste(sample(c(letters, 0:9), 12L, replace = TRUE), collapse = ""))
    if (!visit_id %in% state$visits$visit_id) break
  }
  state$visits <- rbind(state$visits, data.frame(visit_id = visit_id, suburb_id = id, place_id = place_id, visited_on = date, stringsAsFactors = FALSE))
  state$updated_at <- ml_now()
  if (nchar(ml_state_json(state), type = "bytes") > 2 * 1024 * 1024) ml_stop("Your progress has reached the 2 MB backup limit. Export a backup before starting a fresh record.")
  state
}
ml_set_goal <- function(state, goal, data) {
  state <- ml_validate_state(state, data)
  if (!ml_is_integer(goal) || length(goal) != 1L || goal < 1L || goal > nrow(data$suburbs)) ml_stop(paste0("Choose a goal between 1 and ", nrow(data$suburbs), " areas."))
  state$goal <- as.integer(goal); state$updated_at <- ml_now(); state
}
ml_remove_visit <- function(state, visit_id, data) {
  state <- ml_validate_state(state, data)
  if (!ml_is_scalar_string(visit_id) || !visit_id %in% state$visits$visit_id) ml_stop("This check-in is no longer in your journal. Refresh the history and choose again.")
  state$visits <- state$visits[state$visits$visit_id != visit_id, , drop = FALSE]
  rownames(state$visits) <- NULL
  state$updated_at <- ml_now()
  state
}
ml_progress_table <- function(state, data) {
  result <- data$suburbs
  result$familiarity <- state$progress$familiarity[match(result$suburb_id, state$progress$suburb_id)]
  result$familiarity_label <- ml_levels$label[match(result$familiarity, ml_levels$level)]
  result$visit_count <- vapply(result$suburb_id, function(id) sum(state$visits$suburb_id == id), integer(1))
  result$distinct_places <- vapply(result$suburb_id, function(id) length(unique(state$visits$place_id[state$visits$suburb_id == id & nzchar(state$visits$place_id)])), integer(1))
  result$last_visit <- vapply(result$suburb_id, function(id) {
    dates <- state$visits$visited_on[state$visits$suburb_id == id]
    if (length(dates)) max(dates) else ""
  }, character(1))
  result$data_mode <- rep(data$mode, nrow(result))
  result
}
ml_visit_table <- function(state, data) {
  result <- state$visits
  result$suburb_name <- data$suburbs$suburb_name[match(result$suburb_id, data$suburbs$suburb_id)]
  place_rows <- match(result$place_id, data$places$place_id)
  result$place_name <- data$places$place_name[place_rows]
  result$place_category <- data$places$place_category[place_rows]
  result$place_name[!nzchar(result$place_id)] <- "Area-only check-in"
  result$place_category[!nzchar(result$place_id)] <- "Area-only"
  result$data_mode <- rep(data$mode, nrow(result))
  result
}
ml_stats <- function(state, data) {
  visited_suburbs <- length(unique(state$visits$suburb_id))
  list(visited_suburbs = visited_suburbs,
       visited_places = length(unique(state$visits$place_id[nzchar(state$visits$place_id)])),
       checkins = nrow(state$visits), familiar_suburbs = sum(state$progress$familiarity >= 3L),
       rated_suburbs = sum(state$progress$familiarity > 0L), total_suburbs = nrow(data$suburbs),
       total_places = nrow(data$places), coverage_pct = 100 * visited_suburbs / nrow(data$suburbs),
       goal = state$goal, goal_progress = min(visited_suburbs, state$goal))
}

# ---- Person B: visitor-planner data and spatial query helpers ------------------
# Shared input: data/processed/places.csv with name, category, sub, lat, lon.
# Coordinates are WGS84 latitude/longitude; `sub` is the sensor ID for rows whose
# category is "Ped sensor", and a subtype/description for other place categories.
# IMPORTANT: R/prep_places.R uses ps$location_id here, NOT the physical sensor_id.
# Pedestrian aggregates must be joined to verified location_id values offline;
# never rename sensor_id to location_id without checking the source relationship.
# These helpers read existing files only. They do not download data or install packages.
# Optional pedestrian aggregates: data/processed/ped_agg.csv (preferred) or
# ped_agg.rds, containing location_id, weekday, hour, mean_count and optionally
# n_observations. weekday is 1=Monday ... 7=Sunday, or an English weekday name.
# mean_count is an historical mean for that sensor/day/hour, never a live forecast.

ml_error <- function(message) stop(message, call. = FALSE)
ml_columns <- function(frame, required, context) {
  if (!is.data.frame(frame)) ml_error(paste0(context, " must contain a data frame."))
  if (anyDuplicated(names(frame))) ml_error(paste0(context, " has duplicate column names."))
  missing <- setdiff(required, names(frame))
  if (length(missing)) ml_error(paste0(context, " is missing: ", paste(missing, collapse = ", "), "."))
}
ml_text_values <- function(values, context, maximum = 300L) {
  if (!is.character(values) || anyNA(values) || any(!nzchar(trimws(values))) ||
      any(nchar(values, type = "chars") > maximum) || any(grepl("[[:cntrl:]]", values))) {
    ml_error(paste0(context, " must contain non-empty text, no control characters, and at most ", maximum, " characters."))
  }
  values
}
ml_numeric_values <- function(values, context, minimum, maximum, integer = FALSE) {
  if (!is.numeric(values) && !is.character(values)) ml_error(paste0(context, " must be numeric."))
  result <- suppressWarnings(as.numeric(values))
  if (anyNA(result) || any(!is.finite(result)) || any(result < minimum | result > maximum) ||
      (integer && any(result != floor(result)))) {
    ml_error(paste0(context, " must contain ", if (integer) "whole numbers" else "finite numbers", " from ", minimum, " to ", maximum, "."))
  }
  result
}
ml_csv <- function(path) {
  tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                           check.names = FALSE, na.strings = "NA", fileEncoding = "UTF-8-BOM"),
           error = function(e) ml_error(paste0("Cannot read ", basename(path), ": ", conditionMessage(e))))
}
ml_content_ids <- function(places) {
  # Length-prefixed fields avoid delimiter ambiguities. Fixed coordinate precision
  # makes IDs stable after reading the same CSV on another machine or row order.
  keys <- vapply(seq_len(nrow(places)), function(i) {
    parts <- c(enc2utf8(places$name[i]), enc2utf8(places$category[i]), enc2utf8(places$sub[i]),
               sprintf("%.12f", places$lat[i]), sprintf("%.12f", places$lon[i]))
    paste0(nchar(parts, type = "bytes"), ":", parts, collapse = "|")
  }, character(1))
  hash <- function(key) {
    first <- 17; second <- 29
    for (byte in utf8ToInt(key)) {
      first <- (first * 131 + byte) %% 2147483647
      second <- (second * 137 + byte) %% 2147483629
    }
    paste0("place-", sprintf("%010.0f", first), "-", sprintf("%010.0f", second))
  }
  ids <- vapply(keys, hash, character(1), USE.NAMES = FALSE)
  # A collision is a data-preparation error rather than permission to merge places.
  if (anyDuplicated(ids)) ml_error("Place identifier collision: two different records produced the same ID. Check duplicate records or coordinate precision before continuing.")
  ids
}
ml_load_places <- function(project_dir) {
  path <- file.path(project_dir, "data", "processed", "places.csv")
  if (!file.exists(path)) ml_error("Missing data/processed/places.csv. Run the team's offline preparation script or copy the prepared dataset into this project.")
  places <- ml_csv(path)
  columns <- c("name", "category", "sub", "lat", "lon")
  ml_columns(places, columns, "places.csv")
  if (!nrow(places) || nrow(places) > 100000L) ml_error("places.csv must contain between 1 and 100,000 records.")
  places <- places[, columns, drop = FALSE]
  places$name <- ml_text_values(places$name, "Place names")
  places$category <- ml_text_values(places$category, "Place categories", 80L)
  places$sub <- ml_text_values(places$sub, "Place subtypes/location IDs")
  places$lat <- ml_numeric_values(places$lat, "Latitude", -90, 90)
  places$lon <- ml_numeric_values(places$lon, "Longitude", -180, 180)
  duplicates_removed <- sum(duplicated(places))
  places <- places[!duplicated(places), , drop = FALSE]
  places$place_id <- ml_content_ids(places)
  places$location_id <- ifelse(places$category == "Ped sensor", places$sub, "")
  location_ids <- places$location_id[nzchar(places$location_id)]
  if (any(!grepl("^[A-Za-z0-9][A-Za-z0-9_.:-]{0,79}$", location_ids))) ml_error("Ped sensor sub values must be stable location IDs of at most 80 letters, numbers, _, -, ., or :.")
  if (anyDuplicated(location_ids)) ml_error("A location_id appears at multiple distinct records in places.csv. Keep one validated current location per sensor before using proximity comparisons.")
  places <- places[, c("place_id", columns, "location_id"), drop = FALSE]
  rownames(places) <- NULL
  note <- paste0("Loaded ", format(nrow(places), big.mark = ",", trim = TRUE), " prepared locations. ",
                 if (duplicates_removed) paste0(duplicates_removed, " exact duplicate record(s) removed. ") else "",
                 "Locations cover the CBD and surrounding City of Melbourne areas; this is not a complete inventory of greater Melbourne. ",
                 "These are static locations, not live availability, opening hours or current crowd levels.")
  list(places = places, note = note, duplicates_removed = duplicates_removed)
}
ml_distance_km <- function(lat, lon, p_lat, p_lon) {
  # Great-circle surface distance; this does not estimate a walkable route.
  if (length(lat) != 1L || length(lon) != 1L) ml_error("The origin must contain one latitude and longitude.")
  if (length(p_lat) != length(p_lon)) ml_error("Destination latitude and longitude vectors must have equal lengths.")
  lat <- ml_numeric_values(lat, "Origin latitude", -90, 90)
  lon <- ml_numeric_values(lon, "Origin longitude", -180, 180)
  p_lat <- ml_numeric_values(p_lat, "Destination latitude", -90, 90)
  p_lon <- ml_numeric_values(p_lon, "Destination longitude", -180, 180)
  radians <- pi / 180
  delta_lat <- (p_lat - lat) * radians; delta_lon <- (p_lon - lon) * radians
  a <- sin(delta_lat / 2)^2 + cos(lat * radians) * cos(p_lat * radians) * sin(delta_lon / 2)^2
  6371.0088 * 2 * atan2(sqrt(pmin(1, pmax(0, a))), sqrt(pmax(0, 1 - a)))
}
ml_selected_row <- function(places, selected_id) {
  if (!is.character(selected_id) || length(selected_id) != 1L || is.na(selected_id) || !selected_id %in% places$place_id) ml_error("Select a place from the map or search results first.")
  places[match(selected_id, places$place_id), , drop = FALSE]
}
ml_nearby <- function(places, selected_id, categories, radius_km = 0.5, limit = 10L) {
  selected <- ml_selected_row(places, selected_id)
  if (length(radius_km) != 1L) ml_error("Choose one nearby search radius.")
  radius_km <- ml_numeric_values(radius_km, "Nearby radius in kilometres", 0, 100)
  if (length(limit) != 1L) ml_error("Choose one result limit.")
  limit <- ml_numeric_values(limit, "Nearby result limit", 0, 100000, integer = TRUE)
  if (is.null(categories)) categories <- character()
  if (!is.character(categories) || anyNA(categories)) ml_error("Nearby categories must be a character vector.")
  result <- places[places$place_id != selected_id & places$category %in% categories, , drop = FALSE]
  result$distance_m <- 1000 * ml_distance_km(selected$lat, selected$lon, result$lat, result$lon)
  result <- result[result$distance_m <= radius_km * 1000, , drop = FALSE]
  result <- result[order(result$distance_m, result$name, result$place_id, method = "radix"), , drop = FALSE]
  result <- head(result, as.integer(limit)); rownames(result) <- NULL
  result
}
ml_nearest_sensor <- function(places, selected_id) {
  selected <- ml_selected_row(places, selected_id)
  sensors <- places[places$category == "Ped sensor" & nzchar(places$location_id), , drop = FALSE]
  sensors$distance_m <- 1000 * ml_distance_km(selected$lat, selected$lon, sensors$lat, sensors$lon)
  # A selected sensor is always its own context, even if another is colocated.
  if (selected$category == "Ped sensor") {
    result <- sensors[match(selected_id, sensors$place_id), , drop = FALSE]
    rownames(result) <- NULL
    return(result)
  }
  sensors <- sensors[order(sensors$distance_m, sensors$name, sensors$place_id, method = "radix"), , drop = FALSE]
  result <- head(sensors, 1L); rownames(result) <- NULL
  result
}
ml_load_counts <- function(project_dir) {
  directory <- file.path(project_dir, "data", "processed")
  csv <- file.path(directory, "ped_agg.csv"); rds <- file.path(directory, "ped_agg.rds")
  if (!file.exists(csv) && !file.exists(rds)) return(NULL)
  if (file.exists(csv)) {
    counts <- ml_csv(csv); context <- "ped_agg.csv"
  } else {
    counts <- tryCatch(readRDS(rds), error = function(e) ml_error(paste0("Cannot read ped_agg.rds: ", conditionMessage(e))))
    context <- "ped_agg.rds"
  }
  required <- c("location_id", "weekday", "hour", "mean_count")
  if (!("location_id" %in% names(counts)) && "sensor_id" %in% names(counts)) {
    ml_error(paste0(context, " uses sensor_id, but prepared map points use location_id. Member A must use a verified sensor/location crosswalk to export location_id aggregates; do not rename sensor_id blindly."))
  }
  ml_columns(counts, required, context)
  if (!nrow(counts) || nrow(counts) > 1000000L) ml_error(paste0(context, " must contain between 1 and 1,000,000 records."))
  if (!is.character(counts$location_id) && !is.numeric(counts$location_id)) ml_error("Pedestrian location IDs must be text or numbers.")
  counts$location_id <- as.character(counts$location_id)
  if (anyNA(counts$location_id) || any(!grepl("^[A-Za-z0-9][A-Za-z0-9_.:-]{0,79}$", counts$location_id))) ml_error("Pedestrian aggregates contain invalid location IDs.")
  if (!is.character(counts$weekday) && !is.numeric(counts$weekday) && !is.factor(counts$weekday)) ml_error("Weekday must be 1=Monday through 7=Sunday or an English weekday name.")
  weekday <- tolower(trimws(as.character(counts$weekday)))
  day_names <- tolower(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  day_number <- match(weekday, day_names)
  abbreviations <- match(weekday, substr(day_names, 1, 3))
  day_number[is.na(day_number)] <- abbreviations[is.na(day_number)]
  numeric_day <- suppressWarnings(as.numeric(weekday))
  day_number[is.na(day_number)] <- numeric_day[is.na(day_number)]
  counts$weekday <- as.integer(ml_numeric_values(day_number, "Weekday (1=Monday, 7=Sunday)", 1, 7, integer = TRUE))
  counts$hour <- as.integer(ml_numeric_values(counts$hour, "Hour", 0, 23, integer = TRUE))
  counts$mean_count <- ml_numeric_values(counts$mean_count, "Mean pedestrian count", 0, .Machine$double.xmax)
  if (anyDuplicated(counts[, c("location_id", "weekday", "hour"), drop = FALSE])) ml_error("Pedestrian aggregates contain duplicate location_id/weekday/hour combinations.")
  columns <- required
  if ("n_observations" %in% names(counts)) {
    counts$n_observations <- ml_numeric_values(counts$n_observations, "Number of observations", 1, .Machine$integer.max, integer = TRUE)
    columns <- c(columns, "n_observations")
  }
  counts <- counts[, columns, drop = FALSE]
  counts <- counts[order(counts$location_id, counts$weekday, counts$hour, method = "radix"), , drop = FALSE]
  rownames(counts) <- NULL
  counts
}

# ---- Team reference adapter: CLUE areas + real prepared places -----------------
# This adapter depends on core.R's progress helpers and planner_core.R's validated
# text/numeric/content-ID helpers; the delivered dashboard includes all three.
# `suburb_id`/`suburb_name` are the team's agreed progress field names. Their actual
# geographic units here are 13 City of Melbourne CLUE statistical areas, including
# subdivisions of Melbourne and West Melbourne, not a metro-wide suburb inventory.
# The reference branch's sequential `id` is deliberately ignored: content IDs are
# stable when CSV rows are reordered. Pedestrian points use location_id, not the
# physical sensor_id, as verified in the team's data-preparation script.

ml_reference_boolean <- function(value, context) {
  if (is.logical(value) && !anyNA(value)) return(value)
  if (!is.character(value) || anyNA(value)) ml_error(paste0(context, " must contain TRUE or FALSE values."))
  normalized <- tolower(trimws(value))
  if (any(!normalized %in% c("true", "false"))) ml_error(paste0(context, " must contain TRUE or FALSE values."))
  normalized == "true"
}
ml_reference_polygon <- function(path, context) {
  polygon <- tryCatch(sf::st_read(path, quiet = TRUE), error = function(e) ml_error(paste0("Cannot read ", context, ": ", conditionMessage(e))))
  if (!nrow(polygon)) ml_error(paste0(context, " contains no features."))
  if (is.na(sf::st_crs(polygon))) ml_error(paste0(context, " must declare a coordinate reference system."))
  if (any(!as.character(sf::st_geometry_type(polygon)) %in% c("POLYGON", "MULTIPOLYGON"))) ml_error(paste0(context, " must contain polygon or multipolygon geometries."))
  valid <- sf::st_is_valid(polygon)
  if (anyNA(valid) || any(!valid) || any(sf::st_is_empty(polygon))) ml_error(paste0(context, " contains invalid or empty polygons; fix the source during offline data preparation."))
  polygon <- sf::st_transform(polygon, 4326)
  bbox <- sf::st_bbox(polygon)
  if (any(!is.finite(bbox)) || bbox[['xmin']] < -180 || bbox[['xmax']] > 180 || bbox[['ymin']] < -90 || bbox[['ymax']] > 90) ml_error(paste0(context, " has invalid geographic coordinates."))
  polygon
}
ml_reference_area_ids <- function(area_names) {
  # Use the full area label rather than row position or a lossy suburb join.
  ids <- paste0("clue-", gsub("^-+|-+$", "", gsub("[^a-z0-9]+", "-", tolower(area_names))))
  if (any(ids == "clue-") || any(nchar(ids) > 80L) || anyDuplicated(ids)) ml_error("CLUE area names cannot form unique stable IDs. Resolve empty names or slug collisions in the source areas.")
  ids
}
ml_load_team_data <- function(project_dir) {
  if (!requireNamespace("sf", quietly = TRUE)) ml_error("The team area map requires the sf package. Install it separately before running this dashboard.")
  processed <- file.path(project_dir, "data", "processed")
  reference <- file.path(project_dir, "data", "person_b_reference")
  required <- c("name", "category", "sub", "lat", "lon", "area", "in_ftz", "rare")
  primary_places <- file.path(processed, "places.csv")
  primary_areas <- file.path(processed, "areas.geojson")
  use_primary <- FALSE
  if (file.exists(primary_areas)) {
    if (!file.exists(primary_places)) ml_error("data/processed/areas.geojson is present, but its matching enriched places.csv is missing.")
    candidate <- ml_csv(primary_places)
    ml_columns(candidate, required, "data/processed/places.csv accompanying areas.geojson")
    use_primary <- TRUE
  } else if (file.exists(primary_places)) {
    candidate <- ml_csv(primary_places)
    if ("area" %in% names(candidate)) ml_error("Enriched data/processed/places.csv is present, but its matching areas.geojson is missing.")
  }
  directory <- if (use_primary) processed else reference
  places_path <- file.path(directory, "places.csv")
  areas_path <- file.path(directory, "areas.geojson")
  if (!file.exists(places_path) || !file.exists(areas_path)) ml_error("Missing enriched team data. Supply places.csv (including area, in_ftz and rare) and areas.geojson in data/processed, or retain the bundled data/person_b_reference files.")
  places <- if (use_primary) candidate else ml_csv(places_path)
  ml_columns(places, required, "Enriched places.csv")
  if (!nrow(places) || nrow(places) > 100000L) ml_error("Enriched places.csv must contain between 1 and 100,000 records.")
  places$name <- ml_text_values(places$name, "Place names")
  places$category <- ml_text_values(places$category, "Place categories", 80L)
  places$sub <- ml_text_values(places$sub, "Place subtypes/location IDs")
  places$area <- ml_text_values(places$area, "Place CLUE area names", 200L)
  places$lat <- ml_numeric_values(places$lat, "Latitude", -90, 90)
  places$lon <- ml_numeric_values(places$lon, "Longitude", -180, 180)
  places$in_ftz <- ml_reference_boolean(places$in_ftz, "in_ftz")
  places$rare <- ml_reference_boolean(places$rare, "rare")
  # Reject ambiguous source duplicates instead of silently attaching two areas to
  # a single point. Content-hash collisions also raise an explicit validation error.
  places$place_id <- ml_content_ids(places)
  boundaries <- ml_reference_polygon(areas_path, "areas.geojson")
  ml_columns(boundaries, "area", "areas.geojson")
  boundaries$area <- ml_text_values(as.character(boundaries$area), "CLUE polygon area names", 200L)
  if (anyDuplicated(boundaries$area)) ml_error("areas.geojson needs one feature per CLUE area; combine separate pieces as a MULTIPOLYGON.")
  boundaries$suburb_id <- ml_reference_area_ids(boundaries$area)
  boundaries$suburb_name <- boundaries$area
  boundaries <- boundaries[order(boundaries$area, method = "radix"), , drop = FALSE]
  rownames(boundaries) <- NULL
  matched <- match(places$area, boundaries$area)
  if (anyNA(matched)) ml_error("A place's area label does not match any CLUE polygon. Correct the area join during data preparation.")
  # A named join alone is insufficient: reject mislabeled coordinates as well.
  points <- sf::st_as_sf(places[, c("lon", "lat"), drop = FALSE], coords = c("lon", "lat"), crs = 4326)
  memberships <- sf::st_intersects(points, boundaries)
  correct_membership <- vapply(seq_along(memberships), function(i) matched[i] %in% memberships[[i]], logical(1))
  if (any(!correct_membership)) ml_error(paste0(sum(!correct_membership), " place(s) fall outside their stated CLUE area. Correct the spatial join before using this dataset."))
  # Calculate representative points in projected metres, not directly on degrees.
  centres <- sf::st_transform(sf::st_point_on_surface(sf::st_geometry(sf::st_transform(boundaries, 32755))), 4326)
  coordinates <- sf::st_coordinates(centres)
  suburbs <- data.frame(suburb_id = boundaries$suburb_id, suburb_name = boundaries$area,
                        lat = coordinates[, 'Y'], lon = coordinates[, 'X'], stringsAsFactors = FALSE)
  rownames(suburbs) <- NULL
  places$suburb_id <- boundaries$suburb_id[matched]
  places$suburb_name <- places$area
  places$place_name <- places$name
  places$place_category <- places$category
  places$location_id <- ifelse(places$category == "Ped sensor", places$sub, "")
  locations <- places$location_id[nzchar(places$location_id)]
  if (any(!grepl("^[A-Za-z0-9][A-Za-z0-9_.:-]{0,79}$", locations)) || anyDuplicated(locations)) ml_error("Pedestrian markers must have unique valid location_id values in their sub field.")
  fields <- c("place_id", "suburb_id", "suburb_name", "place_name", "place_category", "lat", "lon", "rare", "in_ftz", "sub", "name", "category", "area", "location_id")
  places <- places[, fields, drop = FALSE]
  sensors <- places[places$category == "Ped sensor", , drop = FALSE]
  places <- places[places$category != "Ped sensor", , drop = FALSE]
  rownames(places) <- NULL; rownames(sensors) <- NULL
  ftz_path <- file.path(directory, "ftz.geojson")
  ftz <- if (file.exists(ftz_path)) ml_reference_polygon(ftz_path, "ftz.geojson") else NULL
  note <- paste0("Using ", nrow(suburbs), " City of Melbourne CLUE statistical areas, ", format(nrow(places), big.mark = ",", trim = TRUE),
                 " places and ", nrow(sensors), " pedestrian location markers from the team's prepared ",
                 if (use_primary) "processed dataset. " else "reference dataset. ",
                 "CLUE areas include subdivisions of Melbourne and West Melbourne; they are not all metropolitan suburbs. ",
                 "The tram-zone layer and in-zone flags are approximate team-supplied context, not fare or service advice. ",
                 "The rare-food flag describes the team's subtype classification, not popularity or quality. Static locations do not show live crowds, availability or opening hours.")
  list(suburbs = suburbs, places = places, sensors = sensors, boundaries = boundaries, ftz = ftz,
       mode = "real", note = note, dataset_id = ml_dataset_id(suburbs, places, "real"),
       source_directory = directory, source = if (use_primary) "processed" else "reference")
}

# ---- Person B: desktop interface and browser journal ---------------------------
# Compact map workspace: filters, map, and contextual achievements/visit sidebar.
# CSS is inline and uses local fonts, so the R file has no stylesheet/font dependency.
# Explore keeps the map and primary actions together; nearby details follow in normal page flow.
# Nearby tools use the full page width without an independently scrolling sidebar.
# Server output IDs are shared with A's map, C's Tableau view, and D's achievements.
ml_css <- "
:root{--ink:#203347;--muted:#607080;--line:#dbe3e5;--accent:#176c68;--accent-dark:#105651;--gold:#ba862b;--surface:#fff;--page:#f4f5f1}
*{box-sizing:border-box}
body{margin:0;background:var(--page);color:var(--ink);font:14px/1.5 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif}
.container-fluid{max-width:1480px;margin:auto;padding:14px 20px 12px}
h1,h2,h3,h4{font-family:inherit;font-weight:650;color:var(--ink);line-height:1.3}
h1{font-size:24px;letter-spacing:-.6px;margin:2px 0 0}h2{font-size:23px;letter-spacing:-.3px;margin:0 0 6px}h3{font-size:15px;margin:0 0 12px}h4{font-size:13px;margin:12px 0 8px}
p{margin:0 0 12px}a{color:var(--accent);text-decoration:none}a:hover{text-decoration:underline}
.masthead{display:flex;align-items:center;justify-content:space-between;gap:20px;min-height:66px;margin-bottom:8px}
.brand{display:flex;gap:12px;align-items:center}.compass-mark{display:grid;place-items:center;width:42px;height:42px;flex:none;background:var(--ink);color:#f1d38c;font-size:29px;border-radius:13px;box-shadow:0 3px 0 #d8be82}
.eyebrow{font-size:9px;font-weight:700;letter-spacing:1.4px;color:var(--muted)}
.header-status{text-align:right;min-width:0}.header-stats{display:flex;align-items:center;justify-content:flex-end;gap:16px;font-size:12px;color:var(--muted)}.header-stats strong{color:var(--ink);font-variant-numeric:tabular-nums}
.header-score{display:inline-flex;align-items:center;gap:6px;padding:4px 9px;background:#fff7e7;border:1px solid #e6d3a8;border-radius:20px;color:#735313;white-space:nowrap}.header-score strong{color:#735313}.header-score small{font-size:11px}
.header-utilities{display:flex;justify-content:flex-end;align-items:center;gap:14px;margin-top:5px}.header-utilities>a{font-size:11px;white-space:nowrap}.save-status{font-size:11px;color:var(--muted);max-width:470px}
.nav-tabs{border-bottom:1px solid var(--line);margin-bottom:12px}.nav-tabs>li>a{padding:9px 17px;margin-right:4px;color:var(--muted);font-size:13px;font-weight:600;border:0;border-bottom:2px solid transparent;border-radius:0;background:transparent}
.nav-tabs>li>a:hover,.nav-tabs>li>a:focus{color:var(--ink);background:#eaf0ec;border-color:transparent}.nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus{color:var(--accent-dark);background:transparent;border:0;border-bottom:2px solid var(--accent)}
.nav-pills{display:flex;flex-wrap:wrap;gap:5px;border-bottom:1px solid var(--line);padding-bottom:12px;margin-bottom:16px}.nav-pills>li+li{margin-left:0}.nav-pills>li>a{color:var(--muted);background:#f2f5f3;border-radius:6px;font-size:13px;font-weight:600;padding:8px 14px}.nav-pills>li>a:hover{background:#e5eeea;color:var(--accent-dark)}.nav-pills>li.active>a,.nav-pills>li.active>a:hover,.nav-pills>li.active>a:focus{background:var(--accent);color:#fff}
.panel-card{background:var(--surface);border:1px solid var(--line);border-radius:10px;padding:20px;margin-bottom:14px;min-width:0}
.explore-grid{display:grid;grid-template-columns:212px minmax(300px,1fr) 290px;gap:12px;align-items:start}
.explore-controls,.context-panel{padding:0;min-width:0;overflow:visible}
.explore-controls>.panel-card,.context-panel>.panel-card{margin:0 0 10px;padding:13px}.explore-controls>.panel-card:last-child,.context-panel>.panel-card:last-child{margin-bottom:0}
.map-column{min-width:0;min-height:0;height:560px}.map-card{padding:0;overflow:hidden;height:100%;display:flex;flex-direction:column;margin:0}
.map-heading{display:flex;align-items:center;justify-content:space-between;gap:10px;padding:10px 13px;border-bottom:1px solid var(--line);flex:none}.map-heading h3{margin:0;font-size:14px}.map-heading .micro-note{margin:2px 0 0;font-size:11px}.map-heading .btn{flex:none}
.map-stage{flex:1;min-height:0;background:#e6e8e7}.map-stage #map{height:100%!important;min-height:0}.map-footer{padding:7px 12px;background:#fff;border-top:1px solid var(--line);font-size:11px;color:var(--muted);flex:none}
.map-key{margin-top:3px}.map-key summary{cursor:pointer;color:var(--accent);font-size:11px}.map-key p{max-width:720px;margin:6px 0 2px}.focusbar{display:flex;align-items:center;justify-content:space-between;gap:8px;background:#fff7e7;border-bottom:1px solid #e6d3a8;padding:8px 12px;font-size:11px;flex:none}.focusbar .action-button{white-space:nowrap}#explore-map{scroll-margin-top:12px}
.small-note,.hint,.subtle,.help-block{font-size:12px;color:var(--muted);line-height:1.5}.micro-note,.metric-note{font-size:11px;line-height:1.5;color:var(--muted);margin:5px 0 9px}.divider{border-top:1px solid var(--line);margin:14px 0}
.section-heading{display:flex;justify-content:space-between;gap:12px;align-items:center;margin-bottom:10px}.section-heading h3{margin:0}.section-heading>a{font-size:11px;white-space:nowrap}
.form-group{margin-bottom:11px}.shiny-input-container:not(.shiny-input-container-inline){width:100%}.control-label{font-size:12px;font-weight:600;color:var(--ink);margin-bottom:5px}.checkbox label,.checkbox-inline{font-size:12px;line-height:1.5}.checkbox{margin:6px 0}.checkbox input[type=checkbox]{accent-color:var(--accent)}
.form-control,.selectize-input,.selectize-control.single .selectize-input{background:#fff;border:1px solid #b6c5ca;border-radius:5px;box-shadow:none;color:var(--ink);min-height:34px;font-size:12px}.form-control:focus,.selectize-input.focus{border-color:var(--accent);box-shadow:0 0 0 2px #176c681a}.selectize-control{min-width:0}.selectize-input input,.selectize-dropdown{color:var(--ink)!important}.selectize-dropdown{background:#fff;border:1px solid #b6c5ca;border-radius:5px}.selectize-dropdown .active{background:#eaf3ef}.selectize-dropdown .option{overflow-wrap:anywhere}.input-group-addon{background:#f1f4f2;color:var(--muted);border-color:#b6c5ca}
.irs--shiny .irs-min,.irs--shiny .irs-max{color:var(--muted);background:#edf2ef}.irs--shiny .irs-grid-text{color:var(--muted)}.irs--shiny .irs-single,.irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-bar{background:var(--accent);border-color:var(--accent)}
.btn,.lv-btn{font:600 12px/1.45 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;border-radius:6px;border:1px solid #b7c6cb;box-shadow:none;padding:8px 11px;background:#fff;color:var(--ink);white-space:normal}.btn:hover,.btn:focus{background:#edf4f0;color:var(--ink);border-color:#8aa99e}.btn-primary{background:var(--accent);color:#fff;border-color:var(--accent)}.btn-primary:hover,.btn-primary:focus{background:var(--accent-dark);color:#fff;border-color:var(--accent-dark)}.btn-danger{background:#fff;color:#a32d39;border-color:#dca4ab}.btn-danger:hover,.btn-danger:focus{background:#fff0f1;color:#8c1f2b}.btn-link{background:transparent;border-color:transparent;color:var(--accent);box-shadow:none}.btn-sm{font-size:11px;padding:6px 9px}.full-width{width:100%;margin-bottom:8px}.action-row{display:flex;flex-wrap:wrap;gap:8px;margin:12px 0}.progress-actions{border:0;padding:0;margin:0;min-width:0}.progress-actions:disabled{opacity:.55}.lv-btn{display:block;width:100%;margin:7px 0;text-align:left}.lv-btn.current{outline:2px solid var(--ink);outline-offset:2px}.btn:disabled{opacity:.45;cursor:default}
a:focus-visible,button:focus-visible,input:focus-visible,select:focus-visible,summary:focus-visible,[tabindex]:focus-visible{outline:3px solid #237c9b!important;outline-offset:3px}.skip-link{position:absolute;left:-9999px;top:0;z-index:99999}.skip-link:focus{left:20px;top:8px;padding:10px;background:#fff;color:var(--ink)}
.dot{display:inline-block;width:10px;height:10px;border:1px solid #7b8797;border-radius:50%;margin-right:6px;vertical-align:-1px;background:#657892}.dot.hollow{background:transparent}.sw,.swatch{width:12px;height:12px;display:inline-block;border:1px solid #92a6b4;flex:none}.lg,.level-row{display:flex;align-items:center;gap:7px;font-size:11px;margin:5px 0}.lgn,.level-count{margin-left:auto;color:var(--muted);font-variant-numeric:tabular-nums}
#categories .shiny-options-group{display:grid;grid-template-columns:1fr 1fr;column-gap:8px;row-gap:1px}#categories .checkbox{margin:3px 0}#categories .checkbox label{font-size:11px;white-space:nowrap}#categories .dot{width:8px;height:8px;margin-right:4px}
.filter-card .form-group{margin-bottom:10px}.filter-card .selectize-control{margin-bottom:0}.filter-card .full-width{margin:0}.filter-options{margin:11px 0 12px}.filter-status{font-size:10px;border-radius:9px;background:#e5f0eb;padding:1px 6px;margin-left:6px;color:var(--accent-dark);font-weight:600}.legend-card{border-top:1px solid var(--line);padding-top:11px}.legend-card h3{font-size:12px;margin-bottom:7px}.legend-card .micro-note{margin:7px 0 0;font-size:10px}
.details-note{border-top:1px solid var(--line);padding-top:9px;margin-top:12px}.details-note summary,.place-details summary{display:list-item;list-style:none;cursor:pointer;font-size:12px;font-weight:600;color:var(--accent);padding:3px 20px 6px 0;position:relative}.details-note summary::-webkit-details-marker,.place-details summary::-webkit-details-marker{display:none}.details-note>summary:after,.place-details>summary:after{content:'+';position:absolute;right:2px;top:2px;font-size:18px;line-height:1}.details-note[open]>summary:after,.place-details[open]>summary:after{content:'−'}.details-note[open]>summary{margin-bottom:6px}
.visit-context{display:flex;flex-wrap:wrap;gap:6px;align-items:center;margin:0 0 8px}.visit-area{font-size:12px;font-weight:600}.level-pill{font-size:10px;color:#315679;background:#edf4fb;border:1px solid #cbdcec;padding:2px 6px;border-radius:10px}.selection-card .place-name{font-size:16px;line-height:1.35;font-weight:700;margin:0 0 5px;overflow-wrap:anywhere}.place-kind{font-size:11px;color:var(--muted);margin:0 0 8px}.place-details{font-size:11px;margin:0 0 10px;color:var(--muted)}.place-details p{margin:6px 0;overflow-wrap:anywhere}.selection-card .selectize-input .item{max-width:calc(100% - 14px);white-space:nowrap;overflow:hidden;text-overflow:ellipsis}.checkin-row{display:grid;grid-template-columns:minmax(0,1fr) auto;gap:8px;align-items:end;margin:8px 0}.checkin-row>.form-group{margin-bottom:0}.checkin-row>.btn{height:35px}.undo-link{font-size:11px}.journal-note{font-size:11px;border-top:1px solid var(--line);padding-top:10px;margin-top:8px}
.challenge-star{color:#9a6b14}.challenge-summary{font-size:11px;color:var(--muted);margin:0 0 9px}.achievement-grid{display:flex;flex-direction:column;gap:7px}.ach{display:grid;grid-template-columns:minmax(0,1fr) auto;column-gap:8px;align-items:center;padding:10px;border:1px solid #dfe6e1;border-radius:7px;background:#f7f9f6;min-width:0}.ach.done{background:#fff8e8;border-color:#e3ca8e}.ach.focused{outline:2px solid var(--accent);outline-offset:1px}.ach-t{font:650 11px/1.4 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;color:var(--ink)}.ach>.ach-t,.ach-target,.ach-d{grid-column:1/-1}.ach-d{font-size:11px;line-height:1.45;color:var(--muted);margin:5px 0 6px}.ach-n{font-size:10px;color:var(--muted);text-align:right;font-variant-numeric:tabular-nums}.ach-target{display:flex;align-items:center;justify-content:space-between;gap:5px;width:100%;border:0;padding:0;background:transparent;text-align:left;color:var(--ink)}.ach-target:hover .ach-t{color:var(--accent)}.target-action{font-size:10px;color:var(--accent);white-space:nowrap}.bar,.progress-track{height:6px;width:100%;background:#e1e9e5;border-radius:3px;overflow:hidden;margin:8px 0 0}.bar>div,.progress-fill{height:100%;background:#4d8d80}.ach .bar{margin:0}.ach.done .bar>div{background:#c69a3c}.all-challenges{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px}.goal-status{font-size:12px;color:var(--muted)}
.compact-result{display:flex;align-items:center;justify-content:space-between;gap:12px;width:100%;border:0;border-top:1px solid var(--line);background:transparent;color:var(--ink);padding:9px 0;text-align:left;font-size:12px;line-height:1.45}.compact-result:hover{background:#eef5f1}.compact-result small{display:block;color:var(--muted);white-space:normal}.compact-result>span:first-child{min-width:0;overflow-wrap:anywhere}.compact-result>span:last-child:not(:first-child){flex:none;font-size:11px}.search-results{border:1px solid var(--line);padding:7px 8px;border-radius:4px;margin-bottom:10px;max-height:180px;overflow:auto;font-size:11px}.search-results .compact-result:first-child{border:0}.search-results p{margin:8px 0}.nearby-preview{border-top:1px solid var(--line);margin-top:8px}
.nearby-controls{display:grid;grid-template-columns:220px minmax(0,1fr);gap:18px;align-items:end}.nearby-controls .checkbox-inline{margin-left:0;margin-right:10px}.nearby-grid{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:10px;margin:10px 0}.nearby-section{margin-top:14px}.nearby-preview-grid{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:14px}.nearby-preview-grid>p{padding:10px 0;border-top:1px solid var(--line)}.nearby-card{padding:9px;border:1px solid var(--line);border-radius:5px;background:#f7f9f6;min-width:0}.nearby-card .btn{width:100%;text-align:left;margin-bottom:5px;font-size:11px}.nearby-meta{display:flex;justify-content:space-between;gap:6px;font-size:11px;color:var(--muted)}.nearby-meta>span:last-child{text-align:right}.nearby-location{font-size:10px;margin-top:4px;overflow-wrap:anywhere}.nearby-navigation{display:flex;justify-content:space-between;align-items:center;gap:6px;margin:10px 0;font-size:10px}.nearby-navigation .btn{padding:5px;font-size:10px}.nearby-count{font-size:12px;font-weight:600;margin:10px 0 4px}.nearby-order,.nearby-shared{font-size:11px;color:var(--muted)}.nearby-shared{padding:8px;background:#edf4f0;border-left:2px solid #9abdb0}
.analysis-page{max-width:1080px;margin:0 auto}.page-intro{margin:16px 0}.page-intro p{font-size:13px;color:var(--muted);margin:0}.page-intro.section-heading{align-items:center}.discovery-workspace{padding:18px 22px}.discovery-controls{display:flex;align-items:center;justify-content:space-between;gap:16px;flex-wrap:wrap;margin-bottom:12px}.discovery-controls .shiny-input-container{width:auto}.discovery-controls .checkbox,.discovery-controls .micro-note{margin:0}.analysis-toolbar{display:flex;align-items:center;justify-content:space-between;gap:20px;margin-bottom:12px}.analysis-toolbar h3{margin:0;font-size:16px}.analysis-toolbar .shiny-input-container{width:210px;flex:none;margin:0}.analysis-toolbar .selectize-control{margin:0}
.area-chart-row{display:grid;grid-template-columns:minmax(210px,28%) minmax(0,1fr) 44px;gap:15px;align-items:center;width:100%;min-height:33px;border:0;border-bottom:1px solid #eef1ee;background:transparent;padding:7px 6px;font-size:12px;text-align:left;color:var(--ink)}.area-chart-row:hover{background:#eef5f1}.area-chart-label{line-height:1.4}.area-chart-track{height:11px;background:#edf2f1;border-radius:2px;overflow:hidden}.area-chart-fill{display:block;height:100%;background:#4a8195}.area-chart-value{text-align:right;font-variant-numeric:tabular-nums}.insight-layout{display:grid;grid-template-columns:minmax(0,.85fr) minmax(0,1.15fr);gap:28px;align-items:start}.insight-examples{padding-left:24px;border-left:1px solid var(--line)}.insight-examples>h4{margin-top:0}.data-metric{font-size:32px;font-weight:700;letter-spacing:-.7px;color:var(--accent-dark);margin:4px 0}.data-metric-caption{font-size:13px;line-height:1.5;color:var(--muted)}.insight-stat-row{display:flex;align-items:center;justify-content:space-between;gap:15px;border-bottom:1px solid var(--line);padding:9px 0;font-size:12px}.insight-links{margin-top:12px}.discovery-example-meta{display:block;color:var(--muted);font-size:11px}.discovery-example-value{white-space:nowrap;font-weight:600;font-variant-numeric:tabular-nums}.discovery-notes{margin-top:12px}.discovery-count-table{width:100%;font-size:11px}.discovery-count-table td,.discovery-count-table th{padding:6px 8px;border-bottom:1px solid var(--line);vertical-align:top}.discovery-count-table th{text-align:left}.discovery-notes p{font-size:12px;color:var(--muted)}
.progress-overview{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:0;padding:15px 6px;background:var(--ink);color:#fff;border-radius:10px;margin-bottom:8px}.progress-overview>div{padding:0 18px;border-left:1px solid #ffffff25}.progress-overview>div:first-child{border-left:0}.progress-overview strong{display:block;font-size:25px;font-weight:650;font-variant-numeric:tabular-nums;line-height:1.3}.progress-overview span{display:block;font-size:11px;color:#d6e1e4;margin-top:3px}.progress-overview>div:nth-child(3) strong,.progress-overview>div:nth-child(4) strong{color:#f0d18b}.progress-grid{display:grid;grid-template-columns:minmax(240px,.7fr) minmax(0,1.3fr);gap:24px;align-items:start}.goal-panel{padding-right:24px;border-right:1px solid var(--line)}.goal-panel>h3{font-size:12px;color:var(--muted);margin-bottom:12px}.goal-panel .bar{height:8px}.goal-panel .bar>div{background:#b78a36}.goal-panel .details-note{margin-top:12px}.category-panel>h3{margin-bottom:12px}.category-empty{font-size:12px;color:var(--muted)}.category-empty .btn{margin-top:2px}.progress-records{padding:16px 20px}.progress-records .nav-tabs{margin-bottom:14px}.progress-records .section-heading{margin-bottom:10px}.category-scale{display:flex;justify-content:space-between;gap:10px;font-size:11px;color:var(--muted);margin-bottom:8px}.category-row{display:grid;grid-template-columns:100px minmax(0,1fr) 28px;align-items:center;gap:10px;width:100%;padding:7px 0;border:0;border-top:1px solid #e7ece8;background:transparent;text-align:left;color:var(--ink);font:inherit}.category-row:hover{background:#f1f6f2}.category-name{font-size:12px}.category-track{display:block;height:10px;background:#edf2ef;border-radius:2px;overflow:hidden}.category-fill{display:block;height:100%}.category-value{text-align:right;font-size:12px;font-weight:600}.insight-action{margin-bottom:12px}
.table-wrap{overflow-x:auto}.table{width:100%;color:var(--ink);font-size:12px;background:transparent}.table>thead>tr>th{border-bottom:1px solid #b9cbd0;color:#36536f;font-weight:650;white-space:normal}.table>tbody>tr>td{border-color:#e4eae6;padding:8px;vertical-align:middle}.table-striped>tbody>tr:nth-of-type(odd){background:#f7f9f6}.table-hover>tbody>tr:hover{background:#edf4f0}.table .btn-sm{font-size:11px;padding:4px 7px}.history-navigation{display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:8px;font-size:12px;margin-top:10px}.empty-state{padding:13px;background:#f5f8f5;border:1px solid #e0e8e2;border-radius:6px;color:var(--muted);font-size:12px;line-height:1.5}.empty-state strong{color:var(--ink)}.empty-state .action-button{margin-top:9px}.data-note,.coverage-note{padding:10px 12px;background:#edf4f0;border-left:3px solid #89aea0;font-size:12px;color:#35574c}.about-list{padding-left:18px}.about-list li,.data-help-list li{font-size:12px;line-height:1.6;margin-bottom:7px}.footer-note{font-size:10px;color:var(--muted);margin-top:10px}
.modal-content{border:1px solid #d2ddd6;border-radius:10px;background:#fff;color:var(--ink);box-shadow:0 12px 48px #172b4433}.modal-title{font-family:inherit;font-size:18px;font-weight:650}.modal-content .small-note,.modal-content p{color:var(--muted)}.modal-content .close{color:var(--ink);opacity:.7}.modal-body .file-input .progress{margin-bottom:0}.modal-body .form-group{margin-top:12px}.modal-body .action-row{margin-bottom:0}.shiny-notification{background:#fff;color:var(--ink);border:1px solid #cad9cf;border-left:3px solid var(--accent);border-radius:6px;padding:12px;box-shadow:0 4px 18px #172b4415;font-size:13px;opacity:1}.shiny-notification-error{border-left-color:#bc4350}.shiny-notification-warning{border-left-color:#c68b28}#shiny-notification-panel{width:360px;bottom:18px;right:18px}.shiny-output-error-validation{color:var(--muted)}
.leaflet-container{font-family:inherit;background:#e6e8e7}.leaflet-tooltip{font:12px/1.5 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;color:var(--ink);border:1px solid #aebfd1;max-width:330px;white-space:normal}.leaflet-popup-content{color:var(--ink);line-height:1.5}.pt-cluster div{width:100%;height:100%;border-radius:50%;border:3px solid;background:#fffffff0;display:flex;align-items:center;justify-content:center;position:relative;font:600 13px -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;color:var(--ink)}.pt-cluster i{position:absolute;right:-8px;top:-8px;color:white;font:600 10px sans-serif;border:1px solid white;border-radius:3px;padding:1px 3px}
.when-controls{display:grid;grid-template-columns:minmax(250px,1fr) minmax(200px,1fr);gap:22px;align-items:center}.when-controls .checkbox{margin-top:18px}.time-empty{max-width:820px}.time-empty h3{margin-bottom:10px}.time-empty .btn{margin-top:10px}tableau-viz{display:block;background:#fff;min-height:620px}.datepicker{color:var(--ink);background:#fff}.datepicker table tr td.disabled{color:#8795a5}


@media(max-width:1100px){.container-fluid{padding:12px}.explore-grid{height:auto;grid-template-columns:226px minmax(0,1fr)}.map-column{height:520px}.context-panel{grid-column:1/-1;display:grid;grid-template-columns:1fr 1fr;gap:12px;overflow:visible}.context-panel>.panel-card{margin:0}.header-stats{gap:10px}.save-status{max-width:320px}.insight-layout{gap:20px}.insight-examples{padding-left:18px}}

@media(max-width:760px){.nearby-grid,.nearby-preview-grid{grid-template-columns:repeat(2,minmax(0,1fr))}.masthead{align-items:flex-start;flex-wrap:wrap;gap:12px}.header-status{text-align:left}.header-stats,.header-utilities{justify-content:flex-start}.save-status{max-width:none}.analysis-toolbar{align-items:flex-start;flex-direction:column;gap:10px}.analysis-toolbar .shiny-input-container{width:100%}.discovery-controls{align-items:flex-start;flex-direction:column;gap:6px}.insight-layout{grid-template-columns:1fr}.insight-examples{padding-left:0;padding-top:12px;border-left:0;border-top:1px solid var(--line)}.progress-grid{grid-template-columns:1fr}.goal-panel{padding-right:0;padding-bottom:16px;border-right:0;border-bottom:1px solid var(--line)}.when-controls{grid-template-columns:1fr;gap:8px}.area-chart-row{grid-template-columns:minmax(135px,40%) minmax(0,1fr) 38px;gap:8px}.progress-overview{grid-template-columns:repeat(2,minmax(0,1fr));row-gap:14px}.progress-overview>div:nth-child(3){border-left:0}.nav-tabs>li>a{padding:9px 10px}}
@media(max-width:580px){.nearby-grid,.nearby-preview-grid,.nearby-controls{grid-template-columns:1fr}.explore-grid>div{width:100%}.container-fluid{padding:12px 10px}.explore-grid{display:flex;flex-direction:column}.explore-controls{max-height:none;overflow:visible}.map-column{height:480px;flex:none}.context-panel{display:block}.context-panel>.panel-card{margin-bottom:10px}.all-challenges{grid-template-columns:1fr}.brand h1{font-size:22px}.header-stats{flex-wrap:wrap;gap:8px 12px}.header-utilities{align-items:flex-start}.header-utilities .save-status{max-width:260px}.page-intro.section-heading{align-items:flex-start}.page-intro .btn{max-width:140px}.discovery-workspace{padding:14px}.analysis-toolbar h3{font-size:15px}.map-heading .micro-note{max-width:210px}.map-footer{font-size:10px}#shiny-notification-panel{width:calc(100% - 24px);right:12px}}
@media(prefers-reduced-motion:reduce){*{transition:none!important;animation:none!important;scroll-behavior:auto!important}}
"

# Local storage is scoped to this app origin and data fingerprint. A conflict never
# overwrites another tab's changes. The recovery route is backup, reload, restore.
ml_js <- "
$(function(){
  let storageKey=null, lastSaved=null, locked=false, lastRevision=0;
  let previousKeys=new Set();
  const journalPrefix='melbourne-visitor-journal-v1-';
  const realDataset=/^melbourne-level-up-real-[A-Za-z0-9._-]{1,80}$/;
  const tell=(id,value)=>Shiny.setInputValue(id,value,{priority:'event'});
  let selectionContext=null, selectionConnected=false, selectionProtocol=null;
  const selectionPickers=[['suburb','area'],['visit_place','place']];
  // Selectize's change callbacks also run for asynchronous server setValue and
  // clear calls. A genuine picker action is scoped to its native event method,
  // with try/finally cleanup; focus or a recent-click timeout is not evidence.
  window.mlSelectionChange=function(kind,value,instance){
    if(selectionContext&&selectionContext.kind===kind&&selectionContext.instance===instance){
      selectionContext.changed=true;
    }
  };
  window.mlBindSelection=function(instance,kind){
    if(!instance||typeof instance.onOptionSelect!=='function'||typeof instance.onKeyDown!=='function'||typeof instance.getValue!=='function'||typeof instance.load!=='function'||typeof instance.setValue!=='function'||!instance.settings)return false;
    if(instance.mlSelectionKind===kind)return true;
    // Shiny replaces settings.load when a server-choice URL changes. Associate
    // each request with that loader generation, including its later setValue.
    // Old AJAX responses must neither add old options nor repaint the selection.
    const originalLoad=instance.load, originalSetValue=instance.setValue;
    let loader=null, generation=0, responseContext=null;
    const currentInstance=()=>{
      const element=document.getElementById(kind==='area'?'suburb':'visit_place');
      return element&&element.selectize===instance;
    };
    instance.load=function(fn){
      if(loader!==this.settings.load){loader=this.settings.load;generation++;}
      const requestGeneration=generation;
      return originalLoad.call(this,function(callback){
        return fn.call(this,function(results){
          const response={generation:requestGeneration};
          responseContext=response;
          // The installed Shiny success callback calls setValue immediately
          // after this result callback, in the same JavaScript call stack.
          Promise.resolve().then(()=>{if(responseContext===response)responseContext=null;});
          const fresh=requestGeneration===generation&&currentInstance();
          // Drain Selectize's loading counter even when discarding old options.
          return callback.call(this,fresh?results:[]);
        });
      });
    };
    instance.setValue=function(){
      const userAction=selectionContext&&selectionContext.instance===this;
      if(!userAction&&responseContext&&(responseContext.generation!==generation||!currentInstance()))return;
      return originalSetValue.apply(this,arguments);
    };
    const methods=['onOptionSelect','onKeyDown','onKeyPress','onInput','onPaste','onMouseDown','onClick','onItemSelect'];
    methods.forEach(name=>{
      if(typeof instance[name]!=='function')return;
      const original=instance[name];
      instance[name]=function(event){
        const nativeEvent=event&&(event.originalEvent||event);
        const ownsContext=!selectionContext&&nativeEvent&&nativeEvent.isTrusted===true;
        if(ownsContext)selectionContext={instance:this,kind:kind,changed:false,explicit:false};
        const context=selectionContext;
        if(context&&context.instance===this&&name==='onOptionSelect')context.explicit=true;
        let completed=false;
        try{const result=original.apply(this,arguments);completed=true;return result;}
        finally{
          if(ownsContext){
            selectionContext=null;
            if(completed&&(context.changed||context.explicit)&&selectionProtocol===true){
              const selected=this.getValue();
              const value=selected===null?'':selected;
              if(typeof value==='string')tell('selection_intent',{kind:kind,value:value});
            }
          }
        }
      };
    });
    instance.mlSelectionKind=kind;
    if(selectionConnected)announceSelectionProtocol();
    return true;
  };
  const announceSelectionProtocol=()=>{
    const supported=selectionPickers.every(([id,kind])=>{
      const element=document.getElementById(id);
      return element&&element.selectize&&element.selectize.mlSelectionKind===kind;
    });
    const protocol=supported?true:'unsupported';
    if(selectionProtocol!==protocol){selectionProtocol=protocol;tell('selection_protocol',protocol);}
  };
  const previousJournals=()=>{
    previousKeys=new Set();
    if(typeof localStorage.key!=='function')return [];
    const previous=[];
    try{
      for(let i=0;i<localStorage.length;i++){
        const key=localStorage.key(i);
        if(!key||key===storageKey||!key.startsWith(journalPrefix))continue;
        const dataset=key.slice(journalPrefix.length);
        if(!realDataset.test(dataset))continue;
        try{
          const json=localStorage.getItem(key);
          if(!json||json.length>2*1024*1024)continue;
          const value=JSON.parse(json);
          if(!value||value.schema_version!==1||value.dataset_id!==dataset||
             !Array.isArray(value.progress)||!Array.isArray(value.visits)||
             value.progress.length>20000||value.visits.length>20000||
             typeof value.updated_at!=='string')continue;
          // These are discovery summaries only. R validates the full selected
          // journal before a review can be shown or anything can be restored.
          previous.push({key:key,dataset_id:dataset,updated_at:value.updated_at,
            checkins:value.visits.length,areas:value.progress.length});
        }catch(ignore){}
      }
    }catch(ignore){}
    previous.sort((a,b)=>b.updated_at.localeCompare(a.updated_at)||a.key.localeCompare(b.key));
    const recent=previous.slice(0,20);
    recent.forEach(value=>previousKeys.add(value.key));
    return recent;
  };
  const interactiveFocusTarget=el=>{
    if(!el||!el.isConnected||typeof el.focus!=='function'||
      !el.matches('button,input:not([type=hidden]),select,textarea,a[href],summary')||
      el.matches(':disabled')||!el.getClientRects().length)return false;
    if(el.closest&&el.closest('[inert],[hidden]'))return false;
    const style=window.getComputedStyle?window.getComputedStyle(el):null;
    return !style||(style.visibility!=='hidden'&&style.visibility!=='collapse');
  };
  Shiny.addCustomMessageHandler('ml-load',function(m){
    // ml-load is sent after this Shiny session connects, when inputs are bound.
    storageKey=m.key;
    selectionPickers.forEach(([id,kind])=>{
      const element=document.getElementById(id);
      if(element&&element.selectize)window.mlBindSelection(element.selectize,kind);
    });
    selectionConnected=true; announceSelectionProtocol();
    try{
      lastSaved=localStorage.getItem(storageKey);
      tell('storage_loaded',{json:lastSaved||'',available:true,previous:previousJournals()});
    }
    catch(e){tell('storage_loaded',{json:'',available:false});}
  });
  Shiny.addCustomMessageHandler('ml-load-previous',function(m){
    // Reading an earlier key never changes the current key, compare-and-swap
    // baseline, conflict lock or the original saved journal.
    try{
      if(!m||!previousKeys.has(m.key)||m.key===storageKey)throw new Error('Choose one of the earlier journals shown in this session.');
      const json=localStorage.getItem(m.key);
      if(!json)throw new Error('That earlier journal is no longer available in this browser.');
      if(json.length>2*1024*1024)throw new Error('The earlier journal exceeds the 2 MB backup limit.');
      tell('previous_journal',{key:m.key,json:json,ok:true});
    }catch(e){tell('previous_journal',{key:m&&m.key?m.key:'',json:'',ok:false,error:e.message});}
  });
  Shiny.addCustomMessageHandler('ml-ready',function(m){
    document.querySelectorAll('.progress-actions').forEach(e=>{
      e.disabled=!m.ready;
      // Selectize retains its own startup lock even after the fieldset unlocks.
      e.querySelectorAll('select').forEach(select=>{
        if(select.selectize){if(m.ready)select.selectize.enable();else select.selectize.disable();}
      });
    });
  });
  Shiny.addCustomMessageHandler('ml-sensor-lock',function(m){
    const el=document.getElementById('time_sensor');
    if(!el)return;
    if(el.selectize){if(m.locked)el.selectize.disable();else el.selectize.enable();}
    else el.disabled=m.locked;
  });
  Shiny.addCustomMessageHandler('ml-save',function(m){
    lastRevision=m.revision;
    try{
      if(!storageKey)throw new Error('unavailable');
      const current=localStorage.getItem(storageKey);
      if(locked||current!==lastSaved){locked=true;throw new Error('conflict');}
      localStorage.setItem(storageKey,m.json);lastSaved=m.json;
      tell('storage_saved',{revision:m.revision,ok:true});
    }catch(e){tell('storage_saved',{revision:m.revision,ok:false,conflict:e.message==='conflict'});}
  });
  window.addEventListener('storage',function(e){
    if(storageKey&&(e.key===storageKey||e.key===null)){
      try{if(localStorage.getItem(storageKey)!==lastSaved){locked=true;tell('storage_saved',{revision:lastRevision,ok:false,conflict:true});}}
      catch(ignore){}
    }
  });
  Shiny.addCustomMessageHandler('ml-nearby-nav',function(m){
    const previous=document.getElementById('nearby_previous'),next=document.getElementById('nearby_next');
    if(previous)previous.disabled=!m.previous;
    if(next)next.disabled=!m.next;
  });
  Shiny.addCustomMessageHandler('ml-reload',()=>location.reload());
  Shiny.addCustomMessageHandler('ml-show-map',function(){
    requestAnimationFrame(function(){
      const map=document.getElementById('explore-map');
      if(map){
        map.scrollIntoView({block:'start'});
        const target=[document.getElementById('fit_map'),document.querySelector('#map .leaflet-control-zoom-in')].find(interactiveFocusTarget);
        if(target)target.focus({preventScroll:true});
      }
    });
  });
});
"

ml_empty <- function(...) div(class = "empty-state", ...)
ml_card <- function(title, value, note) div(class = "metric", div(class = "metric-label", title),
  div(class = "metric-value", value), div(class = "metric-note", note))
ml_category_colors <- c(Landmark = "#d55e00", Food = "#e7298a", Park = "#1b9e77", `Tram stop` = "#7570b3", Toilet = "#4d4d4d", `Ped sensor` = "#6c7c93")
ml_color <- function(category) {
  value <- unname(ml_category_colors[as.character(category)])
  value[is.na(value)] <- "#657892"
  value
}
ml_tableau_url <- function(url) {
  url <- trimws(url)
  if (!nzchar(url)) return("")
  if (!grepl("^https://public[.]tableau[.]com/views/[A-Za-z0-9_%.-]+/[A-Za-z0-9_%.-]+/?([?][^<>\"']*)?$", url))
    stop("Use the published Tableau Public /views/workbook/view URL.", call. = FALSE)
  sub("[?].*$", "", url)
}

ml_backup_ui <- function() tagList(
  p(class = "small-note", "Your journal saves in this browser at this address. Keep a backup when changing computers or datasets."),
  downloadButton("backup", "Download journal backup"),
  fileInput("restore_file", "Restore or migrate a backup", accept = ".json"),
  p(class = "small-note", "Review the changes before replacing your journal. Unmatched records are never silently reassigned."),
  uiOutput("previous_journals"),
  tags$details(class = "details-note", tags$summary("CSV exports and recovery"),
    div(class = "action-row", downloadButton("progress_csv", "Area progress CSV"), downloadButton("visits_csv", "Visit history CSV")),
    div(class = "action-row", actionButton("reload", "Reload saved journal"), actionButton("reset", "Start fresh", class = "btn-danger")),
    p(class = "small-note", "Other people using this browser profile can see this journal. Undo is available for this session.")))

ml_ui <- function(data) {
  categories <- c(intersect(names(ml_category_colors), unique(data$places$place_category)), setdiff(unique(data$places$place_category), names(ml_category_colors)))
  first_area <- match("Melbourne (CBD)", data$suburbs$suburb_name)
  if (is.na(first_area)) first_area <- 1L
  sensor_choices <- c("Choose a counting location" = "", if (nrow(data$sensors)) setNames(as.character(data$sensors$location_id), paste(data$sensors$place_name, "·", data$sensors$location_id)) else character())
  fluidPage(
    tags$head(tags$title("Melbourne Explorer"), tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"), tags$style(HTML(ml_css)), tags$script(HTML(ml_js))),
    tags$a(class = "skip-link", href = "#main-content", "Skip to explorer"),
    div(class = "masthead",
      div(class = "brand", span(class = "compass-mark", `aria-hidden` = "true", "✦"),
        div(span(class = "eyebrow", "YOUR CITY, ONE DISCOVERY AT A TIME"), h1("Melbourne Explorer"))),
      div(class = "header-status", uiOutput("stats"),
        div(class = "header-utilities", div(class = "save-status", role = "status", `aria-live` = "polite", textOutput("save_status", inline = TRUE)),
          actionLink("open_sources", "Data & help")))),
    div(id = "main-content", tabindex = "-1", role = "main",
      tabsetPanel(id = "main_tab",
        tabPanel("Explore", value = "explore",
          div(class = "explore-grid",
            div(class = "explore-controls",
              div(class = "panel-card filter-card", div(class = "section-heading", h3("Find a place"), actionLink("clear_filters", "Reset")),
                textInput("place_search", "Search selected categories", placeholder = "Name or type…"), uiOutput("search_results"),
                checkboxGroupInput("categories", "Show on map", choiceValues = categories,
                  choiceNames = lapply(categories, function(category) tagList(span(class = "dot", `aria-hidden` = "true", style = paste0("background:", ml_color(category))), category)), selected = intersect("Landmark", categories)),
                selectizeInput("suburb", "Selected area", choices = setNames(data$suburbs$suburb_id, data$suburbs$suburb_name), selected = data$suburbs$suburb_id[first_area],
                  options = list(onInitialize = I("function(){if(window.mlBindSelection)window.mlBindSelection(this,'area');}"), onChange = I("function(value){if(window.mlSelectionChange)window.mlSelectionChange('area',value,this);}"), onDelete = I("function(){return false;}"))),
                tags$fieldset(class = "progress-actions", disabled = "disabled", tags$legend(class = "sr-only", "Area experience"), actionButton("edit_area", "Set this area's level", class = "full-width")),
                tags$details(class = "details-note filter-options", tags$summary("More filters", uiOutput("filter_status", inline = TRUE)),
                  checkboxInput("include_other_landmarks", "All landmark types", FALSE),
                  p(class = "micro-note", "Default: visitor-focused landmarks. See Data & help for the full selection."),
                  checkboxInput("only_selected_area", "Only the selected area", FALSE),
                  checkboxInput("unvisited_only", "Only unvisited places", FALSE)),
                div(class = "legend-card", h3("Area levels"), uiOutput("legend"),
                  p(class = "micro-note", "Your experience sets the map colour.")))),
            div(class = "map-column",
              div(id = "explore-map", tabindex = "-1", class = "panel-card map-card",
                div(class = "map-heading", div(h3("Explore the city"), span(class = "micro-note", "Select an area to set a level, or a place to check in.")), actionButton("fit_map", "Fit results", class = "btn-sm")),
                uiOutput("focusbar"), div(class = "map-stage", leafletOutput("map", height = "100%")),
                div(class = "map-footer", textOutput("map_caption"),
                  tags$details(class = "map-key", tags$summary("Map key"),
                    p(class = "micro-note", "Number = grouped records; select to zoom or separate overlaps. Category colours match the filters; blue-grey groups mix categories. Hollow = unvisited, filled = checked in.", if (!is.null(data$ftz)) " Dashed outline = approximate tram zone."))))),
            div(class = "context-panel",
              div(class = "panel-card selection-card", h3("Selected place"), uiOutput("selected_summary"),
                selectizeInput("visit_place", "Place in this area", choices = c("This area in general" = ""),
                  options = list(onInitialize = I("function(){if(window.mlBindSelection)window.mlBindSelection(this,'place');}"), onChange = I("function(value){if(window.mlSelectionChange)window.mlSelectionChange('place',value,this);}"))),
                tags$fieldset(class = "progress-actions", disabled = "disabled", tags$legend(class = "sr-only", "Record a visit"),
                  div(class = "checkin-row", dateInput("visit_date", "Visit date", value = ml_today(), min = as.Date("1900-01-01"), max = ml_today(), format = "dd M yyyy"), actionButton("checkin", "Check in", class = "btn-primary")),
                  actionLink("undo", "Undo last change", class = "undo-link"))),
              div(class = "panel-card challenges-card", div(class = "section-heading", h3(span(class = "challenge-star", `aria-hidden` = "true", "✧ "), "Next achievement"), actionLink("open_challenges", "View all")),
                textOutput("challenge_summary", container = function(...) div(class = "challenge-summary", ...)), uiOutput("milestones", class = "achievement-grid")))),
          conditionalPanel("output.has_selected_place === true", class = "nearby-section",
            div(class = "panel-card",
              div(class = "section-heading", h3("Around your selected place"), actionButton("go_when", "Compare visit times →", class = "btn-link")),
              div(class = "nearby-preview", uiOutput("nearby_preview")),
              tags$details(class = "details-note nearby-panel", tags$summary("Browse nearby food, toilets & parks"),
                div(class = "nearby-controls",
                  selectInput("nearby_radius", "Straight-line radius", choices = c("300 m" = "300", "500 m" = "500", "1,000 m" = "1000"), selected = "500"),
                  checkboxGroupInput("nearby_categories", "Include", choices = c("Food", "Toilet", "Park"), selected = c("Food", "Toilet"), inline = TRUE)),
                uiOutput("nearby_results"), div(class = "nearby-navigation", actionButton("nearby_previous", "Previous", class = "btn-sm", disabled = "disabled"), textOutput("nearby_page_status", inline = TRUE), actionButton("nearby_next", "Next", class = "btn-sm", disabled = "disabled")),
                p(class = "micro-note", "Distances use mapped points, not walking routes or entrances."))))),
        tabPanel("When to go", value = "when",
          div(class = "analysis-page", div(class = "page-intro", h2("Choose when to visit"), p("Compare historical foot traffic at nearby counting locations. These measurements do not predict live crowds.")),
            uiOutput("time_availability"),
            conditionalPanel("output.has_time_data === true",
              div(class = "panel-card", div(class = "when-controls", selectizeInput("time_sensor", "Counting location", choices = sensor_choices, selected = ""), checkboxInput("tableau_link", "Follow selected place or area", TRUE)),
                uiOutput("tableau_context"), uiOutput("tableau_view"),
                tags$details(class = "details-note", tags$summary("Local measurements and coverage"), uiOutput("crowd_summary"), div(class = "table-wrap", tableOutput("crowd_table"))))))),
        tabPanel("Discover", value = "discover",
          div(class = "analysis-page", div(class = "page-intro", h2("Discover your next stop"), p("Compare areas, find a nearby break, or explore tram access.")),
            div(class = "panel-card discovery-workspace",
              div(class = "discovery-controls", checkboxInput("discover_all_landmarks", "Include all landmark types", FALSE), span(class = "micro-note", "Source listings, not popularity or venue ratings.")),
              tabsetPanel(id = "discover_view", type = "pills",
                tabPanel("Compare areas", value = "areas",
                  div(class = "analysis-toolbar", h3("What is mapped in each area?"), selectInput("discover_category", "Category", choices = categories, selected = "Landmark")),
                  uiOutput("area_comparison")),
                tabPanel("Nearby amenities", value = "amenities",
                  div(class = "analysis-toolbar", h3("Plan a break near a landmark"), selectInput("discover_amenity", "Look for", choices = c("Food", "Toilet", "Park"), selected = "Toilet")),
                  div(class = "insight-layout", div(class = "insight-summary", uiOutput("amenity_insight")), div(class = "insight-examples", h4("Places to inspect on the map"), uiOutput("amenity_examples")))),
                tabPanel("Tram access", value = "trams",
                  div(class = "analysis-toolbar", h3("How close are landmarks and parks to tram stops?")),
                  div(class = "insight-layout", div(class = "insight-summary", uiOutput("tram_insight")), div(class = "insight-examples", h4("Farthest from a mapped tram stop"), uiOutput("tram_examples")))))))),
        tabPanel("My progress", value = "journal",
          div(class = "analysis-page", div(class = "page-intro section-heading", div(h2("Your exploration"), p("Your visits and experience levels, saved in this browser.")), actionButton("open_backup", "Backup & restore")),
            uiOutput("progress_overview"),
            div(class = "panel-card progress-grid",
              div(class = "goal-panel", h3(span(class = "challenge-star", `aria-hidden` = "true", "◎ "), "Your next milestone"), uiOutput("goal_progress"),
                tags$details(class = "details-note", tags$summary("Change goal"), tags$fieldset(class = "progress-actions", disabled = "disabled", tags$legend(class = "sr-only", "Set exploration goal"), sliderInput("goal", "Areas to visit", min = 1, max = nrow(data$suburbs), value = min(3L, nrow(data$suburbs)), step = 1), actionButton("save_goal", "Save goal", class = "btn-primary")))),
              div(class = "category-panel", h3("Places visited by category"), uiOutput("category_chart"))),
            div(class = "panel-card progress-records", tabsetPanel(id = "journal_tab",
              tabPanel("Visit history", value = "history", div(class = "section-heading", h3("Your check-ins"), actionLink("undo_history", "Undo last change")), uiOutput("history_empty"), div(class = "table-wrap", tableOutput("history")), uiOutput("history_navigation")),
              tabPanel("Area experience", value = "ratings", h3("Your area experience"), p(class = "small-note", "Set a level or return to an area on the map. Check-ins never change your level automatically."), div(class = "table-wrap", tableOutput("passport"))))))))),
    div(class = "footer-note", "City of Melbourne study areas · Information Visualisation group project · ", actionLink("footer_sources", "Sources & limitations")))
}

# ---- Person B: one shared state for the map, journal and teammate modules ----
ml_achievements <- function(state, data) {
  visited_ids <- unique(state$visits$place_id[nzchar(state$visits$place_id)])
  collection <- function(title, desired, eligible, noun) {
    ids <- data$places$place_id[eligible]; target <- min(desired, length(ids))
    list(title = title, description = if (target) paste("Check in at", target, noun) else "No eligible places supplied.",
      target = target, count = sum(ids %in% visited_ids), ids = ids, available = target > 0L)
  }
  progress <- state$progress$familiarity
  area_target <- min(3L, nrow(data$suburbs)); score_target <- min(30L, 5L * nrow(data$suburbs))
  list(
    list(title = "FIRST STEP", description = "Record your first visit.", target = 1L, count = as.integer(nrow(state$visits) > 0), ids = character(), available = TRUE),
    collection("SIGHTSEER", 10L, data$places$place_category == "Landmark" & ml_visitor_landmark(data$places$sub), "visitor landmark records."),
    collection("GREEN THUMB", 5L, data$places$place_category == "Park", "park records."),
    collection("NEW TASTES", 3L, data$places$place_category == "Food", "food records."),
    list(title = "AREA HOPPER", description = paste("Record visits in", area_target, "different study areas."), target = area_target, count = length(unique(state$visits$suburb_id)), ids = character(), available = area_target > 0),
    collection("TRAM EXPLORER", 5L, data$places$place_category == "Tram stop", "tram-stop records."),
    collection("HANDY TO KNOW", 3L, data$places$place_category == "Toilet", "public-toilet records."),
    list(title = "CITY EXPLORER", description = "Rate every study area at Passed through or above.", target = nrow(data$suburbs), count = sum(progress >= 1L), ids = character(), available = nrow(data$suburbs) > 0),
    list(title = "LOCAL STORIES", description = paste("Reach a personal experience score of", score_target, "by rating areas."), target = score_target, count = sum(progress), ids = character(), available = score_target > 0)
  )
}
ml_earned <- function(achievements) vapply(achievements, function(a) isTRUE(a$available) && a$target > 0 && a$count >= a$target, logical(1))

ML_CLUSTER_ICON <- JS("function(cl){var ms=cl.getAllChildMarkers(),n=ms.length,v=0,colors=Array.from(new Set(ms.map(function(m){return m.options.color;}))),col=colors.length===1?colors[0]:'#546a84';ms.forEach(function(m){if(m.options.fillColor!=='#ffffff')v++;});var s=n<10?30:n<50?38:n<200?46:54;return L.divIcon({className:'pt-cluster',iconSize:L.point(s,s),html:'<div style=\"border-color:'+col+'\">'+n+(v?'<i style=\"background:'+col+'\">&#10003;'+v+'</i>':'')+'</div>'});}")

ml_server <- function(data, counts = NULL, counts_problem = "") function(input, output, session) {
  outside_count_locations <- 0L
  if (!is.null(counts) && nrow(counts)) {
    supplied_ids <- unique(counts$location_id)
    outside_count_locations <- sum(!supplied_ids %in% data$sensors$location_id)
    if (!any(supplied_ids %in% data$sensors$location_id)) {
      counts_problem <- "No aggregate location IDs match the supplied counting locations."
      counts <- NULL
    }
  }
  state <- reactiveVal(ml_empty_state(data))
  ready <- reactiveVal(FALSE)
  conflict <- reactiveVal(FALSE)
  undo_stack <- reactiveVal(list())
  pending_import <- reactiveVal(NULL)
  pending_import_details <- reactiveVal(NULL)
  pending_previous_key <- reactiveVal(NULL)
  previous_summaries <- reactiveVal(list())
  pending_reset <- reactiveVal(FALSE)
  pending_remove <- reactiveVal(NULL)
  history_page <- reactiveVal(1L)
  nearby_page <- reactiveVal(1L)
  revision <- reactiveVal(0L)
  save_message <- reactiveVal("Opening this browser's journal…")
  default_area <- data$suburbs$suburb_id[match("Melbourne (CBD)", data$suburbs$suburb_name)]
  if (is.na(default_area)) default_area <- data$suburbs$suburb_id[1]
  active_area <- reactiveVal(default_area)
  active_place <- reactiveVal("")
  focus <- reactiveVal(NULL)
  # Integration hooks: A's map, C's Tableau context, D's achievements/insights.
  selected_suburb <- reactive(active_area())
  selected_place <- reactive(data$places[data$places$place_id == active_place(), , drop = FALSE])
  # Raw boolean for conditionalPanel; accept both live and test session arguments.
  output$has_selected_place <- function(...) nrow(selected_place()) > 0L
  outputOptions(output, "has_selected_place", suspendWhenHidden = FALSE)
  shared_progress <- reactive(ml_progress_table(state(), data))
  shared_visits <- reactive(ml_visit_table(state(), data))
  shared_stats <- reactive(ml_stats(state(), data))
  achievements <- reactive(ml_achievements(state(), data))
  selected_sensor <- reactive({
    p <- selected_place(); sensors <- data$sensors
    if (!nrow(p)) p <- data$suburbs[data$suburbs$suburb_id == active_area(), , drop = FALSE]
    if (!nrow(p) || !nrow(sensors)) return(sensors[FALSE, , drop = FALSE])
    sensors$distance_m <- 1000 * ml_distance_km(p$lat, p$lon, sensors$lat, sensors$lon)
    head(sensors[order(sensors$distance_m, sensors$location_id), , drop = FALSE], 1L)
  })
  nearby_radius <- reactive({
    radius <- suppressWarnings(as.numeric(input$nearby_radius))
    if (length(radius) != 1L || is.na(radius) || !radius %in% c(300, 500, 1000)) radius <- 500
    radius
  })
  nearby_matches <- reactive({
    p <- selected_place()
    if (!nrow(p)) return(data$places[FALSE, , drop = FALSE])
    ml_nearby(data$places, p$place_id, input$nearby_categories, radius_km = nearby_radius() / 1000, limit = max(1L, nrow(data$places)))
  })
  nearby_pages <- reactive(max(1L, ceiling(nrow(nearby_matches()) / 6L)))
  current_nearby_page <- reactive(min(nearby_page(), nearby_pages()))
  nearby_places <- reactive({
    matches <- nearby_matches()
    if (!nrow(matches)) return(matches)
    start <- (current_nearby_page() - 1L) * 6L + 1L
    matches[seq.int(start, min(start + 5L, nrow(matches))), , drop = FALSE]
  })
  # Reset after selection/navigation observers so a simultaneous old Next click
  # cannot skip the first results for the new origin or filters.
  observeEvent(list(active_place(), input$nearby_radius, input$nearby_categories), nearby_page(1L), priority = -10)
  observeEvent(input$nearby_previous, {
    value <- input$nearby_previous
    if (length(value) == 1L && ml_is_integer(value) && value > 0) nearby_page(max(1L, current_nearby_page() - 1L))
  })
  observeEvent(input$nearby_next, {
    value <- input$nearby_next
    if (length(value) == 1L && ml_is_integer(value) && value > 0) nearby_page(min(nearby_pages(), current_nearby_page() + 1L))
  })
  observe({
    session$sendCustomMessage("ml-nearby-nav", list(previous = current_nearby_page() > 1L, `next` = current_nearby_page() < nearby_pages()))
  })
  output$nearby_page_status <- renderText({
    if (!nrow(nearby_matches())) "No results to browse" else paste("Page", current_nearby_page(), "of", nearby_pages(), "· 6 per page")
  })
  set_ready <- function(value) {ready(value); session$sendCustomMessage("ml-ready", list(ready = value && !conflict()))}
  persist <- function() {
    revision(revision() + 1L); save_message("Saving in this browser…")
    session$sendCustomMessage("ml-save", list(json = ml_state_json(state()), revision = revision()))
  }
  can_edit <- function() {
    if (identical(input$selection_protocol, "unsupported")) stop("Selection controls could not initialise safely. Reload the app before recording visits.", call. = FALSE)
    if (conflict()) stop("Another tab changed the saved journal. Download a backup, reload, then restore it if needed.", call. = FALSE)
    if (!ready()) stop("Wait for the journal to load, or recover it using the controls in My progress.", call. = FALSE)
  }
  commit <- function(new_state, message, notify_achievement = FALSE) {
    can_edit(); new_state <- ml_validate_state(new_state, data)
    old <- state(); old_compare <- old; new_compare <- new_state
    old_compare$updated_at <- NULL; new_compare$updated_at <- NULL
    if (identical(old_compare, new_compare)) {
      showNotification("Already up to date.", type = "message", duration = 3); return(invisible(NULL))
    }
    pending_remove(NULL)
    undo_stack(tail(c(undo_stack(), list(old)), 20)); state(new_state); persist()
    newly_earned <- if (notify_achievement) {
      before <- ml_achievements(old, data); after <- ml_achievements(new_state, data)
      vapply(after[ml_earned(after) & !ml_earned(before)], function(a) a$title, character(1))
    } else character()
    if (length(newly_earned)) message <- paste0(message, " · Achievement: ", paste(newly_earned, collapse = ", "), ".")
    showNotification(message, type = "message", duration = 4, id = "journal-feedback")
  }
  safely <- function(expr) tryCatch(force(expr), error = function(e) showNotification(conditionMessage(e), type = "error", duration = 8))
  session$onFlushed(function() {
    session$sendCustomMessage("ml-load", list(key = paste0("melbourne-visitor-journal-v1-", data$dataset_id)))
    # A long-running server may have constructed its UI on an earlier day.
    updateDateInput(session, "visit_date", value = ml_today(), max = ml_today())
  }, once = TRUE)
  observe({
    invalidateLater(60000, session)
    # Keep past dates selected by the visitor; only advance the permitted maximum.
    updateDateInput(session, "visit_date", max = ml_today())
  })
  observeEvent(input$storage_loaded, {
    if (ready()) return()
    x <- input$storage_loaded
    # Discovery comes from the client. Accept only bounded summaries for this
    # application's real-data keys; the full journal is validated on review.
    summaries <- if (is.list(x$previous) && !is.data.frame(x$previous)) head(x$previous, 20L) else list()
    summaries <- Filter(function(item) {
      is.list(item) && ml_is_scalar_string(item$key) && ml_is_scalar_string(item$dataset_id) &&
        grepl("^melbourne-level-up-real-[A-Za-z0-9._-]{1,80}$", item$dataset_id) &&
        identical(item$key, paste0("melbourne-visitor-journal-v1-", item$dataset_id)) &&
        !identical(item$dataset_id, data$dataset_id) &&
        ml_is_scalar_string(item$updated_at) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", item$updated_at) &&
        length(item$checkins) == 1L && ml_is_integer(item$checkins) && item$checkins >= 0 && item$checkins <= 20000 &&
        length(item$areas) == 1L && ml_is_integer(item$areas) && item$areas >= 1 && item$areas <= 20000
    }, summaries)
    if (length(summaries)) summaries <- summaries[!duplicated(vapply(summaries, function(item) item$key, character(1)))]
    previous_summaries(if (isTRUE(x$available)) summaries else list())
    if (is.character(x$json) && length(x$json) == 1L && nzchar(x$json)) {
      tryCatch({state(ml_parse_state(x$json, data)); set_ready(TRUE); save_message("Saved journal restored from this browser.")},
        error = function(e) {save_message("Saved journal could not be restored. Open Backup & restore to recover it."); showNotification(conditionMessage(e), type = "error", duration = NULL)})
    } else {
      set_ready(TRUE)
      save_message(if (!isTRUE(x$available)) "Browser storage is unavailable. Download a backup before leaving." else if (length(previous_summaries()))
        "Earlier dataset journal found · open Backup & restore to review a transfer." else "New journal · changes save in this browser.")
    }
  }, ignoreInit = TRUE)
  observeEvent(input$storage_saved, {
    x <- input$storage_saved
    # Conflicts concern the entire journal; even a late conflict locks edits.
    if (isTRUE(x$conflict)) {
      conflict(TRUE); session$sendCustomMessage("ml-ready", list(ready = FALSE))
      save_message("Another tab changed this journal. Download a backup, then reload; these edits are not saved.")
      return()
    }
    if (conflict()) return()
    if (!identical(as.integer(x$revision), as.integer(revision()))) return()
    save_message(if (isTRUE(x$ok)) "All changes saved in this browser." else if (isTRUE(x$conflict))
      "Another tab changed this journal. Download a backup, then reload; these edits are not saved." else
      "Browser save failed. Keep this tab open and download a backup.")
  })
  output$save_status <- renderText(save_message())
  select_active_area <- function(id) {
    if (!ml_is_scalar_string(id) || !id %in% data$suburbs$suburb_id) return()
    changed <- !identical(active_area(), id)
    active_area(id)
    if (nrow(selected_place()) && selected_place()$suburb_id != active_area()) active_place("")
    if (changed) {
      extent <- sf::st_bbox(data$boundaries[data$boundaries$suburb_id == active_area(), ])
      leafletProxy("map", session) |> fitBounds(extent[["xmin"]], extent[["ymin"]], extent[["xmax"]], extent[["ymax"]])
    }
  }
  observeEvent(input$selection_protocol, {
    if (identical(input$selection_protocol, "unsupported")) {
      set_ready(FALSE)
      save_message("Selection controls could not initialise safely. Reload this app before recording visits.")
      showNotification("Selection controls could not initialise safely. Reload this app before recording visits.", type = "error", duration = NULL, id = "selection-protocol")
    }
  }, priority = -100)
  observeEvent(input$suburb, {
    if (isTRUE(input$selection_protocol) || identical(input$selection_protocol, "unsupported")) return()
    select_active_area(input$suburb)
  }, priority = 20)
  observeEvent(input$visit_place, {
    if (isTRUE(input$selection_protocol) || identical(input$selection_protocol, "unsupported")) return()
    value <- input$visit_place
    if (!ml_is_scalar_string(value)) return()
    if (identical(value, "")) {active_place(""); return()}
    p <- data$places[data$places$place_id == value & data$places$suburb_id == active_area(), , drop = FALSE]
    if (nrow(p)) {
      changed <- !identical(active_place(), value)
      active_place(value)
      if (changed) {
        leafletProxy("map", session) |> setView(p$lon, p$lat, zoom = 16)
      }
    }
  }, priority = 10)
  sync_visit_picker <- function(area_id, selected_id = "") {
    p <- data$places[data$places$suburb_id == area_id, , drop = FALSE]
    p <- p[order(p$place_category, p$place_name), , drop = FALSE]
    current <- selected_id; if (!current %in% p$place_id) current <- ""
    # Search large area lists on the server instead of sending >1,000 options.
    # Include selected with the server choices so a map-clicked place is loaded
    # even when it is absent from Selectize's current search result page.
    labels <- if (nrow(p)) paste(p$place_name, "·", p$place_category) else character()
    repeated <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
    labels[repeated] <- paste0(labels[repeated], " · ", sprintf("%.5f, %.5f", p$lat[repeated], p$lon[repeated]))
    updateSelectizeInput(session, "visit_place", choices = c("This area in general" = "", setNames(p$place_id, labels)), selected = current, server = TRUE)
  }
  observeEvent(active_area(), {
    sync_visit_picker(active_area(), isolate(active_place()))
  })
  observeEvent(state()$goal, updateSliderInput(session, "goal", value = state()$goal))
  area_dialog <- function(id) {
    req(id %in% data$suburbs$suburb_id)
    level <- state()$progress$familiarity[match(id, state()$progress$suburb_id)]
    p <- data$places[data$places$suburb_id == id, , drop = FALSE]
    visited <- unique(state()$visits$place_id)
    showModal(modalDialog(title = data$suburbs$suburb_name[match(id, data$suburbs$suburb_id)], easyClose = TRUE,
      p(class = "modal-stats", paste0(sum(p$place_id %in% visited), " / ", nrow(p), " places checked in · current Lv ", level)),
      p(class = "hint-dark", "Choose the experience that describes you. Check-ins do not change this personal rating automatically."),
      tags$fieldset(class = "progress-actions", disabled = if (!ready() || conflict()) "disabled" else NULL,
        lapply(6:1, function(i) tags$button(type = "button", class = paste("lv-btn", if (level == ml_levels$level[i]) "current"),
          style = paste0("background:", ml_levels$color[i], ";color:", if (ml_levels$level[i] >= 4) "#ffffff" else "#142d38"),
          onclick = paste0("Shiny.setInputValue('set_lv',", jsonlite::toJSON(list(area = id, lv = ml_levels$level[i]), auto_unbox = TRUE), ",{priority:'event'})"),
          paste("Lv", ml_levels$level[i], ml_levels$label[i])))), footer = modalButton("Close")))
  }
  observeEvent(input$edit_area, area_dialog(active_area()))
  observeEvent(input$set_lv, safely({
    req(is.list(input$set_lv)); can_edit()
    updated <- ml_set_level(state(), input$set_lv$area, input$set_lv$lv, data)
    removeModal(); commit(updated, "Area experience updated.", notify_achievement = TRUE)
  }))
  observeEvent(input$checkin, safely({
    # Commit the same canonical selection shown in the summary/map, even while
    # a map-driven Selectize update is still travelling back to the browser.
    can_edit()
    name <- if (nrow(selected_place())) selected_place()$place_name else data$suburbs$suburb_name[match(active_area(), data$suburbs$suburb_id)]
    commit(ml_checkin(state(), active_area(), active_place(), as.character(input$visit_date), data), paste("Check-in recorded:", name), notify_achievement = TRUE)
    history_page(1L)
  }))
  observeEvent(input$save_goal, safely({can_edit(); commit(ml_set_goal(state(), input$goal, data), "Goal updated.")}))
  undo_change <- function() safely({
    can_edit(); stack <- undo_stack()
    if (!length(stack)) {showNotification("No changes to undo in this session.", type = "message"); return()}
    pending_remove(NULL); state(stack[[length(stack)]]); undo_stack(head(stack, -1)); persist(); showNotification("Last change undone.")
  })
  observeEvent(input$undo, undo_change())
  observeEvent(input$undo_history, undo_change())
  observeEvent(input$reset, {
    pending_reset(TRUE)
    showModal(modalDialog(title = "Start a fresh journal?", "This clears this dataset's saved journal. Download a backup first if you want to keep it. This action can be undone during this session.",
      footer = tagList(actionButton("cancel_reset", "Keep journal"), actionButton("confirm_reset", "Start fresh", class = "btn-danger"))))
  })
  observeEvent(input$cancel_reset, {pending_reset(FALSE); removeModal()})
  observeEvent(input$confirm_reset, safely({
    req(isTRUE(pending_reset())); pending_reset(FALSE)
    if (conflict()) stop("Download a backup and reload before starting fresh.")
    removeModal(); set_ready(TRUE)
    pending_remove(NULL); old <- state(); fresh <- ml_empty_state(data)
    undo_stack(tail(c(undo_stack(), list(old)), 20)); state(fresh); persist()
    showNotification("A fresh journal is ready.")
  }))
  observeEvent(input$reload, showModal(modalDialog(title = "Reload the saved journal?", "Download a backup first if some edits could not be saved. Reloading discards unsaved edits in this tab.",
    footer = tagList(modalButton("Keep this session"), actionButton("confirm_reload", "Reload saved journal")))))
  observeEvent(input$confirm_reload, {removeModal(); session$sendCustomMessage("ml-reload", list())})
  clear_import <- function() {
    pending_import(NULL); pending_import_details(NULL); pending_previous_key(NULL)
  }
  review_import <- function(json, source_key = NULL) {
    prepared <- ml_prepare_import(json, data)
    source_id <- if (isTRUE(prepared$migrated)) prepared$review$source_dataset_id else prepared$state$dataset_id
    if (!is.null(source_key) && !identical(source_key, paste0("melbourne-visitor-journal-v1-", source_id))) {
      stop("The earlier journal no longer matches its saved dataset key. Download its original backup before trying again.")
    }
    pending_import(prepared$state); pending_import_details(prepared)
    showModal(modalDialog(title = if (prepared$migrated) "Review dataset transfer" else "Restore this backup?", easyClose = FALSE,
      lapply(ml_import_review_lines(prepared), p),
      if (!prepared$migrated) p("Undo can recover your current journal during this session."),
      div(class = "action-row", downloadButton("import_original", "Download original backup"), downloadButton("backup", "Download current journal")),
      footer = tagList(actionButton("cancel_import", "Cancel"), actionButton("confirm_import", if (prepared$migrated) "Confirm reviewed transfer" else "Restore backup"))))
  }
  output$previous_journals <- renderUI({
    items <- previous_summaries()
    if (!length(items)) return(NULL)
    keys <- vapply(items, function(item) item$key, character(1))
    labels <- vapply(seq_along(items), function(i) {
      item <- items[[i]]
      paste0("Journal ", i, " · ", sub("T", " ", sub("Z$", " UTC", item$updated_at)), " · ", item$checkins, " check-ins · ", item$areas, " areas")
    }, character(1))
    tagList(h3("Earlier dataset journals"), p(class = "small-note", "These saved journals are from earlier real datasets in this browser. Review one before transferring matching records; the original remains unchanged."),
      selectInput("previous_journal_key", "Choose an earlier journal", choices = setNames(keys, labels)),
      actionButton("review_previous", "Review selected journal"))
  })
  observeEvent(input$review_previous, safely({
    clear_import()
    if (conflict()) stop("Download a backup and reload before reviewing another saved journal.")
    key <- input$previous_journal_key
    allowed <- vapply(previous_summaries(), function(item) item$key, character(1))
    if (!ml_is_scalar_string(key) || !key %in% allowed) stop("Choose an earlier journal from the list.")
    pending_previous_key(key)
    session$sendCustomMessage("ml-load-previous", list(key = key))
  }))
  observeEvent(input$previous_journal, safely({
    reply <- input$previous_journal
    requested <- pending_previous_key()
    if (is.null(requested) || !is.list(reply) || !identical(reply$key, requested)) return()
    clear_import()
    if (conflict()) stop("Download a backup and reload before reviewing another saved journal.")
    if (!isTRUE(reply$ok)) stop("The earlier journal could not be read. It may have been removed or browser storage is unavailable.")
    review_import(reply$json, source_key = requested)
  }))
  observeEvent(input$restore_file, safely({
    clear_import()
    if (conflict()) stop("Download a backup, reload this tab, then restore the backup you want to keep.")
    f <- input$restore_file
    if (length(f$size) != 1L || !is.numeric(f$size) || is.na(f$size) || f$size > 2 * 1024^2) stop("Backup must be smaller than 2 MB.")
    review_import(paste(readLines(f$datapath, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  }))
  observeEvent(input$cancel_import, {clear_import(); removeModal()})
  observeEvent(input$confirm_import, safely({
    req(!is.null(pending_import())); if (conflict()) stop("Reload before restoring a backup.")
    migrated <- isTRUE(pending_import_details()$migrated)
    removeModal(); set_ready(TRUE)
    pending_remove(NULL); undo_stack(tail(c(undo_stack(), list(state())), 20)); state(pending_import()); clear_import(); persist()
    showNotification(if (migrated) "Reviewed journal transfer restored. The original backup is unchanged." else "Backup restored.")
  }))
  output$import_original <- downloadHandler(function() paste0("original-melbourne-journal-", ml_today(), ".json"), function(file) {
    req(!is.null(pending_import_details()))
    writeLines(pending_import_details()$source_json, file, sep = "", useBytes = TRUE)
  }, contentType = "application/json")
  output$backup <- downloadHandler(function() paste0("melbourne-journal-", ml_today(), ".json"), function(file) writeLines(ml_state_json(state()), file, sep = "", useBytes = TRUE), contentType = "application/json")
  safe_csv <- function(d, file) {
    for (nm in names(d)) if (is.character(d[[nm]])) d[[nm]] <- ifelse(grepl("^[=+@\\t\\r-]", d[[nm]], perl = TRUE), paste0("'", d[[nm]]), d[[nm]])
    write.csv(d, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  }
  output$progress_csv <- downloadHandler(function() paste0("area-progress-", ml_today(), ".csv"), function(file) safe_csv(shared_progress(), file))
  output$visits_csv <- downloadHandler(function() paste0("check-ins-", ml_today(), ".csv"), function(file) safe_csv(shared_visits(), file))

  filtered_places <- reactive({
    p <- data$places
    f <- focus()
    if (!is.null(f) && f >= 1 && f <= length(achievements())) p <- p[p$place_id %in% achievements()[[f]]$ids, , drop = FALSE]
    else p <- p[p$place_category %in% input$categories, , drop = FALSE]
    if (!isTRUE(input$include_other_landmarks)) p <- p[p$place_category != "Landmark" | ml_visitor_landmark(p$sub), , drop = FALSE]
    if (isTRUE(input$only_selected_area)) p <- p[p$suburb_id == active_area(), , drop = FALSE]
    term <- trimws(if (is.null(input$place_search)) "" else input$place_search)
    if (nzchar(term)) p <- p[grepl(tolower(term), tolower(paste(p$place_name, p$sub)), fixed = TRUE), , drop = FALSE]
    if (isTRUE(input$unvisited_only)) p <- p[!p$place_id %in% state()$visits$place_id, , drop = FALSE]
    p
  })
  output$map <- renderLeaflet({
    extent <- sf::st_bbox(data$boundaries)
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE, minZoom = 10, maxZoom = 19)) |>
      addMapPane("areas", 410) |> addMapPane("zone", 420) |> addMapPane("points", 430) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas, options = providerTileOptions(maxNativeZoom = 16, maxZoom = 19)) |>
      fitBounds(extent[["xmin"]], extent[["ymin"]], extent[["xmax"]], extent[["ymax"]]) |>
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))
    if (!is.null(data$ftz)) m <- m |> addPolygons(data = data$ftz, group = "tram-zone", fill = FALSE, color = "#1d2b53", weight = 2, dashArray = "8 6",
      options = pathOptions(pane = "zone", interactive = FALSE))
    m
  })
  observe({
    s <- data$suburbs; levels <- state()$progress$familiarity[match(s$suburb_id, state()$progress$suburb_id)]
    b <- data$boundaries; lv <- levels[match(b$suburb_id, s$suburb_id)]
    visited <- unique(state()$visits$place_id)
    p <- filtered_places()
    m <- leafletProxy("map", session) |> clearGroup("areas") |> clearGroup("places") |> clearGroup("selected-place") |> clearMarkerClusters()
    m <- m |> addPolygons(data = b, layerId = paste0("area:", b$suburb_id), group = "areas", fillColor = ml_levels$color[lv + 1L],
      fillOpacity = ifelse(lv == 0, .1, .24), color = ifelse(b$suburb_id == active_area(), "#9c6500", "#1d2b53"),
      weight = ifelse(b$suburb_id == active_area(), 4, 1.5),
      label = paste0(b$suburb_name, " · Lv ", lv, " ", ml_levels$label[lv + 1L]),
      highlightOptions = highlightOptions(weight = 4), options = pathOptions(pane = "areas"))
    if (nrow(p)) {
      x <- p; checked <- x$place_id %in% visited
      m <- m |> addCircleMarkers(lng = x$lon, lat = x$lat, group = "places", layerId = paste0("place:", x$place_id),
        radius = ifelse(checked, 7, 5), weight = 2, opacity = 1, fillOpacity = 1,
        color = ml_color(x$place_category), fillColor = ifelse(checked, ml_color(x$place_category), "#ffffff"),
        label = paste0(x$place_name, " · ", x$place_category, " · ", x$suburb_name, ifelse(checked, " · Checked in", " · Not yet visited")),
        options = pathOptions(pane = "points"), clusterId = "places",
        clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE, zoomToBoundsOnClick = TRUE, maxClusterRadius = 45, showCoverageOnHover = FALSE, iconCreateFunction = ML_CLUSTER_ICON))
    }
    selected <- selected_place()
    if (nrow(selected)) m <- m |> addCircleMarkers(lng = selected$lon, lat = selected$lat, group = "selected-place",
      radius = 12, color = "#9c6500", weight = 4, fillOpacity = 0,
      label = paste("Selected:", selected$place_name), options = pathOptions(pane = "points", interactive = FALSE))
  })
  choose_place <- function(id) {
    if (!ml_is_scalar_string(id) || !id %in% data$places$place_id) return()
    p <- data$places[match(id, data$places$place_id), , drop = FALSE]
    active_place(id); active_area(p$suburb_id)
    updateSelectizeInput(session, "suburb", selected = p$suburb_id)
    sync_visit_picker(p$suburb_id, id)
    leafletProxy("map", session) |> setView(p$lon, p$lat, zoom = 16)
  }
  observeEvent(input$selection_intent, {
    if (!isTRUE(input$selection_protocol)) return()
    intent <- input$selection_intent
    if (!is.list(intent) || !ml_is_scalar_string(intent$kind) || !ml_is_scalar_string(intent$value)) return()
    if (identical(intent$kind, "area") && intent$value %in% data$suburbs$suburb_id) {
      select_active_area(intent$value)
      updateSelectizeInput(session, "suburb", selected = active_area())
    } else if (identical(intent$kind, "place")) {
      if (!nzchar(intent$value)) {
        active_place(""); sync_visit_picker(active_area(), "")
      } else if (intent$value %in% data$places$place_id) choose_place(intent$value)
    }
  }, priority = 20)
  observeEvent(input$map_marker_click, {
    key <- input$map_marker_click$id
    if (ml_is_scalar_string(key) && startsWith(key, "place:")) choose_place(sub("^place:", "", key))
  }, priority = 15)
  observeEvent(input$map_shape_click, {
    key <- input$map_shape_click$id
    if (!ml_is_scalar_string(key) || !startsWith(key, "area:")) return()
    id <- sub("^area:", "", key); if (!id %in% data$suburbs$suburb_id) return()
    active_area(id); if (nrow(selected_place()) && selected_place()$suburb_id != id) active_place("")
    updateSelectizeInput(session, "suburb", selected = id)
  }, priority = 15)
  observeEvent(input$nearby_pick, choose_place(input$nearby_pick), priority = 15)
  output$nearby_results <- renderUI({
    p <- selected_place()
    if (!nrow(p)) return(ml_empty("Select a place on the map or in the visit selector to find nearby essentials."))
    if (!length(intersect(input$nearby_categories, c("Food", "Toilet", "Park")))) return(ml_empty("Choose at least one category above to find nearby essentials."))
    n <- nearby_places()
    total <- nrow(nearby_matches())
    radius_label <- paste(format(nearby_radius(), big.mark = ",", trim = TRUE), "m")
    if (!nrow(n)) return(tagList(p(class = "nearby-count", paste("0 matching records within", radius_label, "of", p$place_name)),
      ml_empty("No matching essentials within this radius. Choose another category or a wider radius.")))
    first <- (current_nearby_page() - 1L) * 6L + 1L
    same_location <- n$lat == p$lat & n$lon == p$lon
    tagList(p(class = "nearby-count", role = "status", paste0("Showing ", first, "–", first + nrow(n) - 1L, " of ", total, " matching records within ", radius_label)),
      p(class = "nearby-order", paste("From", p$place_name, "· Nearest first across the selected categories · 6 per page")),
      if (any(same_location)) p(class = "nearby-shared", "Some listings share the selected place's mapped coordinates. This may represent a shared site; it does not establish their exact distance on the ground."),
      div(class = "nearby-grid", lapply(seq_len(nrow(n)), function(i)
        div(class = "nearby-card", tags$button(type = "button", class = "btn btn-sm",
          `aria-label` = paste("Select", n$place_name[i], "on the map"),
          onclick = paste0("Shiny.setInputValue('nearby_pick',", jsonlite::toJSON(n$place_id[i], auto_unbox = TRUE), ",{priority:'event'});document.getElementById('explore-map').focus();document.getElementById('explore-map').scrollIntoView({block:'start'});"), n$place_name[i]),
          div(class = "nearby-meta", span(span(class = "dot", `aria-hidden` = "true", style = paste0("background:", ml_color(n$place_category[i]))), n$place_category[i]),
            span(if (same_location[i]) "Same mapped location" else if (n$distance_m[i] < 1) "Less than 1 m" else paste(round(n$distance_m[i]), "m"))),
          div(class = "nearby-location small-note", n$suburb_name[i], " · ", sprintf("%.5f, %.5f", n$lat[i], n$lon[i]))))))
  })
  observeEvent(input$fit_map, {
    p <- filtered_places()
    if (!nrow(p)) {showNotification("No places match these filters."); return()}
    leafletProxy("map", session) |> fitBounds(min(p$lon) - .001, min(p$lat) - .001, max(p$lon) + .001, max(p$lat) + .001)
  })
  observeEvent(input$focus, {
    f <- input$focus
    if (!ml_is_integer(f) || length(f) != 1L || f < 1 || f > length(achievements()) || !length(achievements()[[f]]$ids)) return()
    focus(if (identical(focus(), as.integer(f))) NULL else as.integer(f))
    removeModal(); updateTabsetPanel(session, "main_tab", selected = "explore")
    targets <- filtered_places()
    if (nrow(targets)) leafletProxy("map", session) |>
      fitBounds(min(targets$lon) - .001, min(targets$lat) - .001, max(targets$lon) + .001, max(targets$lat) + .001)
    else showNotification("No targets match your other filters. Clear the search or area filter to see more.", type = "message")
  })
  observeEvent(input$unfocus, focus(NULL))
  observeEvent(input$categories, focus(NULL), ignoreInit = TRUE)
  output$focusbar <- renderUI({
    if (is.null(focus())) return(NULL)
    div(class = "focusbar", role = "status", span(strong(achievements()[[focus()]]$title),
      " targets · category choices temporarily replaced; other filters still apply."), actionLink("unfocus", "Clear targets"))
  })
  output$map_caption <- renderText({
    selected <- selected_place()
    hidden <- nrow(selected) && !selected$place_id %in% filtered_places()$place_id
    paste(format(nrow(filtered_places()), big.mark = ","), "matching places · Hollow = unvisited; filled = checked in.",
      if (hidden) "The gold selection ring is outside your current filters." else "")
  })
  output$legend <- renderUI(tagList(lapply(6:1, function(i) div(class = "lg", span(class = "sw", style = paste0("background:", ml_levels$color[i])),
    span(paste0("Lv", ml_levels$level[i], " ", ml_levels$label[i])), span(class = "lgn", paste0("×", sum(state()$progress$familiarity == ml_levels$level[i])))))))
  output$stats <- renderUI({
    s <- shared_stats(); score <- sum(state()$progress$familiarity)
    div(class = "header-stats",
      span(class = "header-score", title = "Experience score: sum of your area levels (0–5)",
        span(`aria-hidden` = "true", "✦"), strong(paste0(score, "/", 5L * s$total_suburbs)), tags$small("experience")),
      span(strong(s$checkins), if (s$checkins == 1L) " check-in" else " check-ins"), span(strong(s$visited_places), if (s$visited_places == 1L) " place" else " places"),
      span(strong(sum(ml_earned(achievements()))), if (sum(ml_earned(achievements())) == 1L) " achievement" else " achievements"))
  })
  output$filter_status <- renderUI({
    active <- c(if (isTRUE(input$include_other_landmarks)) "All landmark types",
      if (isTRUE(input$only_selected_area)) "Selected area only",
      if (isTRUE(input$unvisited_only)) "Unvisited only")
    if (!length(active)) return(NULL)
    span(class = "filter-status", title = paste(active, collapse = " · "),
      `aria-label` = paste("Active filters:", paste(active, collapse = ", ")), paste(length(active), "active"))
  })
  output$selected_summary <- renderUI({
    id <- active_area(); name <- data$suburbs$suburb_name[match(id, data$suburbs$suburb_id)]
    lv <- state()$progress$familiarity[match(id, state()$progress$suburb_id)]
    place <- selected_place()
    tagList(if (nrow(place)) tagList(div(class = "place-name", place$place_name),
        p(class = "place-kind", paste(place$place_category, "·", place$sub))),
      div(class = "visit-context", span(class = "visit-area", name), span(class = "level-pill", paste("Lv", lv, ml_levels$label[lv + 1L]))),
      if (nrow(place)) tags$details(class = "place-details", tags$summary("Mapped location"),
        p(class = "small-note", sprintf("%.5f, %.5f · listings may share these coordinates", place$lat, place$lon)))
      else p(class = "small-note", "Select a marker or choose a place below."))
  })
  achievement_card <- function(i) {
    a <- achievements()[[i]]; done <- ml_earned(list(a))[[1]]
    title <- span(class = "ach-t", if (done) "✓ " else "○ ", a$title)
    heading <- if (length(a$ids)) tags$button(type = "button", class = "ach-target",
      `aria-pressed` = if (identical(focus(), i)) "true" else "false", `aria-controls` = "explore-map",
      onclick = paste0("Shiny.setInputValue('focus',", i, ",{priority:'event'})"),
      title, span(class = "target-action", if (identical(focus(), i)) "Clear targets" else "Show targets →")) else title
    div(class = paste("ach", if (done) "done", if (identical(focus(), i)) "focused"), heading, div(class = "ach-d", a$description),
      if (a$available) div(class = "bar", role = "progressbar", `aria-label` = a$title, `aria-valuemin` = 0, `aria-valuemax` = a$target, `aria-valuenow` = min(a$count, a$target),
        div(style = paste0("width:", min(100, 100 * a$count / a$target), "%"))), div(class = "ach-n", if (a$available) paste0(min(a$count, a$target), "/", a$target) else "Unavailable"))
  }
  output$milestones <- renderUI({
    a <- achievements(); pending <- which(!ml_earned(a) & vapply(a, function(x) x$available, logical(1)))
    indices <- head(unique(c(focus(), pending, which(ml_earned(a)))), 1L)
    tagList(lapply(indices, achievement_card))
  })
  output$all_milestones <- renderUI(div(class = "all-challenges", lapply(seq_along(achievements()), achievement_card)))
  output$challenge_summary <- renderText(paste(sum(ml_earned(achievements())), "/", sum(vapply(achievements(), function(a) a$available, logical(1))), "achievements earned"))
  observeEvent(input$open_challenges, showModal(modalDialog(title = "Your achievements", uiOutput("all_milestones"),
    p(class = "small-note", "Collections count distinct supplied records. Repeat visits count once; separate listings may share a location. Area ratings are your own experience."), size = "l", easyClose = TRUE, footer = modalButton("Close"))))
  output$goal_progress <- renderUI({s <- shared_stats(); target <- state()$goal; visited <- s$visited_suburbs
    title <- if (visited == nrow(data$suburbs)) "All study areas visited!" else if (visited > target) "Goal exceeded!" else if (visited == target) "Goal reached!" else paste(visited, "of", target, "areas visited")
    detail <- if (visited >= target) paste(visited, "areas visited · Your target:", target) else paste(target - visited, if (target - visited == 1L) "more area to reach your goal." else "more areas to reach your goal.")
    tagList(h3(title), p(class = "goal-status", detail),
      div(class = "bar", role = "progressbar", `aria-label` = "Areas visited goal", `aria-valuemin` = 0, `aria-valuemax` = target, `aria-valuenow` = min(s$visited_suburbs, target),
        `aria-valuetext` = paste(visited, "areas visited; target", target),
        div(style = paste0("width:", min(100, 100 * s$visited_suburbs / target), "%"))),
      if (visited >= target && visited < nrow(data$suburbs)) p(class = "small-note", "Ready for another challenge? Choose a higher target below."))})
  output$passport <- renderTable({
    d <- shared_progress(); escape <- function(x) as.character(htmltools::htmlEscape(x))
    button <- function(event, label, id) as.character(tags$button(type = "button", class = "btn btn-sm",
      onclick = paste0("Shiny.setInputValue('", event, "',", jsonlite::toJSON(id, auto_unbox = TRUE), ",{priority:'event'})"), label))
    data.frame(Area = escape(d$suburb_name), Experience = paste0("Lv ", d$familiarity, " · ", ml_levels$label[d$familiarity + 1L]),
      `Check-ins` = d$visit_count, `Distinct places` = d$distinct_places,
      `Last visit` = ifelse(nzchar(d$last_visit), escape(d$last_visit), "Not yet recorded"),
      Actions = vapply(d$suburb_id, function(id) paste(button("review_area", "Set level", id), button("browse_area", "Map", id)), character(1)), check.names = FALSE)
  }, rownames = FALSE, spacing = "s", width = "100%", sanitize.text.function = identity)
  output$history_empty <- renderUI(if (!nrow(state()$visits)) ml_empty("No check-ins yet. Choose a place in Explore to start your journal."))
  history_pages <- reactive(max(1L, ceiling(nrow(state()$visits) / 15L)))
  current_history_page <- reactive(min(history_page(), history_pages()))
  observeEvent(input$history_page, {
    value <- input$history_page
    if (ml_is_integer(value) && length(value) == 1L && value >= 1 && value <= history_pages()) history_page(as.integer(value))
  })
  output$history <- renderTable({d <- shared_visits(); if (!nrow(d)) return(NULL)
    d <- d[order(d$visited_on, seq_len(nrow(d)), decreasing = TRUE), , drop = FALSE]
    first <- (current_history_page() - 1L) * 15L + 1L
    d <- d[first:min(nrow(d), first + 14L), , drop = FALSE]
    # Escape every data string before adding our own trusted action markup.
    escape <- function(x) as.character(htmltools::htmlEscape(x))
    data.frame(Date = escape(d$visited_on), Area = escape(d$suburb_name), Place = escape(d$place_name), Category = escape(d$place_category),
      Action = vapply(seq_len(nrow(d)), function(i) as.character(tags$button(type = "button", class = "btn btn-sm",
        `aria-label` = paste("Remove visit to", d$place_name[i], "on", d$visited_on[i]),
        onclick = paste0("Shiny.setInputValue('remove_visit',", jsonlite::toJSON(d$visit_id[i], auto_unbox = TRUE), ",{priority:'event'})"), "Remove")), character(1)), check.names = FALSE)
  }, rownames = FALSE, width = "100%", sanitize.text.function = identity)
  output$history_navigation <- renderUI({
    if (!nrow(state()$visits)) return(NULL)
    page <- current_history_page(); pages <- history_pages()
    nav <- function(label, target, disabled) tags$button(type = "button", class = "btn btn-sm", disabled = if (disabled) "disabled" else NULL,
      onclick = paste0("Shiny.setInputValue('history_page',", target, ",{priority:'event'})"), label)
    div(class = "history-navigation", nav("Previous", page - 1L, page == 1L),
      span(role = "status", paste("Page", page, "of", pages, "·", nrow(state()$visits), "check-ins")), nav("Next", page + 1L, page == pages))
  })
  observeEvent(input$remove_visit, safely({
    pending_remove(NULL)
    can_edit(); id <- input$remove_visit
    if (!ml_is_scalar_string(id) || !id %in% state()$visits$visit_id) stop("This check-in is no longer available.")
    pending_remove(id)
    row <- shared_visits()[match(id, shared_visits()$visit_id), , drop = FALSE]
    showModal(modalDialog(title = "Remove this check-in?", p(strong(row$place_name)), p(paste(row$suburb_name, "·", row$visited_on)),
      p("Only this visit will be removed. Your area rating stays unchanged. You can undo this during the current session."),
      footer = tagList(actionButton("cancel_remove", "Keep visit"), actionButton("confirm_remove", "Remove visit", class = "btn-danger"))))
  }))
  observeEvent(input$cancel_remove, {pending_remove(NULL); removeModal()})
  observeEvent(input$confirm_remove, safely({
    req(!is.null(pending_remove())); id <- pending_remove(); pending_remove(NULL)
    can_edit(); updated <- ml_remove_visit(state(), id, data)
    removeModal(); commit(updated, "Check-in removed. Your area rating is unchanged.")
  }))
  output$insights <- renderUI({
    s <- shared_stats(); if (!s$checkins) return(ml_empty("Your own exploration patterns will appear after your first check-in. No sample visits are added to your journal."))
    d <- shared_progress(); d <- d[d$visit_count > 0, , drop = FALSE]; d <- d[order(d$familiarity, -d$visit_count), , drop = FALSE]
    needs_review <- d$familiarity[1] == 0L
    tagList(h3(paste0(round(s$coverage_pct), "% of the study areas visited")),
      p(paste("Your journal covers", s$visited_suburbs, "of", s$total_suburbs, "areas and", s$visited_places, if (s$visited_places == 1L) "distinct place." else "distinct places.")),
      h3(if (needs_review) "Review your area experience" else "An area to explore further"),
      p(if (needs_review) paste("You have a check-in in", d$suburb_name[1], "but its experience rating is still Lv 0 (Never been). Review the rating to reflect your experience; check-ins do not change it automatically.")
        else paste(d$suburb_name[1], "has one of your lowest experience ratings among visited areas. You can explore more there or review your current rating.")),
      tags$button(type = "button", class = "btn btn-sm insight-action",
        onclick = paste0("Shiny.setInputValue('review_area',", jsonlite::toJSON(d$suburb_id[1], auto_unbox = TRUE), ",{priority:'event'})"), "Review area level"),
      p(class = "hint", "Based on your recorded visits and ratings across these study areas."))
  })
  observeEvent(input$review_area, {
    id <- input$review_area
    if (!ml_is_scalar_string(id) || !id %in% data$suburbs$suburb_id) return()
    area_dialog(id)
  })
  category_counts <- reactive({v <- data$places[data$places$place_id %in% state()$visits$place_id, , drop = FALSE]; table(factor(v$place_category, levels = unique(data$places$place_category)))})
  output$category_chart <- renderUI({
    counts <- category_counts()
    counts <- counts[c(intersect(names(ml_category_colors), names(counts)), setdiff(names(counts), names(ml_category_colors)))]
    maximum <- max(1L, counts)
    if (!sum(counts)) return(div(class = "category-empty",
      p("Your category chart starts with your first place check-in. Area-only visits count towards your area goal."),
      actionButton("open_explore", "Explore places →", class = "btn-sm")))
    tagList(
      div(class = "category-scale", span("Distinct places checked in"), span(paste("Bar scale: 0 to", maximum))),
      lapply(seq_along(counts), function(i) tags$button(type = "button", class = "category-row",
        `aria-label` = paste(names(counts)[i], counts[i], "distinct places visited. Explore this category on the map."),
        onclick = paste0("Shiny.setInputValue('explore_category',", jsonlite::toJSON(names(counts)[i], auto_unbox = TRUE), ",{priority:'event'})"),
        span(class = "category-name", span(class = "dot", `aria-hidden` = "true", style = paste0("background:", ml_color(names(counts)[i]))), names(counts)[i]),
        span(class = "category-track", `aria-hidden` = "true", span(class = "category-fill", style = paste0("width:", 100 * counts[i] / maximum, "%;background:", ml_color(names(counts)[i]), ";border-right:", if (counts[i] > 0) "2px solid #fff1e8" else "0"))),
        span(class = "category-value", counts[i]))))
  })
  observeEvent(input$explore_category, {
    category <- input$explore_category
    if (!ml_is_scalar_string(category) || !category %in% data$places$place_category) return()
    focus(NULL)
    updateCheckboxGroupInput(session, "categories", selected = category)
    updateCheckboxInput(session, "include_other_landmarks", value = TRUE)
    updateCheckboxInput(session, "only_selected_area", value = FALSE)
    updateCheckboxInput(session, "unvisited_only", value = FALSE)
    updateTextInput(session, "place_search", value = "")
    updateTabsetPanel(session, "main_tab", selected = "explore")
    p <- data$places[data$places$place_category == category, , drop = FALSE]
    leafletProxy("map", session) |> fitBounds(min(p$lon) - .001, min(p$lat) - .001, max(p$lon) + .001, max(p$lat) + .001)
    session$sendCustomMessage("ml-show-map", list())
  })

  # Static city analyses do not depend on a visitor's private journal.
  discovery <- ml_discovery_data(data)
  discover_category <- reactive({x <- input$discover_category; if (ml_is_scalar_string(x) && x %in% data$places$place_category) x else "Landmark"})
  discover_amenity <- reactive({x <- input$discover_amenity; if (ml_is_scalar_string(x) && x %in% c("Food", "Toilet", "Park")) x else "Toilet"})
  output$area_comparison <- renderUI(ml_discovery_area_view(discovery, discover_category(), isTRUE(input$discover_all_landmarks)))
  output$amenity_insight <- renderUI(ml_discovery_amenity_insight(discovery, discover_amenity(), isTRUE(input$discover_all_landmarks)))
  output$amenity_examples <- renderUI(ml_discovery_amenity_examples(discovery, discover_amenity(), isTRUE(input$discover_all_landmarks)))
  output$tram_insight <- renderUI(ml_discovery_tram_insight(discovery, isTRUE(input$discover_all_landmarks)))
  output$tram_examples <- renderUI(ml_discovery_tram_examples(discovery, isTRUE(input$discover_all_landmarks)))
  reset_filters <- function(category = intersect("Landmark", unique(data$places$place_category)), all_landmarks = FALSE) {
    focus(NULL)
    updateCheckboxGroupInput(session, "categories", selected = category)
    updateCheckboxInput(session, "include_other_landmarks", value = all_landmarks)
    updateCheckboxInput(session, "only_selected_area", value = FALSE)
    updateCheckboxInput(session, "unvisited_only", value = FALSE)
    updateTextInput(session, "place_search", value = "")
  }
  show_explore <- function() {updateTabsetPanel(session, "main_tab", selected = "explore"); session$sendCustomMessage("ml-show-map", list())}
  observeEvent(input$clear_filters, reset_filters())
  observeEvent(input$open_explore, show_explore())
  browse_area <- function(id, category = NULL, all_landmarks = FALSE) {
    if (!ml_is_scalar_string(id) || !id %in% data$suburbs$suburb_id) return()
    active_area(id); active_place(""); updateSelectizeInput(session, "suburb", selected = id)
    if (!is.null(category)) reset_filters(category, all_landmarks)
    updateCheckboxInput(session, "only_selected_area", value = TRUE)
    extent <- sf::st_bbox(data$boundaries[data$boundaries$suburb_id == id, ])
    leafletProxy("map", session) |> fitBounds(extent[["xmin"]], extent[["ymin"]], extent[["xmax"]], extent[["ymax"]])
    show_explore()
  }
  observeEvent(input$discover_area, {
    x <- input$discover_area
    if (!is.list(x) || !ml_is_scalar_string(x$category) || !x$category %in% data$places$place_category) return()
    browse_area(x$area, x$category, isTRUE(x$all))
  }, priority = 15)
  observeEvent(input$browse_area, browse_area(input$browse_area, unique(data$places$place_category), TRUE), priority = 15)
  observeEvent(input$discover_place, {
    id <- input$discover_place
    if (!ml_is_scalar_string(id) || !id %in% data$places$place_id) return()
    selected <- data$places[match(id, data$places$place_id), ]
    reset_filters(selected$place_category, TRUE); choose_place(id); show_explore()
  }, priority = 15)
  observeEvent(input$search_pick, choose_place(input$search_pick), priority = 15)
  result_button <- function(place, event, detail = NULL) tags$button(type = "button", class = "compact-result",
    onclick = paste0("Shiny.setInputValue('", event, "',", jsonlite::toJSON(place$place_id, auto_unbox = TRUE), ",{priority:'event'})"),
    span(strong(place$place_name), tags$small(paste(place$place_category, "·", place$suburb_name))), if (!is.null(detail)) span(detail))
  output$search_results <- renderUI({
    term <- input$place_search
    if (!ml_is_scalar_string(term) || !nzchar(trimws(term))) return(NULL)
    p <- filtered_places(); p <- p[order(p$place_name, p$place_id), , drop = FALSE]
    if (!nrow(p)) return(div(class = "search-results", role = "status", "No matches. Change the search, categories or area filter; Reset filters clears them all."))
    tagList(p(class = "micro-note", role = "status", paste(nrow(p), "matches · first", min(5L, nrow(p)), "shown")),
      lapply(seq_len(min(5L, nrow(p))), function(i) result_button(p[i, , drop = FALSE], "search_pick")))
  })
  output$nearby_preview <- renderUI({
    origin <- selected_place(); if (!nrow(origin)) return(NULL)
    tagList(p(class = "micro-note", "Closest supplied record in each category · within 500 m"),
      div(class = "nearby-preview-grid", lapply(c("Food", "Toilet", "Park"), function(category) {
        if (!category %in% data$places$place_category) return(p(class = "micro-note", paste(category, "· no records supplied")))
        n <- ml_nearby(data$places, origin$place_id, category, radius_km = .5, limit = 1L)
        if (!nrow(n)) return(p(class = "micro-note", paste(category, "· none mapped within 500 m")))
        result_button(n, "nearby_pick", if (n$distance_m < 1) "Shared point" else paste(round(n$distance_m), "m"))
      })))
  })
  output$progress_overview <- renderUI({
    s <- shared_stats(); score <- sum(state()$progress$familiarity)
    tagList(div(class = "progress-overview", div(strong(paste0(s$visited_suburbs, "/", s$total_suburbs)), span("areas visited")),
      div(strong(s$visited_places), span("distinct places checked in")), div(strong(paste0(score, "/", 5L * s$total_suburbs)), span("experience score")),
      div(strong(sum(ml_earned(achievements()))), span("achievements earned"))),
      p(class = "micro-note", "Experience score adds your area levels (0–5). Levels are your own ratings."),
      if (s$checkins && any(shared_progress()$visit_count > 0 & shared_progress()$familiarity == 0))
        p(class = "small-note", "Some visited areas still have Lv 0. Review Area experience below when you want to update your rating."))
  })
  open_sources <- function() showModal(modalDialog(title = "Data & help", ml_data_help(data), size = "l", easyClose = TRUE, footer = modalButton("Close")))
  observeEvent(input$open_sources, open_sources())
  observeEvent(input$footer_sources, open_sources())
  observeEvent(input$open_backup, showModal(modalDialog(title = "Backup & restore", ml_backup_ui(), size = "l", easyClose = TRUE, footer = modalButton("Close"))))
  tableau_available <- tryCatch(nzchar(ml_tableau_url(Sys.getenv("MELBOURNE_TABLEAU_URL", ""))), error = function(e) FALSE)
  output$has_time_data <- function(...) !is.null(counts) || tableau_available
  outputOptions(output, "has_time_data", suspendWhenHidden = FALSE)
  output$time_availability <- renderUI({
    if (!is.null(counts) || tableau_available) return(NULL)
    div(class = "panel-card time-empty", h3("Time comparisons are not connected yet"),
      p("This version includes counting locations, but no hourly measurements or published time dashboard. A location alone cannot tell you when it is quieter."),
      if (nzchar(counts_problem)) p("The supplied measurement file needs correction before it can be shown."),
      if (nzchar(Sys.getenv("MELBOURNE_TABLEAU_URL", ""))) p("The dashboard address needs correction before it can be shown."),
      p("You can still compare mapped places and nearby amenities in Discover, or return to the map."), actionLink("time_to_discover", "Explore city patterns →"))
  })
  observeEvent(input$time_to_discover, updateTabsetPanel(session, "main_tab", selected = "discover"))

  # A -> B -> C: the source field is location_id, never an assumed sensor_id.
  observeEvent(input$go_when, {
    sensor <- selected_sensor()
    if (!nrow(sensor)) {showNotification("No counting locations are supplied for this dataset."); return()}
    updateSelectInput(session, "time_sensor", selected = sensor$location_id)
    updateTabsetPanel(session, "main_tab", selected = "when")
  })
  observeEvent(list(selected_sensor(), input$tableau_link), {
    sensor <- selected_sensor()
    session$sendCustomMessage("ml-sensor-lock", list(locked = isTRUE(input$tableau_link) && nrow(sensor) > 0L))
    if (isTRUE(input$tableau_link) && nrow(sensor)) updateSelectizeInput(session, "time_sensor", selected = sensor$location_id)
  })
  sensor_context <- reactive({
    near <- selected_sensor()
    if (isTRUE(input$tableau_link) && nrow(near)) return(near$location_id[1])
    if (ml_is_scalar_string(input$time_sensor) && input$time_sensor %in% data$sensors$location_id) return(input$time_sensor)
    ""
  })
  output$tableau_context <- renderUI({
    id <- sensor_context(); sensor <- data$sensors[data$sensors$location_id == id, , drop = FALSE]
    near <- selected_sensor()
    tagList(p(strong("Counting location: "), if (nrow(sensor)) sensor$name[1] else if (nrow(data$sensors)) "Choose a location above" else "None available"),
      if (isTRUE(input$tableau_link) && nrow(near)) tagList(
        p(class = "hint", paste("Nearest supplied location to", if (nrow(selected_place())) selected_place()$place_name else paste(data$suburbs$suburb_name[match(active_area(), data$suburbs$suburb_id)], "representative map point (not an area average)"), "·", format(round(near$distance_m), big.mark = ","), "m in a straight line.")),
        # 1 km is an explicit interface caution threshold, not a coverage claim.
        if (near$distance_m > 1000) div(class = "coverage-note", strong("Limited coverage for this destination"),
          p("The closest counting location is more than 1 km away. Its activity may differ from your destination. Compare it as a separate location, or turn off the link to choose another."))
        else p(class = "hint", "Counts describe this counting location, not crowd density inside the selected place.")))
  })
  output$tableau_view <- renderUI({
    raw <- Sys.getenv("MELBOURNE_TABLEAU_URL", "")
    if (!nzchar(raw)) return(ml_empty(strong("Visit-time comparison is not available yet"),
      p("The historical activity dashboard has not been connected. You can browse counting locations, but there is no weekday or hourly comparison to show yet. Explore remains available for finding places and recording visits.")))
    url <- tryCatch(ml_tableau_url(raw), error = function(e) NULL)
    if (is.null(url)) return(ml_empty("The activity dashboard is unavailable because its connection is not configured correctly. You can continue using the map and journal."))
    field <- Sys.getenv("MELBOURNE_TABLEAU_LOCATION_FIELD", "location_id")
    id <- sensor_context()
    if (!nzchar(id)) return(ml_empty("Choose a counting location above to view its historical activity."))
    filter <- if (nzchar(id)) htmltools::tag("viz-filter", list(field = field, value = id)) else NULL
    tagList(tags$script(type = "module", src = "https://public.tableau.com/javascripts/api/tableau.embedding.3.latest.min.js"),
      htmltools::tag("tableau-viz", list(id = "team-tableau", src = url, width = "100%", height = "620", toolbar = "bottom", filter)),
      p(class = "hint", "Tableau uses the team's published snapshot. Local journal edits are not uploaded automatically. ", tags$a(href = url, target = "_blank", rel = "noopener noreferrer", "Open the published view")))
  })
  active_counts <- reactive({if (is.null(counts)) return(NULL); counts[counts$location_id == sensor_context(), , drop = FALSE]})
  output$crowd_summary <- renderUI({
    d <- active_counts()
    if (nzchar(counts_problem)) return(ml_empty("The supplied pedestrian aggregates need correction before comparison. The Explore map and journal remain available."))
    if (is.null(d)) return(p(class = "hint", "Hourly pedestrian measurements are not available in this version. No busy or quiet times can be inferred from the location map alone."))
    if (!nrow(d)) return(ml_empty("No observations are available for this selected pedestrian location."))
    tagList(p(class = "hint", paste("Historical mean pedestrian counts are available for", nrow(d), "weekday/hour combinations at this location. The table shows the five lowest available averages. Missing combinations are not treated as zero. Consult the team's source period and sampling notes before interpreting them.")),
      if (outside_count_locations) p(class = "small-note", paste(outside_count_locations, "other counting locations in the measurements file are outside this map and are not included here.")))
  })
  output$crowd_table <- renderTable({d <- active_counts(); if (is.null(d) || !nrow(d)) return(NULL)
    d <- head(d[order(d$mean_count), , drop = FALSE], 5)
    data.frame(Weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")[d$weekday],
      Hour = sprintf("%02d:00", d$hour), `Historical mean count` = round(d$mean_count, 1), check.names = FALSE)}, rownames = FALSE, width = "100%")
}

if (!isTRUE(getOption("melbourne.helpers_only", FALSE))) {
  ml_data <- ml_load_team_data(ML_PROJECT_DIR)
  ml_counts_problem <- ""
  ml_counts <- tryCatch(ml_load_counts(ML_PROJECT_DIR), error = function(e) {ml_counts_problem <<- conditionMessage(e); warning(conditionMessage(e), call. = FALSE); NULL})
  shinyApp(ui = ml_ui(ml_data), server = ml_server(ml_data, ml_counts, ml_counts_problem))
}
