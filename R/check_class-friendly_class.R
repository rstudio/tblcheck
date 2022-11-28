friendly_class <- function(object) {
	UseMethod("friendly_class")
}

#' @export
friendly_class.default <- function(object) {
	class <- class(object)
	class_str <- knitr::combine_words(md_code(class))
	glue::glue(
		ngettext(
			length(class),
			"an object with class {class_str}",
			"an object with classes {class_str}"
		)
	)
}

#' @export
friendly_class.character <- function(object) {
	if (!setequal(class(object), "character")) return(NextMethod())
	if (length(object) == 1) return("a text string (class `character`)")
	"a vector of text (class `character`)"
}

#' @export
friendly_class.numeric <- function(object) {
	if (!setequal(class(object), "numeric")) return(NextMethod())
	if (length(object) == 1) return("a number (class `numeric`)")
	"a vector of numbers (class `numeric`)"
}

#' @export
friendly_class.integer <- function(object) {
	if (!setequal(class(object), "integer")) return(NextMethod())
	if (length(object) == 1) return("an integer (class `integer`)")
	"a vector of integers (class `integer`)"
}

#' @export
friendly_class.logical <- function(object) {
	if (!setequal(class(object), "logical")) return(NextMethod())
	if (length(object) == 1) return("a TRUE/FALSE value (class `logical`)")
	"a vector of TRUE/FALSE values (class `logical`)"
}

#' @export
friendly_class.complex <- function(object) {
	if (!setequal(class(object), "complex")) return(NextMethod())
	if (length(object) == 1) return("a complex number (class `complex`)")
	"a vector of complex numbers (class `complex`)"
}

#' @export
friendly_class.raw <- function(object) {
	if (!setequal(class(object), "raw")) return(NextMethod())
	if (length(object) == 1) return("a raw byte value (class `raw`)")
	"a vector of raw byte values (class `raw`)"
}

#' @export
friendly_class.factor <- function(object) {
	if (!setequal(class(object), "factor")) return(NextMethod())
	if (length(object) == 1) return("a factor (class `factor`)")
	"a vector of factors (class `factor`)"
}

#' @export
friendly_class.POSIXct <- function(object) {
	if (!setequal(class(object), c("POSIXct", "POSIXt"))) return(NextMethod())
	if (length(object) == 1) return("a date-time (class `POSIXct`)")
	"a vector of date-times (class `POSIXct`)"
}

#' @export
friendly_class.POSIXlt <- function(object) {
	if (!setequal(class(object), c("POSIXlt", "POSIXt"))) return(NextMethod())
	if (length(object) == 1) return("a date-time (class `POSIXlt`)",)
	"a vector of date-times (class `POSIXlt`)"
}

#' @export
friendly_class.tbl_df <- function(object) {
	if (!setequal(class(object), c("tbl_df", "tbl", "data.frame"))) {
		return(NextMethod())
	}
	"a tibble (class `tbl_df`)"
}

#' @export
friendly_class.grouped_df <- function(object) {
	if (!setequal(class(object), c("grouped_df", "tbl_df", "tbl", "data.frame"))) {
		return(NextMethod())
	}
	"a grouped tibble (class `grouped_df`)"
}

#' @export
friendly_class.rowwise_df <- function(object) {
	if (!setequal(class(object), c("rowwise_df", "tbl_df", "tbl", "data.frame"))) {
		return(NextMethod())
	}
	"a rowwise tibble (class `rowwise_df`)"
}

#' @export
friendly_class.data.frame <- function(object) {
	if (!setequal(class(object), "data.frame")) return(NextMethod())
	"a data frame (class `data.frame`)"
}

#' @export
friendly_class.py_tbl_df <- function(object) {
	if (!setequal(class(object), c("py_tbl_df", "tbl_df", "tbl", "data.frame"))) {
		return(NextMethod())
	}
	"a DataFrame"
}

#' @export
friendly_class.py_grouped_df <- function(object) {
	if (!setequal(class(object), c("py_grouped_df", "py_tbl_df", "grouped_df", "tbl_df", "tbl", "data.frame"))) {
		return(NextMethod())
	}
	"a DataFrame with row labels (i.e. index)"
}

#' @export
friendly_class.list <- function(object) {
	if (!setequal(class(object), "list")) return(NextMethod())
	"a list (class `list`)"
}

#' @export
friendly_class.matrix <- function(object) {
	if (
		!setequal(class(object), "matrix") &&
			!setequal(class(object), c("matrix", "array"))
	) {
		return(NextMethod())
	}
	"a matrix (class `matrix`)"
}

#' @export
friendly_class.array <- function(object) {
	if (!setequal(class(object), "array")) return(NextMethod())
	"an array (class `array`)"
}
