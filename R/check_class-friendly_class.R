#' Generate a human-readable description of an object's class
#'
#' @param object An object whose class will be described
#'
#' @return A [character] string of length 1,
#'   based on the [class] and [length] of `object`.
#' @importFrom glue glue
#' @export
setGeneric("friendly_class", function(object) {
	standardGeneric("friendly_class")
})

#' @rdname friendly_class
setMethod("friendly_class", signature("ANY"), function(object) {
	class <- class(object)
	class_str <- knitr::combine_words(md_code(class))
	glue(
		ngettext(
			length(class),
			"an object with class {class_str}",
			"an object with classes {class_str}"
		)
	)
})

#' @rdname friendly_class
setMethod("friendly_class", signature("character"), function(object) {
	if (!setequal(class(object), "character")) return(callNextMethod())
	if (length(object) == 1) return("a text string (class `character`)")
	"a vector of text (class `character`)"
})

#' @rdname friendly_class
setMethod("friendly_class", signature("numeric"), function(object) {
	if (!setequal(class(object), "numeric")) return(callNextMethod())
	if (length(object) == 1) return("a number (class `numeric`)")
	"a vector of numbers (class `numeric`)"
})

#' @rdname friendly_class
setMethod("friendly_class", signature("integer"), function(object) {
	if (!setequal(class(object), "integer")) return(callNextMethod())
	if (length(object) == 1) return("an integer (class `integer`)")
	"a vector of integers (class `integer`)"
})

#' @rdname friendly_class
setMethod("friendly_class", signature("logical"), function(object) {
	if (!setequal(class(object), "logical")) return(callNextMethod())
	logical <- if (any(is.na(object))) "`TRUE`/`FALSE`/`NA`" else "`TRUE`/`FALSE`"
	if (length(object) == 1) return(glue("a {logical} value (class `logical`)"))
	glue("a vector of {logical} values (class `logical`)")
})

#' @rdname friendly_class
setMethod("friendly_class", signature("complex"), function(object) {
	if (!setequal(class(object), "complex")) return(callNextMethod())
	if (length(object) == 1) return("a complex number (class `complex`)")
	"a vector of complex numbers (class `complex`)"
})

#' @rdname friendly_class
setMethod("friendly_class", signature("raw"), function(object) {
	if (!setequal(class(object), "raw")) return(callNextMethod())
	if (length(object) == 1) return("a raw byte value (class `raw`)")
	"a vector of raw byte values (class `raw`)"
})

#' @rdname friendly_class
setMethod("friendly_class", signature("factor"), function(object) {
	if (!setequal(class(object), "factor")) return(callNextMethod())
	if (length(object) == 1) return("a factor (class `factor`)")
	"a vector of factors (class `factor`)"
})

#' @rdname friendly_class
setMethod("friendly_class", signature("Date"), function(object) {
	if (!setequal(class(object), "Date")) return(callNextMethod())
	if (length(object) == 1) return("a date (class `Date`)")
	"a vector of dates (class `Date`)"
})

#' @rdname friendly_class
setMethod("friendly_class", signature("POSIXt"), function(object) {
	class <- setdiff(class(object), "POSIXt")
	if (!identical(class, "POSIXct") && !identical(class, "POSIXlt")) {
		return(callNextMethod())
	}
	if (length(object) == 1) return(glue("a date-time (class `{class}`)"))
	glue("a vector of date-times (class `{class}`)")
})

#' @rdname friendly_class
#' @importClassesFrom lubridate Period
setMethod("friendly_class", signature("Period"), function(object) {
	if (!setequal(class(object), "Period")) return(callNextMethod())
	if (length(object) == 1) return("a time period (class `Period`)")
	"a vector of time periods (class `Period`)"
})

#' @rdname friendly_class
setMethod("friendly_class", signature("data.frame"), function(object) {
	if (!setequal(class(object), "data.frame")) return(callNextMethod())
	"a data frame (class `data.frame`)"
})

setOldClass(c("tbl_df", "tbl", "data.frame"))
#' @rdname friendly_class
setMethod("friendly_class", signature("tbl_df"), function(object) {
	if (!setequal(class(object), c("tbl_df", "tbl", "data.frame"))) {
		return(callNextMethod())
	}
	"a tibble (class `tbl_df`)"
})

setOldClass(c("grouped_df", "tbl_df", "tbl", "data.frame"))
#' @rdname friendly_class
setMethod("friendly_class", signature("grouped_df"), function(object) {
	if (!setequal(class(object), c("grouped_df", "tbl_df", "tbl", "data.frame"))) {
		return(callNextMethod())
	}
	"a grouped tibble (class `grouped_df`)"
})

setOldClass(c("rowwise_df", "tbl_df", "tbl", "data.frame"))
#' @rdname friendly_class
setMethod("friendly_class", signature("rowwise_df"), function(object) {
	if (!setequal(class(object), c("rowwise_df", "tbl_df", "tbl", "data.frame"))) {
		return(callNextMethod())
	}
	"a rowwise tibble (class `rowwise_df`)"
})

setOldClass(c("py_tbl_df", "tbl_df", "tbl", "data.frame"))
#' @rdname friendly_class
setMethod("friendly_class", signature("py_tbl_df"), function(object) {
	if (!setequal(class(object), c("py_tbl_df", "tbl_df", "tbl", "data.frame"))) {
		return(callNextMethod())
	}
	"a DataFrame"
})

setOldClass(c("py_grouped_df", "py_tbl_df", "grouped_df", "tbl_df", "tbl", "data.frame"))
#' @rdname friendly_class
setMethod("friendly_class", signature("py_grouped_df"), function(object) {
	if (!setequal(class(object), c("py_grouped_df", "py_tbl_df", "grouped_df", "tbl_df", "tbl", "data.frame"))) {
		return(callNextMethod())
	}
	"a DataFrame with row labels (i.e. index)"
})

#' @rdname friendly_class
setMethod("friendly_class", signature("list"), function(object) {
	if (!setequal(class(object), "list")) return(callNextMethod())
	"a list (class `list`)"
})

#' @rdname friendly_class
setMethod("friendly_class", signature("matrix"), function(object) {
	class(object) <- setdiff(class(object), "array")
	if (!setequal(class(object), "matrix")) return(callNextMethod())
	"a matrix (class `matrix`)"
})

#' @rdname friendly_class
setMethod("friendly_class", signature("array"), function(object) {
	if (!setequal(class(object), "array")) return(callNextMethod())
	"an array (class `array`)"
})
