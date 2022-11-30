#' Generate a hint for how to convert one object type to another
#'
#' @param object An object to be compared to `expected`
#' @param expected An object of the expected class
#'
#' @return A [character] string of length 1
#' @export
setGeneric("hinted_class_message", function(object, expected) {
	standardGeneric("hinted_class_message")
})

#' @rdname hinted_class_message
setMethod("hinted_class_message", signature("ANY", "ANY"),
	function(object, expected) {
		NULL
	}
)

#' @rdname hinted_class_message
setMethod("hinted_class_message", signature("rowwise_df", "grouped_df"),
	function(object, expected) {
		paste(
			"Your table is a rowwise data frame,",
			"but I was expecting it to be grouped.",
			"Maybe you need to use `group_by()`?"
		)
	}
)

#' @rdname hinted_class_message
setMethod("hinted_class_message", signature("data.frame", "grouped_df"),
	function(object, expected) {
		paste(
			"Your table isn't a grouped data frame,",
			"but I was expecting it to be grouped.",
			"Maybe you need to use `group_by()`?"
		)
	}
)

#' @rdname hinted_class_message
setMethod("hinted_class_message", signature("grouped_df", "data.frame"),
	function(object, expected) {
		paste(
			"Your table is a grouped data frame,",
			"but I wasn't expecting it to be grouped.",
			"Maybe you need to use `ungroup()`?"
		)
	}
)

#' @rdname hinted_class_message
setMethod("hinted_class_message", signature("data.frame", "rowwise_df"),
	function(object, expected) {
		paste(
			"Your table isn't a rowwise data frame,",
			"but I was expecting it to be rowwise.",
			"Maybe you need to use `rowwise()`?"
		)
	}
)

#' @rdname hinted_class_message
setMethod("hinted_class_message", signature("rowwise_df", "data.frame"),
	function(object, expected) {
		paste(
			"Your table is a rowwise data frame,",
			"but I wasn't expecting it to be rowwise.",
			"Maybe you need to use `ungroup()`?"
		)
	}
)

setOldClass("py_tbl_df")
setOldClass("py_grouped_df")
#' @rdname hinted_class_message
setMethod("hinted_class_message", signature("py_tbl_df", "py_grouped_df"),
	function(object, expected) {
		paste(
			"I was only expecting 1 value for each grouping in the table," ,
			"but you have multiple values per grouping.",
			"Maybe you are missing a `.groupby()` call?"
		)
	}
)

#' @rdname hinted_class_message
setMethod("hinted_class_message", signature("py_grouped_df", "py_tbl_df"),
	function(object, expected) {
		paste(
			"Your table row labels (i.e. index) are not a numbered sequence.",
			"You can tell by the extra spacing around the column names.",
			"You can fix this with `.reset_index()`"
		)
	}
)
