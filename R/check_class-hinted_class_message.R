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

setOldClass(c("grouped_df", "tbl_df", "tbl", "data.frame"))
setOldClass(c("rowwise_df", "tbl_df", "tbl", "data.frame"))
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
