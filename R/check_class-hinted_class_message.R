hinted_class_message <- R7::new_generic(
	"hinted_class_message",
	c("object", "expected")
)

R7::method(
	hinted_class_message,
	list(R7::class_any, R7::class_any)
) <- function(object, expected) {
	NULL
}

R7::method(
	hinted_class_message,
	list(R7::new_S3_class("rowwise_df"), R7::new_S3_class("grouped_df"))
) <- function(object, expected) {
	paste(
		"Your table is a rowwise data frame,",
		"but I was expecting it to be grouped.",
		"Maybe you need to use `group_by()`?"
	)
}

R7::method(
	hinted_class_message,
	list(R7::new_S3_class("data.frame"), R7::new_S3_class("grouped_df"))
) <- function(object, expected) {
	paste(
		"Your table isn't a grouped data frame,",
		"but I was expecting it to be grouped.",
		"Maybe you need to use `group_by()`?"
	)
}

R7::method(
	hinted_class_message,
	list(R7::new_S3_class("grouped_df"), R7::new_S3_class("data.frame"))
) <- function(object, expected) {
	paste(
		"Your table is a grouped data frame,",
		"but I wasn't expecting it to be grouped.",
		"Maybe you need to use `ungroup()`?"
	)
}

R7::method(
	hinted_class_message,
	list(R7::new_S3_class("data.frame"), R7::new_S3_class("rowwise_df"))
) <- function(object, expected) {
	paste(
		"Your table isn't a rowwise data frame,",
		"but I was expecting it to be rowwise.",
		"Maybe you need to use `rowwise()`?"
	)
}

R7::method(
	hinted_class_message,
	list(R7::new_S3_class("rowwise_df"), R7::new_S3_class("data.frame"))
) <- function(object, expected) {
	paste(
		"Your table is a rowwise data frame,",
		"but I wasn't expecting it to be rowwise.",
		"Maybe you need to use `ungroup()`?"
	)
}

R7::method(
	hinted_class_message,
	list(R7::new_S3_class("py_tbl_df"), R7::new_S3_class("py_grouped_df"))
) <- function(object, expected) {
	paste(
		"I was only expecting 1 value for each grouping in the table," ,
		"but you have multiple values per grouping.",
		"Maybe you are missing a `.groupby()` call?"
	)
}

R7::method(
	hinted_class_message,
	list(R7::new_S3_class("py_grouped_df"), R7::new_S3_class("py_tbl_df"))
) <- function(object, expected) {
	paste(
		"Your table row labels (i.e. index) are not a numbered sequence.",
		"You can tell by the extra spacing around the column names.",
		"You can fix this with `.reset_index()`"
	)
}
