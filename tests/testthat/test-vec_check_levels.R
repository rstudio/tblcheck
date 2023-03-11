test_that("number of levels", {
	grade <- tblcheck_test_grade({
		.result <- as.factor(c("a", "b", "b"))
		.solution <- as.factor(c("a", "b", "c"))
		vec_grade_levels()
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "levels_n",
		expected = 3,
		actual = 2
	)
})

test_that("level labels", {
	grade <- tblcheck_test_grade({
		.result <- as.factor(c("a", "b", "c"))
		.solution <- as.factor(c("x", "y", "z"))
		vec_grade_levels()
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "levels",
		missing = c("x", "y", "z"),
		unexpected = c("a", "b", "c")
	)
})

test_that("level order", {
	grade_diffs <- tblcheck_test_grade({
		.result <- as.factor(c("a", "b", "c"))
		.solution <- factor(.result, levels = rev(levels(.result)))
		vec_grade_levels()
	})

	expect_snapshot(grade_diffs)
	expect_problem(grade_diffs$problem, type = "levels_reversed")

	grade_diffs <- tblcheck_test_grade({
		.result <- factor(1:3, c("a", "b", "c"))
		.solution <- factor(1:3, c("c", "a", "b"))
		vec_grade_levels()
	})

	expect_snapshot(grade_diffs)

	expect_problem(
		grade_diffs$problem,
		type = "levels_order",
		expected = c("c", "a", "b"),
		actual = c("a", "b", "c")
	)

	grade <- tblcheck_test_grade({
		.result <- as.factor(c("a", "b", "c", "d", "e"))
		.solution <- factor(.result, levels = c("a", "b", "c", "e", "d"))
		vec_grade_levels()
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "levels_order",
		expected = c("a", "b", "c", "e", "d"),
		actual = c("a", "b", "c", "d", "e")
	)
})
