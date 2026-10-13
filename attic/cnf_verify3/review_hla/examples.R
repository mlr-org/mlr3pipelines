# Minimal executable production examples. Base R alone suffices.
# Run: Rscript attic/cnf_verify3/review_hla/examples.R
source("R/CnfFormula_simplify.R")

universe = list(y = c("0", "1", "2"), x = c("0", "1", "2", "3"))
input = list(
  list(y = "0", x = c("0", "1")),
  list(y = "1", x = c("0", "2")),
  list(y = "2", x = c("0", "3"))
)
result = simplify_cnf(input, universe)
stopifnot(length(result) == 2L)
units = result[lengths(result) == 1L]
nonunits = result[lengths(result) > 1L]
stopifnot(length(units) == 1L, identical(units[[1L]], list(x = "0")))
stopifnot(identical(nonunits[[1L]]$x, "0"))
# The negation of the nonunit demands x != 0, immediately contradicting the unit.
# A second pass therefore deletes that nonunit.
second = simplify_cnf(c(result), universe)
stopifnot(length(second) == 1L, identical(second[[1L]], list(x = "0")))
cat("Unit-equality gap: production keeps x=0 and a nonunit containing x=0.\n")
dput(c(result))

# Reordering literal symbols changes the event schedule for the same formula.
swapped = lapply(input, function(clause) clause[c("x", "y")])
swapped_result = simplify_cnf(swapped, universe)
stopifnot(length(swapped_result) == 1L, identical(swapped_result[[1L]], list(x = "0")))
cat("Changing literal order removes the residual clause in this example.\n")

# Both A and B are initially domain-refutable from the other three clauses.
# Removing A makes B necessary; one sweep correctly leaves a saturated result.
boolean = list(x = c("0", "1"), y = c("0", "1"), z = c("0", "1"))
cyclic = list(
  list(x = "1", y = "1"),
  list(x = "1", z = "1"),
  list(y = "0", z = "1"),
  list(y = "1", z = "0")
)
cyclic_result = simplify_cnf(cyclic, boolean)
stopifnot(length(cyclic_result) == 3L)
stopifnot(identical(cyclic_result[[1L]], list(x = "1", z = "1")))
cat("Deletion-order example: exactly one of A and B is removed.\n")
dput(c(cyclic_result))

# This unit is redundant by propagation through strictly contained donor ranges.
multi_universe = list(x = as.character(0:3), y = as.character(0:2), z = as.character(0:2))
multi = list(
  list(x = c("0", "1")),
  list(x = "0", y = c("0", "1")),
  list(y = c("1", "2"), z = "0"),
  list(y = c("0", "2"), z = "1"),
  list(x = "1", z = "2")
)
multi_result = simplify_cnf(multi, multi_universe)
stopifnot(length(multi_result) == 4L, all(lengths(multi_result) == 2L))
cat("Multivalued propagation chain: the redundant unit is removed.\n")
dput(c(multi_result))
