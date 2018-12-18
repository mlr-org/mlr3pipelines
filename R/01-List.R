# FIXME: add a payload type and always check it?
ListNamedEls = R6Class("ListNamedEls",
  public = list(
    xs = list(),
    get_key = NULL,
    payload_type = NULL,

    initialize = function(xs = list(), payload_type, get_key) {
      assert_string(payload_type)
      assert_function(get_key)
      names(xs) = map_chr(xs, get_key)
      assert_list(xs, names = "unique", type = payload_type)
      self$payload_type = payload_type
      self$xs = xs
      self$get_key = get_key
    },

    add = function(x) {
      assert_class(x, self$payload_type)
      k = self$get_key(x)
      assert_true(BBmisc::`%nin%`(k, self$keys))
      self$xs[[k]] = x
    },

    remove = function(keys) {
      keep = setdiff(self$keys, keys)
      self$xs = self$xs[keep]
    },

    join = function(other) {
      assert_r6(other, "ListNamedEls")
      assert_true(length(intersect(self$keys, other$keys)) == 0L)
      self$xs = c(self$xs, other$xs)
    },

    join_new = function(other) {
      assert_r6(other, "ListNamedEls")
      new_keys = setdiff(other$keys, self$keys)
      self$xs = c(self$xs, other$xs[new_keys])
    },


    print = function(...) BBmisc::catf(self$print_str),

    #FIXME this is bad, but in need this now
    map = function(f) lapply(self$xs, f),
    map_s = function(f) sapply(self$xs, f)
    # FIXME: wie geht das hier dass man den dollar op callt?
    # mapm = function(m) lapply(self$xs, function(x) `$`(x, m))
  ),

  active = list(
    is_empty = function() length(self$xs) == 0L,
    keys = function() names(self$xs),
    print_str = function() ifelse(self$is_empty, "()", paste0("(", collapse(self$keys), ")"))
  )
)

length.ListNamedEls = function(x) length(x$xs)

`[[.ListNamedEls` = function(x, i, j, ...) {
  if (is.character(i)) {
    assert_true(i %in% x$keys)
    x$xs[[i]]
  } else if (is.numeric(i)) {
    # FIXME: hier sollten wir mal wieder auf is_scalar_int checken....
    x$xs[[i]]
  }
}

OpList = R6Class("OpList",
  inherit = ListNamedEls,

  public = list(

    initialize = function(xs = list()) {
      super$initialize(xs, "PipeOp", get_key = function(x) x$id)
    }
  )
)


ElFooBar = R6Class("ElFooBar",
  public = list(
    id = NULL,
    initialize = function(id) {
      self$id = id
    }
  )
)

# x1 = ElFooBar$new("foo")
# x2 = ElFooBar$new("bar")
# xs = ListNamedEls$new(list(x1, x2), "ElFooBar", function(x) x$id)
# xx1 = xs$map(function(x) x)
# xx2 = xs$map_s(function(x) x$id)
# xx2 = xs$mapm("id")




#' ListNamedEls for GraphNodes.
#'
#' @description
#' It is used mainly inside the GraphNode class
#' to store the next and previous nodes.
#'
#' It's not exported.
#'
#' @noRd
#'
GraphNodesList = R6Class("GraphNodesList",
   inherit = ListNamedEls,

   public = list(
     initialize = function(xs = list()) {
       super$initialize(xs, "GraphNode", get_key = function(x) x$pipeop$id)
     },
     set_next = function(nodes) {
       nodes = wrap_nodes(nodes)
       self$map(function(x) x$set_next(nodes))
       GraphNodesList$new(nodes)
     }
   )
)

