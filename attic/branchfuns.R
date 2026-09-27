

discover_active_branch = function(unbranch_pipeop) {
        # take a part of the graph$edges and find the corresponding PipeOpBranch (if any) and its output channel that corresponds to it.
        # I.e. if we have a PipeOp 'A' with input channel 'x', there may be edges that connect here, the 'edges' could then look like
        #    src_id dst_id src_channel dst_channel
        #    B      A      output1     x
        #    C      A      output1     x
        #    C      A      output2     x
        # Suppose that B is connected to the 'alpha' output of a PipeOpBranch with ID 'greek',
        # and C is connected to the 'aleph' output of a PipeOpBranch with ID 'hebrew'.
        # Then the function would return a data.table(id = c('greek', 'hebrew'), channel = c('alpha', 'aleph')).
        # This function iterates through Non-Branch-PipeOPs with single input and output, and uses recursion if it encounters
        # PipeOps with multiple inputs.
        # Note that the function
        # (1) does not PipeOps that are exclusively ancestors of PipeOpBranch PipeOps, and
        # (2) returns as many rows as there are possible paths to any PipeOpBranch, so the return value is not one-to-one with input rows.
        # If a channel is connected to the Graph input without an intermediate PipeOpBranch, the return data.table will contain a row with NAs.
        detect_corresponding_branch_output = function(edges) {
          graphinput = data.table(id = NA_character, channel = NA_character)
          pipeops_visited = new.env(parent = emptyenv())
          detect_corresponding_branch_output_inner = function(edges) {
            if (nrow(edges) == 0) return(list(graphinput))
            result = list()
            for (edge_i in seq_along(edge$src_id)) {
              inpipeop_id = edge$src_id[[edge_i]]
              repeat {
                if (get0(inpipeop_id, pipeops_visited, ifnotfound = FALSE)) break
                assign(inpipeop_id, TRUE, pipeops_visited)
                inpipeop = gm$pipeops[[inpipeop_id]]
                if (inherits(inpipeop, "PipeOpBranch")) {
                  # we found a PipeOpBranch: return the output channel that corresponds to the edge currently being processed
                  result[[length(result) + 1]] = list(edge[edge_i, .(id = src_id, channel = src_channel)])
                  break
                }
                # not a PipeOpBranch: look at all edges the go into the PipeOp currently being processed
                edges_prior = gm$edges[dst_id == inpipeop_id, ]
                if (nrow(edges_prior) != 1) {
                  # unless we have exactly one predecessor, we recurse:
                  # this handles the empty case (connected to graph input) as well as the pipeop-with-multiple-inputs case
                  result[[length(result) + 1]] = detect_corresponding_branch_output_inner(edges_prior)
                  break
                }
                inpipeop_id = edges_prior$src_id[[1]]
              }
            }
            unlist(result, recursive = FALSE, use.names = FALSE)
          }
          unique(rbindlist(detect_corresponding_branch_output_inner(edges)))
        }
        inbranches = gm$pipeops[[unbranch_pipeop]]$input$name
        inroutes = map_dtr(inbranches, function(inchannel) {
          edges = gm$edges[dst_id == unbranch_pipeop & dst_channel == inchannel, ]
          detect_corresponding_branch_output(edges)[, unbranchchannel := inchannel]
        })
        # inroutes:
        # data.table with columns 'id', 'channel', and 'unbranchchannel'.
        # For each input-channel 'unbranchchannel' of the pipeop under investigation, it lists the
        # id(s) and output channel(s) of PipeOpBranches that connect there.

        # add column 'live': is the current route selected?
        inroutes[, live := ifelse(is.na(channel), TRUE, channel == get_pobranch_active_output(id)), by = "id"]
        inroutes_livestats = inroutes[, .(any_live = any(live), all_live = all(live)), by = "unbranchchannel"]
        if (any(inroutes_livestats$any_live != inroutes_livestats$all_live)) {
          first_inconsistency = inroutes_livestats[which(any_live != all_live)[1L], unbranchchannel]
          stopf("Inconsistent selection of PipeOpBranch outputs:\nPipeOp outputs %s are not selected, but conflict with %s",
            paste(inroutes[!live & unbranchchannel == first_inconsistency, unique(sprintf("'%s.%s'", id, channel))], collapse = ", "),
            inroutes[live & unbranchchannel == first_inconsistency, if (any(is.na(id))) "direct Graph input, which is always selected." else sprintf("selected output '%s.%s'.", id[[1]], channel[[1]])]
          )
        }
        inroutes = inroutes[inroutes[any_live == TRUE, unbranchchannel], on = "unbranchchannel"]
        if (length(inroutes) == 1) {
          return(inroutes$unbranchchannel)
        }










      detect_corresponding_branch_state = function(edges) {
        graphinput = data.table(id = NA_character_, channel = NA_character_, live = TRUE, last_pobranch = NA_character_)
        pipeops_visited = new.env(parent = emptyenv())
        pobranch_active = new.env(parent = emptyenv())
        assert_consistent_selection = function(edgeinfo) {
          if (all(edgeinfo$live) == any(edgeinfo$live)) return(invisible(edgeinfo))
          stopf("Inconsistent selection of PipeOpBranch outputs:\nPipeOp outputs %s are not selected, but conflict with %s",
            edgeinfo[!live, unique(last_pobranch)],
            inroutes[live, if (any(is.na(last_pobranch))) "direct Graph input, which is always selected." else sprintf("selected output '%s'.", last_pobranch[[1]])]
          )
        }
        detect_corresponding_branch_state_inner = function(edges) {
          if (nrow(edges) == 0) return(list(graphinput))
          result = list()
          for (edge_i in seq_along(edge$src_id)) {
            inpipeop_id = edge$src_id[[edge_i]]
            repeat {
              if (get0(inpipeop_id, pipeops_visited, ifnotfound = FALSE)) break
           #   assign(inpipeop_id, TRUE, pipeops_visited)
              inpipeop_active_output = get0(inpipeop_id, pobranch_active, ifnotfound = NA_character_)
              if (is.na(inpipeop_active_output)) {
                inpipeop = gm$pipeops[[inpipeop_id]]
                if (inherits(inpipeop, "PipeOpBranch")) {
                  inpipeop_active_output = get_pobranch_active_output(inpipeop)
                  assign(inpipeop_id, inpipeop_active_output, pobranch_active)
                }
              }
              if (!is.na(inpipeop_active_output)) {
                if (inpipeop_active_output == edge$src_channel[[edge_i]]) {
                  prior_state = detect_corresponding_branch_state_inner(gm$edges[dst_id == inpipeop_id, ])
                  assert_consistent_selection(prior_state)
                  current_result = list(edge[edge_i, .(id = src_id, channel = src_channel, live = any(prior_state$live), last_pobranch = sprintf("%s.%s", inpipeop_id, inpipeop_active_output))])
                } else {
                  current_result = list(edge[edge_i, .(id = src_id, channel = src_channel, live = FALSE, last_pobranch = inpipeop_active_output)])
                }

              }
              {
                # we found a PipeOpBranch: return the output channel that corresponds to the edge currently being processed
                result[[length(result) + 1]] = list(edge[, .(id = src_id, channel = src_channel)])
                break
              }
              # not a PipeOpBranch: look at all edges the go into the PipeOp currently being processed
              edges_prior = gm$edges[dst_id == inpipeop_id, ]
              if (nrow(edges_prior) != 1) {
                # unless we have exactly one predecessor, we recurse:
                # this handles the empty case (connected to graph input) as well as the pipeop-with-multiple-inputs case
                result[[length(result) + 1]] = detect_corresponding_branch_output_inner(edges_prior)
                break
              }
              inpipeop_id = edges_prior$src_id[[1]]
            }
          }
          unlist(result, recursive = FALSE, use.names = FALSE)
        }
        unique(rbindlist(detect_corresponding_branch_output_inner(edges)))
      }
