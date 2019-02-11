# # We basically have two scenarios:

# ensemble_selection = function(predictions, truth, ensemble_size) {
#   n_preds = length(predictions)
#   scores = rep(0L, n_preds)
#   for (i in seq_len(ensemble_size)) {

#   }
# }


#  def _fast(self, predictions, labels):
#         """Fast version of Rich Caruana's ensemble selection method."""
#         self.num_input_models_ = len(predictions)

#         ensemble = []
#         trajectory = []
#         order = []

#         ensemble_size = self.ensemble_size

#         if self.sorted_initialization:
#             n_best = 20
#             indices = self._sorted_initialization(predictions, labels, n_best)
#             for idx in indices:
#                 ensemble.append(predictions[idx])
#                 order.append(idx)
#                 ensemble_ = np.array(ensemble).mean(axis=0)
#                 ensemble_performance = calculate_score(
#                     labels, ensemble_, self.task_type, self.metric,
#                     ensemble_.shape[1])
#                 trajectory.append(ensemble_performance)
#             ensemble_size -= n_best

#         for i in range(ensemble_size):
#             scores = np.zeros((len(predictions)))
#             s = len(ensemble)
#             if s == 0:
#                 weighted_ensemble_prediction = np.zeros(predictions[0].shape)
#             else:
#                 # Memory-efficient averaging!
#                 ensemble_prediction = np.zeros(ensemble[0].shape)
#                 for pred in ensemble:
#                     ensemble_prediction += pred
#                 ensemble_prediction /= s

#                 weighted_ensemble_prediction = (s / float(s + 1)) * \
#                                                ensemble_prediction
#             fant_ensemble_prediction = np.zeros(weighted_ensemble_prediction.shape)
#             for j, pred in enumerate(predictions):
#                 # TODO: this could potentially be vectorized! - let's profile
#                 # the script first!
#                 fant_ensemble_prediction[:,:] = weighted_ensemble_prediction + \
#                                              (1. / float(s + 1)) * pred
#                 scores[j] = 1 - calculate_score(
#                     solution=labels,
#                     prediction=fant_ensemble_prediction,
#                     task_type=self.task_type,
#                     metric=self.metric,
#                     all_scoring_functions=False)

#             all_best = np.argwhere(scores == np.nanmin(scores)).flatten()
#             best = self.random_state.choice(all_best)
#             ensemble.append(predictions[best])
#             trajectory.append(scores[best])
#             order.append(best)

#             # Handle special case
#             if len(predictions) == 1:
#                 break

#         self.indices_ = order
#         self.trajectory_ = trajectory
#         self.train_score_ = trajectory[-1]

# # See issue #117
# # #' @include mlr_pipeops.R
# # mlr_pipeops$add("modelavg", PipeOpModelAvg)
