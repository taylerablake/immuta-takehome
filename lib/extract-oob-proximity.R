extract_oob_proximity = function(fit, olddata) {
      pred = predict(fit, olddata, type = "terminalNodes")$predictions
      prox = matrix(NA, nrow(pred), nrow(pred))
      ntree = ncol(pred)
      n = nrow(prox)
      
      if (is.null(fit$inbag.counts)) {
            stop("call ranger with keep.inbag = TRUE")
      }
      
      # Get inbag counts
      inbag = simplify2array(fit$inbag.counts)
      
      for (i in 1:n) {
            for (j in 1:n) {
                  # Use only trees where both obs are OOB
                  tree_idx = inbag[i, ] == 0 & inbag[j, ] == 0
                  prox[i, j] = sum(pred[i, tree_idx] == pred[j, tree_idx]) / sum(tree_idx)
            }
      }
      
      prox
}