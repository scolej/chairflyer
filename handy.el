(defun compilation-tests ()
  (interactive)
  (save-all)
  (compilation-start
   "stack build :tests"))

(gsk "<f11>" 'compilation-tests)
