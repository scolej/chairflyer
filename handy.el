(defun compilation-tests ()
  (interactive)
  (save-all)
  (compilation-start
   "stack test"))

(defun load-eval-arb ()
  (interactive)
  (haskell-process-load-file)
  (haskell-interactive-switch)
  (end-of-buffer)
  (haskell-interactive-mode-kill-whole-line)
  (insert "runTests destinationVs")
  (haskell-interactive-mode-return))

;; TODO
;; there must be a better way to do this. comint?

(gsk "<f11>" 'compilation-tests)
