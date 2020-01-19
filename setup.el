(async-shell-command "stack ghci src/Chairflyer.hs")

(setq haskell-process-type 'stack-ghci)
(run-haskell)

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(require 'flycheck)
(add-hook 'haskell-mode-hook 'flycheck-mode)
