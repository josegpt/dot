(use-package lsp-mode
    :hook ((lsp-mode . lsp-enable-which-key-integration))
    :custom
    (lsp-keymap-prefix "C-c l")
    :commands (lsp lsp-deferred))
