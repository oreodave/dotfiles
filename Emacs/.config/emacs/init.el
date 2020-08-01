;; Load literate
(require 'ob-tangle)
(setq user-emacs-directory "~/.config/emacs/")
(mapc #'(lambda (x) (org-babel-load-file (concat user-emacs-directory x))) (list "config.org"))

;; Programming
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#5f8787" "#dd9999" "#a06666" "#888888" "#999999" "#888888" "#c1c1c1"])
 '(ansi-term-color-vector
   [unspecified "#2e2e2e" "#bc8383" "#7f9f7f" "#d0bf8f" "#6ca0a3" "#dc8cc3" "#8cd0d3" "#b6b6b6"])
 '(custom-safe-themes
   '("0266c89aae71e5b03453145509ee4dd09817377c6df69c384e2e313da86fabd8" "7a89dff27d761c077685970ff22f17d66edef90ac0b083332cfe326af8592dfd" "4de73368daa1be83e74165ae20a393cf205f2023c3dab3fffec6fbf8c742fee7" "b236a062b2e913c08e456cdc3743029951e3fc3e3cb7a579018271e2927b91f7" "6724d9651afb36fbb52d1d2e5a612da7d3f6e8739c30fbc818441798d5526113" "30c98a55535d742be02b7d44f00965ee6acbb892a945f428f1875a210c51d536" "311ac10c551b531e895c110583108d667de691aa46a26a5d324339d22911e94b" "28cd8971a2cfe634ef4a42d5bfb305b8865761017372d5d202bd08590da198d9" "6e1a7768b8023e6c2075e4af23aefba3b3e5db18b06dbb1b18ef1949d6af9d94" "7cc79b0c9da3ad3150256023b9023f363719274939656f57f3d1eae6120c4c13" "12670281275ea7c1b42d0a548a584e23b9c4e1d2dabb747fd5e2d692bcd0d39b" "6a0edb6b0f4c6d0566325cf91a1a34daa179e1979136ce0a528bf83aff9b7719" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "d873c4ec3fa23a2eee0a08ccbb6266ea9511edf2e738e63bd2b7f867dde43cb6" default))
 '(package-selected-packages
   '(magit evil-magit evil-commentary evil-surround use-package))
 '(safe-local-variable-values
   '((org-babel-default-header-args:elisp
      (:results . "none")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
