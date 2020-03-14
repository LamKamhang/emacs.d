;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;;; Code:

;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'winner-mode)



;; Make "C-o" prompt for a target window when there are more than 2
(require-package 'switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-o") 'switch-window)


;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      ;; (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun split-balance-window-horizontally ()
  "Split the window horizontally."
  (interactive)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows)
  )

(defun split-balance-window-vertically ()
  "Split the window vertically."
  (interactive)
  (command-execute 'split-window-vertically)
  (command-execute 'balance-windows)
  )

(global-set-key (kbd "M-|") (split-window-func-with-other-buffer 'split-balance-window-horizontally))
(global-set-key (kbd "C-M-|") (split-window-func-with-other-buffer 'split-balance-window-vertically))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)


;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)



(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "C-c <down>") 'sanityinc/toggle-current-window-dedication)



(unless (memq window-system '(nt w32))
  (windmove-default-keybindings 'control))

(defun rotate-windows (arg)
  "Rotate your windows; use the prefix argument ARG to rotate the other direction."
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window!")
    (let* ((rotate-times (prefix-numeric-value arg))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse 'identity)))
      (dotimes (_ (abs rotate-times))
        (dotimes (i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)))))))

(defvar my-ratio-dict
  '((1 . 1.61803398875)
    (2 . 2)
    (3 . 3)
    (4 . 4)
    (0 . 0.61803398875)
    (9 . 0.5)
    (8 . 0.33333333333)
    (7 . 0.25))
  "The ratio dictionary.")

(defun my-split-window-horizontally (&optional ratio)
  "Split window horizontally and resize the new window.
'C-u number M-x my-split-window-horizontally' uses pre-defined
ratio from `my-ratio-dict'.
Always focus on new window."
  (interactive "P")
  (let* (ratio-val)
    (cond
     (ratio
      (setq ratio-val (cdr (assoc ratio my-ratio-dict)))
      (split-window-horizontally (floor (/ (window-body-width)
                                           (1+ ratio-val)))))
     (t
      (split-window-horizontally)))
    (set-window-buffer (next-window) (current-buffer))
    (windmove-right)
    ;; (if (or (not ratio-val)
    ;;         (>= ratio-val 1))
    ;;     (windmove-right))
    ))

(defun my-split-window-vertically (&optional ratio)
  "Split window vertically and resize the new window.
'C-u number M-x my-split-window-vertically' uses pre-defined
ratio from `my-ratio-dict'.
Always focus on new window."
  (interactive "P")
  (let* (ratio-val)
    (cond
     (ratio
      (setq ratio-val (cdr (assoc ratio my-ratio-dict)))
      (split-window-vertically (floor (/ (window-body-height)
                                         (1+ ratio-val)))))
     (t
      (split-window-vertically)))
    ;; open another window with current-buffer
    (set-window-buffer (next-window) (current-buffer))
    ;; move focus if new window bigger than current one
    (windmove-down)
    ;; (if (or (not ratio-val)
    ;;         (>= ratio-val 1))
    ;;     (windmove-down))
    ))

(global-set-key (kbd "M-\\") 'my-split-window-horizontally)
(global-set-key (kbd "C-M-\\") 'my-split-window-vertically)

(provide 'init-windows)
;;; init-windows.el ends here
