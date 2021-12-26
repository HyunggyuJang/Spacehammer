;;; emacs-everywhere.el --- System-wide popup windows for quick edits -*- lexical-binding: t; -*-

;; Copyright (C) 2021 TEC

;; Author: TEC <https://github.com/tecosaur>
;; Maintainer: TEC <tec@tecosaur.com>
;; Created: February 06, 2021
;; Modified: February 06, 2021
;; Version: 0.0.1
;; Keywords: conenience, frames
;; Homepage: https://github.com/tecosaur/emacs-everywhere
;; Package-Requires: ((emacs "26.3"))

;;; License:

;; This file is part of org-pandoc-import, which is not part of GNU Emacs.
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;  System-wide popup Emacs windows for quick edits

;;; Code:

(require 'cl-lib)
(require 'server)

(defgroup emacs-everywhere ()
  "Customise group for Emacs-everywhere."
  :group 'convenience)

(defcustom emacs-everywhere-markdown-windows
  '("Zulip" "Stack Exchange" "Stack Overflow" ; Sites
    "^(7) " ; Reddit
    "Pull Request" "Issue" "Comparing .*\\.\\.\\." ; Github
    "Discord")
  "For use with `emacs-everywhere-markdown-p'.
Patterns which are matched against the window title."
  :type '(rep string)
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-markdown-apps
  '("Discord")
  "For use with `emacs-everywhere-markdown-p'.
Patterns which are matched against the app name."
  :type '(rep string)
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-force-use-org-mode nil
  "Whether use `org-mode' as editiong mode or not.
This force to set `org-mode' even if it is markdown flavor site."
  :type 'boolean
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-frame-name-format "Emacs Everywhere :: %s — %s"
  "Format string used to produce the frame name.
Formatted with the app name, and truncated window name."
  :type 'string
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-init-hooks
  '(emacs-everywhere-set-frame-name
    emacs-everywhere-set-frame-position
    emacs-everywhere-major-mode
    emacs-everywhere-insert-selection)
  "Hooks to be run before function `emacs-everywhere-mode'."
  :type 'hook
  :group 'emacs-everywhere)

(defcustom emacs-everywhere-determine-mode-alist
  (list
   (cons (lambda () (and (not emacs-everywhere-force-use-org-mode)
                         (fboundp 'markdown-mode)
                         (emacs-everywhere-markdown-p))) 'markdown-mode)
   ;; (cons (lambda () (and (fboundp 'latex-mode)
   ;;                       (emacs-everywhere-latex-p))) 'latex-mode)
   (cons (lambda () t) #'org-mode))
  "List to determine major mode for emacs-everywhere."
  :type 'list
  :group 'emacs-everywhere)


(defcustom emacs-everywhere-final-hooks
  '(emacs-everywhere-return-converted-org-to-gfm)
  "Hooks to be run just before content is copied."
  :type 'hook
  :group 'emacs-everywhere)

(defun spacehammer/alert (message)
  "shows Hammerspoon's hs.alert popup with a MESSAGE"
  (when (and message (eq system-type 'darwin))
    (call-process
     (executable-find "hs")
     nil 0 nil "-c" (concat "hs.alert.show(\"" message "\", 1)"))))

(defun spacehammer/fix-frame ()
  "Fix Emacs frame. It may be necessary when screen size changes.

Sometimes zoom-frm functions would leave visible margins around the frame."
  (let* ((geom (frame-monitor-attribute 'geometry))
         (height (- (first (last geom)) 2))
         (width (nth 2 geom))
         (fs-p (frame-parameter nil 'fullscreen))
         (frame (selected-frame))
         (x (first geom))
         (y (second geom)))
    (when (member fs-p '(fullboth maximized))
      (set-frame-position frame x y)
      (set-frame-height frame height nil t)
      (set-frame-width frame width nil t))
    (when (frame-parameter nil 'full-width)
      (set-frame-width frame width nil t)
      (set-frame-parameter nil 'full-width nil))
    (when (frame-parameter nil 'full-height)
      (set-frame-height frame height nil t)
      (set-frame-parameter nil 'full-height nil))))

(defun spacehammer/move-frame-one-display (direction)
  "Moves current Emacs frame to another display at given DIRECTION

DIRECTION - can be North, South, West, East"
  (let* ((hs (executable-find "hs"))
         (cmd (concat "hs.window.focusedWindow():moveOneScreen" direction "()")))
    (call-process hs nil 0 nil "-c" cmd)
    (spacehammer/fix-frame)))

(defun spacehammer/switch-to-app (pid)
  "Using third party tools tries to switch to the app with the given PID"
  (when (and pid (eq system-type 'darwin))
    (call-process (executable-find "hs") nil 0 nil "-c"
                  (concat "require(\"emacs\").switchToApp (\"" pid "\")"))))

(defvar spacehammer/edit-with-emacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'spacehammer/finish-edit-with-emacs)
    (define-key map (kbd "C-c C-k") 'spacehammer/cancel-edit-with-emacs)
    map))

(define-minor-mode spacehammer/edit-with-emacs-mode
  "Minor mode enabled on buffers opened by spacehammer/edit-by-emacs"
  :init-value nil
  :lighter " editwithemacs"
  :keymap spacehammer/edit-with-emacs-mode-map)

(defvar spacehammer/edit-with-emacs-hook nil
  "Hook for when edit-with-emacs buffer gets activated.
   Hook function must accept arguments:
    - buffer-name - the name of the edit buffer
    - pid         - PID of the app that invoked Edit-with-Emacs
    - title       - title of the app that invoked Edit-with-Emacs")

(defvar spacehammer/before-finish-edit-with-emacs-hook nil
  "Hook for when edit-with-emacs is finished and dedicated buffer and frame are about to get killed.
   Hook function must accept arguments:
    - buffer-name - the name of the edit buffer
    - pid         - PID of the app that invoked Edit-with-Emacs")

(defvar spacehammer/before-cancel-edit-with-emacs-hook nil
  "Hook for when edit-with-emacs is canceled and dedicated buffer and frame are about to get killed.
   Hook function must accept arguments:
    - buffer-name - the name of the edit buffer
    - pid         - PID of the app that invoked Edit-with-Emacs")

(defun spacehammer/edit-with-emacs (&optional name pid title screen)
  "Edit anything with Emacs

PID is a pid of the app (the caller is responsible to set that right)
TITLE is a title of the window (the caller is responsible to set that right)"
  (setq systemwide-edit-previous-app-pid pid)
  (select-frame-by-name "edit")
  (set-frame-position nil 400 400)
  (set-frame-size nil 800 600 t)

  (let* ((buf-name (concat "*edit-with-emacs " title " *"))
         (buffer (get-buffer-create buf-name)))
    (unless (bound-and-true-p global-edit-with-emacs-mode)
      (global-edit-with-emacs-mode 1))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (clipboard-yank)
      (deactivate-mark)
      (delete-other-windows)
      (spacehammer/edit-with-emacs-mode 1))
    (switch-to-buffer buffer)
    (run-hook-with-args 'spacehammer/edit-with-emacs-hook buf-name pid title))
  (when (and pid (eq system-type 'darwin))
    (call-process
     (executable-find "hs") nil 0 nil "-c"
     (concat "require(\"emacs\").editWithEmacsCallback(\""
             pid "\",\"" title "\",\"" screen "\")"))))

(defun spacehammer/turn-on-edit-with-emacs-mode ()
  "Turn on `spacehammer/edit-with-emacs-mode' if the buffer derives from that mode"
  (when (string-match-p "*edit-with-emacs" (buffer-name (current-buffer)))
    (spacehammer/edit-with-emacs-mode t)))

(define-global-minor-mode global-edit-with-emacs-mode
  spacehammer/edit-with-emacs-mode spacehammer/turn-on-edit-with-emacs-mode)

(defvar systemwide-edit-previous-app-pid nil
  "Last app that invokes `spacehammer/edit-with-emacs'.")

(defun spacehammer/finish-edit-with-emacs ()
  (interactive)
  (run-hook-with-args
   'spacehammer/before-finish-edit-with-emacs-hook
   (buffer-name (current-buffer))
   systemwide-edit-previous-app-pid)

  (clipboard-kill-ring-save (point-min) (point-max))
  (kill-buffer)
  (delete-frame)
  (call-process (executable-find "hs") nil 0 nil "-c"
                (concat "require(\"emacs\").switchToAppAndPasteFromClipboard (\"" systemwide-edit-previous-app-pid "\")"))
  (setq systemwide-edit-previous-app-pid nil))

(defun spacehammer/cancel-edit-with-emacs ()
  (interactive)
  (run-hook-with-args
   'spacehammer/before-cancel-edit-with-emacs-hook
   (buffer-name (current-buffer))
   systemwide-edit-previous-app-pid)

  (kill-buffer)
  (delete-frame)
  (spacehammer/switch-to-app systemwide-edit-previous-app-pid)
  (setq systemwide-edit-previous-app-pid nil))

;;;; System-wide org capture
(defvar systemwide-capture-previous-app-pid nil
  "Last app that invokes `spacehammer/activate-capture-frame'.")

(defun spacehammer/activate-capture-frame (&optional pid title keys)
  "Run ‘org-capture’ in capture frame.

PID is a pid of the app (the caller is responsible to set that right)
TITLE is a title of the window (the caller is responsible to set that right)
KEYS is a string associated with a template (will be passed to `org-capture')"
  (setq systemwide-capture-previous-app-pid pid)
  (select-frame-by-name "capture")
  (set-frame-position nil 400 400)
  (set-frame-size nil 1000 400 t)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-capture nil keys))

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (when (and (equal "capture" (frame-parameter nil 'name))
             (not (eq this-command 'org-capture-refile)))
    (spacehammer/switch-to-app systemwide-capture-previous-app-pid)
    (delete-frame)))

(defadvice org-capture-refile
    (after delete-capture-frame activate)
  "Advise ‘org-refile’ to close the frame."
  (delete-frame))

(defadvice user-error
    (before before-user-error activate)
  "Advice"
  (when (eq (buffer-name) "*Org Select*")
    (spacehammer/switch-to-app systemwide-capture-previous-app-pid)))

(provide 'spacehammer)

;;; spacehammer.el ends here
