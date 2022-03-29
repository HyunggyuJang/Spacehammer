(fn edit-with-emacs []
  "Executes emacsclient, evaluating a special elisp function in spacehammer.el
   (it must be pre-loaded), passing PID, title and display-id of the caller."
  (let [current-wind (hs.window.focusedWindow)
        id           (.. "\"" (: current-wind :id) "\"")
        name         (.. "\"" (: (: current-wind :application) :name) "\"")
        title        (.. "\"" (: current-wind :title) "\"")
        run-str      (..
                      "/opt/homebrew/bin/emacsclient"
                      " -e '(progn (setq mac-use-title-bar t) (emacs-everywhere (emacs-everywhere--app-info "
                      id " " name " " title " )))' &")
        co           (coroutine.create (fn [run-str]
                                         (io.popen run-str)))
        prev         (hs.pasteboard.changeCount)
        _            (hs.eventtap.keyStroke [:cmd] :x)
        next         (hs.pasteboard.changeCount)]
    (when (= prev next)         ; Pasteboard was not updated so no text was selected
      (hs.eventtap.keyStroke [:cmd] :a)  ; select all
      (hs.eventtap.keyStroke [:cmd] :x)  ; copy
      )
    (coroutine.resume co run-str)))

(fn run-emacs-fn
  [elisp-fn args]
  "Executes given elisp function in emacsclient. If args table present, passes
   them into the function."
  (let [args-lst (when args (.. " '" (table.concat args " '")))
        run-str  (.. "/opt/homebrew/bin/emacsclient"
                     " -e \"(funcall '" elisp-fn
                     (if args-lst args-lst " &")
                     ")\" &")]
    (io.popen run-str)))

(fn full-screen
  []
  "Switches to current instance of GUI Emacs and makes its frame fullscreen"
  (hs.application.launchOrFocus :Emacs)
  (run-emacs-fn
   (..
    "(lambda ())"
    "(spacemacs/toggle-fullscreen-frame-on)"
    "(spacehammer/fix-frame)")))

(fn vertical-split-with-emacs
  []
  "Creates vertical split with Emacs window sitting next to the current app"
  (let [windows    (require :windows)
        cur-app    (-?> (hs.window.focusedWindow) (: :application) (: :name))
        rect-left  [0  0 .5  1]
        rect-right [.5 0 .5  1]
        elisp      (.. "(lambda ()"
                       " (spacemacs/toggle-fullscreen-frame-off) "
                       " (spacemacs/maximize-horizontally) "
                       " (spacemacs/maximize-vertically))")]
    (run-emacs-fn elisp)
    (hs.timer.doAfter
     .2
     (fn []
       (if (= cur-app :Emacs)
           (do
             (windows.rect rect-left)
             (windows.jump-to-last-window)
             (windows.rect rect-right))
           (do
             (windows.rect rect-right)
             (hs.application.launchOrFocus :Emacs)
             (windows.rect rect-left)))))))

(fn switch-to-app [id]
  "Don't remove! - this is callable from Emacs See: `spacehammer/switch-to-app`
   in spacehammer.el "
  (let [wind (hs.window.get (tonumber id))]
    (when wind (: wind :focus))))

(fn switch-to-app-and-paste-from-clipboard [id]
  "Don't remove! - this is callable from Emacs See:
   `spacehammer/finish-edit-with-emacs` in spacehammer.el."
  (let [wind (hs.window.get (tonumber id))]
    (when wind
      (: wind :focus)
      (hs.timer.doAfter
       0.001
       (fn [] (hs.eventtap.keyStroke [:cmd] :v))))))

(fn maximize
  []
  "Maximizes Emacs GUI window after a short delay."
  (hs.timer.doAfter
   1.5
   (fn []
     (let [app     (hs.application.find :Emacs)
           windows (require :windows)
           modal   (require :lib.modal)]
       (when app
         (: app :activate)
         (windows.maximize-window-frame))))))

{:edit-with-emacs                  edit-with-emacs
 :full-screen                      full-screen
 :maximize                         maximize
 :switchToApp                      switch-to-app
 :switchToAppAndPasteFromClipboard switch-to-app-and-paste-from-clipboard
 :vertical-split-with-emacs        vertical-split-with-emacs}
