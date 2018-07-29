;;; isolate.el --- Surrounding tool with flexable customizations.

;; Author: Yuan Fu <casouri@gmail.com>
;; URL: https://github.com/casouri/isolate
;; Version: 1.0.0
;; Keywords: convenience
;; Package-Requires: ((emacs "25"))

;;; Commentary:
;; 
;; This is a surrouding tool.
;; It features powerful and easy customization system.
;; You can create complex regexp rules easily.
;;
;; More information in README.org.

;;; Code:
;;


;;; Custom

(defgroup isolate
  '((isolate-separator custom-variable))
  "Isolate (surround) region with pairs."
  :group 'convenience
  :prefix "isolate-")

(defcustom isolate-separator ","
  "The separator that splits left string into segments.
It can be a regexp expression but I suggest to use a single character.
By \"character\" I mean \"a string that has a legnth of 1\"."
  :group 'isolate
  :type 'string)

;;; Variable
;;
;; Not all variables, some are under Delete and Change heading.

;;;; Public

(defvar isolate--left-beg nil
  "Mark of beginning of left segments.")

(defvar isolate--left-end nil
  "Mark of end of left segments.")

(defvar isolate--right-beg nil
  "Mark of beginning of right segments.")

(defvar isolate--right-end nil
  "Mark of end of right segments.")

(defvar isolate-quick-shortcut-list
  '(((from . "]") (to . "[, "))
    ((from . ")") (to . "(, "))
    ((from . "}") (to . "{, "))
    ((from . ">") (to . "<, "))
    )
  "Shortcuts for `isolate-quick-xxx' functions.

For example, by default \"]\" is mapped to \"[ \", etc.

Each element is an alist representing a shortcut.
Each shortcut have three possible keys: 'from, 'to and 'condition.
'from and 'to are strings \(not regexp!\),

'condition is a function that takes user input as argument.
'condition is optional.
If 'condition exists and returns nil, the shortcut will be ignored.")

(defvar isolate-pair-list
  '(((to-left . "`") (to-right . "'") (no-regexp . t) (condition . (lambda (_) (if (equal major-mode 'emacs-lisp-mode) t nil))))
    ((to-left . "(") (to-right . ")"))
    ((to-left . "[") (to-right . "]") (no-regexp . t))
    ((to-left . "{") (to-right . "}"))
    ((to-left . "<") (to-right . ">"))
    ((from . "<\\([^ ]+\\).*>") (to-right . (lambda (left) (format "</%s>" (match-string 1 left)))))
    ((to-left . "\\{begin}") (to-right . "\\{end}"))
    ((from . "org-src") (to-left . "#+BEGIN_SRC\n") (to-right . "#+END_SRC\n") (no-regexp . t))
    )
  "Matching pairs.
Each element is an alist with five possible keys: 'from, 'to-left, to-right, no-regexp and condition.
Only ('from or 'to-left) and 'to-right are required.

'right is required, one from 'from and 'to-left is required,
'condition is optional.

1. If only 'to-left, and it equal to user input,
and matches and condition passes,
'to-left is used as left of pair,
'to-right is used as right of pair.

2. If only 'from, and the regexp of from matches user input,
user-input is used as left of pair
and 'to-right is used as right of pair.

3. If both 'from and 'to-left exists,
'from as regexp is used to match user-input,
if it matches, 'to-left is used as left of pair
and 'to-right is used as right of pair.

In addition, 'to-left and 'to-right can be a function
that takes user input as argument and return a string.

If they are functions, and you have a regex 'from,
you can use (match-string num user-input) to get
regexp matched groups.

'condition, if exist, should be a function
that takes user input as argument and return a boolean.
You can use it to check major modes, etc.

'no-regexp only affects delete commands,
if you want to search the matche pair plainly by text
rather than by regexp, add \(no-regexp . t\).

This is especially important for pairs that contains
regexp keywords such as [, \\, +, etc.

A word of 'from:
\"^\" and \"$\" are added automatically to from before matching.
Also don't forget regexp escapes.")

(defvar isolate-setup-hook nil
  "This hook is ran after `isolate--add-setup'.
Which moves the point to the left of the region
and enters `evil-insert-state' if nessessary.")

;;;; Private

(defvar isolate--left-overlay nil
  "Overlay used for highlighting.")

(defvar isolate--right-overlay nil
  "Overlay used for highlighting.")

;;; Base function

;;;; Essential

(defun isolate--match-pair (left-segment pair-list &optional search)
  "Find matching left and right segment for LEFT-SEGMENT.
Return a cons of (left . right).
LEFT-SEGMENT is not everthing left to region,
but one of the segments separated by `isolate-separator'.

PAIR-LIST is the list in which this function looks for match.

SEARCH means this function is called by delete command.

Return (LEFT-SEGMENT . LEFT-SEGMENT) if nothing matches.
This function never returns nil."
  (catch 'return
    (dolist (pair pair-list)
      (let* ((from (alist-get 'from pair))
             (to-left (alist-get 'to-left pair))
             (to-right (alist-get 'to-right pair))
             (condition (alist-get 'condition pair))
             (no-regexp (alist-get 'no-regexp pair))
             (quote-func (if (and search no-regexp) 'regexp-quote 'eval)))
        (when (and (or (not condition) ; no condition
                       (and condition (funcall condition left-segment))) ; condition passes
                   (or (and to-left (equal to-left left-segment)) ; to-left exists & exactly equal
                       (string-match (format "^%s$" from) left-segment))) ; from matches
          (setq to-left (or to-left left-segment))
          (throw 'return
                 (cons (funcall quote-func
                                (if (functionp to-left)
                                    (funcall to-left left-segment)
                                  to-left))
                       (funcall quote-func
                                (if (functionp to-right)
                                    (funcall to-right left-segment)
                                  to-right)))))))
    ;; if no one matches, return left itself
    (cons left-segment left-segment)))

;;;; Helper

(defun isolate--translate-quick-shortcut (left-segment)
  "Translate user input LEFT-SEGMENT if a predefined shorcut exists.
Return LEFT-SEGMENT itself if not."
  (catch 'return
    (dolist (shortcut isolate-quick-shortcut-list)
      (let ((from (alist-get 'from shortcut))
            (to (alist-get 'to shortcut))
            (condition (alist-get 'condition shortcut)))
        (when (and (equal from left-segment)
                 (or (not condition)
                     (funcall condition left-segment)))
            (throw 'return to))))
    left-segment))

(defmacro isolate--setup-marker (left-beg left-end right-beg righ-end)
  "Helper for setting up markers."
  `(save-excursion
     ;; I changed goto-char to set-mark,
     ;; because evil's line selection
     ;; prevents point from moving...
     (set-marker (setq isolate--left-beg (point-marker))
               ,left-beg)
     
     (set-marker (setq isolate--left-end (point-marker))
               ,left-end)
     (set-marker-insertion-type isolate--left-end t)
     
     (set-marker (setq isolate--right-beg (point-marker))
               ,right-beg)
     
     (set-marker (setq isolate--right-end (point-marker))
               ,righ-end)
     (set-marker-insertion-type isolate--right-end t)))


;;;; Fundamental helper

(defmacro isolate--append (seq elt)
  "Append ELT to SEQ destructivly. This is a macro."
  `(if ,seq
       (nconc ,seq (list ,elt))
     (setq ,seq (list ,elt))))

(defmacro isolate--push (elt seq)
  "Push ELT to SEQ destructivly. This is a macro."
  `(if ,seq
       (push ,elt ,seq)
     (setq ,seq (list ,elt))))

(defun isolate--replace-with (str start end)
  "Replace strings between START and END with STR.
This function doesn't move point."
  (save-excursion
    (delete-region start end)
    (goto-char start)
    (insert str)))

;;; Add

;;;; Command

(defun isolate-quick-add (left-segment)
  "Surround region. LEFT-SEGMENT is the left matching pair."
  (interactive "cEnter left segment")
  ;; setup
  (isolate--add-setup-marker)
  (isolate--add-setup)
  (save-excursion
    (let* ((left (isolate--translate-quick-shortcut
                  (char-to-string left-segment)))
           (con (isolate--add left))
           (actual-left (car con))
           (right (cdr con)))
      ;; insert left
      (goto-char isolate--left-beg)
      (insert actual-left)
      ;; insert right
      (goto-char isolate--right-beg)
      (insert right)))
  ;; cleanup
  (isolate--add-cleanup))

(defalias 'isolate-long-add #'isolate-add-mode)

(defun isolate-add-finish ()
  "Finished adding pairs. IOW exit `isolate-add-mode'."
  (interactive)
  (isolate-add-mode -1))

(defun isolate-add-abort ()
  "Abort adding pairs."
  (interactive)
  (isolate--replace-with "" isolate--left-beg isolate--left-end)
  (isolate--replace-with "" isolate--right-beg isolate--right-end)
  (isolate-add-mode -1))

(define-minor-mode isolate-add-mode
  "Enter this mode for surrounding interactivly.
Hit C-c C-c when finished."
  :lighter "ADD"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'isolate-add-finish)
            (define-key map (kbd "C-c C-q") #'isolate-add-abort)
            (define-key map (kbd "C-c C-a") #'isolate-goto-beginning)
            (define-key map (kbd "C-c C-e") #'isolate-goto-end)
            map)
  (if isolate-add-mode
      (progn
        (isolate--add-setup-marker)
        (isolate--add-setup)
        (add-hook 'post-command-hook #'isolate--add-hook t t)
        (advice-add 'message :around #'isolate--add-echo-advice)
        (isolate--disable-autoparens))
    (isolate--add-cleanup)
    (remove-hook 'post-command-hook #'isolate--add-hook t)
    (advice-remove 'message #'isolate--add-echo-advice)
    (isolate--disable-autoparens t)))

(defun isolate-goto-beginning ()
  "Go to beginning of left."
  (interactive)
  (goto-char isolate--left-beg))

(defun isolate-goto-end ()
  "Go to end of left."
  (interactive)
  (goto-char isolate--left-end))


;;;; Function

(defun isolate--add (left)
  "Return (left . right) base on LEFT."
  (let* ((left-list nil)
         (actual-left nil)
         (right nil)
         (right-list nil))
    ;; match left and construct right
    (dolist (segment (split-string left isolate-separator))
      (let* ((con (isolate--match-pair segment isolate-pair-list))
             (left-segment (car con))
             (right-segment (cdr con)))
        (isolate--append left-list left-segment)
        (isolate--push right-segment right-list)))
    ;; replace left and insert right
    (setq actual-left (funcall 'string-join left-list isolate-separator))
    (setq right (apply 'concat right-list))
    (cons actual-left right)))

(defvar isolate--left-old-length 0
  "Length of left before process.")

(defvar isolate--old-cursor-pos 0
  "Cursor position before process.")

(defun isolate--add-hook ()
  "Add isolation to region. This function is used in `isolate-add-mode'.
For actual command use `isolate-quick-add' or `isolate-add-mode'."
  (let* ((left (buffer-substring-no-properties
                isolate--left-beg isolate--left-end))
         (con (isolate--add left))
         (actual-left (car con))
         (right (cdr con)))
    (unless (equal left actual-left)
      (isolate--replace-with actual-left isolate--left-beg isolate--left-end)
      (goto-char isolate--left-end))
    (isolate--replace-with right isolate--right-beg isolate--right-end)))


;;;; Helper


(defun isolate--add-echo-advice (old-func &rest arg)
  "Indicate user that add mode is active. OLD-FUNC is `message'. ARG are args."
  (funcall old-func "[ ADD ]   [ Abort: C-c q ] [ Finish: C-c C-c ] [ Jump BEG: C-c a ] [ Jump END: C-c e ]\n\n%s" (apply 'format arg)))


(defun isolate--add-setup-marker ()
  "Setup the markers."
  (unless isolate--changing
    (isolate--setup-marker (region-beginning)
                           (region-beginning)
                           (region-end)
                           (region-end))))

(defun isolate--add-setup ()
  "Setup highlight and insert. Have to run after `isolate--xxx-setup-marker'."
  ;; highlight
  (overlay-put
   (setq isolate--left-overlay
         (make-overlay isolate--left-beg isolate--left-end nil nil t))
   'face 'highlight)
  (overlay-put
   (setq isolate--right-overlay
         (make-overlay isolate--right-beg isolate--right-end nil nil t))
   'face 'highlight)

  ;; insert
  (goto-char isolate--left-beg)
  (deactivate-mark)
  (run-hooks 'isolate-setup-insert-hook))


(defun isolate--add-cleanup ()
  "Clean up highlight, separator and insert setup."

  (save-excursion
    ;; highlight
    (delete-overlay isolate--left-overlay)
    (delete-overlay isolate--right-overlay)
    ;; separator
    (goto-char isolate--left-beg)
    (while (search-forward isolate-separator isolate--left-end t)
      (replace-match ""))))

(defun isolate--disable-autoparens (&optional enable)
  "Disable paren auto-completion in `isolate-add-mode'.
If ENABLE non-nil, reenable them.

It also disable evil."
  (if (not enable)
      (progn
        (setq isolate--evil
              (if (bound-and-true-p evil-mode)
                  (progn (evil-mode -1) t)
                nil))
        (setq isolate--smartparens
              (if (bound-and-true-p smartparens-mode)
                  (progn (smartparens-mode -1) t)
                nil))
        (setq isolate--paredit
              (if (bound-and-true-p paredit-mode)
                  (progn (paredit-mode -1) t)
                nil))
        (setq isolate--electric-pair
              (if (bound-and-true-p electric-pair-mode)
                  (progn (electric-pair-mode -1) t)
                nil)))
    (when (bound-and-true-p isolate--evil)
      (evil-mode))
    (when (bound-and-true-p isolate--smartparens)
      (smartparens-mode))
    (when (bound-and-true-p isolate--paredit)
      (paredit-mode))
    (when (bound-and-true-p isolate--electric-pair)
      (electric-pair-mode))))

;;; Delete

;;;; Variable

(defvar isolate-delete-extended-pair-list
  '(((to-left . "\\") (to-right . "\\") (no-regexp . t))
    ((to-left . "+") (to-right . "+") (no-regexp . t))
    ((to-left . ".") (to-right . ".") (no-regexp . t))
    ((to-left . "*") (to-right . "*") (no-regexp . t))
    ((to-left . "?") (to-right . "?") (no-regexp . t))
    ((to-left . "^") (to-right . "^") (no-regexp . t))
    ((to-left . "$") (to-right . "$") (no-regexp . t))
    ((from . "<t>") (to-left . "<[^/]+?>") (to-right . "</.+?>"))
    ((from . "<\\([^ ]+\\)[^<>]*>")
     (to-left . (lambda (user-input) (format "<%s *.*?>" (match-string 1 user-input))))
     (to-right . (lambda (user-input) (format "< *?/%s *?>" (match-string 1 user-input))))))
  "Rule list.
Detail see `isolate-pair-list'.")


(defvar isolate--search-level 1
  "The level of matching pairs that isolate searches.
Starts from 1, passed to search function as COUNT.
Don't forget to reset to 1 when exit `isolate-delete-mode'.")

(defvar isolate--search-history nil
  "History of long delete(change) search.")

(defvar isolate--search-success nil
  "If nil, don't delete.")

;;;; Command

;;;;; Quick delete

(defun isolate-quick-delete (left-segment)
  "Delete surrouding matched by LEFT-SEGMENT.
Return t if match, nil if no match."
  (interactive "cEnter left segment")
  ;; match & delete
  (if (isolate--search-pair
       (isolate--translate-quick-shortcut
        (char-to-string left-segment)))
      (progn
        (isolate--replace-with "" isolate--left-beg isolate--left-end)
        (isolate--replace-with "" isolate--right-beg isolate--right-end)
        t)
    (message "No balanced match")
    nil)
  
  ;; cleanup
  (isolate--delete-cleanup-overlay))

;;;;; Long delete

(defvar isolate--delete-minibuffer-map (let ((map minibuffer-local-map))
                                         (define-key map (kbd "C-p") #'isolate-search-up)
                                         (define-key map (kbd "C-n") #'isolate-search-down)
                                         map)
  "The keymap used in `isolate-long-delete'.")

(defvar isolate--delete-main-buffer nil
  "The buffer that edit should apply to.")

(defmacro isolate--delete-with-left (var &rest form)
  "Evaluate FORM with VAR as the user inserted regexp."
  `(when (>= (point-max) (length "Left regexp: "))
     (let ((,var (buffer-substring-no-properties (1+ (length "Left regexp: ")) (point-max))))
       ,@form)))

(defun isolate--delete-hook ()
  "Function run in `post-command-hook'."
  ;; TODO add check for minibuffer window
  (isolate--delete-with-left
   left
   (with-current-buffer isolate--delete-main-buffer
     (when (setq isolate--search-success
                 (isolate--search-pair left isolate--search-level))
       (isolate--delete-update-highlight)))))

(defvar isolate--delete-minibuffer-hook-stage 0
  "Recode stage for `isolate--delete-minibuffer-hook'.")

(defun isolate--delete-minibuffer-hook ()
  "Stage 0: add self to `minibuffer-setup-hook'.

Stage 1: remove self from `minibuffer-setup-hook', add self to `post-self-insert-hook'.

Stage 2: remove self from `post-self-insert-hook', add `isolate--delete-hook' to `post-command-hook'.

Stage 3: remove `isolate--delete-hook' from `post-command-hook'."
  (pcase isolate--delete-minibuffer-hook-stage
    (0 (setq isolate--delete-minibuffer-hook-stage 1)
       (add-hook 'minibuffer-setup-hook #'isolate--delete-minibuffer-hook))
    (1 (setq isolate--delete-minibuffer-hook-stage 2)
       (remove-hook 'minibuffer-setup-hook #'isolate--delete-minibuffer-hook)
       (add-hook 'post-command-hook #'isolate--delete-hook))
    (2 (setq isolate--delete-minibuffer-hook-stage 0)
       (remove-hook 'post-command-hook #'isolate--delete-hook))))


(defun isolate-long-delete ()
  "Delete surrounding by entering regexp."
  (interactive)
  (setq isolate--delete-main-buffer (current-buffer))
  (let ((minibuffer-local-map isolate--delete-minibuffer-map))
    (condition-case nil
        (progn
          (setq isolate--delete-minibuffer-hook-stage 0)
          (isolate--delete-minibuffer-hook)
          (isolate--disable-autoparens)
          (read-string "Left regexp: ")
          (isolate--delete-minibuffer-hook) ; cleanup
          (isolate--disable-autoparens t)
          (isolate-delete-finish)
          (isolate--delete-cleanup))
      ((quit error)
       (isolate--delete-minibuffer-hook) ;cleanup
       (isolate--disable-autoparens t)
       (isolate--delete-cleanup)))))

(defun isolate-delete-finish ()
  "Apply edit."
  (interactive)
  (when isolate--search-success
    (isolate--replace-with "" isolate--left-beg isolate--left-end)
    (isolate--replace-with "" isolate--right-beg isolate--right-end)))

(defun isolate--delete-cleanup ()
  "Cleanup."
  (isolate--delete-cleanup-overlay)
  (setq isolate--search-level 1
        isolate--search-success (if isolate--changing
                                    isolate--search-success
                                  nil)))

(defun isolate-search-up ()
  "Search one level of matching pairs up."
  (interactive)
  (setq isolate--search-level
        (isolate--delete-with-left
         left
         (with-current-buffer isolate--delete-main-buffer
           (setq isolate--search-level (1+ isolate--search-level))
           (if (isolate--search-pair left isolate--search-level)
               (progn
                 (isolate--delete-update-highlight)
                 (message "Up one level"))
             ;; failed, set level back
             (setq isolate--search-level (1- isolate--search-level))
             (message "No further match"))
           isolate--search-level))))

(defun isolate-search-down ()
  "Search one level of matching pairs down."
  (interactive)
  (setq isolate--search-level
        (isolate--delete-with-left
         left
         (with-current-buffer isolate--delete-main-buffer
           (setq isolate--search-level (1- isolate--search-level))
           (if (and (>= isolate--search-level 1)
                    (isolate--search-pair left isolate--search-level))
               (progn
                 (isolate--delete-update-highlight)
                 (message "Down one level"))
             ;; failed, set level back
             (setq isolate--search-level (1+ isolate--search-level))
             (message "No furher match"))
           isolate--search-level))))

;;;; Function

(defun isolate--count-match (regexp beg end)
  "Count number of occurences of REGEXP between BEG and END in current buffer."
  (save-excursion
    (goto-char beg)
    (let ((count 0))
      (while (re-search-forward regexp end t)
        (setq count (1+ count)))
      count)))

(defmacro isolate--jump-backward ()
  "Used in `isolate--find-balance-pair'."
  `(progn
     (unless (re-search-backward left-regexp nil t)
       (throw 'return nil))
     (setq left-beg (match-beginning 0)
           left-end (match-end 0))))

(defmacro isolate--jump-forward ()
  "Used in `isolate--find-balance-pair'."
  `(progn
     (unless (re-search-forward right-regexp nil t)
       (throw 'return nil))
     (setq right-beg (match-beginning 0)
           right-end (match-end 0))))

(defun isolate--simple-balance-pair (left-regexp right-regexp &optional point count)
  "Find the COUNT'th balanced matching pair (LEFT-REGEXP & RIGHT-REGEXP)
centered around POINT.
Count starts from 1.
Nil or omitted COUNT means 0.
Nil or omitted POINT means (point).

Return t if successed, nil if failed.

This is simple version because left-regexp = right-regexp."
  (save-excursion
    (catch 'return
      (let ((count (or count 1))
            left-beg
            left-end
            right-beg
            right-end
            origin-point)
        (setq origin-point (goto-char (or point (point))))
        (dotimes (var count)
          (isolate--jump-backward))
        (goto-char origin-point)
        (dotimes (var count)
          (isolate--jump-forward))
        (isolate--setup-marker left-beg
                                 left-end
                                 right-beg
                                 right-end)
        ;; return t
        t))))

(defun isolate--find-balance-pair (left-regexp right-regexp &optional point count)
  "Find the COUNT'th balanced matching pair (LEFT-REGEXP & RIGHT-REGEXP)
centered around POINT.
Count starts from 1.
Nil or omitted COUNT means 0.
Nil or omitted POINT means (point).

Return t if successed, nil if failed."
  (if (equal left-regexp right-regexp)
      (isolate--simple-balance-pair left-regexp right-regexp point count)
    (save-excursion
      (catch 'return
        (let ((count (or count 1))
              left-beg
              left-end
              right-beg
              right-end
              origin-point
              ;; used in step 2 when handels COUNT
              old-right-count
              new-right-count)
          (setq origin-point (goto-char (or point (point))))
          (isolate--jump-backward)
          ;; left count is 1, right-count is 0
          ;; now point is at the beginning of the first left match
          ;; jump left until left-count = 1 + right-count
          (while (let ((left-count (isolate--count-match left-regexp left-beg origin-point))
                       (right-count (isolate--count-match right-regexp left-beg origin-point)))
                   (< (- left-count right-count) 1))
            (isolate--jump-backward))
          ;; point is at the beginning of correct left match
          ;; now handle COUNT
          ;; nothing hapends if count <= 1.
          (setq old-right-count (isolate--count-match right-regexp left-beg origin-point))
          (setq new-right-count old-right-count)
          (while (> count 1)
            (isolate--jump-backward)
            (setq new-right-count (isolate--count-match right-regexp left-beg origin-point))
            (setq count (+ count (- new-right-count old-right-count 1)))
            (setq old-right-count new-right-count))
          ;; it's time to find right match.
          ;; setup right-beg and righ-end
          (isolate--jump-forward)
          ;; jump right until balanced
          (while (let ((left-count (isolate--count-match left-regexp left-beg right-beg))
                       (right-count (isolate--count-match right-regexp left-end right-end)))
                   (> left-count right-count))
            (isolate--jump-forward))
          (when (< right-beg origin-point)
            (throw 'return nil))
          ;; done! Now it's balanced!
          (isolate--setup-marker left-beg
                                 left-end
                                 right-beg
                                 right-end)
          ;; return t
          t)))))

(defun isolate--search-pair (left &optional count)
  "Seach COUNT'th LEFT and matching right segment.
Return t if matched, nil if not.
Note that COUNT starts from 1, not 0. 0 and nil are treated as 1.

The function sets `isolate--left-beg', `isolate--left-end',
`isolate--right-beg' and `isolate--right-end'.

COUNT is like COUNT in `search-backward-regexp'."
  (catch 'return
    (let ((count (if (or (equal count nil) (equal count 0)) 1 count))
          ;; special treatment for `isolate-quick-delete'
          left-list
          left-regexp
          right-list
          right-regexp
          (left-count 0)
          (right-count 0))
      ;; construct left and right regexp
      (dolist (segment (split-string left isolate-separator))
        (let* ((con (isolate--match-pair
                      segment
                      (append isolate-delete-extended-pair-list isolate-pair-list)
                      t))
               (left-pattern (car con))
               (right-pattern (cdr con)))
          (isolate--append left-list left-pattern)
          (isolate--push right-pattern right-list)))
      ;; construct
      (setq left-regexp (apply 'concat left-list))
      (setq right-regexp (apply 'concat right-list))
      ;; search & return
      (isolate--find-balance-pair left-regexp right-regexp nil count))))

;;;; Helper

(defun isolate--delete-echo-advice (old-func &rest arg)
  "Indicate user that delete mode is active. OLD-FUNC is `message'. ARG are args."
  (funcall old-func "[ DELETE ]   [ Abort: C-c q ] [ Finish: C-c C-c ] [ New match: C-c s ] [ Up: C-c p] [ Down: C-c n ]\n\n%s" (apply 'format arg)))

(defun isolate--delete-cleanup-overlay ()
  "Cleanup overlay."
  (when isolate--left-overlay
    (delete-overlay isolate--left-overlay))
  (when isolate--right-overlay
    (delete-overlay isolate--right-overlay)))

(defun isolate--delete-update-highlight ()
  "Update highlight."
  (isolate--delete-cleanup-overlay)
  (overlay-put
   (setq isolate--left-overlay (make-overlay isolate--left-beg isolate--left-end))
   'face 'highlight)
  (overlay-put
   (setq isolate--right-overlay (make-overlay isolate--right-beg isolate--right-end))
   'face 'highlight))

;;; Change

;;;; Variable

(defvar isolate--changing nil
  "If this is t, `isolate--add-setup-marker' doesn't alter markers.")

;;;; Command

(defun isolate-quick-change (from to)
  "Change FROM to TO."
  (interactive "cChange from\ncChange to")
  (let ((isolate--changing t))
    (when (isolate-quick-delete from)
      (isolate-quick-add to))))

(defun isolate--change-delete-advice (&rest _)
  "Start adding. Advice to `isolate-delete-finish'."
  (interactive)
  (setq isolate--changing nil)
  (when isolate--search-success
    (setq isolate--search-success nil)
    (let ((isolate--changing t))
      (advice-remove 'isolate-delete-finish #'isolate--change-delete-advice)
      (isolate-add-mode))))

(defun isolate-long-change ()
  "Change matching pair of user entered regexp to user enterd pair."
  (interactive)
  (setq isolate--changing t)
  (advice-add 'isolate-delete-finish :after #'isolate--change-delete-advice)
  (isolate-long-delete))

;;; Provide

(provide 'isolate)

;;; isolate.el ends here
