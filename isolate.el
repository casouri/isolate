;;; isolate.el --- Surrounding with powerful yet easy-to-use customization system.

;;; Commentary:
;; 

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

(defvar-local isolate--left-beg nil
  "Mark of beginning of left segments.")

(defvar-local isolate--left-end nil
  "Mark of end of left segments.")

(defvar-local isolate--right-beg nil
  "Mark of beginning of right segments.")

(defvar-local isolate--right-end nil
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
  '(((to-left . "`") (to-right . "'") (condition . (lambda (_) (if (equal major-mode 'emacs-lisp-mode) t nil))))
    ((to-left . "(") (to-right . ")"))
    ((to-left . "[") (to-right . "]"))
    ((to-left . "{") (to-right . "}"))
    ((to-left . "<") (to-right . ">"))
    ((from . "<\\([^ ]+\\).*>") (to-right . (lambda (left) (format "</%s>" (match-string 1 left)))))
    ((to-left . "\\{begin}") (to-right . "\\{end}"))
    ((from . "org-src") (to-left . "#+BEGIN_SRC\n") (to-right . "#+END_SRC\n"))
    )
  "Matching pairs.
Each element is an alist with four possible keys: 'from, 'to-left, to-right and condition.
Only ('from or 'to-left) and 'to-right are required.

Value of 'from is match by user input, it is a regexp string.
\"^\" and \"$\" are added automatically to it before matching.
Also don't forget regexp escapes.

Value of 'condition is a function that takes a string
which is the left segement user inserted.
How or whether to use it is depend on you.
If condition exists, isolate only match the pair
when condition function returns t.
This function can be used to check major mode or some special rules.

When 'from matches or 'to-left completely equal to user input,
and also condition passes,
value of 'to-left and 'to-right defines left and right segments.
If 'to-left is ommited,
it is considered to be the same as the string 'from matched from buffer.

Either 'to-left and 'to-right can be a function that returns a string.
The function takes user input as argument.
Because isolate uses `string-match' to match user input with 'from,
you can extract groups from left segment by (match-string num user-input).
`user-input' is the name of the argument.")

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

(defun isolate--match-pair (left-segment pair-list)
  "Find matching left and right segment for LEFT-SEGMENT.
Return a cons of (left . right).
LEFT-SEGMENT is not everthing left to region,
but one of the segments separated by `isolate-separator'.

PAIR-LIST is the list in which this function looks for match.

Return (LEFT-SEGMENT . LEFT-SEGMENT) if nothing matches.
This function never returns nil."
  (catch 'return
    (dolist (pair pair-list)
      (let ((from (alist-get 'from pair))
            (to-left (alist-get 'to-left pair))
            (to-right (alist-get 'to-right pair))
            (condition (alist-get 'condition pair)))
        (when (and (or (not condition) ; no condition
                       (and condition (funcall condition left-segment))) ; condition passes
                   (or (and to-left (equal to-left left-segment)) ; to-left exists & exactly equal
                       (string-match (format "^%s$" from) left-segment))) ; from matches
          (setq to-left (or to-left left-segment))
          (throw 'return
                 (cons (if (functionp to-left)
                           (funcall to-left left-segment)
                         to-left)
                       (if (functionp to-right)
                           (funcall to-right left-segment)
                         to-right))))))
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
            (define-key map (kbd "C-c q") #'isolate-add-abort)
            (define-key map (kbd "C-c a") #'isolate-goto-beginning)
            (define-key map (kbd "C-c e") #'isolate-goto-end)
            map)
  (if isolate-add-mode
      (progn
        (isolate--add-setup-marker)
        (isolate--add-setup)
        (add-hook 'post-command-hook #'isolate--add-hook t t)
        (advice-add 'message :around #'isolate--add-echo-advice))
    (isolate--add-cleanup)
    (remove-hook 'post-command-hook #'isolate--add-hook t)
    (advice-remove 'message #'isolate--add-echo-advice))
  (isolate--disable-autoparens))

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

(defvar-local isolate--left-old-length 0
  "Length of left before process.")

(defvar-local isolate--old-cursor-pos 0
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
      (isolate--replace-with actual-left isolate--left-beg isolate--left-end))
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

(defun isolate--disable-autoparens ()
  "Disable paren auto-completion in `isolate-add-mode'."
  (if isolate-add-mode
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
                  (progn (elctric-pair-mode -1) t)
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
  '(((from . "<t>") (to-left . "<[^/]+?>") (to-right . "</.+?>"))
    ((from . "<\\([^ ]+\\).*>")
     (to-left . (lambda (user-input) (format "<%s *.*>" (match-string 1 user-input))))
     (to-right . (lambda (user-input) (format "</%s>" (match-string 1 user-input))))))
  "This is an alist for extended pairs for delete function.
Each element is an alist represents a shortcut.
They have four possible keys: 'from, 'to-left, 'to-right and 'condition.

'from is a regexp string that matches user input.
Like `isolate-pair-list', \"^\" and \"$\" are added to 'from
for sole match.

'condition is a function, just like 'condition in `isolate-pair-list',
it takes the user input (same to 'from's value).
If it exist and return nil, the shorcut will be ignored.

If 'from matches and 'condition passes,
'to-left and 'to-right will be used for searching the pair in buffer.

If 'to-left is ommited, it is considerd the same as 'from.

Either of them can be a regexp string or a function that returns a regexp) string.
The function should take the user entered string as argument.
Like `isolate-pair-list', your can use (match-string num user-input)
to match part of user-input.

As for program logic, 'to will then be matched by 'left-regexp
if it exists, or compared with equal with 'left,
in `isolate-pair-list', if they exactly equal to each other,
and condition passes, the corresponding 'right will be used.")


(defvar-local isolate--search-level 1
  "The level of matching pairs that isolate searches.
Starts from 1, passed to search function as COUNT.
Don't forget to reset to 1 when exit `isolate-delete-mode'.")

(defvar-local isolate--delete-left-segment nil
  "The left segment user entered in `isolate-delete-mode' for deletion.
Don't forget to reset this when exit `isolate-delete-mode'.")

(defvar isolate--search-history nil
  "History of long delete(change) search.")

(defvar-local isolate--search-success nil
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

(defalias 'isolate-long-delete #'isolate-delete-mode)

(defun isolate-delete-search ()
  "Search for pair to delete."
  (interactive)
  
  (setq isolate--delete-left-segment
        (completing-read "Enter left segment: "
                         nil
                         ;; TODO figure out hist
                         nil nil nil))
  (setq isolate--search-history
        (delete isolate--delete-left-segment isolate--search-history))
  (push isolate--delete-left-segment isolate--search-history)
  (when (setq isolate--search-success
              (isolate--search-pair isolate--delete-left-segment))
    (isolate--delete-update-highlight)))

(define-minor-mode isolate-delete-mode
  "Delete surrounding by entering regexp."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c p") #'isolate-search-up)
            (define-key map (kbd "C-c n") #'isolate-search-down)
            (define-key map (kbd "C-c s") #'isolate-delete-search)
            (define-key map (kbd "C-c C-c") #'isolate-delete-finish)
            (define-key map (kbd "C-C q") #'isolate-delete-abort)
            map)
  (if isolate-delete-mode
      (progn
        (isolate-delete-search)
        (advice-add 'message :around #'isolate--delete-echo-advice))
    ;; cleanup
    (isolate--delete-cleanup-overlay)
    (setq isolate--search-level 1
          isolate--delete-left-segment nil
          isolate--search-success (if isolate--changing
                                      isolate--search-success
                                    nil))
    (advice-remove 'message #'isolate--delete-echo-advice)))

(defun isolate-delete-finish ()
  "Exit `isolate-delete-mode'."
  (interactive)
  (when isolate--search-success
    (isolate--replace-with "" isolate--left-beg isolate--left-end)
    (isolate--replace-with "" isolate--right-beg isolate--right-end))
  (isolate-delete-mode -1))

(defun isolate-delete-abort ()
  "Exit `isolate-delete-mode' and don't delete."
  (interactive)
  (isolate-delete-mode -1))

(defun isolate-search-up ()
  "Search one level of matching pairs up."
  (interactive)
  (setq isolate--search-level (1+ isolate--search-level))
  (if (isolate--search-pair isolate--delete-left-segment isolate--search-level)
      (progn
        (isolate--delete-update-highlight)
        (message "Up one level"))
    ;; failed, set level back
    (setq isolate--search-level (1- isolate--search-level))
    (message "No further match")))

(defun isolate-search-down ()
  "Search one level of matching pairs down."
  (interactive)
  (setq isolate--search-level (1- isolate--search-level))
  (if (and (>= isolate--search-level 1)
           (isolate--search-pair isolate--delete-left-segment isolate--search-level))
      (progn
        (isolate--delete-update-highlight)
        (message "Down one level"))
    ;; failed, set level back
    (setq isolate--search-level (1+ isolate--search-level))
    (message "No furher match")))

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

(defun isolate--find-balance-pair (left-regexp right-regexp &optional point count)
  "Find the COUNT'th balanced matching pair (LEFT-REGEXP & RIGHT-REGEXP)
centered around POINT.
Count starts from 1.
Nil or omitted COUNT means 0.
Nil or omitted POINT means (point).

Return t if successed, nil if failed."
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
        t))))

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
                     (append isolate-delete-extended-pair-list isolate-pair-list)))
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

(defvar-local isolate--changing nil
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
  (isolate-delete-mode))

;;; Provide

(provide 'isolate)

;;; isolate.el ends here
