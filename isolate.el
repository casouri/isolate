;;; isolate.el --- Fully customizable, easy-to-use surrounding tool.

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
  '(((left . "`") (right . "'") (condition . (lambda (_) (if (equal major-mode 'emacs-lisp-mode) t nil))))
    ((left . "(") (right . ")"))
    ((left . "\\[") (right . "]"))
    ((left . "{") (right . "}"))
    ;; TODO remove this test entry when finished
    ((left . "test") (right-regexp . "regexp-test"))
    ((left . "<") (right . ">"))
    ((left . "<\\(.+?\\) .+?>") (right . (lambda (left) (format "</%s>" (match-string 1 left)))))
    )
  "Matching pairs.
Each element is an alist with four possible keys: left, right, right-regexp and condition.
Only 'left and 'right are required.

Value of 'left and 'right defines left and right segments.
Both segements are strings. Left segement is treated as a regexp pattern.
\"^\" and \"$\" are added automatically to the patterns.
Also don't forget regexp escapes.

Right segement can be a function that returns a string.
The function will be given the left segement user inserted as argument.
Because isolate uses `string-match' to match left segment,
you can extract groups from left segment by (match-string num left-segment).
`left-segment' is the name of the argument.

'right-regex is used by `isolate-long-delete' when it searches pairs
by the left regexp you entered (or translated from `isolate-delete-regexp-shortcut-list').
When searching for the right segment,
isolate will use the plain 'right string if 'right-regexp does not exist.
If 'right-regexp does exist, isolate uses it to match right segment.

Like 'right, you can use a function that returns a string for 'right-regexp.
The function recieves the matched left segment.

Value of 'condition is a function that takes a string
which is the left segement user inserted.
How or whether to use it is depend on you.
If condition exists, isolate only match the pair
when condition function returns t.
This function can be used to check major mode or some special rules.

'condition takes effect on both adding and deleting (therfore also changing).")

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

(defun isolate--match-pair (left-segment &optional use-regexp)
  "Find matching right segment for LEFT-SEGMENT.
LEFT segment is not everthing left to region,
but one of the segments separated by `isolate-separator'.

If USE-REGEXP is non-nil, use 'right-regexp instead of plain 'right.
Detail in `isolate-pair-list'.
If the matched pair doesn't have a 'right-regexp key,
'right will be used instead.

This option is used by `isolate-delete' functions for matching.

Return LEFT-SEGMENT itself if nothing matches.
So this function never returns nil."
  (catch 'return
    (dolist (pair isolate-pair-list)
      (let ((left-pattern (alist-get 'left pair))
            (right (alist-get 'right pair))
            (right-regexp (alist-get 'right-regexp pair))
            (condition (alist-get 'condition pair)))
        (if (or (not condition) (and condition (funcall condition left-segment)))
            ;; 1. further matching
            (if (string-match (format "^%s$" left-pattern) left-segment)
                ;; 2. match
                (throw 'return
                       (if (and right-regexp use-regexp)
                           ;; right-regexp
                           (if (functionp right-regexp)
                               (funcall right-regexp left-segment)
                             right-regexp)
                         ;; plain right
                         (if (functionp right)
                             (funcall right left-segment)
                           right)))
              ;; 2. no match, go to next pair
              nil)
          ;; 1. condition false, go to next pair
          nil)))
    ;; if no one matches, return left itself
    left-segment))

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
    (goto-char ,left-beg)
    (setq isolate--left-beg (point-marker))
    
    (goto-char ,left-end)
    (setq isolate--left-end (point-marker))
    (set-marker-insertion-type isolate--left-end t)
    
    (goto-char ,right-beg)
    (setq isolate--right-beg (point-marker))
    
    (goto-char ,righ-end)
    (setq isolate--right-end (point-marker))
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
    (let ((left (isolate--translate-quick-shortcut
                 (char-to-string left-segment)))
          right-list
          right)
      (goto-char isolate--left-beg)
      (insert left)
      ;; construct right
      (dolist (segment (split-string left isolate-separator))
        (isolate--push (isolate--match-pair segment) right-list))
      (setq right (apply 'concat right-list))
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
            map)
  (if isolate-add-mode
      (progn
        (isolate--add-setup-marker)
        (isolate--add-setup)
        (add-hook 'post-command-hook #'isolate--add t t)
        (advice-add 'message :around #'isolate--add-echo-advice))
    (isolate--add-cleanup)
    (remove-hook 'post-command-hook #'isolate--add t)
    (advice-remove 'message #'isolate--add-echo-advice))
  (isolate--disable-autoparens))


;;;; Function

(defun isolate--add ()
  "Add isolation to region. This function is used in `isolate-add-mode'.
Use `isolate-quick-add' for interactive use."

  (save-excursion
    (let* ((left (buffer-substring-no-properties
                  isolate--left-beg isolate--left-end))
           (left-list (split-string left isolate-separator))
           (right nil)
           (right-list nil))
      ;; match left and construct right
      (dolist (segment left-list)
        (isolate--push (isolate--match-pair segment) right-list))
      ;; insert right
      (setq right (apply 'concat right-list))
      (isolate--replace-with right isolate--right-beg isolate--right-end))))


;;;; Helper

(defun isolate--add-echo-advice (old-func &rest arg)
  "Indicate user that add mode is active. OLD-FUNC is `message'. ARG are args."
  (funcall old-func "[ ADD ]   [ Abort: C-c q ] [ Finish: C-c C-c ]\n\n%s" (apply 'format arg)))


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
  (when (bound-and-true-p evil-mode)
    (evil-insert-state))
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
      (replace-match ""))
    ;; insert
    (when (bound-and-true-p evil-mode)
      (evil-normal-state))))

(defun isolate--disable-autoparens ()
  "Disable paren auto-completion in `isolate-add-mode'."
  (if isolate-add-mode
      (progn
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
    (when (bound-and-true-p isolate--smartparens)
      (smartparens-mode))
    (when (bound-and-true-p isolate--paredit)
      (paredit-mode))
    (when (bound-and-true-p isolate--electric-pair)
      (electric-pair-mode))))

;;; Delete

;;;; Variable

(defvar isolate-delete-regexp-shortcut-list
  '(((from . "<t>") (to . "<\\(.+?\\) .+?>")))
  "This is an alist for shortcuts.
Each element is an alist represents a shortcut.
They have three keys: 'from, 'to and 'condition.
'from is replaced by 'to.
'condition is a function, just like 'condition in `isolate-pair-list',
it takes the user input (same to 'from's value).
If it exist and return nil, the shorcut will be ignored.

When user use `isolate-long-delete',
these shortcuts can save them from entering long and complicated regexp.

For example, when user enters \"<t>\",
isolate translate it to \"^<\\(.+?\\) .+?>$\"
which matchs html tags (left).")

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
  (if (isolate--search-segment
       (regexp-quote
        (isolate--translate-quick-shortcut
         (char-to-string left-segment))))
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
  (setq isolate--search-success
        (isolate--search-segment isolate--delete-left-segment))
  (isolate--delete-update-highlight))

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
  (if (isolate--search-segment isolate--delete-left-segment isolate--search-level)
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
           (isolate--search-segment isolate--delete-left-segment isolate--search-level))
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

(defun isolate--translate-shortcut (segment)
  "Translate SEGMENT to full regexp.
Return SEGMENT if no match."
  (catch 'return
    (dolist (shortcut isolate-delete-regexp-shortcut-list)
      (let ((from (alist-get 'from shortcut))
            (to (alist-get 'to shortcut))
            (condition (alist-get 'condition shortcut)))
        (when (and (equal from segment)
                   ;; 'condition exist and returns t
                   ;; or doesn't exist
                   (or (not condition)
                       (and condition (funcall condition segment))))
          (throw 'return to))))
    ;; return segment if no match
    segment))

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
            (if (> new-right-count old-right-count)
                (setq old-right-count new-right-count)
              (setq count (1- count))))
        ;; it's time to find right match.
        ;; setup right-beg and righ-end
        (isolate--jump-forward)
        ;; jump right until balanced
        (while (let ((left-count (isolate--count-match left-regexp left-beg right-beg))
                     (right-count (isolate--count-match right-regexp left-end right-end)))
                 (> (- left-count right-count) 0))
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

(defun isolate--search-segment (left-segment &optional count)
  "Seach COUNT'th LEFT-SEGMENT and matching right segment.
Return t if matched, nil if not.
Note that COUNT starts from 1, not 0. 0 and nil are treated as 1.

The function sets `isolate--left-beg', `isolate--left-end',
`isolate--right-beg' and `isolate--right-end'.

COUNT is like COUNT in `search-backward-regexp'."
  (catch 'return
    (let ((count (if (or (equal count nil) (equal count 0)) 1 count))
          ;; special treatment for `isolate-quick-delete'
          (left-list (split-string left-segment isolate-separator))
          left-regexp-list
          left-regexp
          right-regexp
          right-regexp-list
          (left-count 0)
          (right-count 0))
      ;; construct left and right regexp
      (dolist (segment left-list)
        (isolate--append left-regexp-list (isolate--translate-shortcut segment))
        (isolate--push (isolate--match-pair (save-excursion
                                              (if (re-search-backward
                                                   (isolate--translate-shortcut segment) nil t)
                                                  (match-string 0)
                                                (throw 'return nil)))
                                            t)
                       right-regexp-list))
      ;; construct
      (setq left-regexp (apply 'concat left-regexp-list))
      (setq right-regexp (apply 'concat right-regexp-list))
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
