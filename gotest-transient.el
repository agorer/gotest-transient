(require 'transient)

(defgroup gotest-transient nil
  "Go test integration with transient ui."
  :group 'go
  :prefix "gotest-transient-")

(cl-defstruct (gotest-transient--node (:constructor gotest-transient--node-create))
  package
  test
  subtest
  output
  status
  elapsed
  expanded)

(define-transient-command gotest-transient-dispatch ()
  "Show popup for running go tests."
  :value '("")
  ["Options"
   [("-nc" "no cache" "-count=1")
    (gotest-transient:-t)
    ("-f" "fail fast" "-failfast")]]
  ["Run tests"
   [("p" "all project" gotest-transient--run-project)]
   [("f" "current file" gotest-transient--run-file)]
   [("tt" "current test" gotest-transient--run-test)]
   [("tr" "current subtest" gotest-transient--run-subtest)]])

(defun gotest-transient--run-project (&optional args)
  "Run all tests for the current project."
  (interactive (list (transient-args 'gotest-transient-dispatch)))
  (transient-save)
  (let ((project-root (gotest-transient--get-project-root))
        (cmd-args (append args '("./..."))))
    (gotest-transient--run-tests project-root cmd-args)))

(defun gotest-transient--read-quoted-argument-for-short-flag (prompt initial-input history)
  "Read a quoted string for use as a argument after a short-form command line flag."
  (let* ((input (read-from-minibuffer prompt initial-input nil nil history))
         (quoted-input input)
         (formatted-input (format " %s" quoted-input)))
    formatted-input))

(transient-define-argument gotest-transient:-t ()
  :description "build tags"
  :class 'transient-option
  :argument "-tags"
  :allow-empty nil
  :key "-t"
  :reader 'gotest-transient--read-quoted-argument-for-short-flag)

(defun gotest-transient--get-project-root ()
  "Return the root directory of the project (the one that contains go.mod)."
  (locate-dominating-file (buffer-file-name) "go.mod"))

(setq gotest-transient--results-buffer nil)
(setq gotest-transient--results-ewoc nil)
(setq gotest-transient--project-root nil)
(defun gotest-transient--run-tests (project-root cmd-args)
  "Run the command line with CMDARGS and show results on buffer."
  (let ((cmdline (append '("go" "test" "-json") cmd-args)))
    (setq gotest-transient--results-buffer (get-buffer-create "*Go test*"))
    (display-buffer gotest-transient--results-buffer)
    (setq gotest-transient--previous-cmd-args cmd-args)
    (setq gotest-transient--project-root project-root)

    (with-current-buffer gotest-transient--results-buffer
      (let ((buffer-read-only nil))
        (erase-buffer))
      (buffer-disable-undo)
      (gotest-mode)

      (setq gotest-transient--results-ewoc (ewoc-create #'gotest-transient--pp-node nil nil t))
      
      (let ((default-directory project-root))
        (setq gotest-transient--stderr-buffer (generate-new-buffer "*Go test tmp (stderr)*"))
        (setq gotest-transient--stderr-process
              (make-pipe-process :name "*Go test (stderr)*"
                                 :buffer gotest-transient--stderr-buffer
                                 :sentinel #'gotest-transient--stderr-sentinel
                                 :filter #'gotest-transient--read-stderr))

        (setq gotest-transient--process-buffer (generate-new-buffer "*Go test tmp*"))
        (setq gotest-transient--process
              (make-process :name "*Go test (stdout)*"
                            :buffer gotest-transient--process-buffer
                            :sentinel #'gotest-transient--process-sentinel
                            :filter (gotest-transient--process-filter-line-buffer
                                     #'gotest-transient--read-stdout)
                            :stderr gotest-transient--stderr-process
                            :command cmdline))))))

(defun gotest-transient--stderr-sentinel (proc event)
  "Acts as the process sentinel for stderr proccess (ignoring all events)."
  (kill-buffer (process-buffer proc)))

(defconst NEWLINE (string-to-char "\n"))

(defun gotest-transient--string-indexof (string char start)
  "Return the index of CHAR within STRING from START."
  (let ((i start)
        (index nil))
    (while (and (< i (length string)) (not index))
      (when (eq char (aref string i))
          (setq index i))
      (setq i (1+ i)))
    index))

(setq gotest-transient--buffer-string nil)
(defun gotest-transient--process-filter-line-buffer (real-filter)
  "Creates a process filter function that is called for lines instead of when flushing."
  (setq gotest-transient--buffer-string "")
  `(lambda (proc string)
     (setq string (concat gotest-transient--buffer-string string))
     (let ((start 0) new-start)
       (while (setf new-start (gotest-transient--string-indexof string NEWLINE start))
         ;;does not include newline
         (,real-filter proc (substring string start new-start))
         (setf start (1+ new-start))) ;;past newline
         
       (setq gotest-transient--buffer-string (substring string start)))))

(defun gotest-transient--read-stderr (proc line)
  "Filters the stderr output from the go compiler."
  (let ((inhibit-quit t))
    (with-local-quit
      (with-current-buffer gotest-transient--results-buffer
        (let ((buffer-read-only nil))
          (insert line))))))
  
(defun gotest-transient--process-sentinel (proc event)
  "Acts as the process sentinel for the run process."
  (with-local-quit
    (with-current-buffer gotest-transient--results-buffer
      (let ((buffer-read-only nil))
        (end-of-buffer)
        (cond
         ((string= event "finished\n")
          (insert "\n====================================\n")
          (insert "The go test command has finished.")
          (insert "\n====================================\n"))
         ((s-prefix-p "exited abnormally" event)
          (insert "\n====================================\n")
          (insert "The go test command has finished with some errors.")
          (insert "\n====================================\n"))
         (t
          (insert "\n====================================\n")
          (insert "The go test command has finished with an abnormal status: ")
          (insert event)
          (insert "\n====================================\n"))))))
  (kill-buffer (process-buffer proc))
  (select-window (get-buffer-window gotest-transient--results-buffer)))

(defun gotest-transient--read-stdout (proc input)
  "Filters the stderr output from the go compiler."
  (let ((inhibit-quit t)
        (event (json-parse-string input)))
    (with-local-quit
      (with-current-buffer gotest-transient--results-buffer
        (when (gotest-transient--is-not-package-event event)
          (cond ((gotest-transient--is-run-event event)
                 (gotest-transient--create-node event))
                ((gotest-transient--is-output-event event)
                 (gotest-transient--add-output event))
                ((gotest-transient--is-result-event event)
                 (gotest-transient--update-status event))))))))
 
(defun gotest-transient--event-action (event)
  (gethash "Action" event))

(defun gotest-transient--is-run-event (event)
  (equal "run" (gotest-transient--event-action event)))

(defun gotest-transient--is-output-event (event)
  (equal "output" (gotest-transient--event-action event)))

(defun gotest-transient--is-result-event (event)
  (let ((action (gotest-transient--event-action event)))
    (or (equal "fail" action) (equal "pass" action))))

(defun gotest-transient--is-not-package-event (event)
  (not (equal (gethash "Test" event) nil)))

(defun gotest-transient--event-package (event)
  (gethash "Package" event))

(defun gotest-transient--event-test (event)
  (let ((data (gethash "Test" event)))
    (car (split-string data "/"))))

(defun gotest-transient--event-subtest (event)
  (let ((data (gethash "Test" event)))
    (car (cdr (split-string data "/")))))

(defun gotest-transient--event-output (event)
  (gethash "Output" event))

(defun gotest-transient--event-elapsed (event)
  (gethash "Elapsed" event))

(defun gotest-transient--find-node (package test subtest)
  (let ((current-node (ewoc-nth gotest-transient--results-ewoc 0)))
      (while (when (not (equal current-node nil))
               (not (and (equal (gotest-transient--node-package (ewoc-data current-node)) package)
                         (equal (gotest-transient--node-test (ewoc-data current-node)) test)
                         (equal (gotest-transient--node-subtest (ewoc-data current-node)) subtest))))
        (setq current-node (ewoc-next gotest-transient--results-ewoc current-node)))
    current-node))

(defun gotest-transient--create-node (event)
  (let* ((package-name (gotest-transient--event-package event))
         (test-name (gotest-transient--event-test event))
         (subtest-name (gotest-transient--event-subtest event)))
    (ewoc-enter-last gotest-transient--results-ewoc
                     (gotest-transient--node-create
                      :package package-name
                      :test test-name
                      :subtest subtest-name
                      :output '()
                      :status "running"
                      :elapsed nil
                      :expanded nil))))
                      
(defun gotest-transient--add-output (event)
  (let* ((output-text (gotest-transient--event-output event))
         (package-name (gotest-transient--event-package event))
         (test-name (gotest-transient--event-test event))
         (subtest-name (gotest-transient--event-subtest event))
         (node (gotest-transient--find-node package-name test-name subtest-name))
         (node-data (ewoc-data node))
         (node-output (gotest-transient--node-output node-data)))
    (setf (gotest-transient--node-output node-data) (add-to-list 'node-output output-text))
    (ewoc-invalidate gotest-transient--results-ewoc node)))

(defun gotest-transient--update-status (event)
  (let* ((elapsed (gotest-transient--event-elapsed event))
         (status (gotest-transient--event-action event))
         (package-name (gotest-transient--event-package event))
         (test-name (gotest-transient--event-test event))
         (subtest-name (gotest-transient--event-subtest event))
         (node (gotest-transient--find-node package-name test-name subtest-name))
         (node-data (ewoc-data node)))
    (setf (gotest-transient--node-status node-data) status)
    (ewoc-invalidate gotest-transient--results-ewoc node)))

(defun gotest-transient-toggle-node ()
  (interactive)
  (let* ((node (ewoc-locate gotest-transient--results-ewoc))
         (node-data (ewoc-data node))
         (expanded (gotest-transient--node-expanded node-data)))
    (setf (gotest-transient--node-expanded node-data) (not expanded))
    (ewoc-invalidate gotest-transient--results-ewoc node)))

(defun gotest-transient-goto-next-error ()
  (interactive)
  (let ((current-node (ewoc-locate gotest-transient--results-ewoc)))
    (when current-node
      (setq current-node (ewoc-next gotest-transient--results-ewoc current-node)))

    (while (when (not (equal current-node nil))
             (not (equal (gotest-transient--node-status (ewoc-data current-node)) "fail")))
      (setq current-node (ewoc-next gotest-transient--results-ewoc current-node)))

    (when current-node
      (ewoc-goto-node gotest-transient--results-ewoc current-node))))

(defun gotest-transient--pp-node (node)
  (if (gotest-transient--node-subtest node)
      (gotest-transient--pp-subtest node)
    (gotest-transient--pp-test node)))

(defun gotest-transient--pp-test (node)
  (insert (upcase (gotest-transient--node-status node)))
  (insert " - ")
  (insert (gotest-transient--node-package node))
  (insert " - ")
  (insert (gotest-transient--node-test node))
  (insert "\n")
  (when (gotest-transient--node-expanded node)
    (gotest-transient--pp-output node)))
        
(defun gotest-transient--pp-subtest (node)
  (gotest-transient--insert-indented (upcase (gotest-transient--node-status node)) 1)
  (insert " - ")
  (insert (gotest-transient--node-package node))
  (insert " - ")
  (insert (gotest-transient--node-test node))
  (insert "/")
  (insert (gotest-transient--node-subtest node))
  (insert "\n")
  (when (gotest-transient--node-expanded node)
    (gotest-transient--pp-output node)))

(defun gotest-transient--pp-output (node)
  (let* ((output-lines (gotest-transient--node-output node))
         (filtered-lines (seq-filter 'gotest-transient--is-not-redundant output-lines)))
    (if filtered-lines
        (dolist (output-line filtered-lines)
          (gotest-transient--insert-indented output-line 2))
      (gotest-transient--insert-indented "*** no output ***\n" 2))))

(defun gotest-transient--is-not-redundant (line)
  (not (or (string-match-p "--- PASS" line)
           (string-match-p "--- FAIL" line)
           (string-match-p "=== RUN" line))))

(defun gotest-transient--insert-indented (text number-of-tabs)
  (dotimes (i number-of-tabs)
    (insert "\t"))
  (insert text))

(define-derived-mode gotest-mode special-mode "gotest-mode"
  "Major mode for running go tests."
  (use-local-map gotest-transient-mode-map)
  (font-lock-add-keywords nil gotest-font-lock-keywords)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory "*Go test*")
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky))

(defvar gotest-transient-mode-map
  (let ((m (make-sparse-keymap)))
    (suppress-keymap m)
    ;; key bindings go here
    (define-key m (kbd "k") 'kill-buffer)
    (define-key m (kbd "q") 'quit-window)
    (define-key m (kbd "TAB") 'gotest-transient-toggle-node)
    (define-key m (kbd "RET") 'gotest-transient-goto-error)
    (define-key m [(f2)] 'gotest-transient-goto-next-error)
    m))

(defface gotest--pass-face '((t :foreground "green"))
  "Face for displaying the status of a passing test."
  :group 'gotest-transient)

(defface gotest--fail-face '((t :foreground "pink" :weight bold))
  "Face for displaying the status of a failed test."
  :group 'gotest-transient)

(defconst gotest-font-lock-keywords
  '(("error\\:" . 'gotest--fail-face)
    ("FAIL" . 'gotest--fail-face)
    ("FAIL" . 'gotest--fail-face)
    ("PASS" . 'gotest--pass-face))
  "Minimal highlighting expressions for go test results.")

(defun gotest-transient--run-file (&optional args)
  "Run all tests for the current file."
  (interactive
   (list
    (transient-args 'gotest-transient-dispatch)))
  (transient-save)
  (let* ((project-root (gotest-transient--get-project-root))
         (file-tests (gotest-transient--get-current-file-tests))
         (cmd-args (append args '("-run") `(,file-tests) '("./..."))))
    (gotest-transient--run-tests project-root cmd-args)))

(defun gotest-transient--get-current-file-tests ()
  "Get tests that should be run for the current file."
  (let ((buffer (gotest-transient--get-test-buffer)))
    (when buffer
      (with-current-buffer buffer
	    (save-excursion
	      (goto-char (point-min))
	      (when (string-match "\.go$" buffer-file-name)
            (let ((regex "^[[:space:]]*func[[:space:]]*\\(Test[^(]+\\)")
                  result)
	          (while (re-search-forward regex nil t)
		        (let ((data (buffer-substring-no-properties
                             (match-beginning 1) (match-end 1))))
                  (setq result (append result (list data)))))
	          (mapconcat 'identity result "|"))))))))

(defun gotest-transient--get-test-buffer ()
  "Get the test buffer for the FILE."
  (if (string-match "_test\.go$" buffer-file-name)
      (current-buffer)
    (let ((ff-always-try-to-create nil)
	  (filename (ff-other-file-name)))
      (when filename
	    (find-file-noselect filename)))))

(defun gotest-transient--run-test (&optional args)
  "Run current test"
  (interactive
   (list
    (transient-args 'gotest-transient-dispatch)))
  (transient-save)
  (let* ((project-root (gotest-transient--get-project-root))
         (test-name (gotest-transient--get-current-test))
         (cmd-args (append args '("-run") `(,test-name) '("./..."))))
    (gotest-transient--run-tests project-root cmd-args)))

(defun gotest-transient--get-current-test ()
  "Get the name of the test the cursor is on"
  (let ((buffer (gotest-transient--get-test-buffer))
        (test-function-regex "func[[:space:]]*\\(Test[^(]+\\)"))
    (with-current-buffer buffer
      (save-excursion
        (end-of-line)
        (if (search-backward-regexp test-function-regex nil t)
            (match-string-no-properties 1)
          (error "Unable to find a test"))))))

(defun gotest-transient--run-subtest (&optional args)
  "Run current subtest"
  (interactive
   (list
    (transient-args 'gotest-transient-dispatch)))
  (transient-save)
  (let* ((project-root (gotest-transient--get-project-root))
         (test-name (gotest-transient--get-current-test))
         (subtest-name (gotest-transient--get-current-subtest test-name))
         (full-name (concat test-name "/" subtest-name))
         (cmd-args (append args '("-run") `(,full-name) '("./..."))))
    (gotest-transient--run-tests project-root cmd-args)))
  
(defun gotest-transient--get-current-subtest (test-name)
  "Get the name of the subtest the cursor is on."
  (let ((buffer (gotest-transient--get-test-buffer))
        (subtest-regex "t.Run(\"\\([^\"]+\\)"))
    (with-current-buffer buffer
      (save-excursion
        (end-of-line)
        (if (search-backward-regexp subtest-regex nil t)
            (s-replace " " "_" (match-string-no-properties 1))
          (error "Unable to find a subtest"))))))

(setq gotest-transient--previous-cmd-args nil)
(defun gotest-transient-run-previous-command ()
  "Run the last command executed if any."
  (interactive)
  
  (message "Running command: go test %s" (s-join " " gotest-transient--previous-cmd-args))
  (let ((project-root (gotest-transient--get-project-root)))
    (if (bound-and-true-p gotest-transient--previous-cmd-args)
        (gotest-transient--run-tests project-root gotest-transient--previous-cmd-args)
      (message "No tests were run previously"))))

(defun gotest-transient-goto-error ()
  "Open / goto buffer where the error in cursor is located."
  (interactive)
  (let* ((current-node (ewoc-locate gotest-transient--results-ewoc))
         (package (gotest-transient--node-package (ewoc-data current-node)))
         (test-folder (gotest-transient--get-package-folder package))
         (error-file (gotest-transient--get-error-file current-node))
         (full-path (concat test-folder "/" (gotest-transient--file-name error-file))))
    (find-file-other-window full-path)
    (goto-line (gotest-transient--file-line error-file))))

(defun gotest-transient--file-name (file-string)
  (car (split-string file-string ":")))

(defun gotest-transient--file-line (file-string)
  (string-to-number (car (cdr (split-string file-string ":")))))

(defun gotest-transient--get-package-folder (package)
    (let* ((base-module (gotest-transient--get-base-module gotest-transient--project-root))
           (relative-folder (string-remove-prefix base-module package)))
      (concat (string-remove-suffix "/" gotest-transient--project-root) relative-folder)))

(defun gotest-transient--get-base-module (project-root)
  "Gets the base module of the project from go-mod"
  (let ((gomod-buffer (find-file-noselect (concat project-root "/go.mod")))
        (regex "^module \\([^\n]+\\)")
        (base-module nil))
    (with-current-buffer gomod-buffer
      (if (search-forward-regexp regex nil t)
          (setq base-module (match-string-no-properties 1))
        (error "No base module found on go.mod"))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer))
      base-module)))

(defun gotest-transient--get-error-file (node)
  (let* ((output (gotest-transient--node-output (ewoc-data node)))
         (error-file-line (car (seq-filter 'gotest-transient--error-file-name-p output)))
         (regex "\\([^ \t]+\\.go:[0-9]+\\)"))
    (when (not error-file-line)
      (error "No error file found on test output"))
    (if (string-match regex error-file-line)
        (match-string-no-properties 1 error-file-line)
      (error "No error file found on test output."))))

(defun gotest-transient--error-file-name-p (line)
  (message line)
  (string-match-p "\\.go:[0-9]+:" line))
  
(provide 'gotest-transient)
