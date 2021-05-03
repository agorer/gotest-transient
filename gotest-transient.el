(require 'transient)

(defgroup gotest-transient nil
  "Go test integration with transient ui."
  :group 'go
  :prefix "gotest-transient-")

(cl-defstruct (gtt--node (:constructor gtt--node-create))
  package
  test
  subtest
  output
  status
  elapsed
  expanded)

(define-transient-command gtt-dispatch ()
  "Show popup for running go tests."
  :value '("")
  ["Options"
   [("-nc" "no cache" "-count=1")
    (gotest-transient:-t)
    ("-f" "fail fast" "-failfast")]]
  ["Run tests"
   [("p" "all project" gtt--run-project)]
   [("f" "current file" gtt--run-file)]
   [("tt" "current test" gtt--run-test)]
   [("tr" "current subtest" gtt--run-subtest)]])

(defun gtt--run-project (&optional args)
  "Run all tests for the current project."
  (interactive (list (transient-args 'gtt-dispatch)))
  (transient-save)
  (let ((project-root (gtt--get-project-root))
        (cmd-args (append args '("./..."))))
    (gtt--run-tests project-root cmd-args)))

(defun gtt--read-quoted-argument-for-short-flag (prompt initial-input history)
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
  :reader 'gtt--read-quoted-argument-for-short-flag)

(defun gtt--get-project-root ()
  "Return the root directory of the project (the one that contains go.mod)."
  (locate-dominating-file (buffer-file-name) "go.mod"))

(setq gtt--results-buffer nil)
(setq gtt--results-ewoc nil)
(setq gtt--project-root nil)
(defun gtt--run-tests (project-root cmd-args)
  "Run the command line with CMDARGS and show results on buffer."
  (let ((cmdline (append '("go" "test" "-json") cmd-args)))
    (setq gtt--results-buffer (get-buffer-create "*Go test*"))
    (display-buffer gtt--results-buffer)
    (setq gtt--previous-cmd-args cmd-args)
    (setq gtt--project-root project-root)

    (with-current-buffer gtt--results-buffer
      (let ((buffer-read-only nil))
        (erase-buffer))
      (buffer-disable-undo)
      (gotest-mode)

      (setq gtt--results-ewoc (ewoc-create #'gtt--pp-node nil nil t))
      
      (let ((default-directory project-root))
        (setq gtt--stderr-buffer (generate-new-buffer "*Go test tmp (stderr)*"))
        (setq gtt--stderr-process
              (make-pipe-process :name "*Go test (stderr)*"
                                 :buffer gtt--stderr-buffer
                                 :sentinel #'gtt--stderr-sentinel
                                 :filter #'gtt--read-stderr))

        (setq gtt--process-buffer (generate-new-buffer "*Go test tmp*"))
        (setq gtt--process
              (make-process :name "*Go test (stdout)*"
                            :buffer gtt--process-buffer
                            :sentinel #'gtt--process-sentinel
                            :filter (gtt--process-filter-line-buffer
                                     #'gtt--read-stdout)
                            :stderr gtt--stderr-process
                            :command cmdline))))))

(defun gtt--stderr-sentinel (proc event)
  "Acts as the process sentinel for stderr proccess (ignoring all events)."
  (kill-buffer (process-buffer proc)))

(defconst NEWLINE (string-to-char "\n"))

(defun gtt--string-indexof (string char start)
  "Return the index of CHAR within STRING from START."
  (let ((i start)
        (index nil))
    (while (and (< i (length string)) (not index))
      (when (eq char (aref string i))
          (setq index i))
      (setq i (1+ i)))
    index))

(setq gtt--buffer-string nil)
(defun gtt--process-filter-line-buffer (real-filter)
  "Creates a process filter function that is called for lines instead of when flushing."
  (setq gtt--buffer-string "")
  `(lambda (proc string)
     (setq string (concat gtt--buffer-string string))
     (let ((start 0) new-start)
       (while (setf new-start (gtt--string-indexof string NEWLINE start))
         ;;does not include newline
         (,real-filter proc (substring string start new-start))
         (setf start (1+ new-start))) ;;past newline
         
       (setq gtt--buffer-string (substring string start)))))

(defun gtt--read-stderr (proc line)
  "Filters the stderr output from the go compiler."
  (let ((inhibit-quit t))
    (with-local-quit
      (with-current-buffer gtt--results-buffer
        (let ((buffer-read-only nil))
          (insert line))))))
  
(defun gtt--process-sentinel (proc event)
  "Acts as the process sentinel for the run process."
  (with-local-quit
    (with-current-buffer gtt--results-buffer
      (let ((buffer-read-only nil))
        (end-of-buffer)
        (cond
         ((string= event "finished\n")
          (insert "\n====================================\n")
          (insert "The go test command has finished.")
          (insert "\n====================================\n"))
         ((s-prefix-p "exited abnormally" event)
          (insert "\n===================================================\n")
          (insert "The go test command has finished with some errors.")
          (insert "\n===================================================\n"))
         (t
          (insert "\n=================================================================\n")
          (insert "The go test command has finished with an abnormal status: ")
          (insert event)
          (insert "\n=================================================================\n"))))))
  (kill-buffer (process-buffer proc))
  (select-window (get-buffer-window gtt--results-buffer)))

(defun gtt--read-stdout (proc input)
  "Filters the stderr output from the go compiler."
  (let ((inhibit-quit t)
        (event (json-parse-string input)))
    (with-local-quit
      (with-current-buffer gtt--results-buffer
        (when (gtt--is-not-package-event event)
          (cond ((gtt--is-run-event event)
                 (gtt--create-node event))
                ((gtt--is-output-event event)
                 (gtt--add-output event))
                ((gtt--is-result-event event)
                 (gtt--update-status event))))))))
 
(defun gtt--event-action (event)
  (gethash "Action" event))

(defun gtt--is-run-event (event)
  (equal "run" (gtt--event-action event)))

(defun gtt--is-output-event (event)
  (equal "output" (gtt--event-action event)))

(defun gtt--is-result-event (event)
  (let ((action (gtt--event-action event)))
    (or (equal "fail" action) (equal "pass" action))))

(defun gtt--is-not-package-event (event)
  (not (equal (gethash "Test" event) nil)))

(defun gtt--event-package (event)
  (gethash "Package" event))

(defun gtt--event-test (event)
  (let ((data (gethash "Test" event)))
    (car (split-string data "/"))))

(defun gtt--event-subtest (event)
  (let ((data (gethash "Test" event)))
    (car (cdr (split-string data "/")))))

(defun gtt--event-output (event)
  (gethash "Output" event))

(defun gtt--event-elapsed (event)
  (gethash "Elapsed" event))

(defun gtt--find-node (package test subtest)
  (let ((current-node (ewoc-nth gtt--results-ewoc 0)))
      (while (when (not (equal current-node nil))
               (not (and (equal (gtt--node-package (ewoc-data current-node)) package)
                         (equal (gtt--node-test (ewoc-data current-node)) test)
                         (equal (gtt--node-subtest (ewoc-data current-node)) subtest))))
        (setq current-node (ewoc-next gtt--results-ewoc current-node)))
    current-node))

(defun gtt--create-node (event)
  (let* ((package-name (gtt--event-package event))
         (test-name (gtt--event-test event))
         (subtest-name (gtt--event-subtest event)))
    (ewoc-enter-last gtt--results-ewoc
                     (gtt--node-create
                      :package package-name
                      :test test-name
                      :subtest subtest-name
                      :output '()
                      :status "running"
                      :elapsed nil
                      :expanded nil))))
                      
(defun gtt--add-output (event)
  (let* ((output-text (gtt--event-output event))
         (package-name (gtt--event-package event))
         (test-name (gtt--event-test event))
         (subtest-name (gtt--event-subtest event))
         (node (gtt--find-node package-name test-name subtest-name))
         (node-data (ewoc-data node))
         (node-output (gtt--node-output node-data)))
    (setf (gtt--node-output node-data) (add-to-list 'node-output output-text t))
    (ewoc-invalidate gtt--results-ewoc node)))

(defun gtt--update-status (event)
  (let* ((elapsed (gtt--event-elapsed event))
         (status (gtt--event-action event))
         (package-name (gtt--event-package event))
         (test-name (gtt--event-test event))
         (subtest-name (gtt--event-subtest event))
         (node (gtt--find-node package-name test-name subtest-name))
         (node-data (ewoc-data node)))
    (setf (gtt--node-status node-data) status)
    (ewoc-invalidate gtt--results-ewoc node)))

(defun gtt-toggle-node ()
  (interactive)
  (let* ((node (ewoc-locate gtt--results-ewoc))
         (node-data (ewoc-data node))
         (expanded (gtt--node-expanded node-data)))
    (setf (gtt--node-expanded node-data) (not expanded))
    (ewoc-invalidate gtt--results-ewoc node)))

(defun gtt-goto-next-error ()
  (interactive)
  (let ((current-node (ewoc-locate gtt--results-ewoc)))
    (when current-node
      (setq current-node (ewoc-next gtt--results-ewoc current-node)))

    (while (when (not (equal current-node nil))
             (not (equal (gtt--node-status (ewoc-data current-node)) "fail")))
      (setq current-node (ewoc-next gtt--results-ewoc current-node)))

    (when current-node
      (ewoc-goto-node gtt--results-ewoc current-node))))

(defun gtt-goto-prev-error ()
  (interactive)
  (let ((current-node (ewoc-locate gtt--results-ewoc)))
    (when current-node
      (setq current-node (ewoc-prev gtt--results-ewoc current-node)))

    (while (when (not (equal current-node nil))
             (not (equal (gtt--node-status (ewoc-data current-node)) "fail")))
      (setq current-node (ewoc-prev gtt--results-ewoc current-node)))

    (when current-node
      (ewoc-goto-node gtt--results-ewoc current-node))))

(defun gtt--pp-node (node)
  (if (gtt--node-subtest node)
      (gtt--pp-subtest node)
    (gtt--pp-test node)))

(defun gtt--pp-test (node)
  (insert (upcase (gtt--node-status node)))
  (insert " - ")
  (insert (gtt--node-test node))
  (insert "\n")
  (when (gtt--node-expanded node)
    (gtt--pp-output node)))
        
(defun gtt--pp-subtest (node)
  (gtt--insert-indented (upcase (gtt--node-status node)) 1)
  (insert " - ")
  (insert (gtt--node-test node))
  (insert "/")
  (insert (gtt--node-subtest node))
  (insert "\n")
  (when (gtt--node-expanded node)
    (gtt--pp-output node)))

(defun gtt--pp-output (node)
  (let* ((output-lines (gtt--node-output node))
         (filtered-lines (seq-filter 'gtt--is-not-redundant output-lines)))
    (if filtered-lines
        (dolist (output-line filtered-lines)
          (gtt--insert-indented output-line 2))
      (gtt--insert-indented "*** no output ***\n" 2))))

(defun gtt--is-not-redundant (line)
  (not (or (string-match-p "--- PASS" line)
           (string-match-p "--- FAIL" line)
           (string-match-p "=== RUN" line))))

(defun gtt--insert-indented (text number-of-tabs)
  (dotimes (i number-of-tabs)
    (insert "\t"))
  (insert text))

(define-derived-mode gotest-mode special-mode "gotest-mode"
  "Major mode for running go tests."
  (use-local-map gtt-mode-map)
  (font-lock-add-keywords nil gotest-font-lock-keywords)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory "*Go test*")
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky))

(defvar gtt-mode-map
  (let ((m (make-sparse-keymap)))
    (suppress-keymap m)
    ;; key bindings go here
    (define-key m (kbd "k") 'kill-buffer)
    (define-key m (kbd "q") 'quit-window)
    (define-key m (kbd "TAB") 'gtt-toggle-node)
    (define-key m (kbd "RET") 'gtt-goto-error)
    (define-key m [(f2)] 'gtt-goto-next-error)
    (define-key m [(S-f2)] 'gtt-goto-prev-error)
    m))

(defface gotest--pass-face '((t :foreground "green"))
  "Face for displaying the status of a passing test."
  :group 'gotest-transient)

(defface gotest--fail-face '((t :foreground "pink" :weight bold))
  "Face for displaying the status of a failed test."
  :group 'gotest-transient)

(defface gotest--file-face '((t :foreground "white" :weight bold))
  "Face for displaying file names on failed tests."
  :group 'gotest-transient)

(defconst gotest-font-lock-keywords
  '(("error\\:" . 'gotest--fail-face)
    ("FAIL" . 'gotest--fail-face)
    ("FAIL" . 'gotest--fail-face)
    ("PASS" . 'gotest--pass-face)
    ("\\([^ \t]+\\.go:[0-9]+\\)" . 'gotest--file-face))
  "Minimal highlighting expressions for go test results.")

(defun gtt--run-file (&optional args)
  "Run all tests for the current file."
  (interactive
   (list (transient-args 'gtt-dispatch)))
  (transient-save)
  (let* ((project-root (gtt--get-project-root))
         (file-tests (gtt--get-current-file-tests))
         (cmd-args (append args '("-run") `(,file-tests) '("./..."))))
    (gtt--run-tests project-root cmd-args)))

(defun gtt--get-current-file-tests ()
  "Get tests that should be run for the current file."
  (let ((buffer (gtt--get-test-buffer)))
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

(defun gtt--get-test-buffer ()
  "Get the test buffer for the FILE."
  (if (string-match "_test\.go$" buffer-file-name)
      (current-buffer)
    (let ((ff-always-try-to-create nil)
	  (filename (ff-other-file-name)))
      (when filename
	    (find-file-noselect filename)))))

(defun gtt--run-test (&optional args)
  "Run current test"
  (interactive
   (list (transient-args 'gtt-dispatch)))
  (transient-save)
  (let* ((project-root (gtt--get-project-root))
         (test-name (gtt--get-current-test))
         (cmd-args (append args '("-run") `(,test-name) '("./..."))))
    (gtt--run-tests project-root cmd-args)))

(defun gtt--get-current-test ()
  "Get the name of the test the cursor is on"
  (let ((buffer (gtt--get-test-buffer))
        (test-function-regex "func[[:space:]]*\\(Test[^(]+\\)"))
    (with-current-buffer buffer
      (save-excursion
        (end-of-line)
        (if (search-backward-regexp test-function-regex nil t)
            (match-string-no-properties 1)
          (error "Unable to find a test"))))))

(defun gtt--run-subtest (&optional args)
  "Run current subtest"
  (interactive
   (list (transient-args 'gtt-dispatch)))
  (transient-save)
  (let* ((project-root (gtt--get-project-root))
         (test-name (gtt--get-current-test))
         (subtest-name (gtt--get-current-subtest test-name))
         (full-name (concat test-name "/" subtest-name))
         (cmd-args (append args '("-run") `(,full-name) '("./..."))))
    (gtt--run-tests project-root cmd-args)))
  
(defun gtt--get-current-subtest (test-name)
  "Get the name of the subtest the cursor is on."
  (let ((buffer (gtt--get-test-buffer))
        (subtest-regex "t.Run(\"\\([^\"]+\\)"))
    (with-current-buffer buffer
      (save-excursion
        (end-of-line)
        (if (search-backward-regexp subtest-regex nil t)
            (s-replace " " "_" (match-string-no-properties 1))
          (error "Unable to find a subtest"))))))

(setq gtt--previous-cmd-args nil)
(defun gtt-run-previous-command ()
  "Run the last command executed if any."
  (interactive)
  
  (message "Running command: go test %s" (s-join " " gtt--previous-cmd-args))
  (let ((project-root (gtt--get-project-root)))
    (if (bound-and-true-p gtt--previous-cmd-args)
        (gtt--run-tests project-root gtt--previous-cmd-args)
      (message "No tests were run previously"))))

(defun gtt-goto-error ()
  "Open / goto buffer where the error in cursor is located."
  (interactive)
  (let* ((current-node (ewoc-locate gtt--results-ewoc))
         (package (gtt--node-package (ewoc-data current-node)))
         (test-folder (gtt--get-package-folder package))
         (error-file (gtt--get-error-file current-node))
         (full-path (gtt--full-path test-folder (gtt--file-name error-file))))
    (find-file-other-window full-path)
    (goto-line (gtt--file-line error-file))))

(defun gtt--file-name (file-string)
  (car (split-string file-string ":")))

(defun gtt--file-line (file-string)
  (string-to-number (car (cdr (split-string file-string ":")))))

(defun gtt--full-path (folder file-name)
  (if (string-prefix-p "/" file-name)
      file-name
    (concat folder "/" file-name)))

(defun gtt--get-package-folder (package)
    (let* ((base-module (gtt--get-base-module gtt--project-root))
           (relative-folder (string-remove-prefix base-module package)))
      (concat (string-remove-suffix "/" gtt--project-root) relative-folder)))

(defun gtt--get-base-module (project-root)
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

(defun gtt--get-error-file (node)
  (let* ((output (gtt--node-output (ewoc-data node)))
         (error-file-lines (seq-filter 'gtt--error-file-name-p output)))
    (message "%s" error-file-lines)
    (cond ((not error-file-lines)
           (error "No error file found on test output"))
          ((equal (length error-file-lines) 1)
           (gtt--get-error-name (car error-file-lines)))
          (t
           (completing-read "Select file: "
                            (mapcar 'gtt--get-error-name error-file-lines))))))

(defun gtt--get-error-name (line)
  (let ((regex "\\([^ \t]+\\.go:[0-9]+\\)"))
    (string-match regex line)
    (match-string-no-properties 1 line)))

(defun gtt--error-file-name-p (line)
  (string-match-p "\\.go:[0-9]+" line))
  
(provide 'gotest-transient)
