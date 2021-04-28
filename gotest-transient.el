(require 'transient)

(defgroup gotest-transient nil
  "Go test integration with transient ui."
  :group 'go
  :prefix "gotest-transient-")

(define-transient-command gotest-transient-dispatch ()
  "Show popup for running go tests."
  :value '("")
  ["Options"
   [("-v" "verbose" "-v")
    ("-nc" "no cache" "-count=1")
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
(defun gotest-transient--run-tests (project-root cmd-args)
  "Run the command line with CMDARGS and show results on buffer."
  (let ((cmdline (append '("go" "test") cmd-args)))
    (setq gotest-transient--results-buffer (get-buffer-create "*Go test*"))
    (display-buffer gotest-transient--results-buffer)

    (with-current-buffer gotest-transient--results-buffer
      (let ((buffer-read-only))
        (erase-buffer))
      (buffer-disable-undo)
      (gotest-mode)
      (let ((default-directory project-root))
        ;; FIXME: [JUANJO] aqui deberiamos crear un modo para el
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
                            :filter #'gotest-transient--read-stdout
                            :stderr gotest-transient--stderr-process
                            :command cmdline))))))

(defun gotest-transient--stderr-sentinel (proc event)
  "Acts as the process sentinel for stderr proccess (ignoring all events)."
  (kill-buffer (process-buffer proc)))

(defun gotest-transient--read-stderr (proc input)
  "Filters the stderr output from the go compiler."
  (let ((inhibit-quit t))
    (with-local-quit
      (with-current-buffer gotest-transient--results-buffer
        (let ((buffer-read-only nil))
          (insert input))))))
  
(defun gotest-transient--process-sentinel (proc event)
  "Acts as the process sentinel for the run process."
  (with-local-quit
    (with-current-buffer gotest-transient--results-buffer
      (let ((buffer-read-only nil))
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
  (let ((inhibit-quit t))
    (with-local-quit
      (with-current-buffer gotest-transient--results-buffer
        (let ((buffer-read-only nil))
          (insert input))))))

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
    m))

(defface gotest--pass-face '((t :foreground "green"))
  "Face for displaying the status of a passing test."
  :group 'gotest-transient)

(defface gotest--fail-face '((t :foreground "pink" :weight bold))
  "Face for displaying the status of a failed test."
  :group 'gotest-transient)

(defconst gotest-font-lock-keywords
  '(("error\\:" . 'gotest--fail-face)
    ("^\s*FATAL.*" . 'gotest--fail-face)
    ("^\s*FAIL.*" . 'gotest--fail-face)
    ("^\s*--- FATAL.*" . 'gotest--fail-face)
    ("^\s*--- FAIL:.*" . 'gotest--fail-face)
    ("^\s*--- PASS.*" . 'gotest--pass-face)
    ("^\s*PASS.*" . 'gotest--pass-face)
    ("^\s*ok.*" . 'gotest--pass-face)
    )
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
    (message test-name)
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

(defun gotest-transient--run-subtest (file function &optional args)
  "Run current subtest"
  (interactive
   (list
    (buffer-file-name)
    (transient-args 'gotest-transient-dispatch)))
  (message "Running current test.")
  (transient-save))

(provide 'gotest-transient)
