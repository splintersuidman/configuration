#!/usr/bin/env -S guile -e main -s
!#

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (web uri)
             (srfi srfi-11))

(define browser "firefox")

;; See <https://debbugs.gnu.org/db/15/15228.html>.
(define open-process
  (@@ (ice-9 popen) open-process))

(define dmenu-default-args
  (list (cons 'bottom #f)
        (cons 'grab #f)
        (cons 'case-insensitive #f)
        (cons 'lines #f)
        (cons 'monitor #f)
        (cons 'prompt #f)
        (cons 'font #f)
        (cons 'normal-background #f)
        (cons 'normal-foreground #f)
        (cons 'selected-background #f)
        (cons 'selected-foreground #f)
        (cons 'embed-window #f)))

(define (dmenu-format-args args)
  (if
   (null? args) '()
   (let ((option (caar args))
         (value  (cdar args))
         (rest   (cdr args)))
     (cond
      ;; Bottom
      ((eq? option 'bottom)
       (if value
           (cons "-b" (dmenu-format-args rest))
           (dmenu-format-args rest)))
      ;; Grab
      ((eq? option 'grab)
       (if value
           (cons "-f" (dmenu-format-args rest))
           (dmenu-format-args rest)))
      ;; Case insensitive
      ((eq? option 'case-insensitive)
       (if value
           (cons "-i" (dmenu-format-args rest))
           (dmenu-format-args rest)))
      ;; Lines
      ((and (eq? option 'lines) value)
       (cons "-l" (cons (number->string value)
                        (dmenu-format-args rest))))
      ;; Monitor
      ((and (eq? option 'monitor) value)
       (cons "-m" (cons (number->string value)
                        (dmenu-format-args rest))))
      ;; Prompt
      ((and (eq? option 'prompt) value)
       (cons "-p" (cons value
                        (dmenu-format-args rest))))
      ;; Font
      ((and (eq? option 'font) value)
       (cons "-fn" (cons value
                         (dmenu-format-args rest))))
      ;; Normal background
      ((and (eq? option 'normal-background) value)
       (cons "-nb" (cons value
                         (dmenu-format-args rest))))
      ;; Normal foreground
      ((and (eq? option 'normal-foreground) value)
       (cons "-nf" (cons value
                         (dmenu-format-args rest))))
      ;; Selected background
      ((and (eq? option 'selected-background) value)
       (cons "-sb" (cons value
                         (dmenu-format-args rest))))
      ;; Selected foreground
      ((and (eq? option 'selected-foreground) value)
       (cons "-sf" (cons value
                         (dmenu-format-args rest))))
      ;; Embed window
      ((and (eq? option 'embed-window) value)
       (cons "-w" (cons value
                        (dmenu-format-args rest))))
      (else (dmenu-format-args rest))))))

(define (dmenu-prompter args options)
  "Runs dmenu, with arguments in @var{args}. The elements of the list
@var{options} are written to the input port of the program. The first
line in the output of the program is returned, or #f if the output is
empty."
  (let-values (((read-port write-port pid)
                (apply open-process
                       OPEN_BOTH
                       "dmenu"
                       (dmenu-format-args args))))
    (for-each (λ (option)
                (display option write-port)
                (newline write-port))
              options)
    (close-port write-port)
    (let ((output (read-line read-port)))
      (close-port read-port)
      (if (eof-object? output)
          #f
          output))))

(define (alist-keys alist)
  "Return a list of the keys of an alist."
  (map car alist))

(define (alist-values alist)
  "Return a list of the values of an alist."
  (map cdr alist))

(define (prompt-alist prompter args alist)
  (let* ((choice (prompter args (alist-keys alist)))
         (value (assoc-ref alist choice)))
    value))

(define (run-screenshot-prompt prompter args)
  (let* ((options
          '(("Full screen" . "scrot")
            ("Focused window" . "scrot --focused")
            ("Focused window with border" . "scrot --focused --border")
            ("Select window or rectangle" . "scrot --select")))
         (action (prompt-alist prompter args options)))
    (when action
      (system action))))

(define (run-news-prompt prompter args)
  (let* ((options
          '(("NOS" . "https://nos.nl/")
            ("The Guardian" . "https://theguardian.com/")
            ("de Volkskrant" . "https://volkskrant.nl/")
            ("De Groene Amsterdammer" . "https://groene.nl/")))
         (website (prompt-alist prompter args options)))
    (when website
      (system (string-append browser " " website)))))

(define (run-program program-name)
  (λ args
    (system (string-join (cons program-name args) " "))))

(define (run-clipboard-prompt prompter args)
  (let* ((options
          (list (cons "mpv"
                      (run-program "mpv"))
                (cons "Browser"
                      (run-program browser))
                (cons "Searx.dev"
                      (λ (s)
                        ((run-program browser)
                         ;; TODO: or can this be done with a POST
                         ;; request?
                         (string-append "https://searx.dev/?q="
                                        (uri-encode s)))))
                (cons "DuckDuckGo"
                      (λ (s)
                        ((run-program browser)
                         (string-append "https://duckduckgo.com/?q="
                                        (uri-encode s)))))))
         (action (prompt-alist prompter args options))
         (port (open-input-pipe "xclip -o"))
         (clipboard (read-string port)))
    (close-pipe port)
    (when action
      (action clipboard))))

(define (run-emoji-prompt prompter args)
  (let* ((file (open-input-file (string-append (getenv "HOME")
                                               "/.local/share/emoji")))
         (input (read-string file))
         (options (string-split input #\newline))
         (choice (prompter (acons 'lines 10 args)
                           options)))
    (close-port file)
    (when choice
      (let* ((xclip (open-output-pipe "xclip -selection clipboard"))
             (emoji (car (string-split choice #\space))))
        (display emoji xclip)
        (close-pipe xclip)
        (system (string-append "notify-send \"'"
                               emoji
                               "' gekopieerd naar klembord\""))))))

(define (run-wikipedia-prompt prompter args)
  (let ((lemma (prompter (acons 'prompt "Lemma: " args) '())))
    (when lemma
      (let ((language (prompter (acons 'prompt "Taal: " args)
                                '("nl" "en"))))
        (when language
          ((run-program browser)
           (string-append
            "https://"
            language
            ".wikipedia.org/wiki/Special:Search?search="
            (uri-encode lemma))))))))

(define (run-wiktionary-prompt prompter args)
  (let ((lemma (prompter (acons 'prompt "Lemma: " args) '())))
    (when lemma
      (let ((language (prompter (acons 'prompt "Taal: " args)
                                '("nl" "en"))))
        (when language
          ((run-program browser)
           (string-append
            "https://"
            language
            ".wiktionary.org/wiki/"
            (uri-encode lemma))))))))

(define (run-main-prompt prompter args)
  (let* ((options
          (list (cons "Screenshot" run-screenshot-prompt)
                (cons "Nieuws"     run-news-prompt)
                (cons "Klembord"   run-clipboard-prompt)
                (cons "Emoji"      run-emoji-prompt)
                (cons "Wikipedia"  run-wikipedia-prompt)
                (cons "Wiktionary" run-wiktionary-prompt)))
         (action (prompt-alist prompter args options)))
    (when action
      (action prompter args))))

(define xrdb
  (let* ((port (open-input-pipe "xrdb -query"))
         (output (read-string port))
         (lines (string-split
                 output
                 (λ (c) (char=? c #\newline)))))
    (map
     (λ (line)
       (let ((segments (string-split
                        line
                        (λ (c) (char=? c #\tab)))))
         (cons (string-drop-right (car segments) 1)
               (string-join (cdr segments)
                            "\t"))))
     (filter (λ (s) (not (string-null? s)))
             lines))))

(define (main args)
  (run-main-prompt
   dmenu-prompter
   `((font . ,(string-drop (assoc-ref xrdb "URxvt.font") 4))
     (case-insensitive . #t)
     (normal-background . ,(assoc-ref xrdb "*background"))
     (normal-foreground . ,(assoc-ref xrdb "*foreground"))
     (selected-background . ,(assoc-ref xrdb "*color2"))
     (selected-foreground . ,(assoc-ref xrdb "*color8")))))
