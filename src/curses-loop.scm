(module
 curses-loop
 (main-loop
  log-msg)
 (import scheme)
 (cond-expand
  (chicken-5 (import (chicken base))
             (import (chicken type))
             (import (chicken format))
             (import (chicken condition))
             (import (prefix (chicken file posix) p:))
             (import (prefix ncurses c:)))
  (else (import chicken)
        (use (prefix ncurses c:))
        (use (only extras printf format))
        (use (prefix posix p:))))

 (define-constant +NOINPUT+ #\x1fffff)

 (define *debug-log* '())
 (define (log-msg #!rest r)
   (set! *debug-log* (cons r *debug-log*)))

 (define (main-loop2 state update render done?)
   (define (parse-key last cur)
     (log-msg last: last cur: cur)
     (cond [(not cur) #f]
           [(eq? #\escape cur) #f]
           [(not last) (list key: cur)]
           [(eq? #\escape last) (list key: 'meta cur)]
           [else #f]))

   (let ([fd (p:file-open "/dev/tty" p:open/rdwr)])
     (c:newterm #f fd fd))

   (c:raw)
   (c:cbreak)
   (c:keypad (c:stdscr) #t)
   (c:noecho)
   ;; (timeout 1)

   (render state)

   (let lp ([last-c #f]
            [state state])
     (define (next cc #!optional new-state)
       (unless (and new-state (done? new-state))
         (when new-state
           (render new-state))
         (lp cc (or new-state state))))

     (let* ([c (c:getch)])
       (cond [(eq? +NOINPUT+ c) (c:halfdelay 1) (next last-c)]
             [(parse-key last-c c) =>
              (lambda (msg) (next #f (update state msg)))]
             [else (next c)])))
   (c:endwin)
   state)

 ;; (: main-loop ('s ('s 'msg -> 's) ('s -> *) ('s -> *) -> 's))
 (define (main-loop state update draw done?)
   (handle-exceptions exn
     (begin
       (when (c:stdscr) (c:endwin))
       (for-each print (reverse *debug-log*))
       (print-exception2 exn)
       (exit 1))
     (main-loop2 state update draw done?)))

 (define (print-exception2 exn #!optional while)
   (and-let* ([cc (or (get-condition-property exn 'exn 'cc #f)
                      (get-condition-property exn 'exn 'call-chain #f))])
     (##sys#really-print-call-chain
      (current-output-port) cc
      "############################## Call chain: ##############################\n"))
   (printf "\n#################### Exception ~a~a: ####################\n~a~a:\n~a\n"
           (map car (condition->list exn))
           (if while (string-append " while " while) "")
           (let ([loc (get-condition-property exn 'exn 'location #f)])
             (if loc (format "In `~a':\n" loc) ""))
           (get-condition-property exn 'exn 'message "<no message>")
           (let ([args (get-condition-property exn 'exn 'arguments)])
             (if (null? args) "" (format "~a\n" args)))))

 )
