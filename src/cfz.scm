(module
 cfz
 ()
 (import scheme)

 (cond-expand
  (chicken-5
   (import (chicken base))
   (import (only (chicken format) format))
   (import (only (chicken process-context) command-line-arguments))
   (import (chicken type))
   (import (only (chicken io) read-lines read-line))
   (import (only (chicken string) string-split))
   (import (only (srfi 1) filter drop append! reverse!))
   (import (only (srfi 18) make-thread thread-start! thread-yield!))
   (import (only typed-records defstruct))
   (import (prefix curses-loop loop:))
   (import (only matchable match))
   (import (prefix ncurses c:))
   (import (chicken condition))
   (import (only (chicken bitwise) arithmetic-shift))
   (import (only (chicken irregex) irregex irregex-search))
   (import (prefix mailbox mb:))
   )
  (else (import chicken)
        (use (only data-structures string-split))
        (use (only extras read-lines read-line))
        (use (only matchable match))
        (use (only srfi-1 drop filter))
        (use (only typed-records defstruct))
        (use (prefix curses-loop loop:))
        (use (prefix ncurses c:))
        (use (only irregex irregex irregex-search))
        ))

 (define-type strings (list-of string))
 (define-type cands strings)

 (define backspace #\x107)
 (define selection-style (+ c:A_REVERSE c:A_BOLD))

;;; ** --- State -------------------------------------------------------------------

 (defstruct state
   [(cur-cand 0) : fixnum]
   [(old-cand -1) : fixnum]
   [(cand-offs 0) : fixnum]

   [(query "") : string]
   [(old-query #f) : (or false string)]
   [(cands '()) : (list-of string)]
   [(input '()) : (list-of string)]

   [(retval #f) : (or boolean (list-of string))]
   [(done? #f) : boolean]
   [(mb-input (mb:make-mailbox)) : (struct mailbox)])

;;; ** --- Render ------------------------------------------------------------------

 (define-syntax with-attrs
   (ir-macro-transformer
    (lambda (e inj cmp)
      (apply
       (lambda (attrs #!rest body)
         `(begin
            (c:attron ,attrs)
            ,@body
            (c:attroff ,attrs)))
       (cdr e)))))

 (define (idx-to-line idx offs count line-min line-max)
   (let ([i (- idx offs)]
         [h (- line-max line-min)])
     (if (or (< i 0) (<= count i) (< h i))
         #f
         (+ line-min (- h i)))))

 (: print-candidates (strings fixnum fixnum fixnum -> *))
 (define (print-candidates cands cur-cand cand-offs max-line)
   (let recur ([line max-line]
               [cs (drop cands cand-offs)])
     (when (and (<= 0 line)
                (not (null? cs)))
       (c:mvprintw line 0 "~a" (car cs))
       (recur (sub1 line)
              (cdr cs)))))

 (define (redraw-selection cands n-cands cand-offs old-cand new-cand c-mi c-ma)
   (define (cand-to-line idx) (idx-to-line idx cand-offs n-cands c-mi c-ma))
   (and-let* ([l (cand-to-line new-cand)])
     (with-attrs
      selection-style
      (c:mvprintw l 0 "~a" (list-ref cands new-cand))))
   (and-let* ([l (cand-to-line old-cand)]
              [(< old-cand n-cands)])
     (c:mvprintw l 0 "~a" (list-ref cands old-cand))))

 (define (render state)
   (define cand-win-min 0)
   (define cand-win-max (- (c:LINES) 2))
   (define cands-height (add1 (- cand-win-max cand-win-min)))
   (define redraw-cands? #f)

   (unless (eq? (state-query state) (state-old-query state))
     (state-old-query-set! state (state-query state))
     (let ([old-cands (state-cands state)])
       (state-cands-set! state (filter-input (state-input state) (state-query state)))
       (unless (equal? old-cands (state-cands state))
         (set! redraw-cands? #t))))

   (define n-cands (length (state-cands state)))
   (define cur-cand (let ([cc (state-cur-cand state)])
                      (cond [(< cc n-cands) cc]
                            [else (state-cur-cand-set! state 0)
                                  0])))
   (define cand-offs (if (not (idx-to-line cur-cand (state-cand-offs state)
                                           n-cands cand-win-min cand-win-max))
                         (begin (set! redraw-cands? #t)
                                (let ([o (max 0 (- cur-cand (arithmetic-shift cands-height -1)))])
                                  (state-cand-offs-set! state o)
                                  o))
                         (state-cand-offs state)))

   (when redraw-cands?
     (c:clear)
     (print-candidates (state-cands state) cur-cand cand-offs cand-win-max))

   (redraw-selection (state-cands state) n-cands
                     cand-offs (state-old-cand state) cur-cand
                     cand-win-min cand-win-max)

   (with-attrs
    c:A_BOLD
    (c:mvprintw (sub1 (c:LINES)) 0
                "~a ~a > ~a"
                (length (state-input state))
                n-cands
                (state-query state)
                (list (state-old-query state))))

   (c:refresh))

;;; ** --- Logic -------------------------------------------------------------------

 (: filter-input (strings string --> strings))
 (define (filter-input input query)
   (define (make-pred q)
     (let ([irxs (map (lambda (s) (irregex `(: ,s) 'i)) (string-split q " "))])
       (lambda (line)
         (let lp ([ixs irxs])
           (or (null? ixs)
               (and (irregex-search (car ixs) line)
                    (lp (cdr ixs))))))))
   (if (string=? "" query)
       input
       (filter (make-pred query) input)))

 (define (num-cands cands)
   (length cands))

 (define (find-parent state cands)
   (state-old-cand-set! state (state-cur-cand state))
   (let* ([cur (list-ref cands (state-cur-cand state))]
          [cur-len (string-length cur)])
     (let recur ([best-len 0]
                 [best -1]
                 [cs cands]
                 [idx 0])
       (cond
        [(null? cs)
         (state-cur-cand-set! state (if (= -1 best) (state-cur-cand state) best))]
        [else
         (let* ([c (car cs)]
                [cl (string-length c)])
           (cond
            [(or (not (< cl cur-len))
                 (not (< best-len cl))
                 (not (string=? (substring cur 0 cl) c)))
             (recur best-len best (cdr cs) (add1 idx))]
            [else
             (recur cl idx (cdr cs) (add1 idx))]))]))))

 (define (init-state)
   (make-state))

 (define (get-message state)
   (thread-yield!)
   (let ([mb-in (state-mb-input state)])
     (cond [(not (mb:mailbox-empty? mb-in)) (mb:mailbox-receive! mb-in)]
           [else #f])))

;;; ** --- Update ------------------------------------------------------------------

 (define (update state msg)
   (loop:log-msg "update" (car msg))
   (define cand-up
     (lambda (cands #!optional (count 1))
       (unless (null? cands)
         (state-old-cand-set! state (state-cur-cand state))
         (state-cur-cand-set!
          state (modulo (+ count (state-cur-cand state)) (num-cands cands))))))
   (define (back? c) (or (eq? backspace c) (eq? c:KEY_BACKSPACE c)))
   (define (set-query new) (state-query-set! state new))
   (define (set-return-val v)
     (state-done?-set! state #t)
     (state-retval-set! state v))
   ;; (match '(foo: a #\a) [(or (foo: 'a #\a) (foo: 'b #\b))]) ;; TODO: matchable bug?
   (match msg
     [('key: 'meta #\j) (cand-up (state-cands state) -1)]
     [('key: 'meta #\k) (cand-up (state-cands state))]
     [('key: 'meta #\u) (find-parent state (state-cands state))]
     [('key: 'meta #\escape) (set-return-val #t)]
     [('key: 'meta #\g) (set-return-val #t)]
     [('key: 'meta #\e) (error 'eee)]
     [('key: #\alarm) (set-return-val #t)]
     [('key: #\newline) (set-return-val
                         (let ([cands (filter-input (state-input state) (state-query state))])
                           (if (null? cands)
                               #f
                               (list (list-ref cands (state-cur-cand state))))))]
     [('key: (? back?))
      (set-query
       (substring
        (state-query state)
        0
        (max 0 (sub1 (string-length (state-query state))))))]
     [('key: c) (set-query (string-append (state-query state) (format "~a" c)))]
     [('add-strings: strs)
      (state-old-query-set! state #f) ; refilter
      (state-input-set! state (append! (state-input state) strs))]
     [t (error 'update "unknown msg" t)])
   state)

 (define (read-input-loop mb-out port)
   (let lp ([lines '()]
            [c 0])
     (let ([l (read-line port)])
       (cond [(or (eof-object? l) (= 5000 c))
              (mb:mailbox-send! mb-out (list add-strings: (reverse! lines)))
              (when (not (eof-object? l))
                (lp '() 0))]
             [else (lp (cons l lines) (add1 c))]))))

 (define (main input-file output-file)
   (let* ([state (init-state)]
          [input (if input-file
                     (with-input-from-file input-file
                       (lambda ()
                         (mb:mailbox-send! (state-mb-input state)
                                           (list add-strings: (read-lines)))))
                     (thread-start!
                      (make-thread
                       (lambda ()
                         (read-input-loop (state-mb-input state) (current-input-port))))))])
     (let* ([ret (state-retval (loop:main-loop state
                                               update
                                               render
                                               get-message
                                               state-done?))])
       (define (pres)
         (when (list? ret)
           (for-each print ret)))
       (cond [(not ret) (exit 1)]
             [output-file (with-output-to-file output-file
                            (lambda ()
                              (loop:log-msg "outputting")
                              (pres)))]
             [else (pres)]))))

 (match (command-line-arguments)
   [(in-file out-file) (main in-file out-file)]
   [() (main #f #f)]
   [t (print "expected args: in-file out-file, got " t)
      (exit 1)])
 )
