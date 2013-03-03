#!/opt/local/bin/gosh

(use text.html-lite)
(use text.tree)
(use www.cgi)
(use rfc.uri)
(use file.util)
(use gauche.collection)
(use srfi-27)

(define *width*  30)
(define *height* 15)
(define *file-path* "/tmp/lifegame_cells")

(define (make-random-block w h)
  (map (lambda (v)
		 (map (lambda (a) (= (random-integer 3) 0))
			  (make-list w #f)))
	   (make-list h #f)))

(define (block-ref block x y)
  (ref (ref block y) x))

(define (modefy-list ls ith val)
  (define (head ls ith)
	(if (= ith 0)
	  '()
	  (cons (car ls) (head (cdr ls) (- ith 1)))))
  (define (tail ls ith)
	(if (= ith 0)
	  ls
	  (tail (cdr ls) (- ith 1))))
  (let 
	((h (head ls ith))
	 (t (tail ls (+ ith 1)))
	 (v (ref ls ith)))
	(append (append h  (cons val '())) t)))

(define (get-width-height block)
  (values
	  (size-of (ref block 0))
	  (size-of block)))

(define (count-naghbour block)
  (receive
	(width height)
	(get-width-height block)

	(define (round x w)
	  (cond ((<  x 0) (- w 1))
			((>= x w) 0)
			(else x)))

	(define (check-xy x y)
	  (if (block-ref block (round x width) (round y height)) 1 0))

	(define (count-round x y)
	  (+ (check-xy (+ x 1) (- y 1))
		 (check-xy (+ x 1) y     )
		 (check-xy (+ x 1) (+ y 1))
		 (check-xy x       (- y 1))
		 (check-xy x       (+ y 1))
		 (check-xy (- x 1) (- y 1))
		 (check-xy (- x 1) y     )
		 (check-xy (- x 1) (+ y 1))))

	(let yloop
		 ((y 0)
		  (yres ()))
		 (if (= y height)
		   (reverse yres)
		   (yloop (+ y 1)
				  (cons (let xloop
						  ((x 0)
						   (res ()))
						  (if (= x width)
							(reverse res)
							(xloop (+ x 1) 
								   (cons (count-round x y) res ))))
						yres))))))

(define (next-gen block count-block)
  (let yloop
	((yline block)
	 (yls count-block))
	(if (null? yls)
	  '()
	  (cons 
		(let xloop
		  ((xline (car yline))
		   (rest (car yls)))
		  (if (null? rest)
			'()
			(cons
			  (cond ((<= (car rest) 1) #f)
					((= (car rest) 2) (if (car xline) #t #f))
					((<= (car rest) 3) #t)
					(else #f))
			  (xloop (cdr xline) (cdr rest)))))
		(yloop (cdr block) (cdr yls))))))

;; CGI implement below

(define (make-block-table block)
  (html:table
	(let loop
	  ((row block))
	  (if (null? row)
		'()
		(cons
		  (html:tr
			(map (lambda (x) (html:td (if x "■" "□") ))
				 (car row)))
		  (loop 
			(cdr row)))))))

(define (retrive-block error-proc)
  (guard (exc
		   ((<read-error> exc) (error-proc))
		   (else (error-proc)))
		 (with-input-from-file
		   *file-path*
		   (lambda ()
			 (let 
			   ((data (read)))
				(if (pair? data)
				  data
				  (raise 'not-pair)))))))

(define (calc-next-gen block)
  (next-gen block (count-naghbour block)))

(define (main args)

  (define (clear-view rediredt-uri)
	(begin
	  (sys-remove *file-path*)
	  (html:html
		(html:html
		  (html:head
			(html:meta :http-equiv "refresh" :content #`"0 ; URL=,rediredt-uri")
			(html:meta :charset "UTF-8")
			(html:title "clear")))
		(html:body "clear cells"))))

  (define (has-rval rval params )
	(if (find (lambda (x) (equal? (car x) rval)) params) #t #f))

  (random-source-randomize!  default-random-source)
  (cgi-main
	(let* 
	  ((uri (sys-getenv "REQUEST_URI"))
	   (path (car (string-split uri "?")))
	   (pre-block  (retrive-block (lambda () (make-random-block *width* *height*))))
	   (post-block (calc-next-gen pre-block)))

	  ;; XXX
	  (with-output-to-file
		*file-path*
		(lambda ()
		  (write post-block)))

	  (lambda (params)
		`(,(cgi-header)
		   ,(html-doctype)
		   ,(cond
			  ((has-rval "clear" params) (clear-view path))
			  (else
				(html:html
				  (html:head
					(html:meta :charset "UTF-8")
					(html:title "Life Game"))
				  (html:body
					(html:div (html:a  :href uri "[Reload to succeed next generation]"))
					(html:div (html:a  :href (string-append path "?clear=1") "[Clear]"))
					(html:hr)
					(make-block-table post-block))))))))))
