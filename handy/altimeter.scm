(use-modules
 (sxml simple)
 (ice-9 binary-ports)
 (rnrs bytevectors))

(define (read-file file)
  (call-with-input-file file
   (lambda (in)
     (utf8->string (get-bytevector-all in)))))

(define (verts vs)
  (string-join
   (map
    (lambda (cs)
      (string-join (map number->string cs) ","))
    vs)
   " "))

(define pi 3.1415926535)

(define svg
  `(svg (@ (version "1.1")
           (viewBox "-100 -100 200 200"))
        (style ,(read-file "style.css"))
        (g
         (@ (transform "scale(3)"))
         (circle (@ (r 30)
                    (fill "black")))
         ,(map
           (lambda (d)
             `(polyline
               (@ (stroke "white")
                  (stroke-width "1")
                  (transform ,(format #f "rotate(~f)" d))
                  (points ,(verts '((0 25)
                                    (0 29)))))))
           (iota 10 0 36))
         ,(map
           (lambda (d)
             `(polyline
               (@ (stroke "white")
                  (stroke-width "0.7")
                  (transform ,(format #f "rotate(~f)" d))
                  (points ,(verts '((0 27)
                                    (0 29)))))))
           (iota 50 0 (/ 360 10 5)))
         ;; fixme I think Emacs SVG renderer does not account for dominant-baseline
         ,(map
           (lambda (n)
             (let ((r 20)
                   (d (+ pi (* 2 pi (/ n -10.0)))))
               `(text (@ (class "altNumbers")
                         (text-anchor "middle")
                         (dominant-baseline "middle")
                         (x ,(* r (sin d)))
                         (y ,(+ 0 (* r (cos d)))))
                      ,(number->string n))))
           (iota 10 0))
         (text (@ (text-anchor "middle")
                  (id "altLabel")
                  (class "asText")
                  (x -5)
                  (y -5))
               "ALT")
         (g (@ (transform ,(format #f "translate(~f,~f)" 13 -2)))
            (rect (@ (width 12)
                     (height 5)
                     (fill "#333")
                     (stroke "white")
                     (stroke-width 0.5)))
            (text (@  (id "subscale")
                      (class "asText")
                      (dominant-baseline "middle")
                      (y 3.5))
                  "1013"))
         ;; fixme extra minor ticks under/over major ticks
         (polygon
          (@ (id "thousands")
             (fill "white")
             (points ,(verts '((-1.5 0) (-2 8) (0 15) (2 8) (1.5 0))))))
         (polygon
          (@ (id "hundreds")
             (transform "rotate(30)")
             (fill "white")
             (points ,(verts '((-1 0) (-1 23) (0 25) (1 23) (1 0))))))
         (circle (@ (r "3")
                    (fill "#333"))))))

(call-with-output-file "altimeter.svg"
  (lambda (out)
    (sxml->xml svg out)))
