; Alyssa Miles

(defun coeff (term)
  (car term)
)

(defun degree (term)
  (cadr term)
)

(defun negTerm (term)
  (list (coeff term) (- 0 ( degree term )) )
)

(defun compareTerms (u v)
  (cond 
    ((> (degree u) (degree v)) t)
    (t nil)
  )
)

(defun addTerms (u v)
  (cond
    ((= (degree u) (degree v)) (list (+ (coeff u) (coeff v)) (degree u)))
    (t (list u v))
  )
)

(defun mulTerms (u v)
  (list (* (coeff u) (coeff v)) (+ (degree u) (degree v)))
)

(defun size (poly)
  (cond
    ((null poly) 0)
    (t (+ 1 (size (cdr poly))))
  )
)

(defun leadingTerm (poly)
  (car poly)
)

; Adds the first two terms of the poly.
; Appends either t or nil to the front of the list depending on if the terns were combined or not. 
(defun addFirstTwoTerms (poly)
  (append 
    (cond
      (( listp  (car ( addTerms (car poly) (cadr poly) ) ) ) ( append '(nil) (addTerms (car poly) (cadr poly) )))
      (t (append '(t) (list ( addTerms (car poly) (cadr poly) ))))
    )
    (cddr poly)
  )
)

(defun sortCoeffAsc (u v) (< (coeff u) (coeff v)))
(defun sortDegreeDesc (u v) (> (degree u) (degree v)))





; Assumes that the poly is sorted already.
(defun addAllTerms (poly)
  (cond 
    ((null (cdr poly)) poly)
    ( (car (addFirstTwoTerms poly) ) (addAllTerms (cdr (addFirstTwoTerms poly))) )
    ( t (append (list (car poly)) (addAllTerms(cdr poly) ) ))
      
  )
)

(defun removeZeros (poly)
  (cond
    ((null poly) '()) ; This should only happen if the poly's coeffs are all zero.
    ((= 0 (coeff (car poly))) (removeZeros (cdr poly)))
    (t poly)
  )
)

(defun normalize (poly)
  (addAllTerms (sort (removeZeros (sort poly 'sortCoeffAsc)) 'sortDegreeDesc))
)

(defun negPoly (poly)
  (append (list (list (- 0 (coeff (car poly))) (degree (car poly))))
	  (cond
	    ((null (cdr poly)) '())
	    (t (negPoly (cdr poly)))
	    )
  )
)

(defun addPolys (u v)
  (normalize (append u v))
  )


(defun subPolys (u v)
  (normalize (append u (negPoly v)))
  )

(defun mulPolyByOneTerm (term poly)
  (append (list (list (* (coeff term) (coeff (car poly))) (+ (degree term) (degree (car poly)))))
	  (cond
	    ((null (cdr poly)) '())
	    (t (mulPolyByOneTerm term (cdr poly)))
	    )
  )
  )

(defun mulPolys (u v)

  (normalize (append (mulPolyByOneTerm (car u) v)
	  (cond
	    ((null (cdr u)) '())
	    (t (mulPolys (cdr u) v))
	    )
	  )
  
	     ))

(defun evalTerm (c d x)
  (cond
    ((= 0 d) c)
    (t (* x (evalTerm c (- d 1) x )))

    )
  )

(defun evalPoly (f x)
  (cond
    ((null (cdr f)) (evalTerm (coeff (car f)) (degree (car f)) x))
    (t (+ (evalTerm (coeff (car f)) (degree (car f)) x) (evalPoly (cdr f) x)))
    )
  )
