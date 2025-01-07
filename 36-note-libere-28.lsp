================

 NOTE LIBERE 28

================

  To build up a library is to create a life.
  It’s never just a random collection of books.

-------------------------------------------------
Assegnazione di un valore ad una serie di simboli
-------------------------------------------------

Supponiamo di voler assegnare uno stesso valore (per esempio 0) a più di una variabile/simbolo (per esempio a, b e c).

Dobbiamo scrivere:

(setq a 0)
(setq b 0)
(setq c 0)

Possiamo scrivere una macro per semplificare il processo:

(define-macro (set= val)
  (map set (args) (dup val (length (args)))))

I parametri della macro sono:
1) il valore da assegnare (val)
2) serie di simboli a cui assegnare il valore (args)

Proviamo:

(set= 2 a b c d)
;-> (2 2 2 2)
(println a { } b { } c { } d)
;-> 2 2 2 2

Come si può notare, la macro assegna il valore anche a simboli non ancora definiti (la variabile d).

(set= '(0 1) v1 v2)
;-> ('(0 1) '(0 1))
(println v1 { } v2)
'(0 1) '(0 1)

============================================================================

