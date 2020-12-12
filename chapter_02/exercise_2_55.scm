(car ''abracadabra)
;; quote

'abracadabra
;; abracadabra

''abracadabra
;; (quote abracadabra)

#|

the interpreter evaluates 'xyz as (quote xyz)
''xyz -> (quote (quote xyz)) -> (quote xyz)
car ''xyz -> quote
|#
