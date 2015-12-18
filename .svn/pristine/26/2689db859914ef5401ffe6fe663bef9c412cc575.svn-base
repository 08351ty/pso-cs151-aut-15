#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/uchicago151.rkt")

;;Defines type CardBrand with different credit card brands
(define-type CardBrand
  (U 'ColEx 'Passport 'NoviceCard 'Uncover))

;;Defines structure including CardBrand and card number
(define-struct CreditCard
  ([type : CardBrand]
   [num : Integer]))

;;Takes in a CardBrand and returns first digit of valid credit card
(: brand-digit (-> CardBrand Integer))
(define (brand-digit cb)
  (cond
    [(boolean=? (symbol=? cb 'ColEx) #t) 3]
    [(boolean=? (symbol=? cb 'Passport) #t) 4]
    [(boolean=? (symbol=? cb 'NoviceCard) #t) 5]
    [(boolean=? (symbol=? cb 'Uncover) #t) 6]
    [else 0]))
(check-expect (brand-digit 'ColEx) 3)

;;Checks if the card number corresponds with the claimed brand
(: brand-valid? (-> CreditCard Boolean))
(define (brand-valid? bv)
  (if (= (string-length (number->string (CreditCard-num bv))) 16)
      (if (= 3(quotient (CreditCard-num bv) (expt 10 15)) (brand-digit (CreditCard-type bv))) #t #f) #f))
(check-expect (brand-valid? (CreditCard 'ColEx 3234123412341234)) #t)
(check-expect (brand-valid? (CreditCard 'ColEx 32341234123412346)) #f)

;;Constructs a CreditCard with the inputted card number
(: build-card (-> Integer CreditCard))
(define (build-card i)
  (local
    {(define num (quotient i (expt 10 15)))}
    (cond
      [(= num 3) (CreditCard 'ColEx i)]
      [(= num 4) (CreditCard 'Passport i)]
      [(= num 5) (CreditCard 'NoviceCard i)]
      [(= num 6) (CreditCard 'Uncover i)]
      [else (error "not a valid number")])))
(check-expect (build-card 3333333333333333) (CreditCard 'ColEx 3333333333333333))
(check-error (build-card 2333333333333333) "not a valid number")

;;Define type IntTree which is either an IntNode or symbol 'IEmpty
(define-type IntTree (U IntNode 'IEmpty))

;;Defines structure IntNode which holds int value of node, leftnode and rightnode
(define-struct IntNode
  ([val   : Integer]
   [left  : IntTree]
   [right : IntTree]))

;;Define type StringTree which is either a StringNode or symbol 'SEmpty
(define-type StringTree (U StringNode 'SEmpty))

;;Defines structure StringNode which holds string value of node, leftnode and rightnode
(define-struct StringNode
  ([val   : String]
   [left  : StringTree]
   [right : StringTree]))

;;Mirrors the entire IntTree
(: mirror (-> IntTree IntTree))
(define (mirror t)
  (cond
    [(IntNode? t) (IntNode (IntNode-val t) (mirror (IntNode-right t)) (mirror (IntNode-left t)))]
    [else 'IEmpty]))
(check-expect (mirror (IntNode 1 (IntNode 2 'IEmpty 'IEmpty) (IntNode 3 'IEmpty 'IEmpty)))
              (IntNode 1 (IntNode 3 'IEmpty 'IEmpty) (IntNode 2 'IEmpty 'IEmpty)))

;;Changes inputted IntTree into StringTree
(: int-tree->string-tree (-> IntTree StringTree))
(define (int-tree->string-tree t)
  (cond
    [(IntNode? t) (StringNode (number->string (IntNode-val t))
                              (int-tree->string-tree (IntNode-left t))
                              (int-tree->string-tree (IntNode-right t)))]
    [else 'SEmpty]))
(check-expect (int-tree->string-tree (IntNode 1 (IntNode 2 'IEmpty 'IEmpty) (IntNode 3 'IEmpty 'IEmpty)))
              (StringNode "1" (StringNode "2" 'SEmpty 'SEmpty) (StringNode "3" 'SEmpty 'SEmpty)))

;;Concatenates all the raight nodes to form a string
(: right-edge (-> StringTree String))
(define (right-edge tree)
  (cond
    [(StringNode? tree) (string-append (StringNode-val tree) (right-edge (StringNode-right tree)))]
    [else ""]))
(check-expect (right-edge (StringNode "hi" (StringNode "hello" 'SEmpty 'SEmpty) (StringNode "hey" 'SEmpty 'SEmpty)))
              "hihey")

;;Defines type 3Tree which is either a 3Node or symbol '3Empty
(define-type 3Tree (U 3Node '3Empty))

;;Defines structure 3Node which contains root, leftnode, middlenode and rightnode
(define-struct 3Node
  ([root : Integer]
   [lsub : 3Tree]
   [msub : 3Tree]
   [rsub : 3Tree]))

;;Defines the test node that we will be working and testing with
(define node-structure (3Node 1
       (3Node 2
              (3Node 3 '3Empty '3Empty '3Empty)
              (3Node 4 '3Empty '3Empty '3Empty)
              '3Empty)
       (3Node 8
              '3Empty
              (3Node 7
                     (3Node 5 '3Empty '3Empty '3Empty)
                     '3Empty
                     (3Node 6 '3Empty '3Empty '3Empty))
              '3Empty)
       (3Node 9
              '3Empty
              '3Empty
              (3Node 0 '3Empty '3Empty '3Empty))))

;;Counts the number of nodes in the tree
(: num-nodes (-> 3Tree Integer))
(define (num-nodes tree)
    (cond
      [(3Node? tree) (+ 1 (num-nodes (3Node-lsub tree))
                        (num-nodes (3Node-rsub tree))
                        (num-nodes (3Node-msub tree)))]
      [else 0]))
(check-expect (num-nodes node-structure) 10)

;;Returns the sum of all the nodes in the tree
(: sum-nodes (-> 3Tree Integer))
(define (sum-nodes tree)
  (cond
    [(3Node? tree) (+ (3Node-root tree)
                      (sum-nodes (3Node-lsub tree))
                      (sum-nodes (3Node-msub tree))
                      (sum-nodes (3Node-rsub tree)))]
    [else 0]))
(check-expect (sum-nodes node-structure) 45)

;;Returns the height of the tree
(: height (-> 3Tree Integer))
(define (height tree)
    (cond
      [(3Node? tree) (+ 1 (max (height (3Node-lsub tree))
                               (height (3Node-msub tree))
                               (height (3Node-rsub tree))))]
      [else 0]))
(check-expect (height node-structure) 4)

;;Check whether the tree contains the argument x
(: contains? (-> 3Tree Integer Boolean))
(define (contains? tree x)
  (cond
    [(3Node? tree) (or (= x (3Node-root tree))
                       (contains? (3Node-lsub tree) x)
                       (contains? (3Node-msub tree) x)
                       (contains? (3Node-rsub tree) x))]
    [else #f]))
(check-expect (contains? node-structure 3) #t)
(check-expect (contains? node-structure 11) #f)

;;Returns the value of the leftmost node in the tree
(: leftmost (-> 3Tree (U Integer 'nothing)))
(define (leftmost tree)
  (cond
    [(3Node? tree) (if (3Node? (3Node-lsub tree))
                       (leftmost (3Node-lsub tree))
                       (3Node-root tree))]
    [else 'nothing]))
(check-expect (leftmost node-structure) 3)

;;Finds the max height of each subtree of the root node
(: max-branch (-> 3Tree Integer))
(define (max-branch tree)
  (cond
    [(3Node? tree) (max (height (3Node-lsub tree))
                        (height (3Node-msub tree))
                        (height (3Node-rsub tree)))]
    [else 0]))
(check-expect (max-branch node-structure) 3)

;;Checks if the current node is a leaf
(: check-leaf (-> 3Node Boolean))
(define (check-leaf nd)
  (cond
    [(or (3Node? (3Node-lsub nd)) (3Node? (3Node-msub nd)) (3Node? (3Node-rsub nd))) #t]
    [else #f]))

;;Returns the value of the farthest item from the root node
(: farthest-item (-> 3Tree (U Integer 'nothing)))
(define (farthest-item tree)
  (cond
    [(3Node? tree) (cond
                      [(and (= (max-branch tree) (height (3Node-lsub tree))) (check-leaf tree)) (farthest-item (3Node-lsub tree))]
                      [(and (= (max-branch tree) (height (3Node-msub tree))) (check-leaf tree)) (farthest-item (3Node-msub tree))]
                      [(and (= (max-branch tree) (height (3Node-rsub tree))) (check-leaf tree)) (farthest-item (3Node-rsub tree))]
                      [else (3Node-root tree)]
                      )]
    [else 'nothing]))            
(check-expect (farthest-item node-structure) 5)

;;Define the incremented tree, which has values +1 that of previous tree
(define node-structure-increment (3Node 2
       (3Node 3
              (3Node 4 '3Empty '3Empty '3Empty)
              (3Node 5 '3Empty '3Empty '3Empty)
              '3Empty)
       (3Node 9
              '3Empty
              (3Node 8
                     (3Node 6 '3Empty '3Empty '3Empty)
                     '3Empty
                     (3Node 7 '3Empty '3Empty '3Empty))
              '3Empty)
       (3Node 10
              '3Empty
              '3Empty
              (3Node 1 '3Empty '3Empty '3Empty))))

;;Adds 1 to all nodes of the tree
(: increment (-> 3Tree 3Tree))
(define (increment tree)
  (cond
    [(3Node? tree) (3Node (+ 1 (3Node-root tree)) (increment (3Node-lsub tree)) (increment (3Node-msub tree)) (increment (3Node-rsub tree)))]
    [else tree]))
(check-expect (increment node-structure) node-structure-increment)

;;Defines the type StepFunction that is either a Real of a SFNode
(define-type StepFunction (U Real SFNode))

;;Defines SFNode with a left StepFunction and right StepFunction
(define-struct SFNode
  ([left  : StepFunction]
   [right : StepFunction]))

;;Helper function for eval-sf, determines the interval of argument i
(: eval-sf-interval (-> StepFunction Exact-Rational Exact-Rational Exact-Rational Real))
(define (eval-sf-interval sf i lb rb)
  (cond
    [(SFNode? sf) (cond
                    [(> i (/ (+ lb rb) 2)) (eval-sf-interval (SFNode-right sf) i (/ (+ lb rb) 2) rb)]
                    [else (eval-sf-interval (SFNode-left sf) i lb (/ (+ lb rb) 2))]
                          )]
    [else sf]))

;;Evaluates a StepFunction and returns value of the inputted number
(: eval-sf (-> StepFunction Exact-Rational Real))
(define (eval-sf sf x)
  (eval-sf-interval sf x 0 1))
(check-expect (eval-sf (SFNode (SFNode 2 (SFNode 6 3)) 4) 1/3) 6)
(check-expect (eval-sf (SFNode (SFNode 2 (SFNode 6 3)) 4) 99/100) 4)
(check-expect (eval-sf (SFNode (SFNode 2 (SFNode 6 3)) 4) 1/4) 2)
(check-expect (eval-sf (SFNode (SFNode 2 (SFNode 6 3)) 4) 7/16) 3)

(test)