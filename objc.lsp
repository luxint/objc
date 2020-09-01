;;@module objc.lsp
;;@description Minimalistic Cocoa / Objective-C bridge

(import "/System/Library/Frameworks/Foundation.framework/Foundation")
(import "/System/Library/Frameworks/AppKit.framework/AppKit")
(set '@class (import "libobjc.dylib" "objc_getClass"))
(set '@selector (import "libobjc.dylib" "sel_getUid"))
(set '@send (import "libobjc.dylib" "objc_msgSend"))

;; @syntax (-> pointer selector [arg1 arg2 ...])
;; @param <ptr-selector> [<arg1> <arg2> ..]
;; @param <str-selector> [<arg1> <arg2> ..]
;; @return <pointer>
;; principal function sending a message to an instance or classname with a variable 
;; number of arguments, use when the arguments are NOT floating points or structs
;; returns a pointer

(define (-> ptr sel)
	(when (string? ptr) (set 'ptr (@class ptr)))
  (eval (flat (list '@send ptr (@selector sel) (args)))))

;; @syntax (-> pointer selector [arg1 arg2 ...])
;; @param <ptr-selector> [<arg1> <arg2> ..]
;; @param <str-selector> [<arg1> <arg2> ..]
;; @return <pointer>
;; function sending a message to an instance or classname with a variable number 
;; of arguments, use when the arguments contain floats or structs. The address of the return 
;; value is returned

(define (=> target selector)	
	(when (string? target)
		(set 'target (@class target)))
	(set 'sel (@selector selector))
	(set 'method (-> target "methodSignatureForSelector:" sel))
	(set 'invocation (-> "NSInvocation" "invocationWithMethodSignature:" method))
	(-> invocation "setSelector:" sel)
	(doargs (arg)
		(-> invocation "setArgument:atIndex:" (address arg) (+ $idx 2)))
	(-> invocation "invokeWithTarget:" target)
	(set 'return (dup "\000" (-> method "methodReturnLength")))
	(-> invocation "getReturnValue:" (address return))
	(address return))

; utility functions
;; utility string function 
;; @syntax (@nss str)
;; @param <str> 
;; @return <pointer>
(define (@nss str)
	(-> "NSString" "stringWithUtf8String:" str))

(struct 'Point "double" "double")
(struct 'Rect "Point" "Point")

;; utility function to return a point structure
;; @syntax (makepoint int-w int-h)
;; @param <int-w> width
;; @param <int-w> height
;; @return packed point structure
(define (makepoint w h)
	(pack Point w h))

;; utility function to return a rect structure
;; @syntax (makepoint int-w int-h)
;; @param <int-x> left x coordinate
;; @param <int-y> top y coordinate
;; @param <int-w> width
;; @param <int-w> height
;; @return packed rect structure
(define (makerect x y w h)
	(pack Rect (pack Point x y) (pack Point w h)) )

;; test function to display a window with a button
;; @syntax (test)

(define (test)
	(set 'nsapp (-> "NSApplication" "sharedApplication"))
	(-> nsapp "setActivationPolicy:" 1)
	(set 'window (-> "NSWindow" "alloc"))
	(set 'rect (makerect 150 150 200 300))
	(set 'mask 15 'backing 2 'defer 0)
	(=> window "initWithContentRect:styleMask:backing:defer:" rect mask backing defer)
	(set 'button (-> "NSButton" "alloc"))
	(=> button "initWithFrame:" (makerect 100 150 80 40))
	(-> button "setBezelStyle:" 2)
	(-> (-> window "contentView") "addSubview:" button)
	(-> window "makeKeyAndOrderFront:" window)
	(-> nsapp "activateIgnoringOtherApps:" 0)
	(-> nsapp "run"))