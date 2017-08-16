# trivial-renamer

Trivial-renamer is a lisp tool for renaming named objects using regular expressions, and maintaining mappings between old and new names.

In order to accomplish that, TRIVIAL-RENAMER requires that you provide it with three things:

1) A function that returns the object's original name `:get-name (lambda (obj)...)`;
2) A function that categorizes objects you intend to use `:categorize (lambda (obj)...)`;
3) A set of renaming rules for each category.  Each category matches to a list of  (regex . replacement-string) pairs.

In addition, TRIVIAL-RENAMER nees two functions:

1) A function that is called after rule-based renaming takes place `:normal (lambda (newname obj renamer)...)`
2) A function called if no rule was found `:default (lambda (oldname obj renamer)...)`

Using that information TRIVIAL-RENAMER can apply these rules to objects you provide, renaming them on-the-fly, or caching the result for performance if `:cache T`, and keeping a one-to-one name correspondence if `:one-to-one T`.

Any number of different renamers can be active at the same time.


## Rationale and use case

The raison d'etre of this library is a cffi binding generator.  Translating C identifier names to Lisp required keeping custom name translation rules based on type of C object, its namespace, and even the object's actual name for scalpel-precision renaming.

Afterwards, I still had to deal with the mess of maintaining a relationship between old and new names, uniqueness of mapping, etc.

## Dependencies

Alexandria, cl-ppcre

## Example

The defaults create a renamer that expects string objects.  A simple downcase renaming function is provided to test the functionality.  To demonstrate categorization, we shall use the first 3 characters as a category.  Keep in mind that objects and categories need not be strings!

```
(ql:quickload :trivial-renamer)
(defparameter *test* 
  (make-instance 'renamer:renamer
  :default (lambda (str obj renamer) (string-downcase str))

  :categorize (lambda (obj) (subseq obj 0 3))
  
(renamer:rename "HELLO WORLD" *test*)
"hello world"

(renamer:rule-add (*test* "mon" '(("e" . "3")))) 

(renamer:rename "monkey" *test*)
"monk3y"
```

## Keys to initialze the renamer

 KEY | DEFAULT | DESCRIPTION
 ---   -------   -----------
:OVERWRITE | T | if T, overwrite rules if category is introduced more than once. Otherwise, add new rules in front.
:VALIDATE | NIL |`(lambda (key value r))` optional function to validate a rule.  If it returns, rule is valid.
:CATEGORIZE | obj copy | `(lambda (obj))` optional function that returns object's category.  Default: object.
:GET-NAME | obj copy |  `(lambda (obj))` optional function that returns object's original name.  Default: object.
:DEFAULT | obj copy | `:default (lambda (oldname obj renamer)...)` called if no rule is found, return string.
:NORMAL | obj copy | `:default (lambda (newname obj renamer)...)` called after rule is processed, return string.
:ONE-TO-ONE | T | if T, enforce one-to-one correspondence of old and new names.
:CACHE | T | if T, memoize each call to rename, and when matched, return previous result immediately.

  
  
