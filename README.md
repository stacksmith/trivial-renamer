# trivial-renamer

Trivial-renamer is a lisp tool for renaming objects using regular expressions, and maintaining mappings between old and new names.  It may be useful as a component in projects such as file managers, MP3 renamers, and FFI generators.

TRIVIAL-RENAMER operates on 'categorized' names.  Some examples of categories (which may be arbitrary lisp objects) are: file types/extionsions, music album or songs, or FFI types. 

Prior to renaming, TRIVIAL-RENAMER requires that you provide it with rules for categories.  Each category can have one or more rules; each rule is a pair containing a regex and the substitution string.  These are usually bulk-loaded as a list or rules, although you can add them dynamically.  

The renaming process consists of rule-based renaming followed by a call to a 'normal' function you provide.  If no rules can be found, the 'default' renaming function is called.  If requested, the renamer (a) cache the results, and/or (b) check for 1-1 correspondence by maintaining a reverse cache.

After renaming, the hashtables built are made available to your application.  

Any number of different renamers can be active at the same time.

## Dependencies

Alexandria, cl-ppcre

## Example

The defaults create a renamer that expects string objects.  A simple downcase renaming function is provided to test the functionality.  

```
(ql:quickload :trivial-renamer)
(defparameter *test* 
  (make-instance 'renamer:renamer
  :default (lambda (str obj renamer) (string-downcase str))
  
(renamer:rename "HELLO WORLD" t *test*); category T will default
"hello world"

(renamer:rule-add *test* 'cat1 '(("e" . "3"))) 

(renamer:rename "monkey" 'cat1 *test*)
"monk3y"
```


## Rationale and use case

The raison d'etre of this library is a cffi binding generator.  Translating C identifier names to Lisp required keeping custom name translation rules based on type of C object, its namespace, and even the object's actual name, for total control of renaming.

After renaming, there is still the daunting task of maintaining a relationship between old and new names, uniqueness of mapping, etc.

The required functionality appeared to be well suited for a library.


## Keys to initialize the renamer

 KEY | DEFAULT | DESCRIPTION
 --- | ------- | -----------
:OVERWRITE | T | if T, overwrite rules if category is introduced more than once. Otherwise, add new rules in front.
:VALIDATE | NIL |`(lambda (key value r))` optional function to validate a rule.  If it returns, rule is valid.
:CATEGORIZE | obj copy | `(lambda (obj))` optional function that returns object's category. 
:GET-NAME | obj copy |  `(lambda (obj))` optional function that returns object's original name. 
:DEFAULT | obj copy | `:default (lambda (oldname obj renamer)...)` called if no rule is found, return string.
:NORMAL | obj copy | `:default (lambda (newname obj renamer)...)` called after rule is processed, return string.
:ONE-TO-ONE | T | if T, enforce one-to-one correspondence of old and new names.
:CACHE | T | if T, memoize each call to rename, and when matched, return previous result immediately.


## Functions

```
(rule-add renamer category rule)

rule is a list of (regex . newstring) pairs.  Each matched regex is substituted with newstring.


(rules-add renamer rules)

Rules is a list in the form of
'(category ((regex . newstring) (regex .newstring))
  category ((...))
  ...)
  

(rename obj renamer)

Returns the new name of obj


If cache is enabled, the object to name hashtable is available as (old->new renamer)  

Similarly, if 1-1 is enabled, the new name to obj hashtable is (new->old renamer)

