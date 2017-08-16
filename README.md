# TRIVIAL-RENAMER

TRIVIAL-RENAMER (RENAME) is a configurable Lisp tool for renaming objects using regular expressions, and maintaining mappings between old and new names.  It may be useful as a component in projects such as file managers, MP3 renamers, and FFI generators.

TRIVIAL-RENAMER operates on 'categorized' names.  Some examples of categories (which may be arbitrary lisp objects) are: file types/extensions, music album or songs, or FFI types. 

Prior to renaming, TRIVIAL-RENAMER requires that you provide it with rules for categories.  Each category can have one or more rules; each rule is a pair containing a regex and the substitution string.  These are usually bulk-loaded as a list or rules, although you can add them dynamically.  

The renaming process consists of rule-based renaming followed by a call to a 'normal' function you provide.  If no rules can be found, the 'default' renaming function is called.  If requested, the renamer will (a) cache the results, and/or (b) check for 1-1 correspondence by maintaining a reverse cache.

Both the format of the rules and the transformation function are configurable; the default is to perform regex text substitution based on rules that contain `(regex . newstring)`.

After renaming, the hashtables built are made available to your application.  

Any number of different renamers can be active at the same time.

## Dependencies

Alexandria, cl-ppcre

## Example

The defaults create a renamer that expects string objects.  A simple downcase renaming function is provided to test the functionality.  

```
CL-USER> (ql:quickload :trivial-renamer)
...
CL-USER> (make-instance 'rename:renamer)
#<TRIVIAL-RENAMER:RENAMER {...}>

CL-USER> (rename:please "HELLO WORLD") ;; default is downcasing
"hello world"

CL-USER> (rename:rule 'cat1 '(("e" . "3")))  ;; add a regex substitution rule

(renamer:please "monkey" 'cat1)
"monk3y"
```
## Defaults

The example above shows a number of defaults in use.  For instance, both normal and default renaming routines start out as lowercasing the string, unless set.  If the renamer itself is not specified, `rename:*default-renamer*` is used which is useful for testing.  If the category is not specified, T is used for category.  See the documentation below for more information.

## Rationale and use case

The raison d'etre of this library is a cffi binding generator.  Translating C identifier names to Lisp required keeping custom name translation rules based on type of C object, its namespace, and even the object's actual name, for total control of renaming.

After renaming, there is still the daunting task of maintaining a relationship between old and new names, uniqueness of mapping, etc.

The required functionality appeared to be well suited for a library.


## Keys to initialize the renamer

 KEY | DEFAULT | DESCRIPTION
 --- | ------- | -----------
:TEST | #'equal | equality test for category hashtable.
:OVERWRITE | T | if T, overwrite rules if category is introduced more than once. Otherwise, add new rules in front.
:VALIDATE | NIL |`(lambda (key value r))` optional function to validate a rule.  If it returns, rule is valid.
:DEFAULT | lowercase | `:default (lambda (oldname renamer)...)` called if no rule is found, return string.
:NORMAL | obj copy | `:default (lambda (newname renamer)...)` called after rule is processed, return string.
:ONE-TO-ONE | T | if T, enforce one-to-one correspondence of old and new names.
:CACHE | T | if T, memoize each call to rename, and when matched, return previous result immediately.
:TRANSFORM | * | renaming function; see below

## Functions

### (rename:rule category &optional renamer)

 rule | a list of (regex . newstring) pairs.  Each matched regex is substituted with newstring.
 category | any object that works as a key in the rule hashtable (with :TEST)


### (rename:rules list &optional renamer)

list should contain rules:
'(category ((regex . newstring) (regex .newstring))
  category ((...))
  ...)
  

### (rename:please oldname &optional renamer)

Returns the new name.

If cache is enabled, the object to name hashtable is available as (old->new renamer)  

Similarly, if 1-1 is enabled, the new name to obj hashtable is (new->old renamer)

### (rename:reset &optional renamer)

Clear the renamer data and rules.

## Transform function

(lambda name category rules renamer)

It is possible to completely change the behavior of TRIVIAL-RENAMER by installing a different version of this function: the format of the rules, the actual transformations, etc.

The default transform expects each rule to be in the form of `(regex . replacement)`, and repeatedly attempts to perform the replacements on the name.

