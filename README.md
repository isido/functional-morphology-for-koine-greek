Functional Morphology for Koine Greek
=====================================

Introduction
------------

This is a program attempting to provide morphology for Koine Greek. It
is based on Functional Morphology by Markus Forsberg and Aarne Ranta
[1]. The original FM package implemented Latin morphology, this Greek
package is based on the Latin definitions and some of the old Latin
code still exists in files. The old code will gradually disappear, as
the Greek support becomes more complete.


Building and running
--------------------

To build the project, you can use the Haskell Tool Stack
(https://docs.haskellstack.org/en/stable/README/). After you have
installed the Stack, you can just type

    $ stack build
	
which will build the executable.

When you have successfully compiled the program, 

     $ stack exec morpho_greek

will run the program.

     $ stack exec morpho_greek -h

will give you a list of command line options. A very small dictionary is
provided.

To actually run the program with sample dictionary, type

     $ stack exec morpho_greek greek.lexicon

Now you can try inflections for words contained in the dictionary. If
the program recognizes word, it will output morphological analysis for
the word.

Status and Todo
---------------

This project has been in a long hiatus, due to lack of time, and the
fact that it was written in Haskell 98, which didn't compile in modern
GHCs. I finally updated the program to compile with lots of warnings,
but I still don't have much time to spend on this. Anyway, the overall
plan is as follows (not necessary in order):

1) Fix warnings and clean up code
2) Implement more Greek grammar
3) Try to expose the analyser as a web service
4) Handle accents correctly

Currently, the morphological analysis is very much incomplete. Support
is most complete for nouns, there is some incomplete support for
verbal paradigms.

*Note:* the program does not understand accents! Breathing marks also
cause problems so they are not consistently in use. Iota subscriptum,
on the other hand, is relevant!

Even though the targetted Greek dialect is Koine Greek, I've used
"Oxford Grammar of Classical Greek" by James Morwood (Oxford
University Press 2001) as the basis for definitions and grammatical
categories. This might change in future, if I find a handy and
trustworthy grammar for Koine dialect.

Authors
-------

Functional Morphology framework was written by Markus Forsberg and
Aarne Ranta. The Greek implementation is by Ilja Sidoroff
(ilja.sidoroff  @  iki.fi). It is licensed by terms of GNU General
Public License.



[1] http://www.cs.chalmers.se/~markus/FM/
