# Concrete Haskell library

## Installation

End-users with a recent GHC version can run `cabal install concrete-haskell`.  

For development, the environment is managed by [Stack](https://docs.haskellstack.org/en/stable/README/) which is quite heavy-weight as it completely sand-boxes the full Haskell compiler and all libraries by default.  You will need the zlib and bz2lib compression libraries installed, and you can install stack and put it on your executable path with:

```
mkdir -p ~/.local/bin
wget https://www.stackage.org/stack/linux-x86_64-static -O - | tar xpz -C ~/.local/bin --strip-components=1 stack-1.4.0-linux-x86_64-static/stack
export PATH=~/.local/bin:${PATH}
```

You can then invoke stack from within this repository:

```
cs concrete-haskell
stack setup
stack init --omit-packages
```

Edit `stack.yaml` such that the corresponding entries are:

```
packages:
  - .

extra-deps:
  - thrift-0.10.0
  - megaparsec-5.3.0

allow-newer: true
```

You should now be able to run `stack build`, `stack install`, `stack test`, and so forth.  The first time you run one of these commands, it will take a while to build all the dependencies and so forth (maybe 10 minutes or so), but thereafter should only be recompiling code that changes.

## Inspecting Data

Currently, the `inspect_communications` tool only handles gzipped Communications (*not* Tar files of Communications) due to this being the focus of `ingest_communications`.  Expanding to handle various compression (gz, bz2, etc) and archiving (tar, zip) formats is straightforward and this functionality will be here soon.

## Ingesting Data

This library was initially created to leverage Haskell's powerful monadic parsing libraries as a uniform way of converting various existing data formats into Concrete Communications.  In general, the command to ingest a text file in format FORMAT is:

```
cat INPUT.txt | stack exec ingest_communications -f FORMAT -t TYPE -I ID -s KIND1 -s KIND2 > OUTPUT.gz
```

### Implementation State

It currently handles:

*  JSON

In the near future, it should handle:

*  CONLL-X
*  Email (RFC2822)
*  CSV


This uses the parser associated with FORMAT, telling it to produce Communications where comm.type=TYPE, comm.id=ID, sections of kinds KIND1 and KIND2 are considered "content" and others are considered "metadata", and writes them to standard output as a gzipped stream (*not* as a gzipped Tar file).  If your format is supported by concrete-haskell, check the brief description above to determine if you need to do anything beforehand.  If your data is valid according to the relevant spec, it should work: failure on e.g. valid JSON is a bug, so please open a ticket.

### Defining a New Parser

Adding the functionality to ingest a new format is, in principal, very simple.  Let's assume the format is called `Foobar`, and you have a fresh clone of the concrete-haskell repository.  First, you might want to create a new branch:

```
git checkout -b parse-Foobar
```

Make sure that you can build the vanilla library:

```
stack build
```

### Simple Changes

In the `concrete-haskell.cabal` file, add `Data.Concrete.Parsers.Foobar` to the list of exposed modules.  Then, in the file `src/Data/Concrete/Parsers.hs`, add `import qualified Data.Concrete.Parsers.Fubar as Fubar` to the imports, and `("Fubar", ("Fubar format", Fubar.arrayOfObjectsP)` to the variable `communicationParsers`.  Finally, copy the file `src/Data/Concrete/Parsers/JSON.hs` to `src/Data/Concrete/Parsers/Fubar.hs`, and at the top change `module Data.Concrete.Parsers.JSON` to `module Data.Concrete.Parsers.Fubar`.  You should now be able to build the project and run `cat file.txt | stack exec ingest_communications -f Fubar ... > out.gz`: of course, it's still a JSON parser, but you can now focus just on modifying `src/Data/Concrete/Parsers/Fubar.hs`.

### The Real Work: Fubar.hs

**NOTE: in Haskell, a "parser" and a "parse rule" are actually the same type, although colloquially the former refers to a "top-level" rule, defined in terms of the latter**

At its core, this is a straightforward context-free grammar describing the input format, almost a direct translation of RFC7159 into the formalisms of the [Megaparsec](https://hackage.haskell.org/package/megaparsec-5.3.0/docs/Text-Megaparsec.html) library.  This looks very much like the ubiquitous Backus-Naur form.  Using the magic of [Monad transformers](https://hackage.haskell.org/package/mtl), a `Bookkeeper` structure is threaded throughout the parsing process.  Each parse rule has full access to the `Bookkeeper`, which is responsible for gathering information to build the Communication *currently being parsed*:

```
data Bookkeeper = Bookkeeper { communication :: Communication
                             , valueMap :: Map String String
                             , path :: [String]
                             , sections :: [Section]
                             , action :: CommunicationAction
                             , contentSections :: [String]
                             , commId :: Text
                             , commType :: String
                             }
```

When a Communication is fully-assembled, it's UUID values are populated, it's fed to a function of type `CommunicationAction :: Communication -> IO ()`, and the `Bookkeeper` is reset, to start building the *next* Communication.  In this fashion, the ingester never needs to store more than one Communication in memory at a time.

So, there are two primary tasks for writing a `Fubar` ingester:

1.  Write the parse rules that match the `Fubar` specification
2.  Augment the rules to modify the `Bookkeeper` such that Communications are built correctly

Writing parsers (1) in Haskell is a joy, but you can probably get pretty far just modifying the JSON parser, looking at snippets online, etc.  If you leave the line `arrayOfObjectsP :: CommunicationParser ()` alone, you can't go too far astray!  But, it's a demanding language, a powerful library, and we can't really cover how to write a parser in this introduction.  The [Megaparsec](https://hackage.haskell.org/package/megaparsec-5.3.0/docs/Text-Megaparsec.html) documentation should be useful, and feel free to open tickets with specific questions or problems you encounter.

Augmenting the rules (2) is easier than writing the parser, but less fun: it involves a lot of arcane Concrete-structure-foo and parse-state introspection.  To help with this, there are a few special functions you can drop right into your parse rules to handle common tasks.  In the JSON code, the first of these you see is:

```
communicationRule :: (Communication -> Communication) -> CommunicationParser a -> CommunicationParser a
```

Don't bother tracking down its definition, it's ugly!  But it does something very pretty: the signature shows it takes a function for modifying a Communication (for JSON we give it `id`, because we don't need to make any adjustments) and a parse rule, and returns a *new* parse rule.  The semantics, in plain English, are that it takes one of your parse rules and says "this corresponds to a Communication, so, start with a fresh `Bookkeeper`, and after the rule has been applied, turn everything we've accumulated (in the `Bookkeeper`) into a new Communication and apply our `CommunicationAction` to it".  It takes care of recording the raw text that was matched, syncs the appropriate TextSpans, generates the UUIDs, makes a Communication id based on a template supplied on the command line (see below), and all that good stuff.  And you can even make arbitrary last-minute adjustments by supplying something other than `id` as the first argument.  Bottom line, you probably want to use this on the rule that matches whatever you consider a `Communication` in your data (a single email, a line in a CSV, a certain XML tag, etc).

The other special function you see in the JSON code is:

```
sectionRule :: (Section -> Section) -> CommunicationParser a -> CommunicationParser a
```

Look familiar?  Similar to the `communicationRule`, it is used to state that a given parse rule corresponds to a *Section*.  Again, avoid the messy code: it boils down to saying "record the start and end of where this rule matches, create a new Section, and push it onto the list of Sections in the `Bookkeeper`".  There are two other important fields for a Section: `label` and `kind`.  Where do these come from?  Recall that `Bookkeeper` has a field called `path`: this is intended to be used in a stack-like fashion to track the current structural location.  As a simple example, if parsing the Bible, you would start parsing a book, you push (["Genesis"]), then a chapter (["1", "Genesis"]) and a verse (["1", "1", "Genesis"]).  When you finish the current verse, you pop (["1", "Genesis"]), start the next verse and push (["2", "1", "Genesis"]) and so forth.  At any given time, we get a `label` for the current section by reverse-joining the stack with ".".  Take a look at the parse rules `arrayP`, `arrayEntryP`, and `pairP`: those `modify'` calls are simply pushing and popping the indices and key values for JSON arrays and objects, so we end up with Section labels like "person.friends.3.name".  As for the `kind` field, the user has provided a set of Section labels that should be treated as "content" ("-s body -s description") and the rest are treated as "metadata".

Two final points: first, you'll notice that, while most of the time `sectionRule` is given `id` as its first argument, for the `stringP` rule it is given `adjustTextSpan 1 (-1)`.  The reason is simple: the outer quotation-marks of a JSON string aren't part of the string itself.  Second, we need to explain where the Communication `id` field comes from.  It is a string specified on the command line with substitution patterns that are filled by the Sections.  For example, if the Communication has a Section with label "user.name", whose TextSpan points to the string "Brian", and the command line specification was "XYZ ${user.name}", the Communication id would be "XYZ Brian".  Often the best choice is simply the name of the source and the Section label of its unique identifier, e.g. "Twitter ${id}" or "Reddit ${id}".  An empty substitution ("${}") is replaced with the 0-based index of the Communication in the current ingest process, and so can be used to arbitrarily differentiate Communications in a given data set.

Hopefully, this has given you a solid overview of how the Concrete Haskell ingester works, so modifying `Fubar.hs` seems more tractable.
