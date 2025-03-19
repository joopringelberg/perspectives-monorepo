# Parsing forked
I have modified the type of `IndentParser` in the file Indent.purs. It used to be:

	type IndentParser s a = ParserT s (State Position) a

and now is:

	type IndentParser m s a = ParserT s (StateT Position m) a

This is to make it possible to base parsers on another monad then Identity (e.g. Aff, to log information to the console). As a consequence, all references to IndentParser had to be adapted, too (only the file IndentParser contains such references).

## How to use
If you refer to Parser in your spago.dhall (`parser`), then add to your `packages.dhall` the following reference:

```
  with parsing.repo = "https://github.com/joopringelberg/purescript-parsing.git"
  with parsing.version = "v7.0.0-transformer-tagged"
```

for the latest version published in purescript-contrib, combined with those modifications.

## How to develop
Only the source file Indent.purs has been changed. I executed the following steps to produce the current version from an old modified version and the upstream version v7.0.0.

* Update the master in Github with all upstream commits.
* Pull those changes to the local repository (in the master).
* Pull the tags to local:

```
git fetch --all --tags
```
* Checkout the latest released tag and turn it into a branch:

```
git checkout tags/<version tag> -b <new branch name>
```
* Merge the previous branch with modifications with the new one:

```
git merge <previous branch with modifications>
```
* Add a tag.
* push the new branch to remote:

```
git push origin <new branch name>
```
