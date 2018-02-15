# Cloogle

This is the core system of [Cloogle][], the [Clean][] language search engine.
The source code of the web frontend is in
[clean-cloogle/cloogle.org](https://github.com/clean-cloogle/cloogle.org).

## Structure
The type database is a Clean data structure that can be stored on the disk
using generic JSON encode and decode functions. The functions in `CloogleDB`
provide low-level access to modify the database and search in it.

The `CloogleDBFactory` module hooks into the Clean compiler to provide
functions to populate a type database by parsing Clean source code. For
indexing documentation it was needed to patch the Clean compiler. To this end,
some modules from the frontend of the compiler have been replicated in
`/compiler-patch`. This directory should be added to the clm include paths
before the paths of the compiler.

The functions in `Search` provide a higher level API to the type database,
using the common Cloogle types defined in
[libcloogle](https://github.com/clean-cloogle/libcloogle).

## Dependencies

The Clean compiler is needed to build the database (not to use it).
Unfortunately, it is [developed on subversion][cocl] which means we can't add
it as a git submodule. The compiler patches are compatible with revision 2796
(2017-04-28). The following make target takes care of the setup:

```make
clean-compiler:
	svn checkout -r 2796 https://svn.cs.ru.nl/repos/clean-compiler/branches/itask/ clean-compiler
	$(MAKE) -j -C clean-compiler/main/Unix
	$(MAKE) -j -C clean-compiler/backendC/CleanCompilerSources -f Makefile.linux64
	ln -s ../../backendC/CleanCompilerSources/backend.a clean-compiler/backend/Clean\ System\ Files/backend_library
```

## Clean documentation
Cloogle indexes documentation of the syntax elements it stores, through
functions in `Doc`. Docblocks are comments that start with `/**` and have a
leading asterisk on every line (leading whitespace is ignored). The first part
of the docblock is taken as a general description. Below the description,
documentation fields can be added with `@`. Currently, documentation fields
should have only one line.

An example is below:

```clean
/**
 * Apply a function to every element in a list.
 *
 * @param The function
 * @param The list
 * @result The new list
 */
map :: (a -> b) [a] -> [b]
```

For short documentation items, doclines, starting with `//*` can be used. When
documenting a constructor, or record field, they should be placed *after* the
item they document. Doclines are only supported for constructors and record
fields. For example:

```clean
/**
 * A date in the Gregorian calendar
 */
:: Date
	= { day   :: Int  //* The day of the month, starting with 1
	  , month :: Int  //* The month (January is 1)
	  , year  :: Int  //* The year
	  }
```

The tables below describe which fields and documentation types can be used for
different syntax elements, and what they should document.

|              | Description | `@param` | `@result` | `@var` | `@representation`
|--------------|-------------|----------|-----------|--------|-------------------
| Class        | ![][y]      |          |           | ![][y] |
| Class member | ![][y]      | ![][y]   | ![][y]    |        |
| Constructor  | ![][y]      |          |           |        |
| Function     | ![][y]      | ![][y]   | ![][y]    |        |
| Generic      | ![][y]      | ![][y]   | ![][y]    | ![][y] |
| Instance     |             |          |           |        |
| Macro        | ![][y]      | ![][y]   | ![][y]    |        |
| Module       | ![][y]      |          |           |        |
| Record field | ![][y]      |          |           |        |
| Type         | ![][y]      |          |           | ![][y] | ![][y], for type synonyms

| Field             | Description
|-------------------|-------------
| `@param`          | Parameters of a function(-like)
| `@representation` | The representation of a synonym type
| `@result`         | The result of a function
| `@return`         | A deprecated synonym of `@result`
| `@var`            | Type variables of types, classes and generics

## Copyright &amp; License
Copyright &copy; Mart Lubbers and Camil Staps.
Licensed under MIT; See the `LICENSE` file.

[Clean]: http://clean.cs.ru.nl
[Cloogle]: https://cloogle.org
[cocl]: https://svn.cs.ru.nl/repos/clean-compiler

[y]: http://i.stack.imgur.com/iro5J.png
