# HaXPath
HaXPath is a library and embedded domain-specifc language which uses strongly-typed Haskell expressions to represent
XPaths. It is available on Hackage [here](https://hackage.haskell.org/package/HaXPath).

## Motivation
In many contexts when querying XML documents in Haskell we often need to use `String` values to represent the
XPaths we want to use. These `String` expressions can quickly become hard to manage as they do not take advantage of
Haskell's type system, particularly for more complex XPaths. We may not know until run-time whether the XPath is even
syntactically valid. HaXPath does not have its own XPath engine to run the queries, rather it is expected to be used
in combination with other libraries which have such functionality. Instead, we can simply convert the strongly-typed
XPath expressions to `String` or `Text` and send them to our favourite APIs.

## HaXPath API
HaXPath provides two core APIs: the standard API (`HaXPath` module) allows for expressing generic XPaths, while
the schematic API (`HaXPath.Schematic` module) is a layer of abstraction built upon the standard API which constrains
XPath expressions so they must follow a specifc document schema.

### Standard API
`HaXPath` modules are expected to be imported qualified as otherwise you will get name conflicts with the Prelude. The
operators however need not be qualified, and can conveniently be imported directly from `HaXPath.Operators`. All
operators are suffixed with `.`, with the exception of the `#` operator.

Some basic examples:

https://github.com/hgrano/HaXPath/tree/master/examples/HaXPath/Examples.hs

### Schematic API
The schematic API provides further constraints than the standard API by only allowing paths that are valid with respect
to some custom schema. Take for example the following XML document for a restaurant menu:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<menu>
  <item name="Belgian Waffles" price="$5.95"></item>
  <item name="Strawberry Waffles" price="$7.95"></item>
  <item name="French Toast" price="$4.50"></item>
</menu>
```
It should be fairly intuitive that there is an underlying schema to the above document. We can express this using the
`HaXPath.Schematic` module:

https://github.com/hgrano/HaXPath/tree/master/examples/HaXPath/Schematic/Examples.hs
