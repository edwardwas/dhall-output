    let types = ./types.dhall 

in  let foobar = types.FooBar

in  let cons = constructors foobar

in  cons.Foo { foo = 2 } : foobar
