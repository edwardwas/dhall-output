    let types = ./types.dhall 

in  let foobar = types.FooBar

in  let cons = constructors foobar

in  {_1 = cons.Foo { foo = 2 } : foobar, _2  = {bleep = "Hi"} : types.TestNewtype}
