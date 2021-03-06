# cast_trait_object

This crate offers functionality for casting between trait objects using only
safe Rust and no platform specific code. If you want to downcast to concrete
types instead of other trait objects then this crate can't help you, instead
use something like the [`downcast-rs`] crate.

<!-- Generate README.md using `cargo readme --no-license > README.md` -->
<!-- Generate documentation using `cargo +nightly doc --features docs` -->

## Usage

You should use the [`DynCast`] trait in trait bounds or as a supertrait and
then do casts using the methods provided by the [`DynCastExt`] trait. The
[`DynCast`] trait takes a type parameter that should be a "config" type
generated by the [`create_dyn_cast_config`] macro, this type defines from
which trait and to which trait a cast is made. Types that need to allow casting
to meet the [`DynCast`] trait bound can then implement it via the
[`impl_dyn_cast`] macro.

## Examples

```rust
use cast_trait_object::{create_dyn_cast_config, impl_dyn_cast, DynCast, DynCastExt};

create_dyn_cast_config!(SuperToSubCast = Super => Sub);
create_dyn_cast_config!(SuperUpcast = Super => Super);
trait Super: DynCast<SuperToSubCast> + DynCast<SuperUpcast> {}
trait Sub: Super {}

struct Foo;
impl Super for Foo {}
impl Sub for Foo {}
impl_dyn_cast!(Foo as Super => Sub, Super);

let foo: &dyn Super = &Foo;
// Casting to a sub trait is fallible (the error allows us to keep using the
// `dyn Super` trait object if we want which can be important if we are casting
// movable types like `Box<dyn Trait>`):
let foo: &dyn Sub = foo.dyn_cast().ok().unwrap();
// Upcasting to a supertrait is infallible:
let foo /*: &dyn Super*/ = foo.dyn_upcast::<dyn Super>();
```

When implementing the [`DynCast`] trait via the [`impl_dyn_cast`] macro you
can also list the created "config" types instead of the source and target
traits:

```rust
impl_dyn_cast!(Foo => SuperToSubCast, SuperUpcast);
```

If the `proc-macros` feature is enabled (which it is by default) we can also
use procedural attribute macros to write a little bit less boilerplate:

```rust
use cast_trait_object::{dyn_cast, dyn_upcast, DynCastExt};

#[dyn_cast(Sub)]
#[dyn_upcast]
trait Super {}
trait Sub: Super {}

struct Foo;
#[dyn_cast(Sub)]
#[dyn_upcast]
impl Super for Foo {}
impl Sub for Foo {}
```

Note that `#[dyn_upcast]` does the same as `#[dyn_cast(Super)]` but it is a bit
clearer about intentions:

```rust
use cast_trait_object::{dyn_cast, DynCastExt};

#[dyn_cast(Super, Sub)]
trait Super {}
trait Sub: Super {}

struct Foo;
#[dyn_cast(Super, Sub)]
impl Super for Foo {}
impl Sub for Foo {}

// Upcasting still works:
let foo /*: &dyn Super*/ = foo.dyn_upcast::<dyn Super>();
```

## How it works

### How is the conversion preformed

Using the [`DynCast`] trait as a supertraits adds a couple of extra methods
to a trait object's vtable. These methods all essentially take a pointer to
the type and returns a new fat pointer which points to the wanted vtable.
There are a couple of methods since we need to generate one for each type of
trait object, so one for each of `&dyn Trait`, `&mut dyn Trait`,
`Box<dyn Trait>`, `Rc<dyn Trait>` and `Arc<dyn Trait>`. Note that these methods
are entirely safe Rust code, this crate doesn't use or generate any unsafe
code at all.

The [`DynCastExt`] trait then abstracts over the different types of trait
objects so that when a call is made using the [dyn_cast](DynCastExt::dyn_cast)
method the compiler can inline that static method call to the correct method
on the trait object.

### Why "config" types

We have to generate "config" types since we need to uniquely identify each
[`DynCast`] supertrait based on which trait it is casting from and into.
Originally this was just done using two type parameters on the trait, something
like `DynCast<dyn Super, dyn Sub>`, but that caused compile errors when they were
used as a supertrait of one of the mentioned traits. So now the traits are
"hidden" as associated types on a generated "config" type. To make this "config"
type more ergonomic we also implement a [`GetDynCastConfig`] trait to easily
go from the source trait and target trait to a "config" type via something
like `<dyn Source as GetDynCastConfig<dyn Target>>::Config`. This allows
the macros ([`impl_dyn_cast`], [`dyn_cast`] and [`dyn_upcast`]) to take traits
as arguments instead of "config" types, it also makes type inference work for
the [`DynCastExt`] trait.

### How does the macros know if a type implements a "target" trait or not

When a type implementing [`DynCast`] for a specific config and therefore
source to target trait cast the generated code must choose if the cast is
going to succeed or not. We want to return `Ok(value as &dyn Target)` if
the type implements the `Target` trait and `Err(value as &dyn Source)` if
it doesn't.

We can use a clever hack to determine if the type implements the `Target`
trait. See the [`impls`](https://crates.io/crates/impls) crate's
[github page](https://github.com/nvzqz/impls#how-it-works) for how this hack
works. In short the hack allows getting a const bool that is `true` if a type
implements a trait and `false` otherwise.

So we could generate something like:

```rust,compile_fail
trait Source {}
trait Target {}

struct Foo;
impl Source for Foo {}

const IMPLEMENTS_TRAIT: bool = false /* Really should use impls!(Foo: Target) */;

impl Foo {
    fn cast(&self) -> Result<&dyn Target, &dyn Source> {
        if IMPLEMENTS_TRAIT {
            // Compile time error here:
            Ok(self)
          //   ^^^^ the trait `Target` is not implemented for `Foo`
        } else {
            Err(self)
        }
    }
}
```

But it fails to compile even though we will never actually run the code that
coerces `Foo` to `Target`. So since coercing a type to a trait it doesn't
implement is a type error we need to use our const value to affect the types
in the generated code somehow.

This can be done by using the const value as a length of an array to get a
type (note that a bool can be converted to a `usize`). Once we have a type we
can use Rust's powerful type system to choose different methods based on our
initial value:

```rust
struct Choose<T>(T);
impl Choose<[(); 0]> {
    fn foo(arg: usize) -> &'static str { "false" }
}
impl Choose<[(); 1]> {
    fn foo(arg: String, arg2: bool) -> bool { true }
}
// These methods have the same name but are actually totally different methods:
let foo: &'static str = Choose::<[(); 0]>::foo(1);
let foo: bool = Choose::<[(); 1]>::foo("some text".to_string(), false);
```

or using a trait:

```rust
trait AltChoose { type Result; }
struct A;
impl A {
    fn foo(arg: usize) -> &'static str { "false" }
}
impl AltChoose for [(); 0] { type Result = A; }
struct B;
impl B {
    fn foo(arg: String, arg2: bool) -> bool { true }
}
impl AltChoose for [(); 1] { type Result = B; }

// These methods have the same name but are actually totally different methods:
let foo: &'static str = <[(); 0] as AltChoose>::Result::foo(1);
let foo: bool = <[(); 1] as AltChoose>::Result::foo("some text".to_string(), false);
```

So the [`impl_dyn_cast`] macro works by generate a const `bool` that indicates
if a type implements a trait and then uses that const value with one of the
above hacks to determine which helper method to call when implementing the
[`DynCast`] trait. This way the generated code doesn't call the helper method
that preform the coercion to the `Target` trait unless the type actually
implements it.

## Alternatives

The [`intertrait`] crate offers similar functionality to this crate but has
a totally different implementation, at least as of [`intertrait`] version
`0.2.0`. It uses the [`linkme`] crate to create a registry of [`std::any::Any`]
type ids for types that can be cast into a certain trait object. This means
it probably has some runtime overhead when it looks up a cast function in
the global registry using a [`TypeId`]. It also means that it can't work on
all platforms since the [`linkme`] crate needs to offer support for them. This
is a limitation that this crate doesn't have.

The [`traitcast`] crate works similar to [`intertrait`] in that it has a
global registry that is keyed with [`TypeId`]s. But it differs in that it
uses the [`inventory`] crate to build the registry instead of the [`linkme`]
crate. The [`inventory`] crate uses the [`ctor`] crate to run some code before
`main`, something that is generally discouraged and this is something that
[`intertrait`] actually mentions as an advantage to its approach.

The [`traitcast_core`] library allow for a more low level API that doesn't
depend on a global registry and therefore also doesn't depend on a crate like
[`linkme`] or [`inventory`] that needs platform specific support. Instead it
requires that you explicitly create a registry and register all your types
and their casts with it.

The [`downcast-rs`] crate offers downcasting to concrete types but not
directly casting from one trait object to another trait object. So it has a
different use case and both it and this crate could be useful in the same
project.

## References

The following GutHub issue [Clean up pseudo-downcasting from VpnProvider supertrait to subtraits with better solution · Issue #21 · jamesmcm/vopono](https://github.com/jamesmcm/vopono/issues/21)
inspired this library.

## License

This project is released under either:

- [MIT License](https://github.com/Lej77/cast_trait_object/blob/master/LICENSE-MIT)
- [Apache License (Version 2.0)](https://github.com/Lej77/cast_trait_object/blob/master/LICENSE-APACHE)

at your choosing.

[`std::any::Any`]: https://doc.rust-lang.org/std/any/trait.Any.html
[`TypeId`]: https://doc.rust-lang.org/std/any/struct.TypeId.html
[`downcast-rs`]: https://crates.io/crates/downcast-rs
[`intertrait`]: https://crates.io/crates/intertrait
[`traitcast`]: https://crates.io/crates/traitcast
[`traitcast_core`]: https://crates.io/crates/traitcast_core
[`linkme`]: https://crates.io/crates/linkme
[`inventory`]: https://crates.io/crates/inventory
[`ctor`]: https://crates.io/crates/ctor
