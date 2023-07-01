![Concoct](https://github.com/matthunz/viewbuilder/blob/main/logo.png?raw=true)

[![crate](https://img.shields.io/crates/v/concoct.svg)](https://crates.io/crates/concoct)
[![Rust Documentation](https://img.shields.io/badge/api-rustdoc-blue.svg)](https://docs.rs/concoct)
[![CI](https://github.com/matthunz/concoct/actions/workflows/rust.yml/badge.svg)](https://github.com/matthunz/concoct/actions/workflows/rust.yml)

Generic UI compiler and runtime in rust.

```rust
use concoct::{composable, compose, remember};

#[composable]
fn app() {
    let count = compose!(remember(|| 0));
    dbg!(count);
}
```

## Compiler
The compiler comes in the form of the `#[composable]` attribute macro.
For example:
```rust
#[composable]
fn f() -> i32
// will become:
fn f() -> impl Composable<Output = i32>
```

## Remember
```rust
#[composable]
pub fn remember<T, F>(f: F) -> T
where
    T: Clone + Hash + PartialEq + 'static,
    F: FnOnce() -> T + 'static,
{
    composer.cache(false, f)
}
```

```rust
#[must_use]
pub fn remember<T, F>(f: F) -> impl concoct::Composable<Output = T>
where
    T: Clone + Hash + PartialEq + 'static,
    F: FnOnce() -> T + 'static,
{
    #[allow(non_camel_case_types)]
    struct remember_composable<T, F> {
        f: F,
        _marker0: std::marker::PhantomData<T>,
    }
    impl<T, F> concoct::Composable for remember_composable<T, F>
    where
        T: Clone + Hash + PartialEq + 'static,
        F: FnOnce() -> T + 'static,
    {
        type Output = T;
        fn compose(
            self,
            composer: &mut impl concoct::Compose,
            changed: u32,
        ) -> Self::Output {
            ();
            let Self { f, .. } = self;
            composer
                .start_replaceable_group(
                    std::any::TypeId::of::<remember_composable<T, F>>(),
                );
            let output = { { composer.cache(false, f) } };
            composer.end_replaceable_group();
            output
        }
    }
    remember_composable {
        f,
        _marker0: std::marker::PhantomData,
    }
}
```