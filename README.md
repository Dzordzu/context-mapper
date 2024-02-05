# Context Mapper

*Single rust macro for generating different maps*

## Links

* [Documentation](https://docs.rs/context-mapper/0.1.0/context_mapper/)
* [Repository](https://github.com/Dzordzu/context-mapper)
* [Crate](https://crates.io/crates/context-mapper)

## Example

```rust
#[derive(ContextMapper)]
#[context_mapper(
    impl(
        context = info::general
        converter = MyConv::to_info,
        type = info::Info,
        fn = general_info
        vis = pub(crate)
    ),
    function(
        context = info::all
        converter = MyConv::to_info,
        type = info::Info,
        fn = all_info
    ),

)]
struct Person {
    name: String,
    address: info::Address,
    age: usize,

    /// Let's hide it for the geneal info, but show for the rest
    #[context_attribute(context(name=info::general, skip))]
    is_a_reptile: bool
}

// …

let person = Person {
    //…
};

person.general_info();
all_info(&person);
```
