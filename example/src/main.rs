use std::collections::HashMap;
use context_mapper::ContextMapper;
use context_mapper::IntoType;
use serde::Serialize;

pub trait MyMagicTrait {
    fn mapp(&self) -> HashMap<String, String>;
}

pub trait MyConvTrait {
    fn to_string(&self) -> String;
}

impl MyConvTrait for usize {
    fn to_string(&self) -> String {
        "USIZE".to_string()
    }
}

impl MyConvTrait for i32 {
    fn to_string(&self) -> String {
        "i32".to_string()
    }
}

impl MyConvTrait for String {
    fn to_string(&self) -> String {
        self.clone() + " IS STRING"
    }
}

fn my_function<T: std::fmt::Display>(v: T) -> usize {
    v.to_string().parse::<usize>().unwrap() + 2
}

fn my_other_function(string: &String) -> usize {
    10
}


#[derive(ContextMapper)]
#[context_mapper(trait())]
#[context_mapper(
    trait(
        context=my_beautiful_context,
        converter=MyConvTrait::to_string,
        type=String,
        simple(
            path = MyMagicTrait,
            method_name = mapp
        )
    )
)]
#[context_mapper(
    impl(
        context=impls::context,
        converter=MyConvTrait::to_string,
        type=String,
        naming=magic_fna,
        visibility=pub(crate),
    )
)]
#[context_mapper(
    function(
        context=fun::context,
        converter=MyConvTrait::to_string,
        type=String,
        naming=magic_fn,
        visibility=pub(crate),
    )
)]
#[context_mapper(
    trait(
        context=other_context,
        type=usize,
        converter=my_function,
    )
)]
pub struct X {
    #[context_attribute(
        context(
            name=other_context,
            converter=my_other_function,
            rename="my_other_name",
        )
    )]
    pub x: String,

    #[context_attribute(
        context(
            name=other_context,
            move
        )
    )]
    pub y: usize,
    pub y0: i32,

    #[context_attribute(context(skip))]
    #[context_attribute(context(name=fun::context, skip))]
    pub z: i32,

    #[context_attribute(context(name=impls::context, skip))]
    pub z0: i32,
}

fn main() {

    let x = X {
        x: "A".to_string(),
        y: 5,
        y0: 10,
        z: 5,
        z0: 100
    };

    let v : HashMap<String, usize> = x.into_type_map();
    let vs : HashMap<String, String> = x.into_type_map();
    let vsm : HashMap<String, String> = MyMagicTrait::mapp(&x);
    let fun: HashMap<String, String> = magic_fn(&x);
    let impls : HashMap<String, String> = x.magic_fna();

    println!("{:#?}", v);
    println!("{:#?}", vs);
    println!("{:#?}", vsm);
    println!("{:#?}", fun);
    println!("{:#?}", impls);
}
