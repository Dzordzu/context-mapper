//! Universal library for struct mapping. Provides multiple methods to
//! generate HashMaps with values of any type
//!
//! # Example problem
//!
//! Let's assume that we've got this struct
//! ```
//! struct Person {
//!     name: String
//!     /// Age in years
//!     age: usize,
//!     /// Dept in $ in the certain banks
//!     dept: BTreeMap<String, f64>,
//!     /// Amount of money in banks. Accounts in $
//!     bank_account: BTreeMap<String, f64>,
//!     /// Amount of money in the wallet in $
//!     wallet: f64,
//!     /// Amount of money in the crypto in crypto-wallets. Transformed to $
//!     crypto: BTreeMap<String, f64>
//! }
//! ```
//!
//! Now, we want to get a few info:
//! 1. `avg(&self) -> HashMap<String, f64>` returning average amount of money in every field
//! 2. `max(&self) -> HashMap<String, f64>` returning max amount of money in every field
//! 3. `info(&self) -> HashMap<String, String>` returning general information about fields
//! 4. `detail_info(&self) -> HashMap<String, String>`. Similar to 3, but with more info
//!
//! We would need to implement it manually. It's cumbersome and inconvinient. That's where
//! `context_mapper` becomes a handy tool:
//!
//! ```
//! #[derive(ContextMapper)]
//! // F64 traits
//! #[context_mapper(
//!     trait(
//!         context = p01::average,
//!         type = f64,
//!         converter = MyAvgTrait::avg,
//!         generic(
//!             name = Averager,
//!             method_name = avg
//!         )
//!     ),
//!     trait(
//!         context = p02::max,
//!         type = f64,
//!         converter = MyMaxTrait::max,
//!         simple(
//!             name = Maximizer,
//!             method_name = max
//!         )
//!     )
//! )]
//! // String traits
//! #[context_mapper(
//!     trait(
//!         context = p03::general,
//!         type = String,
//!         converter = std::string::ToString::to_string,
//!         generic(
//!             path = GenInfo,
//!             method_name = info
//!         )
//!     ),
//!     trait(
//!         context = p04::max,
//!         type = f64,
//!         converter = generic_info,
//!         simple(
//!             path = my::DetailedInfo,
//!             method_name = info
//!         )
//!     )
//! )]
//! struct Person {
//!     #[context_attribute(context(name=p01::average, skip))]
//!     #[context_attribute(context(name=p02::max, skip))]
//!     name: String
//!
//!     /// Age in years
//!     #[context_attribute(context(name=p01::average, skip))]
//!     #[context_attribute(context(name=p02::max, skip))]
//!     age: usize,
//!
//!     /// Dept in $ in the certain banks
//!     #[context_attribute(
//!         context(name=p02::max, converter=punishment::with_interests)
//!     )]
//!     dept: BTreeMap<String, f64>,
//!
//!     /// Amount of money in banks. Accounts in $
//!     bank_account: BTreeMap<String, f64>,
//!     /// Amount of money in the wallet in $
//!     wallet: f64,
//!     /// Amount of money in the crypto in crypto-wallets. Transformed to $
//!     crypto: BTreeMap<String, f64>
//! }
//! ```
//!
//! # Macros documentation
//! See [`ContextMapper`] for detailed info
//!
//! # Comparsion to similar libraries
//!
//! ## serde
//!
//! The main difference between `context_mapper` and `serde` is the approach.
//! `serde` defines single, powerful way of serializing / deserializing
//! structures, enums etc. On the other hand `context_mapper` does not enforce
//! any standard on the serialization. Its main purpose is to simplify generation of
//! the repetitive struct -> map mappings. Also, it does not support nested structures
//!
//! ## structmap
//!
//! While being inspiration for this library, `structmap` is more similar 
//! to serde, than `context_mapper`. Both libraries provide way of converision
//! of the struct to map of strings. However `structmap` allows users to convert
//! objects from map and then convert them to map. On the other hand it does not
//! allow multiple contexts (multiple maps generation)
//!
pub use context_mapper_derive::ContextMapper;
use std::collections::HashMap;

pub struct Error(pub String);
impl Error {
    pub fn new<T: std::error::Error>(s: T) -> Self {
        Self(s.to_string())
    }
}

impl<T: std::error::Error> From<T> for Error {
    fn from(value: T) -> Self {
        Self(value.to_string())
    }
}

pub trait IntoType<ContextType> {
    fn into_type_map(&self) -> HashMap<String, ContextType>;
}

pub trait TryIntoType<ContextType> {
    type Error;
    fn try_into_type_map(&self) -> Result<HashMap<String, ContextType>, Self::Error>;
}
