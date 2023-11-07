use std::{hash::{Hash, Hasher}, sync::RwLock};
use ahash::{AHashMap as HashMap, AHashSet as HashSet};

use crate::{parse::{TypeSpec, FuncCode}, Type, CompData, FuncCodeId, TypeRelation, FuncQuery, TypeName, errors::TResult};

use super::get_type_spec::GenericTable;

// This "CachedTypeRetrieval" trait is a hack for querying the hashmap with three borrows.
// It's a little complex, but it prevents unnecessary cloning.
//
// https://www.reddit.com/r/rust/comments/owa4oa/borrowed_complex_struct_hashmap_keys/

pub trait CachedTypeRetrieval {
    fn borrowed(&self) -> BorrowedCachedTypeRetrieval;
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct OwnedCachedTypeRetrieval {
    spec: TypeSpec,
    generics: Vec<Type>,
    relation: Option<Type>,
}

impl CachedTypeRetrieval for OwnedCachedTypeRetrieval {
    fn borrowed(&self) -> BorrowedCachedTypeRetrieval {
        BorrowedCachedTypeRetrieval {
            spec: &self.spec,
            generics: &self.generics,
            relation: self.relation.as_ref(),
        }
    }
}

impl<'a> std::borrow::Borrow<dyn CachedTypeRetrieval + 'a> for OwnedCachedTypeRetrieval {
    fn borrow(&self) -> &(dyn CachedTypeRetrieval + 'a) {
        self
    }
}

impl<'a> PartialEq for (dyn CachedTypeRetrieval + 'a) {
    fn eq(&self, other: &Self) -> bool { self.borrowed().eq(&other.borrowed()) }
}
impl<'a> Eq for (dyn CachedTypeRetrieval + 'a) {}
impl<'a> Hash for (dyn CachedTypeRetrieval + 'a) {
    fn hash<H: Hasher>(&self, state: &mut H) { self.borrowed().hash(state) }
}

#[derive(Hash, PartialEq, Eq, Clone, Copy)]
pub struct BorrowedCachedTypeRetrieval<'a> {
    pub spec: &'a TypeSpec,
    pub generics: &'a [Type],
    pub relation: Option<&'a Type>,
}

impl<'a> BorrowedCachedTypeRetrieval<'a> {
    pub fn to_owned(&self) -> OwnedCachedTypeRetrieval {
        OwnedCachedTypeRetrieval {
            spec: self.spec.clone(),
            generics: self.generics.to_vec(),
            relation: self.relation.cloned(),
        }
    }
}

impl<'a> CachedTypeRetrieval for BorrowedCachedTypeRetrieval<'a> {
    fn borrowed(&self) -> BorrowedCachedTypeRetrieval { *self }
}

#[derive(Default)]
pub struct Caches {
    pub realized_type_specs: RwLock<HashMap<OwnedCachedTypeRetrieval, TResult<Type>>>,
}

impl Caches {
    pub fn clear(&mut self) {
        self.realized_type_specs.write().unwrap().clear();
    }
}

pub fn cache_type_specs(
    comp_data: &mut CompData,
    code_id: FuncCodeId,
    generic_table: &FuncQuery,
) {
    //let for_code = &comp_data.func_code[code_id];
    //let mut specs = Vec::new();
    //for_code.type_specs(&mut specs);

    //let all_names = HashSet::<TypeName>::new();

    //while !specs.is_empty() {
    //    let working_through = std::mem::replace(&mut specs, Vec::new());
    //    for spec in working_through {
    //        let retrieval = BorrowedCachedTypeRetrieval { 
    //            spec, 
    //            generics: &generic_table.generics, 
    //            relation: generic_table.relation.inner_type(), 
    //        };

    //        if !comp_data.caches.realized_type_specs.contains_key((&retrieval as &dyn CachedTypeRetrieval)) {
    //            let Ok(new_type) = comp_data.get_spec(spec, generic_table)
    //                else { continue };
    //            
    //            comp_data.caches.realized_type_specs.insert(OwnedCachedTypeRetrieval {
    //                spec: retrieval.spec.clone(),
    //                generics: retrieval.generics.to_vec(),
    //                relation: retrieval.relation.cloned(),
    //            }, new_type);

    //            let mut names = Vec::new();
    //            spec.get_names(&mut names);
    //            for name in names {
    //                specs.push(&comp_data.typedefs[&name]);
    //            }
    //        }
    //    }
    //}
}
