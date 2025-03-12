use core::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

use crate::ast::Value;

pub type Scope=HashMap<String, Value>;

pub type MutableEnvironment=Rc<RefCell<Environment>>;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Environment {
    scope: Scope,
    outer: Option<MutableEnvironment>,
}

impl Environment {
    pub fn new(outer: Option<MutableEnvironment>) -> Environment {
        Environment { scope: Scope::new(), outer }
    }
    pub fn set(&mut self, id: String, value: Value) {
        self.scope.insert(id, value);
    }
    pub fn get(&self, id: &str) -> Option<Value> {
        if let Some(value) = self.scope.get(id) {
            Some(value.clone())
        } else if self.outer.is_some() {
            match &self.outer {
                Some(scope) => scope.borrow().get(id).clone(),
                None => None
            }
        } else {
            None
        }
    }
}
