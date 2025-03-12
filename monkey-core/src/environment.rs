#![allow(unused_imports)]

use core::cell::RefCell;
use std::fmt::Display;
use std::ops::Deref;
use std::rc::{Rc, Weak};
use std::collections::HashMap;

use crate::ast::Value;

pub type Scope=HashMap<String, Value>;

pub type MutableEnvironment=Weak<RefCell<Environment>>;

#[derive(Debug, Clone, Default)]
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
                Some(scope) => {
                    scope
                    .upgrade()?
                    .try_borrow()
                    .ok()?
                    .get(id)
                    .clone()
                },
                None => None
            }
        } else {
            None
        }
    }
}

impl Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Inner scope:")?;
        for (id, item) in self.scope.iter() {
            writeln!(f, "{id} = {item}")?;
        }
        writeln!(f, "Outer scope:")?;
        if let Some(outer) = &self.outer {
            let outer_scope = outer.upgrade().unwrap();
            writeln!(f, "{}", outer_scope.as_ref().borrow())?;
        }
        Ok(())
    }
}
