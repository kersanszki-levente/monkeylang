use std::cell::RefCell;
use std::fmt::Debug;
use std::fmt::Display;
use std::rc::{Rc, Weak};
use std::collections::HashMap;

use crate::ast::Value;

pub type Index=HashMap<String, Value>;

pub type CallerEnvironment=Weak<RefCell<Environment>>;
pub type SharedEnvironment=Rc<RefCell<Environment>>;

pub struct MissingIdentifierError(String);

impl Debug for MissingIdentifierError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

type EnvironmentAccessResult=Result<Value, MissingIdentifierError>;

#[derive(Debug, Clone, Default)]
pub struct Environment{
    inner: Index,
    outer: Option<CallerEnvironment>,
}

impl Environment {
    pub fn new(outer: Option<CallerEnvironment>) -> Environment {
        Environment { inner: Index::new(), outer }
    }
    pub fn new_shared(outer: Option<CallerEnvironment>) -> SharedEnvironment {
        let env = Environment { inner: Index::new(), outer };
        Rc::new(RefCell::new(env))
    }
    pub fn set(&mut self, id: String, value: Value) {
        self.inner.insert(id, value);
    }
    pub fn get(&self, id: &str) -> EnvironmentAccessResult {
        if let Some(value) = self.inner.get(id) {
            Ok(value.clone())
        } else if self.outer.is_some() {
            match &self.outer {
                Some(scope) => {
                    scope
                    .upgrade()
                    .ok_or(MissingIdentifierError(format!("{id} does not exist")))?
                    .try_borrow()
                    .map_err(|_| MissingIdentifierError("Compiler error".to_string()))?
                    .get(id)
                },
                None => Err(MissingIdentifierError(format!("{id} does not exist")))
            }
        } else {
            Err(MissingIdentifierError(format!("{id} does not exist")))
        }
    }
}

impl Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (id, item) in self.inner.iter() {
            writeln!(f, "{id} = {item}")?;
        }
        if let Some(outer) = &self.outer {
            if let Some(outer_scope) = outer.upgrade() {
                writeln!(f, "{}", outer_scope.try_borrow().unwrap())?;
            }
        }
        Ok(())
    }
}
