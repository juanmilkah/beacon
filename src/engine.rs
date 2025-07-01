use std::collections::HashMap;
use std::fmt;

use crate::compiler::{Instruction, OpCode};

#[derive(Debug, Clone, PartialEq)]
pub enum StackValue {
    Text(String),
    Int(i32),
    Float(f32),
    Bool(bool),
    Table(String),
    Column(String),
    Criterion(String),
}

impl fmt::Display for StackValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StackValue::Text(t) => write!(f, "{t}"),
            StackValue::Int(i) => write!(f, "{i}"),
            StackValue::Float(fl) => write!(f, "{fl}"),
            StackValue::Bool(b) => write!(f, "{b}"),
            StackValue::Table(t) => write!(f, "{t}"),
            StackValue::Column(c) => write!(f, "{c}"),
            StackValue::Criterion(c) => write!(f, "{c}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Row {
    pub data: HashMap<String, StackValue>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Table {
    pub name: String,
    pub columns: Vec<String>,
    pub column_types: HashMap<String, String>,
    pub rows: Vec<Row>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Database {
    pub name: String,
    pub tables: HashMap<String, Table>,
}

#[derive(Debug, Default)]
pub struct BytecodeEngine {
    pub stack: Vec<StackValue>,
    pub databases: HashMap<String, Database>,
    pub current_db: Option<String>,
    pub pc: usize,
}

impl BytecodeEngine {
    pub fn new() -> Self {
        Self::default()
    }

    fn push(&mut self, val: StackValue) {
        self.stack.push(val);
    }

    fn pop(&mut self) -> Result<StackValue, String> {
        self.stack
            .pop()
            .ok_or(format!("Stack underflow: pc => {}", self.pc))
    }

    fn peek(&mut self) -> Result<&StackValue, String> {
        self.stack.last().ok_or("Stack empty".to_string())
    }

    fn parse_value(&self, val: &str) -> Result<StackValue, String> {
        if let Some(text) = val.strip_prefix("Text:") {
            Ok(StackValue::Text(text.to_string()))
        } else if let Some(int) = val.strip_prefix("Int:") {
            let int = int
                .parse::<i32>()
                .map_err(|_| format!("Invalid Integer: {int}"))?;
            Ok(StackValue::Int(int))
        } else if let Some(float) = val.strip_prefix("Float:") {
            let float = float
                .parse::<f32>()
                .map_err(|_| format!("Invalid float: {float}"))?;
            Ok(StackValue::Float(float))
        } else if let Some(bool) = val.strip_prefix("Bool:") {
            let bool = bool
                .parse::<bool>()
                .map_err(|_| format!("Invalid bool: {bool}"))?;
            Ok(StackValue::Bool(bool))
        } else {
            Ok(StackValue::Text(val.to_string()))
        }
    }

    fn compare_values(&self, value1: &StackValue, value2: &StackValue, criterion: &str) -> bool {
        match (value1, value2) {
            (StackValue::Int(a), StackValue::Int(b)) => match criterion {
                ">" => a > b,
                "<" => a < b,
                "<=" => a <= b,
                ">=" => a >= b,
                "=" => a == b,
                _ => false,
            },
            (StackValue::Text(a), StackValue::Text(b)) => match criterion {
                "=" => a == b,
                "!=" => a != b,
                _ => false,
            },
            (StackValue::Float(a), StackValue::Float(b)) => match criterion {
                ">" => a > b,
                "<" => a < b,
                "<=" => a <= b,
                ">=" => a >= b,
                "=" => a == b,
                _ => false,
            },
            (StackValue::Bool(a), StackValue::Bool(b)) => match criterion {
                "=" => a == b,
                "!=" => a != b,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn execute(&mut self, instructions: &[Instruction]) -> Result<Vec<Row>, String> {
        // reset counter
        self.pc = 0;
        let mut result: Vec<Row> = Vec::new();

        while self.pc < instructions.len() {
            let instruction = &instructions[self.pc];

            match instruction.opcode {
                OpCode::Update => {
                    // insert data back to table

                    let current_table = match self.pop()? {
                        StackValue::Table(name) => name,
                        other => return Err(format!("Expected table name, got {other:?}")),
                    };
                    if let Some(db_name) = &self.current_db {
                        if let Some(database) = self.databases.get_mut(db_name) {
                            // Find the table being updated (last table on stack or current context)
                            if let Some(table) = database.tables.get_mut(&current_table) {
                                table.rows.extend_from_slice(&result);
                                // not fully correct
                            }
                        }
                    }
                }
                OpCode::SetColumn => {
                    let value = self.pop()?;
                    let column_name = match self.pop()? {
                        StackValue::Column(name) => name,
                        other => return Err(format!("Expected column name, got {other:?}")),
                    };
                    for row in &mut result {
                        row.data.insert(column_name.clone(), value.clone());
                    }
                }
                OpCode::Halt => break,
                OpCode::Filter => {
                    let value = self.pop()?;

                    let column_name = match self.pop()? {
                        StackValue::Column(name) => name,
                        other => return Err(format!("Expected Column name, got: {other}")),
                    };
                    let criterion = match self.pop()? {
                        StackValue::Criterion(c) => c,
                        other => return Err(format!("Expected criterion, got {other:?}")),
                    };

                    if !result.is_empty() {
                        result = result
                            .iter()
                            .filter(|row| {
                                if let Some(data_value) = row.data.get(&column_name) {
                                    self.compare_values(data_value, &value, &criterion)
                                } else {
                                    false
                                }
                            })
                            .cloned()
                            .collect();
                    }
                }

                OpCode::Delete => {
                    let table_name = match self.pop()? {
                        StackValue::Table(name) => name,
                        other => return Err(format!("Expected table name, got {other:?}")),
                    };
                    if let Some(db_name) = &self.current_db {
                        if let Some(database) = self.databases.get_mut(db_name) {
                            if let Some(table) = database.tables.get_mut(&table_name) {
                                table.rows = result.clone();
                            }
                        }
                    }
                }

                OpCode::Select => {
                    let mut columns_to_select = Vec::new();
                    let mut table_name = String::new();

                    while let Ok(value) = self.peek() {
                        match value {
                            StackValue::Table(name) => {
                                table_name = name.to_string();
                                self.pop()?;
                                break;
                            }
                            StackValue::Column(c) => {
                                columns_to_select.push(c.to_string());
                                self.pop()?;
                            }
                            _ => {
                                break;
                            }
                        }
                    }

                    if let Some(db_name) = self.current_db.as_ref() {
                        if let Some(db) = self.databases.get(db_name) {
                            if let Some(table) = db.tables.get(&table_name) {
                                let rows = &table.rows;

                                if columns_to_select.contains(&"*".to_string()) {
                                    result = rows.to_vec();
                                } else {
                                    for row in rows {
                                        let mut new_row_data = HashMap::new();
                                        for col in &columns_to_select {
                                            if let Some(data) = row.data.get(col) {
                                                new_row_data.insert(col.clone(), data.clone());
                                            }
                                        }
                                        result.push(Row { data: new_row_data });
                                    }
                                }
                            }
                        }
                    }
                }
                OpCode::Insert => {
                    let mut columns = Vec::new();
                    let mut values = Vec::new();

                    while let Ok(val) = self.pop() {
                        match val {
                            StackValue::Table(_) => {
                                self.push(val); // push back table
                                break;
                            }
                            StackValue::Column(c) => columns.push(c),
                            other => values.push(other),
                        }
                    }

                    let table_name = match self.pop()? {
                        StackValue::Table(name) => name,
                        other => return Err(format!("Expected table name, got {other:?}")),
                    };

                    columns.reverse();
                    values.reverse();

                    let mut row_data = HashMap::new();

                    for (i, val) in columns.into_iter().enumerate() {
                        if let Some(entry) = values.get(i) {
                            row_data.insert(val.to_string(), entry.clone());
                        }
                    }

                    let row = Row { data: row_data };

                    if let Some(db_name) = self.current_db.as_ref() {
                        if let Some(db) = self.databases.get_mut(db_name) {
                            if let Some(table) = db.tables.get_mut(&table_name) {
                                table.rows.push(row);
                            }
                        }
                    }
                }
                OpCode::DropTable => {
                    let table_name = match self.pop()? {
                        StackValue::Table(name) => name,
                        other => format!("Expected table name, got: {other}"),
                    };

                    if let Some(db_name) = self.current_db.as_ref() {
                        if let Some(db) = self.databases.get_mut(db_name) {
                            db.tables.remove(&table_name);
                        }
                    }
                }
                OpCode::CreateTable => {
                    let mut columns = Vec::new();
                    let mut column_types = HashMap::new();

                    // Collect column definitions from stack
                    while let Ok(value) = self.peek() {
                        match value {
                            StackValue::Column(col) => {
                                let col_name = col.clone();
                                columns.push(col_name.clone());
                                self.pop()?;
                                if let Ok(type_str) = self.peek()
                                    && let StackValue::Text(text) = type_str
                                    && (text.starts_with("Int")
                                        || text.starts_with("Text")
                                        || text.starts_with("Float")
                                        || text.starts_with("Bool"))
                                {
                                    column_types.insert(col_name.clone(), text.to_string());
                                    self.pop()?;
                                }
                            }

                            StackValue::Table(_) => {
                                break;
                            }
                            _ => {
                                self.pop()?;
                                break;
                            }
                        }
                    }

                    let table_name = match self.pop()? {
                        StackValue::Table(name) => name,
                        other => return Err(format!("Expected table name, got {other:?}")),
                    };

                    columns.reverse(); // Stack reverses order

                    let table = Table {
                        name: table_name.clone(),
                        columns,
                        column_types,
                        rows: Vec::new(),
                    };

                    if let Some(db_name) = &self.current_db {
                        if let Some(database) = self.databases.get_mut(db_name) {
                            database.tables.insert(table_name, table);
                        }
                    }
                }
                OpCode::CreateDatabase => {
                    let db_name = match self.pop()? {
                        StackValue::Text(name) => name,
                        other => return Err(format!("Expected database name, got: {other}")),
                    };

                    let db = Database {
                        name: db_name.clone(),
                        tables: HashMap::new(),
                    };

                    self.databases.insert(db_name.clone(), db);
                    self.current_db = Some(db_name);
                }
                OpCode::DropDatabase => {
                    let db_name = match self.pop()? {
                        StackValue::Text(name) => name,
                        other => return Err(format!("Expected database name, got: {other}")),
                    };

                    self.databases.remove(&db_name);
                    if self.current_db.as_ref() == Some(&db_name) {
                        self.current_db = None;
                    }
                }
                OpCode::LoadTable => {
                    let table_name = instruction
                        .operand
                        .as_ref()
                        .ok_or("LoadTable needs operand".to_string())?;
                    self.stack.push(StackValue::Table(table_name.clone()));
                }
                OpCode::LoadColumn => {
                    let col_name = instruction
                        .operand
                        .as_ref()
                        .ok_or("LoadColumn needs operand".to_string())?;
                    self.stack.push(StackValue::Column(col_name.clone()));
                }
                OpCode::LoadValue => {
                    let val = instruction
                        .operand
                        .as_ref()
                        .ok_or("LoadValue needs operand".to_string())?;
                    let value = self.parse_value(val)?;
                    self.stack.push(value);
                }
                OpCode::LoadCriterion => {
                    let criterion = instruction
                        .operand
                        .as_ref()
                        .ok_or("LoadCriterion needs operand".to_string())?;
                    self.stack.push(StackValue::Criterion(criterion.clone()));
                }
            }
            self.pc += 1;
        }

        Ok(result)
    }

    pub fn print_tables(&self) {
        if let Some(db_name) = &self.current_db {
            if let Some(database) = self.databases.get(db_name) {
                println!("Database: {db_name}");
                for (table_name, table) in &database.tables {
                    println!("  Table: {table_name}");
                    println!("    Columns: {:?}", table.columns);
                    println!("    Rows: {}", table.rows.len());
                    for (i, row) in table.rows.iter().enumerate() {
                        println!("      Row {}: {:?}", i, row.data);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod bytecode_engine {

    use super::*;

    #[test]
    fn test_simple() {
        let mut vm = BytecodeEngine::new();

        // Example: CREATE DATABASE test;
        let create_db_instructions = vec![
            Instruction {
                opcode: OpCode::LoadValue,
                operand: Some("test".to_string()),
            },
            Instruction {
                opcode: OpCode::CreateDatabase,
                operand: None,
            },
            Instruction {
                opcode: OpCode::Halt,
                operand: None,
            },
        ];

        let _ = vm.execute(&create_db_instructions).is_ok();

        // Example: CREATE TABLE users (name TEXT, age INT);
        let create_table_instructions = vec![
            Instruction {
                opcode: OpCode::LoadTable,
                operand: Some("users".to_string()),
            },
            Instruction {
                opcode: OpCode::LoadValue,
                operand: Some("Text".to_string()),
            },
            Instruction {
                opcode: OpCode::LoadColumn,
                operand: Some("name".to_string()),
            },
            Instruction {
                opcode: OpCode::LoadValue,
                operand: Some("Int".to_string()),
            },
            Instruction {
                opcode: OpCode::LoadColumn,
                operand: Some("age".to_string()),
            },
            Instruction {
                opcode: OpCode::CreateTable,
                operand: None,
            },
            Instruction {
                opcode: OpCode::Halt,
                operand: None,
            },
        ];

        let _ = vm.execute(&create_table_instructions).is_ok();

        // Example: INSERT INTO users (name, age) VALUES ('Alice', 25);
        let insert_instructions = vec![
            Instruction {
                opcode: OpCode::LoadTable,
                operand: Some("users".to_string()),
            },
            Instruction {
                opcode: OpCode::LoadColumn,
                operand: Some("name".to_string()),
            },
            Instruction {
                opcode: OpCode::LoadColumn,
                operand: Some("age".to_string()),
            },
            Instruction {
                opcode: OpCode::LoadValue,
                operand: Some("Text:Alice".to_string()),
            },
            Instruction {
                opcode: OpCode::LoadValue,
                operand: Some("Int:25".to_string()),
            },
            Instruction {
                opcode: OpCode::Insert,
                operand: None,
            },
            Instruction {
                opcode: OpCode::Halt,
                operand: None,
            },
        ];

        let _ = vm.execute(&insert_instructions).is_ok();

        // Example: SELECT * FROM users;
        let select_instructions = vec![
            Instruction {
                opcode: OpCode::LoadTable,
                operand: Some("users".to_string()),
            },
            Instruction {
                opcode: OpCode::LoadColumn,
                operand: Some("*".to_string()),
            },
            Instruction {
                opcode: OpCode::Select,
                operand: None,
            },
            Instruction {
                opcode: OpCode::Halt,
                operand: None,
            },
        ];

        let results = vm.execute(&select_instructions).unwrap();

        assert!(!results.is_empty());
    }
}
