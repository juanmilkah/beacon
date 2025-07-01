/*
 * DATABASE ARCHITECTURE DOCUMENTATION
 *
 * This is a complete database implementation built from scratch in Rust.
 * The system follows a traditional database architecture with five main layers:
 *
 * 1. TOKENIZER
 *    - Takes raw SQL text input
 *    - Breaks it into meaningful tokens (keywords, identifiers, operators, literals)
 *    - Example: "SELECT name FROM users" -> [SELECT, name, FROM, users]
 *    - Handles whitespace, comments, and syntax validation at character level
 *
 * 2. PARSER
 *    - Takes tokens from tokenizer
 *    - Builds Abstract Syntax Tree (AST) representing SQL structure
 *    - Validates SQL grammar and syntax rules
 *    - Example: SELECT statement becomes tree with columns, table, conditions
 *    - Reports syntax errors with helpful messages
 *
 * 3. COMPILER
 *    - Takes AST from parser
 *    - Converts high-level SQL operations into low-level bytecode instructions
 *    - Performs query optimization and planning
 *    - Example: SELECT becomes series of LoadTable, LoadColumn, Select opcodes
 *    - Handles type checking and semantic validation
 *
 * 4. BYTECODE ENGINE (Virtual Machine)
 *    - Executes compiled bytecode instructions
 *    - Stack-based architecture for operand management
 *    - Implements all SQL operations (CREATE, INSERT, SELECT, UPDATE, DELETE)
 *    - Manages program counter and instruction flow
 *    - Returns query results or confirmation of data modifications
 *
 * 5. STORAGE LAYER
 *    - Handles persistent data storage and retrieval
 *    - Manages databases, tables, rows, and columns
 *    - Implements data types (Text, Int, Float, Bool)
 *    - Provides CRUD operations for the bytecode engine
 *    - Currently in-memory but designed for disk persistence
 *
 * DATA FLOW:
 * SQL Text -> Tokenizer -> Tokens -> Parser -> AST -> Compiler -> Bytecode -> VM -> Storage
 *
 * This layered approach provides:
 * - Clean separation of concerns
 * - Easy testing and debugging of individual components
 * - Extensibility for new SQL features
 * - Performance optimization at each layer
 * - Standard database architecture patterns
 */

pub mod compiler;
pub mod engine;
pub mod parser;
pub mod storage;
pub mod tokenizer;

fn main() -> Result<(), String> {
    let mut vm = engine::BytecodeEngine::new();
    let stmts = [
        "create database dev;",
        "create table cats(name text, age int);",
        "insert into cats (name, age) values('foo', 10);",
        "insert into cats (name, age) values('bar', 15);",
        // "update cats set name = 'baz' where name = 'foo';",
        "delete from cats where name = 'foo';",
        "select * from cats;",
    ];

    let mut bytecodes = Vec::with_capacity(4);
    for s in stmts {
        let stmt = parser::parse_statement(s)?;
        // dbg!(&stmt);
        let bytecode = compiler::compile(&stmt);
        bytecodes.push(bytecode);
    }

    let mut rows = Vec::new();

    for (i, item) in bytecodes.iter().enumerate() {
        if i == 4 {
            println!("{item:#?}",);
        }

        let r = vm.execute(item)?;
        rows.push(r);
    }

    println!("{rows:#?}");
    // let r = vm.execute(selected);
    // println!("{r:#?}");

    // vm.print_tables();
    Ok(())
}
