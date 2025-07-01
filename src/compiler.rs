use crate::parser::{
    ColumnExpr, Condition, CreateFamily, CreateStatement, Criterion, DeleteStatement, DropFamily,
    DropStatement, InsertStatement, MultiCondition, ResultColumn, SelectFrom, SelectStatement,
    Statement, UpdateStatement, Value,
};

// The compiler
#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    CreateTable,
    CreateDatabase,
    DropDatabase,
    DropTable,

    Insert,
    Update,
    Delete,
    Select,
    // stack ops
    LoadTable,
    LoadColumn,
    LoadValue,
    LoadCriterion,
    //
    Filter,
    SetColumn,
    Halt,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub opcode: OpCode,
    pub operand: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Compiler {
    pub instructions: Vec<Instruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }

    fn emit(&mut self, opcode: OpCode, operand: Option<String>) {
        self.instructions.push(Instruction { opcode, operand })
    }

    fn compile_select_statement(&mut self, stmt: &SelectStatement) {
        let core = &stmt.core;

        match &core.from {
            SelectFrom::Table(table) => self.emit(OpCode::LoadTable, Some(table.clone())),
        }

        for col in &core.result_columns {
            match col {
                ResultColumn::Star => self.emit(OpCode::LoadColumn, Some("*".to_string())),
                ResultColumn::Expr(aliased_column) => {
                    self.compile_column_expr(&aliased_column.expr)
                }
            }
        }

        self.emit(OpCode::Select, None);

        if let Some(conditions) = &core.conditions {
            for condition in conditions {
                match condition {
                    Condition::Single(single_condition) => {
                        self.emit(
                            OpCode::LoadCriterion,
                            Some(single_condition.name.to_string()),
                        );
                    }
                    Condition::Double(double_condition) => {
                        self.emit(
                            OpCode::LoadCriterion,
                            Some(double_condition.name.to_string()),
                        );
                        self.compile_value(&double_condition.value);
                    }
                    Condition::Multi(multi_condition) => {
                        self.compile_multi_condition(multi_condition);
                    }
                }
            }
        }
    }

    fn compile_create_statement(&mut self, stmt: &CreateStatement) {
        match &stmt.core.family {
            CreateFamily::Database(create_database) => {
                self.emit(OpCode::LoadValue, Some(create_database.name.clone()));
                self.emit(OpCode::CreateDatabase, None);
            }
            CreateFamily::Table(create_table) => {
                self.emit(OpCode::LoadTable, Some(create_table.name.clone()));

                for col in &create_table.columns {
                    self.compile_column_expr(col);
                }

                self.emit(OpCode::CreateTable, None);
            }
        }
    }

    fn compile_update_statement(&mut self, stmt: &UpdateStatement) {
        let core = &stmt.core;

        self.emit(OpCode::LoadTable, Some(core.table.clone()));

        self.emit(OpCode::LoadColumn, Some("*".to_string())); // load columns to select
        self.emit(OpCode::Select, None); // select them

        // handle the filteration
        if let Some(conditions) = &core.conditions {
            self.compile_conditions(conditions);
        }

        self.emit(OpCode::LoadTable, Some(core.table.clone()));

        for col in &core.columns {
            if let ColumnExpr::Assignment(assignment) = col {
                self.emit(OpCode::LoadColumn, Some(assignment.name.clone()));
                self.compile_value(&assignment.value);
                self.emit(OpCode::SetColumn, None);
            }
        }

        self.emit(OpCode::Update, None); // not complete
    }

    fn compile_drop_statement(&mut self, stmt: &DropStatement) {
        match &stmt.core.family {
            DropFamily::Database(drop_database) => {
                self.emit(OpCode::LoadValue, Some(drop_database.name.clone()));
                self.emit(OpCode::DropDatabase, None);
            }
            DropFamily::Table(drop_table) => {
                self.emit(OpCode::LoadValue, Some(drop_table.name.clone()));
                self.emit(OpCode::DropTable, None);
            }
        }
    }

    fn compile_column_expr(&mut self, col: &ColumnExpr) {
        match col {
            ColumnExpr::Name(column_name) => {
                self.emit(OpCode::LoadColumn, Some(column_name.name.clone()))
            }
            ColumnExpr::Typed(typed_column) => {
                // flipped these
                self.emit(OpCode::LoadValue, Some(typed_column.data_type.to_string()));
                self.emit(OpCode::LoadColumn, Some(typed_column.col_name.clone()));
            }
            ColumnExpr::Assignment(value_assignment) => {
                self.emit(OpCode::LoadColumn, Some(value_assignment.name.clone()));
                self.compile_value(&value_assignment.value);
            }
        }
    }

    fn compile_insert_statement(&mut self, stmt: &InsertStatement) {
        let core = &stmt.core;

        self.emit(OpCode::LoadTable, Some(core.table.clone()));

        for col in &core.columns {
            self.compile_column_expr(col);
        }

        for row in &core.values {
            for val in row {
                self.compile_value(val);
            }
        }

        self.emit(OpCode::Insert, None);
    }

    fn compile_value(&mut self, value: &Value) {
        let v = match value {
            Value::Text(t) => format!("Text:{t}"),
            Value::Int(i) => format!("Int:{i}"),
            Value::Bool(b) => format!("Bool:{b}"),
            Value::Float(f) => format!("Float:{f}"),
        };

        self.emit(OpCode::LoadValue, Some(v))
    }

    fn compile_criterion(&mut self, criterion: &Criterion) {
        let cr = match criterion {
            Criterion::GreaterThan => ">",
            Criterion::EqualTo => "=",
            Criterion::LessThan => "<",
            Criterion::GreaterEqualTo => ">=",
            Criterion::LessEqualTo => "<=",
            Criterion::NotEqualTo => "!=",
        };

        self.emit(OpCode::LoadCriterion, Some(cr.to_string()));
    }

    fn compile_multi_condition(&mut self, condition: &MultiCondition) {
        self.compile_criterion(&condition.criterion);
        self.emit(OpCode::LoadColumn, Some(condition.column.clone()));
        self.compile_value(&condition.value);
        self.emit(OpCode::Filter, None);
    }

    fn compile_conditions(&mut self, conditions: &[MultiCondition]) {
        for condition in conditions {
            self.compile_multi_condition(condition);
        }
    }

    fn compile_delete_statement(&mut self, stmt: &DeleteStatement) {
        let core = &stmt.core;
        self.emit(OpCode::LoadTable, Some(core.table.clone()));

        self.emit(OpCode::Delete, None);

        if let Some(conditions) = &core.conditions {
            self.compile_conditions(conditions);
        }
    }

    fn compile_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Select(select_statement) => self.compile_select_statement(select_statement),
            Statement::Create(create_statement) => self.compile_create_statement(create_statement),
            Statement::Drop(drop_statement) => self.compile_drop_statement(drop_statement),
            Statement::Insert(insert_statement) => self.compile_insert_statement(insert_statement),
            Statement::Update(update_statement) => self.compile_update_statement(update_statement),
            Statement::Delete(delete_statement) => self.compile_delete_statement(delete_statement),
        }
    }

    fn finish(mut self) -> Vec<Instruction> {
        self.instructions.push(Instruction {
            opcode: OpCode::Halt,
            operand: None,
        });
        self.instructions
    }
}

pub fn compile(stmt: &Statement) -> Vec<Instruction> {
    let mut compiler = Compiler::new();
    compiler.compile_statement(stmt);
    compiler.finish()
}
