use super::*;

impl TokenKind {
    fn infix_binding_power(&self) -> Option<(u8, u8)> {
        let result = match self {
            T![=] => (1, 2),
            T![||] => (3, 4),
            T![&&] => (5, 6),
            T![==] | T![!=] => (7, 8),
            T![<] | T![>] | T![<=] | T![>=] => (9, 10),
            T![+] | T![-] | T![mod] => (11, 12),
            T![*] | T![/] => (13, 14),
            T![^] => (22, 21),
            T![.] => (31, 32),
            _ => return None,
        };
        Some(result)
    }
}

impl<'input, I> Parser<'input, I>
where
    I: Iterator<Item = Token>,
{
    pub(crate) fn parse_input(&mut self, name: impl Into<String>) -> Result<AST> {
        let mut items = Vec::new();
        loop {
            let next = self.peek();
            match next {
                T![fn] => {
                    let func = self.parse_fn()?;
                    items.push(func);
                }
                T![class] => {
                    let class = self.parse_class()?;
                    items.push(class);
                }
                T![eof] => break,
                found => match self.parse_stmt() {
                    Ok(stmt) => items.push(stmt),
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            found,
                            expected: vec![
                                T![fn],
                                T![class],
                                T![line comment],
                                T![block comment],
                                T![let],
                                T![return],
                                T![if],
                                T![for],
                            ],
                            position: self.position(),
                        })
                    }
                },
            }
        }
        Ok(AST::File {
            name: name.into(),
            elements: items,
        })
    }

    pub(crate) fn parse_fn(&mut self) -> Result<AST> {
        self.consume(T![fn])?;
        let name = self.text().to_string();
        self.consume(T![ident])?;
        self.consume(T!['('])?;
        let mut params = Vec::new();
        while !self.at(T![')']) {
            let param_name = self.text().to_string();
            self.consume(T![ident])?;
            params.push(param_name);
            if !self.try_consume(T![,]) && !self.at(T![')']) {
                break;
            }
        }
        self.consume(T![')'])?;
        self.consume(T!['{'])?;
        let mut body = Vec::new();
        while !self.at(T!['}']) {
            let stmt = self.parse_stmt()?;
            body.push(stmt);
        }
        self.try_consume(T!['}']);
        Ok(AST::FnDef { name, params, body })
    }

    pub(crate) fn parse_class(&mut self) -> Result<AST> {
        self.consume(T![class])?;
        let name = self.text().to_string();
        self.consume(T![ident])?;
        self.consume(T!['{'])?;
        let mut fields = Vec::new();
        let mut methods = Vec::new();
        while !self.at(T!['}']) {
            let next = self.peek();
            match next {
                T![fn] => {
                    methods.push(self.parse_fn()?);
                    continue;
                }
                _ => {}
            }
            let field_name = self.text().to_string();
            self.consume(T![ident])?;
            fields.push(field_name);
        }
        self.try_consume(T!['}']);
        Ok(AST::ClassDef {
            name,
            fields,
            methods,
        })
    }

    pub(crate) fn parse_stmt(&mut self) -> Result<AST> {
        let next = self.peek();
        let ast = match next {
            T![let] => {
                self.consume(T![let])?;
                let var_name = self.text().to_string();
                self.consume(T![ident])?;
                self.consume(T![=])?;
                let value = self.parse_expr()?;
                self.try_consume(T![;]);
                AST::LetStmt {
                    var_name,
                    value: Box::new(value),
                }
            }
            T![return] => {
                self.consume(T![return])?;
                let value = self.parse_expr()?;
                self.try_consume(T![;]);
                AST::RetStmt {
                    value: Box::new(value),
                }
            }
            T![if] => {
                let (condition, body) = self.parse_if()?;
                let mut elses = Vec::new();
                loop {
                    if !self.at(T![else]) {
                        break;
                    }
                    self.consume(T![else])?;
                    let (condition, body) = self.parse_if()?;
                    elses.push(AST::IfStmt {
                        condition: Box::new(condition),
                        body,
                        elses: vec![],
                    })
                }
                AST::IfStmt {
                    condition: Box::new(condition),
                    body,
                    elses,
                }
            }
            T![for] => {
                self.consume(T![for])?;
                let var_name = self.text().to_string();
                self.consume(T![ident])?;
                self.consume(T![in])?;
                let current_allow_obj_literals = self.allow_object_literals;
                self.allow_object_literals = false;
                let target = self.parse_expr()?;
                self.allow_object_literals = current_allow_obj_literals;
                self.consume(T!['{'])?;
                let mut body = Vec::new();
                while !self.at(T!['}']) {
                    let stmt = self.parse_stmt()?;
                    body.push(stmt);
                }
                self.try_consume(T!['}']);
                AST::ForLoop {
                    var_name,
                    target: Box::new(target),
                    body,
                }
            }
            T![ident] => {
                // allow assignments and function and method calls as statements
                let position = self.position();
                let expr = self.parse_expr()?;
                match &expr {
                    AST::BinaryExpr { op, .. } if op == "=" => {
                        self.try_consume(T![;]);
                        expr
                    }
                    AST::BinaryExpr { op, rhs, .. } if op == "." => match rhs.as_ref() {
                        AST::FnCall { .. } => {
                            self.try_consume(T![;]);
                            expr
                        }
                        _ => return Err(ParseError::InvalidExpressionStatement { position }),
                    },
                    AST::FnCall { .. } => {
                        self.try_consume(T![;]);
                        expr
                    }
                    _ => return Err(ParseError::InvalidExpressionStatement { position }),
                }
            }
            T![line comment] | T![block comment] => self.parse_comment()?,
            found => {
                return Err(ParseError::UnexpectedToken {
                    found,
                    expected: vec![T![let], T![return], T![if], T![for]],
                    position: self.position(),
                })
            }
        };
        Ok(ast)
    }

    fn parse_if(&mut self) -> Result<(AST, Vec<AST>)> {
        self.consume(T![if])?;
        let current_allow_obj_literals = self.allow_object_literals;
        self.allow_object_literals = false;
        let condition = self.parse_expr()?;
        self.allow_object_literals = current_allow_obj_literals;
        self.consume(T!['{'])?;
        let mut body = Vec::new();
        while !self.at(T!['}']) {
            let stmt = self.parse_stmt()?;
            body.push(stmt);
        }
        self.try_consume(T!['}']);
        Ok((condition, body))
    }

    pub(crate) fn parse_expr(&mut self) -> Result<AST> {
        self.parse_expr_bp(0)
    }

    pub(crate) fn parse_comment(&mut self) -> Result<AST> {
        let next = self.peek();
        let ast = match next {
            T![line comment] => {
                let text = self.text().trim_start_matches("//").to_string();
                self.consume(T![line comment])?;
                AST::Comment { text: text }
            }
            T![block comment] => {
                let text = self
                    .text()
                    .trim_start_matches("/*")
                    .trim_end_matches("*/")
                    .to_string();
                self.consume(T![block comment])?;
                AST::Comment { text }
            }
            found => {
                return Err(ParseError::UnexpectedToken {
                    found,
                    expected: vec![T![line comment], T![block comment]],
                    position: self.position(),
                });
            }
        };
        Ok(ast)
    }

    pub(crate) fn parse_expr_bp(&mut self, binding_power: u8) -> Result<AST> {
        let next = self.peek();
        let mut lhs = match next {
            lit @ T![bool] | lit @ T![int] | lit @ T![float] | lit @ T![string] => {
                let text = self.text();
                let ast = match lit {
                    T![bool] => AST::BoolLiteral {
                        value: text.parse().unwrap(),
                    },
                    T![int] => AST::IntLiteral {
                        value: text.parse().unwrap(),
                    },
                    T![float] => AST::FloatLiteral {
                        value: text.parse().unwrap(),
                    },
                    T![string] => AST::StringLiteral {
                        text: text[1..(text.len() - 1)].to_string(),
                    },
                    _ => unreachable!(),
                };
                self.consume(lit)?;
                ast
            }
            T![line comment] | T![block comment] => self.parse_comment()?,
            T![ident] => {
                let name = self.text().to_string();
                self.consume(T![ident])?;
                if self.at(T!['(']) {
                    //  function call
                    self.consume(T!['('])?;
                    let mut args = Vec::new();
                    while !self.at(T![')']) {
                        let arg = self.parse_expr()?;
                        args.push(arg);
                        if !self.try_consume(T![,]) && !self.at(T![')']) {
                            // If there is no comma, there cannot be more expressions following.
                            // If the next token is not the closing paren of the argument list,
                            // there is an error somewhere and we stop parsing the argument list.
                            break;
                        }
                    }
                    self.try_consume(T![')']);
                    AST::FnCall {
                        name: name.to_string(),
                        args,
                    }
                } else if self.at(T!['{'])
                    && self.allow_object_literals
                    && binding_power <= T![=].infix_binding_power().unwrap().1
                {
                    // Object literal -- only allow at the beginning of an expr or on the RHS of an assignment
                    // otherwise, `for x in foo.bar {}` parses `foo.bar {}` as an object literal
                    self.consume(T!['{'])?;
                    let mut fields = Vec::new();
                    while !self.at(T!['}']) {
                        let field_name = self.text().to_string();
                        self.consume(T![ident])?;
                        self.consume(T![:])?;
                        let value = self.parse_expr()?;
                        fields.push(AST::FieldLiteral {
                            name: field_name,
                            value: Box::new(value),
                        });
                        if !self.try_consume(T![,]) && !self.at(T!['}']) {
                            break;
                        }
                    }
                    self.try_consume(T!['}']);
                    AST::ObjectLiteral {
                        class: name,
                        fields,
                    }
                } else {
                    AST::Ident { name }
                }
            }
            T![class] => {
                self.consume(T![class])?;
                AST::Ident {
                    name: "class".to_string(),
                }
            }
            T![ref] => {
                self.consume(T![ref])?;
                let value = self.parse_expr_bp(18)?;
                AST::RefExpr {
                    value: Box::new(value),
                }
            }
            T!['('] => {
                // parse subexpression in parens with reset binding power
                self.consume(T!['('])?;
                let lhs = self.parse_expr()?;
                self.try_consume(T![')']);
                lhs
            }
            T!['['] => {
                self.consume(T!['['])?;
                let mut entries = Vec::new();
                while !self.at(T![']']) {
                    let entry = self.parse_expr()?;
                    entries.push(entry);
                    if !self.try_consume(T![,]) && !self.at(T![']']) {
                        break;
                    }
                }
                self.try_consume(T![']']);
                AST::ListLiteral { elements: entries }
            }
            found => {
                return Err(ParseError::UnexpectedToken {
                    found,
                    expected: vec![
                        T![bool],
                        T![int],
                        T![float],
                        T![string],
                        T![line comment],
                        T![block comment],
                        T![ident],
                        T!['('],
                        T!['['],
                    ],
                    position: self.position(),
                });
            }
        };

        loop {
            let next = self.peek();
            let op = match next {
                T![+]
                | T![-]
                | T![*]
                | T![/]
                | T![=]
                | T![.]
                | T![&&]
                | T![||]
                | T![!=]
                | T![==]
                | T![<]
                | T![<=]
                | T![>]
                | T![>=] => next,
                T![eof] => break,
                T!['('] | T![')'] | T!['['] | T![']'] | T!['{'] | T!['}'] | T![,] | T![;] => break,
                found => {
                    return Err(ParseError::UnexpectedToken {
                        found,
                        expected: vec![
                            T![+],
                            T![-],
                            T![*],
                            T![/],
                            T![=],
                            T![.],
                            T![&&],
                            T![||],
                            T![!=],
                            T![==],
                            T![<],
                            T![<=],
                            T![>],
                            T![>=],
                        ],
                        position: self.position(),
                    })
                }
            };
            if let Some((left_binding_power, right_binding_power)) = op.infix_binding_power() {
                if left_binding_power < binding_power {
                    break;
                }

                self.consume(op)?;
                let rhs = self.parse_expr_bp(right_binding_power)?;
                lhs = AST::BinaryExpr {
                    op: op.to_string(),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                };
                continue;
            }

            break;
        }
        Ok(lhs)
    }
}
