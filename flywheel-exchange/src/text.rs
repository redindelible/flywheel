use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::iter::FusedIterator;
use std::sync::LazyLock;

use aho_corasick::{AhoCorasick, Anchored, Input, MatchKind, StartKind};
use arcstr::ArcStr;
use enum_map::{EnumMap, enum_map};
use logos::{Lexer, Logos};
use strum::VariantArray;

use crate::interchange::{Function, Global, Instruction, InstructionKind, Terminator, TerminatorKind, TupleType, Type};
use crate::{Block, BlockID};

#[derive(Logos, PartialEq, Eq, Copy, Clone, Hash, Debug)]
#[logos(skip r"[ \t\r\n]+")]
pub enum Token {
    Eof,
    Error,
    #[token("define")]
    Def,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token("/")]
    Slash,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[regex(r"%[a-zA-Z0-9._\-]+")]
    Name,
    #[regex(r"@[a-zA-Z0-9._\-]+")]
    LabelName,
    #[regex(r"[a-zA-Z][a-zA-Z0-9._\-]+")]
    ReservedName,
    #[regex(r"-?[0-9]+(\.[0-9]+)?")]
    Number,
}

pub type ParseResult<'s, T> = Result<T, ParseError<'s>>;

#[derive(Debug)]
pub enum ParseError<'s> {
    UnexpectedCharacters { after_slice: &'s str },
    UnexpectedEof { after_slice: &'s str },
    InvalidLiteral { as_type: &'static str, slice: &'s str },

    UnexpectedToken { expected: Token, found: Token, slice: &'s str },

    TooManyLocals { slice: &'s str },
    DuplicatedLocal { slice: &'s str },
    UnresolvedLocal { slice: &'s str },

    TooManyLabels { slice: &'s str },
    UnresolvedLabel { slice: &'s str },
    DuplicatedLabel { slice: &'s str },

    UnknownInstruction { slice: &'s str },
    InstructionAfterTerminator { slice: &'s str },
    NoTerminator { slice: &'s str },

    NotAType { slice: &'s str },
}

static INSTRUCTION_NAMES: LazyLock<EnumMap<InstructionKind, &'static str>> = LazyLock::new(|| {
    enum_map! {
            InstructionKind::LoadConst => "load.const",
            InstructionKind::LoadLocal => "load.local",
            InstructionKind::StoreLocal => "store.local",
            InstructionKind::LoadUnit => "load.unit",
            InstructionKind::LoadInteger => "load.int",
            InstructionKind::LoadFloat => "load.float",
            InstructionKind::Upcast => "cast.up",
            InstructionKind::Call => "call",
            InstructionKind::LessEqual => "le",
            InstructionKind::Equal => "eq",
            InstructionKind::Add => "add",
            InstructionKind::Mul => "mul",
            InstructionKind::Div => "div",
            InstructionKind::Mod => "mod",
    }
});

static TERMINATOR_NAMES: LazyLock<EnumMap<TerminatorKind, &'static str>> = LazyLock::new(|| {
    enum_map! {
            TerminatorKind::IfElse => "branch",
            TerminatorKind::Jump => "jump",
            TerminatorKind::Loop => "loop",
            TerminatorKind::Return => "ret",
    }
});

struct ModuleParser<'s> {
    current: Token,
    lexer: Lexer<'s, Token>,

    instruction_matcher: AhoCorasick,

    locals: HashMap<&'s str, u32>,
    blocks: HashMap<&'s str, BlockInfo>,
}

struct BlockInfo {
    id: BlockID,
    is_defined: bool,
}

impl<'s> ModuleParser<'s> {
    fn instruction_matcher() -> AhoCorasick {
        static MATCHER: LazyLock<AhoCorasick> = LazyLock::new(|| {
            let mut patterns = vec![];
            patterns.extend_from_slice(INSTRUCTION_NAMES.as_slice());
            patterns.extend_from_slice(TERMINATOR_NAMES.as_slice());
            let instruction_matcher = AhoCorasick::builder()
                .start_kind(StartKind::Anchored)
                .match_kind(MatchKind::LeftmostLongest)
                .build(patterns)
                .unwrap();
            instruction_matcher
        });

        MATCHER.clone()
    }

    pub fn new(text: &'s str) -> Self {
        let mut lexer = Lexer::new(text);
        let current = lexer.next().map_or(Token::Eof, |tok| tok.unwrap_or(Token::Error));
        let instruction_matcher = Self::instruction_matcher();

        ModuleParser { current, lexer, instruction_matcher, locals: HashMap::new(), blocks: HashMap::new() }
    }

    fn name_from(&mut self, s: &str) -> ArcStr {
        ArcStr::from(s)
    }

    fn advance(&mut self) {
        self.current = self.lexer.next().map_or(Token::Eof, |err| err.unwrap_or(Token::Error));
    }

    fn expect(&mut self, token: Token) -> ParseResult<'s, &'s str> {
        if self.current == token {
            let s = self.lexer.slice();
            self.advance();
            Ok(s)
        } else if self.current == Token::Error {
            Err(ParseError::UnexpectedCharacters { after_slice: self.lexer.slice() })
        } else if self.current == Token::Eof {
            Err(ParseError::UnexpectedEof { after_slice: self.lexer.slice() })
        } else {
            Err(ParseError::UnexpectedToken { expected: token, found: self.current, slice: self.lexer.slice() })
        }
    }

    fn add_local(&mut self, name: &'s str) -> ParseResult<'s, u32> {
        let Ok(index) = u32::try_from(self.locals.len()) else { return Err(ParseError::TooManyLocals { slice: name }) };
        if let Some(_old) = self.locals.insert(name, index) {
            return Err(ParseError::DuplicatedLocal { slice: name });
        }
        Ok(index)
    }

    fn label_reference(&mut self, label_name: &'s str, is_definition: bool) -> ParseResult<'s, BlockID> {
        let labels_count = self.blocks.len();
        let label_info = match self.blocks.entry(label_name) {
            Entry::Occupied(occupied) => occupied.into_mut(),
            Entry::Vacant(vacant) => {
                let Ok(id) = u32::try_from(labels_count) else {
                    return Err(ParseError::TooManyLabels { slice: label_name });
                };
                vacant.insert(BlockInfo { id: BlockID(id), is_defined: false })
            }
        };
        if is_definition {
            if label_info.is_defined {
                return Err(ParseError::DuplicatedLabel { slice: label_name });
            } else {
                label_info.is_defined = true;
            }
        }
        Ok(label_info.id)
    }

    fn next_global(&mut self) -> Option<ParseResult<'s, Global>> {
        let global: ParseResult<Global> = match self.current {
            Token::Def => self.parse_function_definition().map(Global::Function),
            Token::Eof => return None,
            Token::Error => Err(ParseError::UnexpectedCharacters { after_slice: self.lexer.slice() }),
            _ => panic!(),
        };
        Some(global)
    }

    fn parse_function_definition(&mut self) -> ParseResult<'s, Function> {
        self.blocks.clear();

        self.expect(Token::Def)?;
        let name = self.expect(Token::Name)?;
        let name = self.name_from(name);

        let mut parameters = vec![];
        self.expect(Token::LeftParen)?;
        while self.current != Token::RightParen {
            let param_type = self.parse_type()?;

            parameters.push(param_type);

            if self.current != Token::Comma {
                break;
            } else {
                self.advance();
            }
        }
        self.expect(Token::RightParen)?;

        let return_type = self.parse_type()?;

        self.expect(Token::LeftBrace)?;
        let mut blocks = HashMap::new();
        while self.current != Token::RightBrace {
            self.locals.clear();

            let label_name = self.expect(Token::LabelName)?;
            let label_id = self.label_reference(label_name, true)?;

            let mut retained_locals = vec![];
            let mut new_locals = vec![];
            let mut before_slash = true;
            self.expect(Token::LeftBracket)?;
            while self.current != Token::RightBracket {
                let local_type = self.parse_type()?;
                let local_name = self.expect(Token::Name)?;
                self.add_local(local_name)?;
                if before_slash {
                    retained_locals.push(local_type);
                } else {
                    new_locals.push(local_type);
                }

                if self.current == Token::Comma {
                    self.expect(Token::Comma)?;
                } else if self.current == Token::Slash && !before_slash {
                    self.expect(Token::Slash)?;
                    before_slash = false;
                } else {
                    break;
                }
            }
            self.expect(Token::RightBracket)?;

            let mut instructions = vec![];
            let mut terminator = None;
            while self.current != Token::RightBrace && self.current != Token::LabelName {
                let instruction_name = self.expect(Token::ReservedName)?;
                if terminator.is_some() {
                    return Err(ParseError::InstructionAfterTerminator { slice: instruction_name });
                }

                let match_ = self.instruction_matcher.find(Input::new(instruction_name).anchored(Anchored::Yes));
                let Some(match_) = match_.filter(|m| m.len() == instruction_name.len()) else {
                    return Err(ParseError::UnknownInstruction { slice: instruction_name });
                };
                let index = match_.pattern().as_usize();
                if index < InstructionKind::VARIANTS.len() {
                    instructions.push(self.parse_instruction(InstructionKind::VARIANTS[index])?);
                } else {
                    let term_kind = TerminatorKind::VARIANTS[index - InstructionKind::VARIANTS.len()];
                    terminator = Some(self.parse_terminator(term_kind)?);
                }
            }

            let Some(terminator) = terminator else {
                return Err(ParseError::NoTerminator { slice: label_name });
            };
            blocks.insert(label_id, Block { retained_locals, new_locals, instructions, terminator });
        }
        self.expect(Token::RightBrace)?;

        Ok(Function { name, parameters, return_type, blocks })
    }

    fn parse_instruction(&mut self, instr_kind: InstructionKind) -> ParseResult<'s, Instruction> {
        let instr = match instr_kind {
            InstructionKind::LoadConst => {
                let global_name = self.expect(Token::Name)?;
                let global_name = self.name_from(global_name);

                Instruction::LoadConst { name: global_name }
            }
            InstructionKind::LoadLocal => {
                let local_name = self.expect(Token::Name)?;
                let Some(&index) = self.locals.get(local_name) else {
                    return Err(ParseError::UnresolvedLocal { slice: local_name });
                };

                Instruction::LoadLocal { index }
            }
            InstructionKind::StoreLocal => {
                let local_name = self.expect(Token::Name)?;
                let Some(&index) = self.locals.get(local_name) else {
                    return Err(ParseError::UnresolvedLocal { slice: local_name });
                };
                Instruction::StoreLocal { index }
            }
            InstructionKind::LoadUnit => Instruction::LoadUnit,
            InstructionKind::LoadInteger => {
                let literal = self.expect(Token::Number)?;
                let literal = literal
                    .parse::<i64>()
                    .map_err(|_| ParseError::InvalidLiteral { as_type: "i64", slice: literal })?;
                Instruction::LoadInteger(literal)
            }
            InstructionKind::LoadFloat => {
                let literal = self.expect(Token::Number)?;
                let literal = literal
                    .parse::<f64>()
                    .map_err(|_| ParseError::InvalidLiteral { as_type: "f64", slice: literal })?;
                Instruction::LoadFloat(literal)
            }
            InstructionKind::Upcast => {
                let ty = self.parse_type()?;
                Instruction::Upcast { to_ty: ty }
            }
            InstructionKind::Call => {
                let literal = self.expect(Token::Number)?;
                let literal = literal
                    .parse::<u32>()
                    .map_err(|_| ParseError::InvalidLiteral { as_type: "u32", slice: literal })?;
                Instruction::Call { arguments: literal }
            }
            InstructionKind::LessEqual => Instruction::LessEqual,
            InstructionKind::Equal => Instruction::Equal,
            InstructionKind::Add => Instruction::Add,
            InstructionKind::Mul => Instruction::Mul,
            InstructionKind::Div => Instruction::Div,
            InstructionKind::Mod => Instruction::Mod,
        };

        Ok(instr)
    }

    fn parse_terminator(&mut self, term_kind: TerminatorKind) -> ParseResult<'s, Terminator> {
        let term = match term_kind {
            TerminatorKind::IfElse => {
                let true_label = self.expect(Token::LabelName)?;
                let false_label = self.expect(Token::LabelName)?;
                Terminator::IfElse {
                    true_target: self.label_reference(true_label, false)?,
                    false_target: self.label_reference(false_label, false)?,
                }
            }
            TerminatorKind::Jump => {
                let label = self.expect(Token::LabelName)?;
                Terminator::Jump { target: self.label_reference(label, false)? }
            }
            TerminatorKind::Loop => {
                let label = self.expect(Token::LabelName)?;
                Terminator::Loop { target: self.label_reference(label, false)? }
            }
            TerminatorKind::Return => Terminator::Return,
        };
        Ok(term)
    }

    fn parse_type(&mut self) -> ParseResult<'s, Type> {
        match self.current {
            Token::ReservedName => {
                let ty = match self.lexer.slice() {
                    "int" => Type::Integer,
                    "float" => Type::Float,
                    "unit" => Type::Unit,
                    _ => return Err(ParseError::NotAType { slice: self.lexer.slice() }),
                };
                self.advance();
                Ok(ty)
            }
            Token::Name => {
                let ty = Type::Name(self.name_from(self.lexer.slice()));
                self.advance();
                Ok(ty)
            }
            Token::LeftParen => {
                self.expect(Token::LeftParen)?;
                let mut parts = vec![];
                while self.current != Token::RightParen {
                    parts.push(self.parse_type()?);
                    if self.current != Token::Comma {
                        break;
                    } else {
                        self.advance();
                    }
                }
                self.expect(Token::RightParen)?;
                Ok(TupleType::new(parts).into())
            }
            other => Err(match other {
                Token::Eof => ParseError::UnexpectedEof { after_slice: self.lexer.slice() },
                Token::Error => ParseError::UnexpectedCharacters { after_slice: self.lexer.slice() },
                _ => ParseError::NotAType { slice: self.lexer.slice() },
            }),
        }
    }
}

impl<'s> Iterator for ModuleParser<'s> {
    type Item = ParseResult<'s, Global>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_global()
    }
}

impl FusedIterator for ModuleParser<'_> {}

#[cfg(test)]
mod test {
    use crate::interchange::Global;
    use crate::text::ModuleParser;

    #[test]
    fn test_parser() {
        let s = include_str!("../test/collatz.flyi");
        for global in ModuleParser::new(s) {
            match global.unwrap() {
                Global::Function(f) => {
                    dbg!(f);
                }
            }
        }
    }
}
