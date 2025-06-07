use crate::file_ast::FileAST;
use crate::source::Location;

pub trait AsDebugTree {
    fn as_debug_tree(&self, context: &FileAST) -> PrettyTree;
}

pub struct PrettyOptions {
    pub(crate) indent: String,
    pub(crate) _max_width: usize,
}

pub enum PrettyTree {
    StructBranch { name: String, children: Vec<(String, PrettyTree)> },
    TupleStructBranch { name: String, children: Vec<PrettyTree> },
    ListBranch { children: Vec<PrettyTree> },
    Leaf(String),
}

impl PrettyTree {
    pub fn from_struct<S>(name: impl Into<String>, children: impl IntoIterator<Item = (S, PrettyTree)>) -> PrettyTree
    where
        S: Into<String>,
    {
        let name = name.into();
        let children = children.into_iter().map(|(s, n)| (s.into(), n)).collect();
        PrettyTree::StructBranch { name, children }
    }

    pub fn from_tuple_struct(name: impl Into<String>, children: impl IntoIterator<Item =PrettyTree>) -> PrettyTree {
        PrettyTree::TupleStructBranch { name: name.into(), children: children.into_iter().collect() }
    }

    pub fn from_list(items: impl IntoIterator<Item =PrettyTree>) -> PrettyTree {
        PrettyTree::ListBranch { children: items.into_iter().collect() }
    }

    pub fn from_option(maybe: Option<PrettyTree>) -> PrettyTree {
        match maybe {
            Some(node) => PrettyTree::from_tuple_struct("Some", [node]),
            None => PrettyTree::from_string("None"),
        }
    }

    pub fn from_string(text: impl Into<String>) -> PrettyTree {
        PrettyTree::Leaf(text.into())
    }

    pub fn from_location(location: Location) -> PrettyTree {
        PrettyTree::from_string(format!("{location:?}"))
    }
}

impl PrettyTree {
    pub fn render_delimited<'a>(
        start: &str,
        end: &str,
        children: impl IntoIterator<Item = (String, &'a PrettyTree)> + 'a,
        options: &PrettyOptions,
        line_prefix: &str,
    ) -> String {
        let mut output = start.to_string();
        let child_line_prefix = String::from_iter([line_prefix, options.indent.as_str()]);
        let mut is_first = true;
        for (field_prefix, field) in children {
            if !is_first {
                output.push(',');
            } else {
                is_first = false;
            }
            output.push('\n');
            output.push_str(&child_line_prefix);
            output.push_str(&field_prefix);
            output.push_str(&field.render(options, &child_line_prefix));
        }
        output.push('\n');
        output.push_str(line_prefix);
        output.push_str(end);
        output
    }

    pub fn render(&self, options: &PrettyOptions, line_prefix: &str) -> String {
        match self {
            PrettyTree::StructBranch { name, children } => PrettyTree::render_delimited(
                &format!("{} {{", name),
                "}",
                children.iter().map(|(field_name, field)| (format!("{}: ", field_name), field)),
                options,
                line_prefix,
            ),
            PrettyTree::TupleStructBranch { name, children } => PrettyTree::render_delimited(
                &format!("{} (", name),
                ")",
                children.iter().map(|field| (String::new(), field)),
                options,
                line_prefix,
            ),
            PrettyTree::ListBranch { children } => PrettyTree::render_delimited(
                "[",
                "]",
                children.iter().map(|field| (String::new(), field)),
                options,
                line_prefix,
            ),
            PrettyTree::Leaf(text) => text.clone(),
        }
    }
}