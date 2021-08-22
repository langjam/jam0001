use proc_macro::TokenStream;
use proc_macro2::{Literal, Span, TokenTree};
use quote::quote;
use syn::fold::Fold;
use syn::{parse_macro_input, parse_quote, Ident, ItemFn};

struct CommentErrorFold {
    comments: Vec<Literal>,
}

impl CommentErrorFold {
    fn new() -> Self {
        CommentErrorFold {
            comments: Vec::new(),
        }
    }
}

impl Fold for CommentErrorFold {
    fn fold_attribute(&mut self, attr: syn::Attribute) -> syn::Attribute {
        match attr.path.segments.iter().cloned().next() {
            Some(segment) if segment.ident == Ident::new("doc", Span::call_site()) => {
                for token in attr.tokens.clone().into_iter() {
                    if let TokenTree::Literal(literal) = token {
                        self.comments.push(literal);
                    }
                }
            }
            _ => {}
        }

        attr
    }

    fn fold_expr_try(&mut self, expr_try: syn::ExprTry) -> syn::ExprTry {
        let mut expr_try = syn::fold::fold_expr_try(self, expr_try);

        // Only comment expr_try when we have comments
        if !self.comments.is_empty() {
            let comments: Vec<Literal> = self.comments.drain(..).collect();
            let expr = &expr_try.expr;
            expr_try = parse_quote! {
                #expr.wrap_err_with(|| {
                    let lines = [#(#comments),*];
                    let count_leading_spaces = |s: &str| s.bytes().take_while(|&b| b == b' ').count();
                    let offset = lines
                        .iter()
                        .filter(|l| !l.trim().is_empty())
                        .map(|&s| count_leading_spaces(s))
                        .min()
                        .unwrap_or_default();

                    let mut text = "\n".to_string();
                    for line in lines {
                        text.push('\n');
                        if !line.trim().is_empty() {
                            text.push_str(&line[offset..]);
                        }
                    }

                    let mut skin = ::termimad::MadSkin::default_dark();

                    format!("{}", ::termimad::FmtText::from(&skin, &text, Some(100)))
                })?
            }
        }

        expr_try
    }
}

#[proc_macro_attribute]
pub fn wrap(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut fold = CommentErrorFold::new();
    let item: ItemFn = parse_macro_input!(item);
    let item = fold.fold_item_fn(item);
    (quote! { # item }).into()
}
