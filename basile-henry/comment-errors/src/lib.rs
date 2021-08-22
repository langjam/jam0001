use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use syn::fold::Fold;
use syn::{parse_macro_input, parse_quote, ItemFn, Lit, LitStr, Meta, MetaNameValue};

struct CommentErrorFold {
    comments: Vec<String>,
}

impl CommentErrorFold {
    fn new() -> Self {
        CommentErrorFold {
            comments: Vec::new(),
        }
    }

    fn get_comment(&mut self) -> String {
        // Remove leading indent
        let count_leading_spaces = |s: &str| s.bytes().take_while(|&b| b == b' ').count();
        let offset = self
            .comments
            .iter()
            .filter(|l| !l.trim().is_empty())
            .map(|s| count_leading_spaces(s))
            .min()
            .unwrap_or_default();

        let mut text = "\n".to_string();
        for line in self.comments.drain(..) {
            text.push('\n');
            if !line.trim().is_empty() {
                text.push_str(&line[offset..]);
            }
        }

        text
    }
}

fn comment_post_process(comment: &str) -> proc_macro2::TokenStream {
    use regex::Regex;

    let anchor_regex = Regex::new(r"<\w+>").unwrap();
    let anchors = anchor_regex
        .find_iter(comment)
        .map(|m| {
            let anchor = m
                .as_str()
                .strip_prefix('<')
                .unwrap()
                .strip_suffix('>')
                .unwrap();
            Ident::new(anchor, Span::call_site())
        })
        .collect::<Vec<_>>();

    if anchors.is_empty() {
        Lit::from(LitStr::new(comment, Span::call_site())).to_token_stream()
    } else {
        let format_str = anchor_regex.replace_all(comment, "{}");

        quote! {
            &format!(#format_str, #(&#anchors),*)
        }
    }
}

impl Fold for CommentErrorFold {
    fn fold_attribute(&mut self, attr: syn::Attribute) -> syn::Attribute {
        if attr.path.is_ident("doc") {
            if let Ok(Meta::NameValue(MetaNameValue {
                lit: Lit::Str(lit_str),
                ..
            })) = attr.parse_meta()
            {
                self.comments.push(lit_str.value());
            }
        }

        attr
    }

    fn fold_expr_try(&mut self, expr_try: syn::ExprTry) -> syn::ExprTry {
        let mut expr_try = syn::fold::fold_expr_try(self, expr_try);

        // Only comment expr_try when we have comments
        if !self.comments.is_empty() {
            let comment = comment_post_process(&self.get_comment());
            let expr = &expr_try.expr;
            expr_try = parse_quote! {
                #expr.wrap_err_with(|| {
                    let mut skin = ::termimad::MadSkin::default_dark();
                    format!("{}", ::termimad::FmtText::from(&skin, #comment, Some(100)))
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
