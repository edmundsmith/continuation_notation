extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;

use core::iter::*;
use syn::*;
use syn::fold::Fold;
use syn::punctuated::{Punctuated, Pair};
use proc_macro2::*;
use quote::*;

//The macro uses 'cont' as abbreviation for 'continuation syntax'
const INVOCATION_NAME : &'static str = "cont";

//Trying to parse an Expr with trailing Parens(Punct(Args,Comma)) needs
//a separator to avoid parsing it as an ExprCall(OrigExpr,Args)
type SEP = Token![,];

#[proc_macro_attribute]
pub fn use_cps(_attr:proc_macro::TokenStream, body:proc_macro::TokenStream) -> proc_macro::TokenStream {
    let tree = parse2::<ItemFn>(body.clone().into()).unwrap();
    let mut f = Folder;
    let folded = syn::fold::fold_item_fn(&mut f, tree);
    //println!("Macro use_cps:\nBefore:\n{}\n\nAfter:\n{}",
    //    body.clone().to_string(), folded.clone().to_token_stream().to_string());

    //I don't know if I'll want a more complex quote than this identity quote yet
    (quote! {
        #folded
    }).into()
}

///For parsing call parameters that may also contain a hole/_ token
#[derive(Debug, Clone)]
enum ContArg {
    Expr(Expr),
    Hole(Token![_]),
}

impl syn::parse::Parse for ContArg {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        input.parse::<Token![_]>().map(ContArg::Hole)
            .or_else(|_|input.parse::<Expr>().map(ContArg::Expr))
    }
}

///Contains the details of the cont! call within the #[use_cps] body
///Parsed from `cont!(func_expr, (args*) as cont_patterns)`
#[derive(Debug, Clone)]
struct ParsedInvocation {
    func_expr: Expr,
    paren_token: syn::token::Paren,
    args: Punctuated<ContArg, Token![,]>,
    tok_as: Token![as],
    asyncness: Option<Token![async]>,
    moves: Option<Token![move]>,
    cont_patterns: Punctuated<Pat, Token![,]>,
}

impl syn::parse::Parse for ParsedInvocation {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let func_expr = input.parse::<Expr>()?;
        let _x = input.parse::<SEP>()?;
        let parens;
        let paren_token = parenthesized!(parens in input);
        let args = parens.parse_terminated::<ContArg,Token![,]>(ContArg::parse)?;
        let tok_as = input.parse::<Token![as]>()?;
        let asyncness: Option<Token![async]> = if input.peek(Token![async]) {
            Some(input.parse::<Token![async]>().unwrap())
        } else {None};
        let moves: Option<Token![move]> = if input.peek(Token![move]) {
            Some(input.parse::<Token![move]>().unwrap())
        } else {None};
        let cont_patterns = Punctuated::<Pat, Token![,]>::parse_terminated(input)?;
        Ok(ParsedInvocation{
            func_expr, paren_token, args, tok_as, asyncness, moves, cont_patterns
        })
    }
}

///Visitor for Fold that performs the block -> closure transform
struct Folder;

impl syn::fold::Fold for Folder {
    //Walk over the block; if a cont! is found, feed it the unwalked remainder
    fn fold_block(&mut self, block:Block) -> Block {
        let mut replacement = Block { brace_token: block.brace_token, stmts: Vec::new() }; 
        let mut refold_needed = false;
        for (i, stmt) in block.stmts.iter().enumerate() {
            
            match stmt {
                //Duplicate logic over similar structure...
                Stmt::Semi(Expr::Macro(em), semi) => {
                    let callexpr = construct_exprcall_from_macro(em.clone(), i, &block.stmts);
                    match callexpr {
                        None => {
                            replacement.stmts.push(stmt.clone())
                        },
                        Some(e) => {
                            refold_needed = true;
                            replacement.stmts.push(Stmt::Semi(e, semi.clone()));
                            break;
                        }
                    }
                },
                Stmt::Expr(Expr::Macro(em)) => {
                    let callexpr = construct_exprcall_from_macro(em.clone(), i, &block.stmts);
                    match callexpr {
                        None => {
                            replacement.stmts.push(stmt.clone())
                        },
                        Some(e) => {
                            refold_needed = true;
                            replacement.stmts.push(Stmt::Expr(e));
                            break;
                        }
                    }
                },
                _ => {
                    replacement.stmts.push(syn::fold::fold_stmt(self, stmt.clone()));
                }
            }
        }

        if refold_needed {
            syn::fold::fold_block(self, replacement)
        } else {
            replacement
        }
    }
}

///Common code for taking a macro `e` and its position `i` in the block `stmts`
///and constructing the replacement closure call from it
fn construct_exprcall_from_macro(e:ExprMacro,i:usize,stmts:&Vec<Stmt>) -> Option<Expr> {
    let makro = &e.mac;
    if !makro.path.is_ident(INVOCATION_NAME) {
        return None
    }
    //From here onwards, a parse error is an error in the cont! call
    let parsed_invocation: ParsedInvocation = parse2(makro.tokens.clone()).unwrap();
    //println!("Parsed cont invocation {:?}", parsed_invocation);
    let asyncness: Option<Token![async]> = parsed_invocation.asyncness;
    let staticness: Option<Token![static]> = None;
    let captures: Option<Token![move]> = parsed_invocation.moves;
    let remaining_stmts : Vec<_> = stmts[i+1..stmts.len()].into();
    let formed_closure = Expr::Closure(ExprClosure {
        attrs: Vec::new(),
        asyncness: asyncness,
        movability: staticness,
        capture: captures,
        or1_token: Default::default(),
        inputs: parsed_invocation.cont_patterns,
        or2_token: Default::default(),
        output: ReturnType::Default, //Infer return type
        body: Box::new(Expr::Block(ExprBlock{
            attrs: Vec::new(),
            label: None,
            block: Block{
                brace_token: Default::default(),
                stmts: remaining_stmts,
            }
        }))
    });
    //Replaces holes in the continue call's args with the formed closure
    let hole_filler: &dyn Fn(ContArg)->Expr = &|a| match a {
        ContArg::Hole(_) => formed_closure.clone(),
        ContArg::Expr(e) => e.clone()
    };
    let replaced_args = Punctuated::<Expr, Token![,]>::from_iter(
        parsed_invocation.args.into_pairs()
        .map(|a| match a {
            Pair::Punctuated(e, t) => Pair::Punctuated(hole_filler(e),t),
            Pair::End(e) => Pair::End(hole_filler(e))
        }));
    let cont_call_expr = Expr::Call(ExprCall{
        func: Box::new(parsed_invocation.func_expr),
        //attrs: Default::default(),
        attrs: e.attrs,
        paren_token: Default::default(),
        args: replaced_args,
    });
    //println!("Newly formed continuation call:\n{}", cont_call_expr.to_token_stream().to_string());
    Some(cont_call_expr)
}
