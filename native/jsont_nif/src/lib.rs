use rustler::{
    types::{atom, tuple::make_tuple, Encoder},
    Atom, Binary, Env, NewBinary, Term,
};
use std::io::Write;
mod decoder;
mod encoder;

rustler::init!("Elixir.Jsont.NifBridge");

#[rustler::nif(schedule = "DirtyCpu")]
fn encode<'a>(
    env: Env<'a>,
    term: Term<'a>,
    bigint_as_string: bool,
    strip_elixir_struct: bool,
) -> (Atom, Term<'a>) {
    match encoder::encode(env, term, bigint_as_string, strip_elixir_struct) {
        Ok(out) => {
            let mut bin = NewBinary::new(env, out.len());
            if let Err(e) = bin.as_mut_slice().write_all(&out) {
                return (atom::error(), encode_error(env, e.into()));
            }
            let term = Binary::from(bin).to_term(env);
            (atom::ok(), term)
        }
        Err(e) => (atom::error(), encode_error(env, e)),
    }
}

#[rustler::nif(schedule = "DirtyCpu")]
fn decode<'a>(env: Env<'a>, term: Term<'a>, validate_unicode: bool) -> (Atom, Term<'a>) {
    let binary = match Binary::from_iolist(term) {
        Ok(binary) => binary,
        Err(e) => {
            return (atom::error(), encode_error(env, Error::Rustler(e)));
        }
    };
    match decoder::decode(env, binary, validate_unicode) {
        Ok(term) => (atom::ok(), term),
        Err(e) => (atom::error(), encode_error(env, e)),
    }
}

#[cold]
fn encode_error<'a>(env: Env<'a>, e: Error<'a>) -> Term<'a> {
    match e {
        Error::Tuple(atom, terms) => {
            let terms = [&[atom.encode(env)], terms.as_slice()].concat();
            make_tuple(env, &terms)
        }
        Error::Rustler(rustler::Error::BadArg) => atom::badarg().encode(env),
        Error::Rustler(rustler::Error::Atom(a) | rustler::Error::RaiseAtom(a)) => {
            Atom::from_str(env, a)
                .unwrap_or_else(|_| atom::error())
                .encode(env)
        }
        Error::Rustler(rustler::Error::Term(t) | rustler::Error::RaiseTerm(t)) => t.encode(env),
    }
}

enum Error<'a> {
    Rustler(rustler::Error),
    Tuple(Atom, Vec<Term<'a>>),
}

impl<'a> Error<'a> {
    fn tuple(atom: Atom, terms: Vec<Term<'a>>) -> Self {
        Self::Tuple(atom, terms)
    }
}

impl<'a> std::fmt::Debug for Error<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Error")
    }
}

impl<'a> std::fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl<'a> std::error::Error for Error<'a> {}

impl<'a> From<rustler::Error> for Error<'a> {
    fn from(error: rustler::Error) -> Self {
        Self::Rustler(error)
    }
}

impl<'a> From<serde_json::Error> for Error<'a> {
    fn from(error: serde_json::Error) -> Self {
        Self::Rustler(rustler::Error::Term(Box::new(format!("{}", error))))
    }
}

impl<'a> From<std::io::Error> for Error<'a> {
    fn from(error: std::io::Error) -> Self {
        Self::Rustler(rustler::Error::Term(Box::new(format!("{}", error))))
    }
}
