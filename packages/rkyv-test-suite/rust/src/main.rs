// The oracle `fuzz.ts` holds S.rkyv to. Each stdin line is `<type id> <hex>`;
// the answer is `ERR <rkyv's reason>` when rkyv's validator refuses the bytes,
// else `OK <dump> <hex>`: the value in the canonical text both sides print, and
// the bytes rkyv's own serializer writes for it.
#![allow(non_camel_case_types, dead_code)]

use std::fmt::Write as _;
use std::io::{BufRead, Write};

pub trait Dump {
    fn dump(&self, o: &mut String);
}

macro_rules! dump_int {
    ($($t:ty),*) => {$(impl Dump for $t { fn dump(&self, o: &mut String) { write!(o, "{}", self).unwrap() } })*};
}
dump_int!(u8, i8, u16, i16, u32, i32, u64, i64, u128, i128);

impl Dump for f32 {
    fn dump(&self, o: &mut String) {
        if self.is_nan() { o.push_str("nan") } else { write!(o, "{:08x}", self.to_bits()).unwrap() }
    }
}
impl Dump for f64 {
    fn dump(&self, o: &mut String) {
        if self.is_nan() { o.push_str("nan") } else { write!(o, "{:016x}", self.to_bits()).unwrap() }
    }
}
impl Dump for bool {
    fn dump(&self, o: &mut String) { o.push(if *self { 't' } else { 'f' }) }
}
impl Dump for char {
    fn dump(&self, o: &mut String) { write!(o, "c{}", *self as u32).unwrap() }
}
impl Dump for String {
    fn dump(&self, o: &mut String) {
        o.push('s');
        for b in self.as_bytes() { write!(o, "{:02x}", b).unwrap() }
    }
}
impl Dump for () {
    fn dump(&self, o: &mut String) { o.push_str("()") }
}
impl<T: Dump> Dump for Vec<T> {
    fn dump(&self, o: &mut String) {
        o.push('[');
        for (i, x) in self.iter().enumerate() { if i > 0 { o.push(',') } x.dump(o) }
        o.push(']');
    }
}
impl<T: Dump> Dump for Option<T> {
    fn dump(&self, o: &mut String) {
        match self { None => o.push('N'), Some(x) => { o.push('S'); x.dump(o) } }
    }
}
impl<T: Dump> Dump for Box<T> {
    fn dump(&self, o: &mut String) { (**self).dump(o) }
}
macro_rules! dump_tuple {
    ($($n:tt $t:ident),*) => {
        impl<$($t: Dump),*> Dump for ($($t,)*) {
            fn dump(&self, o: &mut String) {
                o.push('(');
                $( if $n > 0 { o.push(',') } self.$n.dump(o); )*
                o.push(')');
            }
        }
    };
}
dump_tuple!(0 A);
dump_tuple!(0 A, 1 B);
dump_tuple!(0 A, 1 B, 2 C);
dump_tuple!(0 A, 1 B, 2 C, 3 D);
dump_tuple!(0 A, 1 B, 2 C, 3 D, 4 E);

fn hex(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes { write!(s, "{:02x}", b).unwrap() }
    s
}

fn unhex(s: &str) -> rkyv::util::AlignedVec<16> {
    let mut v = rkyv::util::AlignedVec::<16>::with_capacity(s.len() / 2);
    for i in (0..s.len()).step_by(2) { v.push(u8::from_str_radix(&s[i..i + 2], 16).unwrap()) }
    v
}

fn check<T>(bytes: &[u8]) -> String
where
    T: Dump + rkyv::Archive,
    T::Archived: for<'a> rkyv::bytecheck::CheckBytes<rkyv::api::high::HighValidator<'a, rkyv::rancor::Error>>
        + rkyv::Deserialize<T, rkyv::api::high::HighDeserializer<rkyv::rancor::Error>>,
    T: for<'a> rkyv::Serialize<rkyv::api::high::HighSerializer<rkyv::util::AlignedVec, rkyv::ser::allocator::ArenaHandle<'a>, rkyv::rancor::Error>>,
{
    match rkyv::from_bytes::<T, rkyv::rancor::Error>(bytes) {
        Err(e) => format!("ERR {}", e.to_string().replace('\n', " ")),
        Ok(value) => {
            let mut dump = String::new();
            value.dump(&mut dump);
            let again = rkyv::to_bytes::<rkyv::rancor::Error>(&value).unwrap();
            format!("OK {} {}", dump, hex(&again))
        }
    }
}

include!("generated.rs");

fn main() {
    let stdin = std::io::stdin();
    let mut out = std::io::BufWriter::new(std::io::stdout());
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let (id, rest) = line.split_once(' ').unwrap_or((&line, ""));
        let bytes = unhex(rest);
        writeln!(out, "{}", dispatch(id.parse().unwrap(), &bytes)).unwrap();
        out.flush().unwrap();
    }
}
