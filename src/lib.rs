//! # Mime
//!
//! Mime is now Media Type, technically, but `Mime` is more immediately
//! understandable, so the main type here is `Mime`.
//!
//! ## What is Mime?
//!
//! Example mime string: `text/plain;charset=utf-8`
//!
//! ```rust
//! # #[macro_use] extern crate mime;
//! # fn main() {
//! let plain_text: mime::Mime = "text/plain;charset=utf-8".parse().unwrap();
//! assert_eq!(plain_text, mime!(Text/Plain; Charset=Utf8));
//! # }
//! ```

#![doc(html_root_url = "https://hyperium.github.io/mime.rs")]
#![cfg_attr(test, deny(warnings))]
#![cfg_attr(all(feature = "nightly", test), feature(test))]
#![cfg_attr(feature = "heap_size", feature(custom_derive, plugin))]
#![cfg_attr(feature = "heap_size", plugin(heapsize_plugin))]
#![cfg_attr(feature = "nightly", feature(deprecated))]

#[macro_use]
extern crate log;

#[cfg(feature = "nightly")]
#[cfg(test)]
extern crate test;

#[cfg(feature = "serde")]
extern crate serde;

#[cfg(feature = "serde")]
#[cfg(test)]
extern crate serde_json;

#[cfg(feature = "heap_size")]
extern crate heapsize;
extern crate unicase;

use unicase::UniCase;

use std::borrow::Cow;
use std::fmt;
use std::str::FromStr;

/// Mime, or Media Type. Encapsulates common registers types.
///
/// Consider that a traditional mime type contains a "top level type",
/// a "sub level type", and 0-N "parameters". And they're all strings.
/// Strings everywhere. Strings mean typos. Rust has type safety. We should
/// use types!
///
/// So, Mime bundles together this data into types so the compiler can catch
/// your typos.
///
/// This improves things so you can use match without Strings:
///
/// ```rust
/// use mime;
///
/// let mime: Mime = "application/json".parse().unwrap();
///
/// match mime {
///     Mime(TopLevel::Application, SubLevel::Json, _) => println!("matched json!"),
///     _ => ()
/// }
/// ```
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "heap_size", derive(HeapSizeOf))]
pub struct Mime {
    type_: Name,
    subtype: Name,
    suffix: Option<Name>,
    params: Vec<Param>,
}

/// The old Mime constructor.
#[cfg_attr(feature = "nightly", deprecated(note = "TODO"))]
#[allow(non_snake_case)]
pub fn Mime(top: Name, sub: Name, params: Vec<Param>) -> Mime {
    Mime {
        type_: top,
        subtype: sub,
        suffix: None,
        params: params
    }
}


/// Easily create a Mime without having to import so many enums.
///
/// # Example
///
/// ```
/// # #[macro_use] extern crate mime;
///
/// # fn main() {
/// let json = mime!(Application/Json);
/// let plain = mime!(Text/Plain; Charset=Utf8);
/// let text = mime!(Text/Html; Charset=("bar"), ("baz")=("quux"));
/// let img = mime!(Image/_);
/// # }
/// ```
#[macro_export]
macro_rules! mime {
    ($top:tt / $sub:tt) => (
        mime!($top / $sub;)
    );

    ($top:tt / $sub:tt ; $($attr:tt = $val:tt),*) => (
        $crate::Mime {
            type_: __mime__ident_or_ext!($top),
            subtype: __mime__ident_or_ext!($sub),
            suffix: None,
            params: vec![ $((__mime__ident_or_ext!($attr), __mime__ident_or_ext!($val))),* ]
        }
    );
}

#[doc(hidden)]
#[macro_export]
macro_rules! __mime__ident_or_ext {
    (_) => (
        $crate::Star
    );
    (($inner:expr)) => (
        $inner.parse().unwrap()
    );
    ($var:ident) => (
        $crate::$var
    )
}

#[derive(Clone, Hash, Eq)]
pub struct Name(UniCase<Cow<'static, str>>);

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("Name")
            .field(&(self.0).0)
            .finish()
    }
}

impl Name {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl ::std::ops::Div<Name> for Name {
    type Output = Mime;

    fn div(self, rhs: Name) -> Mime {
        Mime {
            type_: self,
            subtype: rhs,
            suffix: None,
            params: Vec::new()
        }
    }
}

impl ::std::ops::Add<Name> for Mime {
    type Output = Mime;
    fn add(mut self, rhs: Name) -> Mime {
        self.suffix = Some(rhs);
        self
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Name) -> bool {
        self.0 == other.0
    }
}

impl PartialEq<String> for Name {
    fn eq(&self, other: &String) -> bool {
        self.0 == UniCase(other)
    }
}

impl PartialEq<str> for Name {
    fn eq(&self, other: &str) -> bool {
        self.0 == UniCase(other)
    }
}

impl<'a> PartialEq<&'a str> for Name {
    fn eq(&self, other: &&'a str) -> bool {
        self.0 == UniCase(*other)
    }
}

impl PartialEq<Name> for String {
    fn eq(&self, other: &Name) -> bool {
        UniCase(self) == other.0
    }
}

impl PartialEq<Name> for str {
    fn eq(&self, other: &Name) -> bool {
        UniCase(self) == other.0
    }
}

impl<'a> PartialEq<Name> for &'a str {
    fn eq(&self, other: &Name) -> bool {
        UniCase(*self) == other.0
    }
}

impl fmt::Display for Name {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(&self.0)
    }
}

#[derive(Debug, Clone)]
pub struct NameError(());

impl FromStr for Name {
    type Err = NameError;
    fn from_str(s: &str) -> Result<Name, NameError> {
        trace!("Name::from_str({:?})", s);
        if s.len() == 0 {
            return Err(NameError(()));
        }
        let bytes = s.as_bytes();
        if !is_restricted_name_first_char(bytes[0]) {
            return Err(NameError(()));
        }
        for &c in &bytes[1..] {
            trace!("Name iter: {:?}", c);
            if !is_restricted_name_char(c) {
                return Err(NameError(()));
            }

        }
        Ok(Name(UniCase(s.to_owned().into())))
    }
}

#[cfg(feature = "heap_size")]
impl heapsize::HeapSizeOf for Name {
    fn heap_size_of_children(&self) -> usize {
        self.0.heap_size_of_children()
    }
}

macro_rules! name {
    ($($name:ident, $text:expr;)+) => (
    $(
    #[allow(non_upper_case_globals)]
    pub const $name: Name = Name(UniCase(Cow::Borrowed($text)));
    )+
    );
}

name! {
    Star, "*";
    // top level
    Text, "text";
    Image,  "image";
    Audio, "audio";
    Video, "video";
    Application, "application";
    Multipart, "multipart";
    Message, "message";
    Model, "model";


    // common text/*
    Plain, "plain";
    Html, "html";
    Xml, "xml";
    Javascript, "javascript";
    Css, "css";
    EventStream, "event-stream";

    // common application/*
    Json, "json";
    WwwFormUrlEncoded, "x-www-form-urlencoded";
    Msgpack, "msgpack";
    OctetStream, "octet-stream";

    // multipart/*
    FormData, "form-data";

    // common image/*
    Png, "png";
    Gif, "gif";
    Bmp, "bmp";
    Jpeg, "jpeg";

    Charset, "charset";
    Boundary, "boundary";

}

pub type Param = (Name, Value);

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Value(Cow<'static, str>);

/*
enum ValueCase {
    Sensitive(Cow<'static, str>),
    Insensitive(UniCase<Cow<'static, str>>)
}
*/

impl PartialEq<str> for Value {
    fn eq(&self, s: &str) -> bool {
        self.0 == s
    }
}

impl<'a> PartialEq<&'a str> for Value {
    fn eq(&self, s: &&'a str) -> bool {
        self.0 == *s
    }
}

impl PartialEq<Value> for str {
    fn eq(&self, v: &Value) -> bool {
        self == v.0
    }
}

impl<'a> PartialEq<Value> for &'a str {
    fn eq(&self, v: &Value) -> bool {
        *self == v.0
    }
}

impl AsRef<str> for Value {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ValueError(());

impl FromStr for Value {
    type Err = ValueError;
    fn from_str(raw: &str) -> Result<Self, Self::Err> {
        // values must be restrict-name-char or "anything goes"
        let trimmed = raw.trim_matches('"');
        let is_quoted = trimmed.len() != raw.len();

        if is_quoted {
            Ok(Value(Cow::Owned(raw.to_owned())))
        } else {
            Name::from_str(raw)
                .map(|name| Value((name.0).0))
                .map_err(|_| ValueError(()))
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.0)
    }
}

#[allow(non_upper_case_globals)]
pub const Utf8: Value = Value(Cow::Borrowed("utf-8"));

impl fmt::Display for Mime {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(fmt, "{}/{}", self.type_, self.subtype));
        if let Some(ref suffix) = self.suffix {
        try!(write!(fmt, "+{}", suffix));
        }
        fmt_params(&self.params, fmt)
    }
}

impl Mime {
    pub fn get_param<N: PartialEq<Name>>(&self, attr: N) -> Option<&Value> {
        self.params.iter().find(|&&(ref name, _)| attr == *name).map(|&(_, ref value)| value)
    }
}

#[derive(Debug, Clone)]
pub struct MimeError(());

impl From<NameError> for MimeError {
    fn from(_: NameError) -> MimeError {
        MimeError(())
    }
}

impl FromStr for Mime {
    type Err = MimeError;
    fn from_str(raw: &str) -> Result<Mime, MimeError> {
        let slash = match raw.find('/') {
            Some(i) => i,
            None => return Err(MimeError(()))
        };

        let type_ = try!(Name::from_str(&raw[..slash]));
        let raw = &raw[slash + 1..];

        let sc = match raw.find(';') {
            Some(i) => i,
            None => raw.len()
        };

        let (subtype, suffix) = match raw[..sc].rfind('+') {
            Some(plus) => (try!(Name::from_str(&raw[..plus])), Some(try!(Name::from_str(&raw[plus + 1..])))),
            None => (try!(Name::from_str(&raw[..sc])), None)
        };

        // params
        let params = if raw.len() > sc {
            try!(params_from_str(&raw[sc+1..]).map_err(|_| MimeError(())))
        } else {
            Vec::new()
        };

        Ok(Mime {
            type_: type_,
            subtype: subtype,
            suffix: suffix,
            params: params,
        })
    }
}

#[cfg(feature = "serde")]
impl serde::ser::Serialize for Mime {
    fn serialize<S>(&self, serializer: &mut S) -> Result<(), S::Error>
        where S: serde::ser::Serializer
    {
        serializer.serialize_str(&*format!("{}",self))
    }
}

#[cfg(feature = "serde")]
impl serde::de::Deserialize for Mime {
    fn deserialize<D>(deserializer: &mut D) -> Result<Self, D::Error>
        where D: serde::de::Deserializer
    {
        let string: String = try!(serde::Deserialize::deserialize(deserializer));
        let mime: Mime = match FromStr::from_str(&*string) {
            Ok(mime) => mime,
            Err(_) => return Err(serde::de::Error::custom("Invalid serialized mime")),
        };
        Ok(mime)
    }
}

fn params_from_str(mut raw: &str) -> Result<Vec<Param>, ()> {
    let mut params = Vec::new();

    raw = raw.trim_left();

    while raw.len() != 0 {
        let attr = match raw.find('=') {
            Some(eq) => match Name::from_str(&raw[..eq]) {
                Ok(n) => {
                    raw = &raw[eq+1..];
                    n
                },
                Err(_) => return Err(())
            },
            None => return Err(())
        };

        raw = raw.trim_left();

        let val_end = if raw.as_bytes()[0] == b'"' {
            raw = &raw[1..];
            match raw.find('"') {
                Some(q) => q,
                None => return Err(())
            }
        } else {
            raw.find(';').unwrap_or(raw.len())
        };

        match Value::from_str(&raw[..val_end]) {
            Ok(v) => params.push((attr, v)),
            Err(_) => return Err(())
        }

        if raw.len() > val_end {
            raw = &raw[val_end + 1..].trim_left();
        } else {
            break;
        }
    }

    Ok(params)
}

// From [RFC6838](http://tools.ietf.org/html/rfc6838#section-4.2):
//
// > All registered media types MUST be assigned top-level type and
// > subtype names.  The combination of these names serves to uniquely
// > identify the media type, and the subtype name facet (or the absence
// > of one) identifies the registration tree.  Both top-level type and
// > subtype names are case-insensitive.
// >
// > Type and subtype names MUST conform to the following ABNF:
// >
// >     type-name = restricted-name
// >     subtype-name = restricted-name
// >
// >     restricted-name = restricted-name-first *126restricted-name-chars
// >     restricted-name-first  = ALPHA / DIGIT
// >     restricted-name-chars  = ALPHA / DIGIT / "!" / "#" /
// >                              "$" / "&" / "-" / "^" / "_"
// >     restricted-name-chars =/ "." ; Characters before first dot always
// >                                  ; specify a facet name
// >     restricted-name-chars =/ "+" ; Characters after last plus always
// >                                  ; specify a structured syntax suffix
//
fn is_restricted_name_first_char(c: u8) -> bool {
    match c {
        b'a'...b'z' |
        b'A'...b'Z' |
        b'0'...b'9' => true,
        _ => false
    }
}

fn is_restricted_name_char(c: u8) -> bool {
    if is_restricted_name_first_char(c) {
        true
    } else {
        match c {
            b'!' |
            b'#' |
            b'$' |
            b'&' |
            b'-' |
            b'^' |
            b'.' |
            b'+' |
            b'_' => true,
            _ => false
        }
    }
}


#[inline]
fn fmt_params(params: &[Param], fmt: &mut fmt::Formatter) -> fmt::Result {
    for param in params {
        try!(fmt_param(param, fmt));
    }
    Ok(())
}

#[inline]
fn fmt_param(param: &Param, fmt: &mut fmt::Formatter) -> fmt::Result {
    let (ref attr, ref value) = *param;
    write!(fmt, "; {}={}", attr, value)
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    #[cfg(feature = "nightly")]
    use test::Bencher;
    use super::*;

    #[test]
    fn test_mime_ops() {
        let png = Image/Png;
        assert_eq!(png.type_, Image);
        assert_eq!(png.subtype, Png);
        assert_eq!(png.suffix, None);

        let xhtml = Text/Html+Xml;
        assert_eq!(xhtml.type_, Text);
        assert_eq!(xhtml.subtype, Html);
        assert_eq!(xhtml.suffix, Some(Xml));
    }

    #[test]
    fn test_mime_show() {
        let mime = mime!(Text/Plain);
        assert_eq!(mime.to_string(), "text/plain");
        let mime = mime!(Text/Plain; Charset=Utf8);
        assert_eq!(mime.to_string(), "text/plain;charset=utf-8");
    }

    #[test]
    fn test_mime_from_str() {
        assert_eq!(Mime::from_str("text/plain").unwrap(), mime!(Text/Plain));
        assert_eq!(Mime::from_str("text/html+xml").unwrap(), Text/Html+Xml);
        assert_eq!(Mime::from_str("TEXT/PLAIN").unwrap(), mime!(Text/Plain));
        assert_eq!(Mime::from_str("text/plain; charset=utf-8").unwrap(), mime!(Text/Plain; Charset=Utf8));
        assert_eq!(Mime::from_str("text/plain;charset=\"utf-8\"").unwrap(), mime!(Text/Plain; Charset=Utf8));
        assert_eq!(Mime::from_str("text/plain; charset=utf-8; foo=bar").unwrap(),
            mime!(Text/Plain; Charset=Utf8, ("foo")=("bar")));
    }

    #[test]
    fn test_mime_parse_error() {
        assert!(Mime::from_str("text/").is_err());
    }

    #[test]
    fn test_case_sensitive_values() {
        assert_eq!(Mime::from_str("text/plain; foo=ABCDEFG").unwrap(),
                   mime!(Text/Plain; ("foo")=("ABCDEFG")));
        assert_eq!(Mime::from_str("text/plain; charset=UtF-8; foo=ABCDEFG").unwrap(),
                   mime!(Text/Plain; Charset=Utf8, ("foo")=("ABCDEFG")));
    }

    #[test]
    fn test_get_param() {
        let mime = Mime::from_str("text/plain; charset=utf-8; foo=bar").unwrap();
        assert_eq!(mime.get_param(Charset), Some(&Utf8));
        assert_eq!(mime.get_param("charset"), Some(&Utf8));
        assert_eq!(mime.get_param("foo").unwrap(), "bar");
        assert_eq!(mime.get_param("baz"), None);
    }

    #[test]
    fn test_value_as_str() {
        assert_eq!(Utf8.as_ref(), "utf-8");
    }

    #[test]
    fn test_value_eq_str() {
        assert_eq!(Utf8, "utf-8");
        assert_eq!("utf-8", Utf8);
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_serialize_deserialize() {
        use serde_json;

        let mime = Mime::from_str("text/plain; charset=utf-8; foo=bar").unwrap();
        let serialized = serde_json::to_string(&mime).unwrap();
        let deserialized: Mime = serde_json::from_str(&serialized).unwrap();
        assert_eq!(mime, deserialized);
    }

    #[cfg(feature = "nightly")]
    #[bench]
    fn bench_show(b: &mut Bencher) {
        let mime = mime!(Text/Plain; Charset=Utf8, ("foo")=("bar"));
        b.bytes = mime.to_string().as_bytes().len() as u64;
        b.iter(|| mime.to_string())
    }

    #[cfg(feature = "nightly")]
    #[bench]
    fn bench_from_str(b: &mut Bencher) {
        let s = "text/plain; charset=utf-8; foo=bar";
        b.bytes = s.as_bytes().len() as u64;
        b.iter(|| s.parse::<Mime>())
    }
}
