use byteutils;
use columnvalueops::ColumnValueOps;
use std::borrow::Cow;
use std::fmt;
use types::DbType;
use types::F64NoNaN;

#[derive(Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Variant {
    Null,
    Bytes(Vec<u8>),
    StringLiteral(String),
    SignedInteger(i64),
    UnsignedInteger(u64),
    Float(F64NoNaN),
}

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Variant::Null => write!(f, "NULL"),
            &Variant::Bytes(ref v) => write!(f, "{:?}", v),
            &Variant::StringLiteral(ref s) => write!(f, "{}", s),
            &Variant::SignedInteger(n) => write!(f, "{}", n),
            &Variant::UnsignedInteger(n) => write!(f, "{}", n),
            &Variant::Float(n) => write!(f, "{}", *n),
        }
    }
}

impl fmt::Debug for Variant {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self)
    }
}

fn from_bool(value: bool) -> Variant {
    Variant::UnsignedInteger(if value { 1 } else { 0 })
}

impl ColumnValueOps for Variant {
    fn from_string_literal(s: Cow<str>) -> Result<Variant, Cow<str>> {
        Ok(Variant::StringLiteral(s.into_owned()))
    }

    fn from_number_literal(string: Cow<str>) -> Result<Variant, Cow<str>> {
        if let Ok(number) = string.parse() {
            Ok(Variant::SignedInteger(number))
        } else if let Ok(number) = string.parse() {
            Ok(Variant::UnsignedInteger(number))
        } else if let Ok(number) = string.parse() {
            Ok(Variant::Float(F64NoNaN::new(number)?))
        } else {
            Err(string)
        }
    }

    fn from_f64(value: f64) -> Variant {
        Variant::Float(F64NoNaN::new(value).unwrap())
    }

    fn to_f64(&self) -> Result<f64, String> {
        match self.cast(DbType::F64)? {
            Variant::Float(float) => Ok(*float),
            var => Err(format!("Expected Float, actually {}", var)),
        }
    }

    fn from_u64(value: u64) -> Variant {
        Variant::UnsignedInteger(value)
    }

    fn to_u64(&self) -> Result<u64, String> {
        let num = self.cast(DbType::Integer {
            signed: false,
            bytes: 8,
        })?;

        match num {
            Variant::UnsignedInteger(int) => Ok(int),
            var => Err(format!("Expected UnsignedInteger, actually {}", var)),
        }
    }

    fn from_bytes(dbtype: DbType, bytes: Cow<[u8]>) -> Result<Variant, String> {
        match dbtype {
            DbType::Null => Ok(Variant::Null),
            DbType::ByteDynamic => Ok(Variant::Bytes(bytes.into_owned())),
            DbType::ByteFixed(n) => {
                if bytes.len() as u64 != n {
                    Err(format!("{:?} cannot be converted to {:?}", bytes, dbtype))
                } else {
                    Ok(Variant::Bytes(bytes.into_owned()))
                }
            },
            DbType::Integer { signed, bytes: n } => {
                if bytes.len() != n as usize {
                    Err(format!("{:?} cannot be converted to {:?}", bytes, dbtype))
                } else {
                    if signed {
                        Ok(Variant::SignedInteger(byteutils::read_sdbinteger(&bytes)))
                    } else {
                        Ok(Variant::UnsignedInteger(byteutils::read_udbinteger(&bytes)))
                    }
                }
            },
            DbType::F64 => {
                let f = byteutils::read_dbfloat(&bytes);
                Ok(Variant::Float(F64NoNaN::new(f)?))
            },
            DbType::String => {
                let len = bytes.len();
                if len > 0 && bytes[len - 1] == 0 {
                    let s = String::from_utf8_lossy(&bytes[0..len - 1]);
                    Ok(Variant::StringLiteral(s.into_owned()))
                } else {
                    Err(format!("{:?} cannot be converted to {:?}", bytes, dbtype))
                }
            },
        }
    }

    fn to_bytes(&self, dbtype: DbType) -> Result<Box<[u8]>, String> {
        let bytes = self.cast(dbtype)?;

        match (bytes, dbtype) {
            (Variant::Null, DbType::Null) => {
                // NULL has no data.
                Err(format!("{:?} cannot be converted to bytes", dbtype))
            },
            (Variant::Bytes(v), DbType::ByteDynamic) => Ok(v.into_boxed_slice()),
            (Variant::StringLiteral(s), DbType::String) => {
                Ok((s + "\0").into_bytes().into_boxed_slice())
            },
            (
                Variant::SignedInteger(v),
                DbType::Integer {
                    signed: true,
                    bytes,
                },
            ) => {
                let mut buf = vec![0; bytes as usize];
                byteutils::write_sdbinteger(v, &mut buf);
                Ok(buf.into_boxed_slice())
            },
            (
                Variant::UnsignedInteger(v),
                DbType::Integer {
                    signed: false,
                    bytes,
                },
            ) => {
                let mut buf = vec![0; bytes as usize];
                byteutils::write_udbinteger(v, &mut buf);
                Ok(buf.into_boxed_slice())
            },
            (Variant::Float(v), DbType::F64) => {
                let mut buf = [0; 8];
                byteutils::write_dbfloat(*v, &mut buf);
                Ok(Box::new(buf))
            },
            _ => Err(format!("{:?} cannot be converted to bytes", dbtype)),
        }
    }

    fn get_dbtype(&self) -> DbType {
        match self {
            &Variant::Null => DbType::Null,
            &Variant::Bytes(ref bytes) => DbType::ByteFixed(bytes.len() as u64),
            &Variant::StringLiteral(..) => DbType::String,
            &Variant::SignedInteger(..) => DbType::Integer {
                signed: true,
                bytes: 8,
            },
            &Variant::UnsignedInteger(..) => DbType::Integer {
                signed: false,
                bytes: 8,
            },
            &Variant::Float(..) => DbType::F64,
        }
    }

    fn to_3vl(&self) -> i8 {
        fn b(value: bool) -> i8 {
            if value {
                1
            } else {
                -1
            }
        }

        match self {
            &Variant::Null => 0,
            &Variant::Bytes(ref bytes) => b(!bytes.is_empty()),
            &Variant::StringLiteral(ref s) => b(!s.is_empty()),
            &Variant::SignedInteger(n) => b(n != 0),
            &Variant::UnsignedInteger(n) => b(n != 0),
            &Variant::Float(n) => b(*n != 0.0),
        }
    }

    fn from_3vl(value: i8) -> Self {
        match value {
            -1 => from_bool(false),
            0 => Variant::Null,
            1 => from_bool(true),
            _ => panic!(),
        }
    }

    // None: self or rhs is NULL, or comparison is otherwise invalid
    // -1: self < rhs
    // 0: self == rhs
    // 1: self > rhs
    fn compare(&self, rhs: &Self) -> Result<Option<i8>, String> {
        let dbtype = self.get_dbtype();
        let var = rhs.clone().cast(dbtype)?;

        let result = match (self, &var) {
            (&Variant::Null, _) | (_, &Variant::Null) => None,
            (&Variant::UnsignedInteger(l), &Variant::UnsignedInteger(r)) => Some(if l < r {
                -1
            } else if l > r {
                1
            } else {
                0
            }),
            (&Variant::SignedInteger(l), &Variant::SignedInteger(r)) => Some(if l < r {
                -1
            } else if l > r {
                1
            } else {
                0
            }),
            (&Variant::Float(l), &Variant::Float(r)) => Some(if l < r {
                -1
            } else if l > r {
                1
            } else {
                0
            }),
            (&Variant::Bytes(ref l), &Variant::Bytes(ref r)) => Some(if l < r {
                -1
            } else if l > r {
                1
            } else {
                0
            }),
            (&Variant::StringLiteral(ref l), &Variant::StringLiteral(ref r)) => Some(if l < r {
                -1
            } else if l > r {
                1
            } else {
                0
            }),
            _ => unreachable!(),
        };

        Ok(result)
    }

    fn cast(&self, dbtype: DbType) -> Result<Self, String> {
        match (self, dbtype) {
            (e @ Variant::Null, DbType::Null) |
            (e @ Variant::Bytes(_), DbType::ByteDynamic) |
            (e @ Variant::StringLiteral(_), DbType::String) |
            (e @ Variant::SignedInteger(_), DbType::Integer { signed: true, .. }) |
            (e @ Variant::UnsignedInteger(_), DbType::Integer { signed: false, .. }) |
            (e @ Variant::Float(_), DbType::F64) => Ok(e.clone()),
            (e, DbType::String) => {
                // every variant can be converted to a string
                Ok(Variant::StringLiteral(e.to_string()))
            },
            (e, DbType::ByteDynamic) => {
                // every variant can be converted to their byte representation
                e.to_bytes(e.get_dbtype())
                    .map(|b| Variant::Bytes(b.into_vec()))
            },
            (Variant::Bytes(bytes), v) => {
                // every variant can be converted from their byte representation
                let r: &[u8] = &bytes;
                ColumnValueOps::from_bytes(v, r.into())
            },
            (Variant::Float(float), DbType::Integer { signed, .. }) => {
                // truncates
                if signed {
                    Ok(Variant::SignedInteger(**float as i64))
                } else {
                    Ok(Variant::UnsignedInteger(**float as u64))
                }
            },
            // TODO: overflow checks!
            (Variant::UnsignedInteger(integer), DbType::F64) => {
                Ok(Variant::Float(F64NoNaN::new(*integer as f64)?))
            },
            (Variant::SignedInteger(integer), DbType::F64) => {
                Ok(Variant::Float(F64NoNaN::new(*integer as f64)?))
            },
            (Variant::UnsignedInteger(integer), DbType::Integer { signed: true, .. }) => {
                Ok(Variant::SignedInteger(*integer as i64))
            },
            (Variant::SignedInteger(integer), DbType::Integer { signed: false, .. }) => {
                Ok(Variant::UnsignedInteger(*integer as u64))
            },
            (v, t) => Err(format!("{:?} cannot be cast to {:?}", v, t)),
        }
    }

    fn concat(&self, rhs: &Self) -> Result<Self, String> {
        Ok(match (self, rhs) {
            (&Variant::StringLiteral(ref l), &Variant::StringLiteral(ref r)) => {
                Variant::StringLiteral(format!("{}{}", l, r))
            },
            (e @ &Variant::StringLiteral(_), rhs) => {
                let it = rhs.clone().cast(DbType::String)?;
                e.concat(&it)?
            },
            (e, _) => e.clone(),
        })
    }

    fn add(&self, rhs: &Self) -> Result<Self, String> {
        // TODO: treat overflow!
        let dbtype = self.get_dbtype();

        Ok(match (self, rhs.clone().cast(dbtype)?) {
            (&Variant::UnsignedInteger(l), Variant::UnsignedInteger(r)) => {
                Variant::UnsignedInteger(l + r)
            },
            (&Variant::SignedInteger(l), Variant::SignedInteger(r)) => {
                Variant::SignedInteger(l + r)
            },
            (&Variant::Float(l), Variant::Float(r)) => Variant::Float(F64NoNaN::new(*l + *r)?),
            _ => self.clone(),
        })
    }

    fn sub(&self, rhs: &Self) -> Result<Self, String> {
        // TODO: treat overflow!
        let dbtype = self.get_dbtype();
        Ok(match (self, rhs.clone().cast(dbtype)?) {
            (&Variant::UnsignedInteger(l), Variant::UnsignedInteger(r)) => {
                Variant::UnsignedInteger(l - r)
            },
            (&Variant::SignedInteger(l), Variant::SignedInteger(r)) => {
                Variant::SignedInteger(l - r)
            },
            (&Variant::Float(l), Variant::Float(r)) => Variant::Float(F64NoNaN::new(*l - *r)?),
            _ => self.clone(),
        })
    }

    fn mul(&self, rhs: &Self) -> Result<Self, String> {
        // TODO: treat overflow!
        let dbtype = self.get_dbtype();
        Ok(match (self, rhs.clone().cast(dbtype)?) {
            (&Variant::UnsignedInteger(l), Variant::UnsignedInteger(r)) => {
                Variant::UnsignedInteger(l * r)
            },
            (&Variant::SignedInteger(l), Variant::SignedInteger(r)) => {
                Variant::SignedInteger(l * r)
            },
            (&Variant::Float(l), Variant::Float(r)) => Variant::Float(F64NoNaN::new(*l * *r)?),
            _ => self.clone(),
        })
    }

    fn div(&self, rhs: &Self) -> Result<Self, String> {
        // TODO: treat overflow!
        let dbtype = self.get_dbtype();
        Ok(match (self, rhs.clone().cast(dbtype)?) {
            (&Variant::UnsignedInteger(_), Variant::UnsignedInteger(0)) => Variant::Null,
            (&Variant::SignedInteger(_), Variant::SignedInteger(0)) => Variant::Null,

            (&Variant::UnsignedInteger(l), Variant::UnsignedInteger(r)) => {
                Variant::UnsignedInteger(l / r)
            },
            (&Variant::SignedInteger(l), Variant::SignedInteger(r)) => {
                Variant::SignedInteger(l / r)
            },
            (&Variant::Float(l), Variant::Float(r)) => {
                if r == F64NoNaN::new(0.0)? {
                    Variant::Null
                } else {
                    Variant::Float(F64NoNaN::new(*l / *r)?)
                }
            },
            _ => self.clone(),
        })
    }

    fn negate(&self) -> Self {
        // TODO: treat overflow!
        match self {
            &Variant::SignedInteger(n) => Variant::SignedInteger(-n),
            &Variant::UnsignedInteger(n) => Variant::SignedInteger(-(n as i64)),
            &Variant::Float(n) => Variant::Float(F64NoNaN::new(-*n).unwrap()),
            &Variant::Null | &Variant::Bytes(..) | &Variant::StringLiteral(..) => self.clone(),
        }
    }
}
