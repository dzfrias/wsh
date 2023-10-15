// Lots of this module's contents are from
// https://doc.rust-lang.org/stable/nightly-rustc/src/rustc_serialize/leb128.rs.html and
// https://docs.rs/wasmparser/latest/src/wasmparser/binary_reader.rs.html#415

use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("io: {0}")]
    Io(#[from] ::std::io::Error),
    #[error("leb128 integer too long")]
    IntTooLong,
    #[error("leb128 integer too large")]
    IntTooLarge,
}

macro_rules! impl_read_unsigned_leb128 {
    ($fn_name:ident, $int_ty:ty) => {
        #[inline]
        pub fn $fn_name<T>(decoder: &mut T) -> ::std::result::Result<$int_ty, $crate::Error>
        where
            T: ::std::io::Read,
        {
            use ::byteorder::ReadBytesExt;

            let byte = decoder.read_u8()?;
            if (byte & 0x80) == 0 {
                return Ok(byte as $int_ty);
            }
            let mut result = (byte & 0x7F) as $int_ty;
            let mut shift = 7;
            loop {
                let byte = decoder.read_u8()?;
                result |= ((byte & 0x7F) as $int_ty) << shift;
                if shift >= (<$int_ty>::BITS - 7) && (byte >> (<$int_ty>::BITS - shift)) != 0 {
                    let err = if byte & 0x80 != 0 {
                        Error::IntTooLong
                    } else {
                        Error::IntTooLarge
                    };
                    return Err(err);
                }
                shift += 7;
                if (byte & 0x80) == 0 {
                    return Ok(result);
                }
            }
        }
    };
}

impl_read_unsigned_leb128!(read_u16_leb128, u16);
impl_read_unsigned_leb128!(read_u32_leb128, u32);
impl_read_unsigned_leb128!(read_u64_leb128, u64);
impl_read_unsigned_leb128!(read_u128_leb128, u128);
impl_read_unsigned_leb128!(read_usize_leb128, usize);

macro_rules! impl_read_signed_leb128 {
    ($fn_name:ident, $int_ty:ty) => {
        #[inline]
        pub fn $fn_name<T>(decoder: &mut T) -> ::std::result::Result<$int_ty, $crate::Error>
        where
            T: ::std::io::Read,
        {
            use ::byteorder::ReadBytesExt;

            let mut result = 0;
            let mut shift = 0;
            let mut byte;

            loop {
                byte = decoder.read_u8()?;
                result |= <$int_ty>::from(byte & 0x7F) << shift;

                if shift >= <$int_ty>::BITS - 7 {
                    let continuation_bit = (byte & 0x80) != 0;
                    let sign_and_unused_bit = (byte << 1) as i8 >> (<$int_ty>::BITS - shift);
                    if continuation_bit || (sign_and_unused_bit != 0 && sign_and_unused_bit != -1) {
                        let err = if continuation_bit {
                            Error::IntTooLong
                        } else {
                            Error::IntTooLarge
                        };
                        return Err(err);
                    }

                    return Ok(result);
                }

                shift += 7;
                if (byte & 0x80) == 0 {
                    break;
                }
            }

            if (shift < <$int_ty>::BITS) && ((byte & 0x40) != 0) {
                // sign extend
                result |= (!0 << shift);
            }

            Ok(result)
        }
    };
}

impl_read_signed_leb128!(read_i16_leb128, i16);
impl_read_signed_leb128!(read_i32_leb128, i32);
impl_read_signed_leb128!(read_i64_leb128, i64);
impl_read_signed_leb128!(read_i128_leb128, i128);
impl_read_signed_leb128!(read_isize_leb128, isize);

macro_rules! impl_write_unsigned_leb128 {
    ($fn_name:ident, $int_ty:ty) => {
        #[inline]
        pub fn $fn_name<W>(out: &mut W, mut value: $int_ty) -> ::std::io::Result<()>
        where
            W: ::std::io::Write,
        {
            loop {
                if value < 0x80 {
                    out.write_all(&[value as u8])?;
                    break;
                } else {
                    out.write_all(&[((value & 0x7f) | 0x80) as u8])?;
                    value >>= 7;
                }
            }

            Ok(())
        }
    };
}

impl_write_unsigned_leb128!(write_u16_leb128, u16);
impl_write_unsigned_leb128!(write_u32_leb128, u32);
impl_write_unsigned_leb128!(write_u64_leb128, u64);
impl_write_unsigned_leb128!(write_u128_leb128, u128);
impl_write_unsigned_leb128!(write_usize_leb128, usize);
