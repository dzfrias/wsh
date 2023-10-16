// Lots of this module's contents are from
// https://doc.rust-lang.org/stable/nightly-rustc/src/rustc_serialize/leb128.rs.html and
// https://docs.rs/wasmparser/latest/src/wasmparser/binary_reader.rs.html#415

use std::io;

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

impl_read_signed_leb128!(read_s16_leb128, i16);
impl_read_signed_leb128!(read_s32_leb128, i32);
impl_read_signed_leb128!(read_s64_leb128, i64);
impl_read_signed_leb128!(read_s128_leb128, i128);

macro_rules! impl_write_unsigned_leb128 {
    ($fn_name:ident, $int_ty:ty) => {
        #[inline]
        pub fn $fn_name<W>(out: &mut W, mut value: $int_ty) -> ::std::io::Result<usize>
        where
            W: ::std::io::Write,
        {
            let mut written = 0;
            loop {
                written += 1;
                if value < 0x80 {
                    out.write_all(&[value as u8])?;
                    break;
                } else {
                    out.write_all(&[((value & 0x7f) | 0x80) as u8])?;
                    value >>= 7;
                }
            }

            Ok(written)
        }
    };
}

impl_write_unsigned_leb128!(write_u16_leb128, u16);
impl_write_unsigned_leb128!(write_u32_leb128, u32);
impl_write_unsigned_leb128!(write_u64_leb128, u64);
impl_write_unsigned_leb128!(write_u128_leb128, u128);

macro_rules! impl_write_signed_leb128 {
    ($fn_name:ident, $int_ty:ty) => {
        #[inline]
        pub fn $fn_name<W>(out: &mut W, mut value: $int_ty) -> ::std::io::Result<usize>
        where
            W: ::std::io::Write,
        {
            let mut written = 0;
            loop {
                let mut byte = (value as u8) & 0x7f;
                value >>= 7;
                let more = !(((value == 0) && ((byte & 0x40) == 0))
                    || ((value == -1) && ((byte & 0x40) != 0)));

                if more {
                    byte |= 0x80; // Mark this byte to show that more bytes will follow.
                }

                out.write_all(&[byte])?;
                written += 1;

                if !more {
                    break;
                }
            }

            Ok(written)
        }
    };
}

impl_write_signed_leb128!(write_s16_leb128, i16);
impl_write_signed_leb128!(write_s32_leb128, i32);
impl_write_signed_leb128!(write_s64_leb128, i64);
impl_write_signed_leb128!(write_s128_leb128, i128);

pub fn read_s33_leb128<T>(decoder: &mut T) -> std::result::Result<i64, Error>
where
    T: io::Read,
{
    use byteorder::ReadBytesExt;

    let byte = decoder.read_u8()?;
    if (byte & 0x80) == 0 {
        return Ok(((byte as i8) << 1) as i64 >> 1);
    }

    let mut result = (byte & 0x7F) as i64;
    let mut shift = 7;
    loop {
        let byte = decoder.read_u8()?;
        result |= ((byte & 0x7F) as i64) << shift;
        if shift >= 25 {
            let continuation_bit = (byte & 0x80) != 0;
            let sign_and_unused_bit = (byte << 1) as i8 >> (33 - shift);
            if continuation_bit || (sign_and_unused_bit != 0 && sign_and_unused_bit != -1) {
                return Err(Error::IntTooLong);
            }
            return Ok(result);
        }
        shift += 7;
        if (byte & 0x80) == 0 {
            break;
        }
    }
    let ashift = 64 - shift;
    Ok((result << ashift) >> ashift)
}

#[inline]
pub fn write_s33_leb128<W>(out: &mut W, value: i64) -> io::Result<usize>
where
    W: io::Write,
{
    write_s64_leb128(out, value)
}

#[inline]
pub fn write_f32_leb128<W>(out: &mut W, value: f32) -> io::Result<usize>
where
    W: io::Write,
{
    let bits = value.to_bits();
    out.write_all(&bits.to_le_bytes())?;
    Ok(4)
}

#[inline]
pub fn write_f64_leb128<W>(out: &mut W, value: f64) -> io::Result<usize>
where
    W: io::Write,
{
    let bits = value.to_bits();
    out.write_all(&bits.to_le_bytes())?;
    Ok(8)
}
