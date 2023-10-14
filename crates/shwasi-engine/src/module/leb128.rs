#![allow(dead_code)]

// Lots of this module's contents are from
// https://doc.rust-lang.org/stable/nightly-rustc/src/rustc_serialize/leb128.rs.html

macro_rules! impl_read_unsigned_leb128 {
    ($fn_name:ident, $int_ty:ty) => {
        #[inline]
        pub fn $fn_name<T>(decoder: &mut T) -> Option<$int_ty>
        where
            T: ::std::io::Read,
        {
            use ::byteorder::ReadBytesExt;

            // The first iteration of this loop is unpeeled. This is a
            // performance win because this code is hot and integer values less
            // than 128 are very common, typically occurring 50-80% or more of
            // the time, even for u64 and u128.
            let byte = decoder.read_u8().ok()?;
            if (byte & 0x80) == 0 {
                return Some(byte as $int_ty);
            }
            let mut result = (byte & 0x7F) as $int_ty;
            let mut shift = 7;
            loop {
                let byte = decoder.read_u8().ok()?;
                if (byte & 0x80) == 0 {
                    result |= (byte as $int_ty).checked_shl(shift)?;
                    return Some(result);
                } else {
                    result |= ((byte & 0x7F) as $int_ty).checked_shl(shift)?;
                }
                shift += 7;
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
        pub fn $fn_name<T>(decoder: &mut T) -> Option<$int_ty>
        where
            T: ::std::io::Read,
        {
            use ::byteorder::ReadBytesExt;

            let mut result = 0;
            let mut shift = 0;
            let mut byte;

            loop {
                byte = decoder.read_u8().ok()?;
                result |= <$int_ty>::from(byte & 0x7F).checked_shl(shift)?;
                shift += 7;

                if (byte & 0x80) == 0 {
                    break;
                }
            }

            if (shift < <$int_ty>::BITS) && ((byte & 0x40) != 0) {
                const X: $int_ty = 0;
                // sign extend
                result |= (!X.checked_shl(shift)?);
            }

            Some(result)
        }
    };
}

impl_read_signed_leb128!(read_i16_leb128, i16);
impl_read_signed_leb128!(read_i32_leb128, i32);
impl_read_signed_leb128!(read_i64_leb128, i64);
impl_read_signed_leb128!(read_i128_leb128, i128);
impl_read_signed_leb128!(read_isize_leb128, isize);
