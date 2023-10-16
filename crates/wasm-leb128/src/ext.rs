use std::io;

mod private_write {
    use std::io;

    pub trait Sealed {}
    impl<W> Sealed for W where W: io::Write + ?Sized {}
}

use crate::Error;

pub trait Leb128WriteExt: private_write::Sealed {
    fn write_u16_leb128(&mut self, value: u16) -> io::Result<usize>;
    fn write_u32_leb128(&mut self, value: u32) -> io::Result<usize>;
    fn write_u64_leb128(&mut self, value: u64) -> io::Result<usize>;
    fn write_u128_leb128(&mut self, value: u128) -> io::Result<usize>;

    fn write_s16_leb128(&mut self, value: i16) -> io::Result<usize>;
    fn write_s32_leb128(&mut self, value: i32) -> io::Result<usize>;
    fn write_s33_leb128(&mut self, value: i64) -> io::Result<usize>;
    fn write_s64_leb128(&mut self, value: i64) -> io::Result<usize>;
    fn write_s128_leb128(&mut self, value: i128) -> io::Result<usize>;

    fn write_f32_leb128(&mut self, value: f32) -> io::Result<usize>;
    fn write_f64_leb128(&mut self, value: f64) -> io::Result<usize>;
}

macro_rules! impl_leb_write_ext {
    ($name:ident, $arg:ty) => {
        fn $name(&mut self, value: $arg) -> io::Result<usize> {
            $crate::$name(self, value)
        }
    };
}

impl<W> Leb128WriteExt for W
where
    W: io::Write + ?Sized,
{
    impl_leb_write_ext!(write_u16_leb128, u16);
    impl_leb_write_ext!(write_u32_leb128, u32);
    impl_leb_write_ext!(write_u64_leb128, u64);
    impl_leb_write_ext!(write_u128_leb128, u128);

    impl_leb_write_ext!(write_s16_leb128, i16);
    impl_leb_write_ext!(write_s32_leb128, i32);
    impl_leb_write_ext!(write_s33_leb128, i64);
    impl_leb_write_ext!(write_s64_leb128, i64);
    impl_leb_write_ext!(write_s128_leb128, i128);

    impl_leb_write_ext!(write_f32_leb128, f32);
    impl_leb_write_ext!(write_f64_leb128, f64);
}

mod private_read {
    use std::io;

    pub trait Sealed {}
    impl<R> Sealed for R where R: io::Read + ?Sized {}
}

pub trait Leb128ReadExt: private_read::Sealed {
    fn read_u16_leb128(&mut self) -> Result<u16, Error>;
    fn read_u32_leb128(&mut self) -> Result<u32, Error>;
    fn read_u64_leb128(&mut self) -> Result<u64, Error>;
    fn read_u128_leb128(&mut self) -> Result<u128, Error>;

    fn read_s16_leb128(&mut self) -> Result<i16, Error>;
    fn read_s32_leb128(&mut self) -> Result<i32, Error>;
    fn read_s33_leb128(&mut self) -> Result<i64, Error>;
    fn read_s64_leb128(&mut self) -> Result<i64, Error>;
    fn read_s128_leb128(&mut self) -> Result<i128, Error>;
}

macro_rules! impl_leb_read_ext {
    ($name:ident, $result:ty) => {
        fn $name(&mut self) -> ::std::result::Result<$result, $crate::Error> {
            $crate::$name(self)
        }
    };
}

impl<R> Leb128ReadExt for R
where
    R: io::Read + ?Sized,
{
    impl_leb_read_ext!(read_u16_leb128, u16);
    impl_leb_read_ext!(read_u32_leb128, u32);
    impl_leb_read_ext!(read_u64_leb128, u64);
    impl_leb_read_ext!(read_u128_leb128, u128);

    impl_leb_read_ext!(read_s16_leb128, i16);
    impl_leb_read_ext!(read_s32_leb128, i32);
    impl_leb_read_ext!(read_s33_leb128, i64);
    impl_leb_read_ext!(read_s64_leb128, i64);
    impl_leb_read_ext!(read_s128_leb128, i128);
}
