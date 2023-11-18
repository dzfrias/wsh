use std::ops::*;

pub trait IntOp: Sized {
    type FloatType;

    // IUnOp
    fn clz(self) -> Self;
    fn ctz(self) -> Self;
    fn popcnt(self) -> Self;

    // IBinOp
    fn add(self, rhs: Self) -> Self;
    fn sub(self, rhs: Self) -> Self;
    fn mul(self, rhs: Self) -> Self;
    fn divs(self, rhs: Self) -> Option<Self>;
    fn divu(self, rhs: Self) -> Option<Self>;
    fn rems(self, rhs: Self) -> Option<Self>;
    fn remu(self, rhs: Self) -> Option<Self>;
    fn and(self, rhs: Self) -> Self;
    fn or(self, rhs: Self) -> Self;
    fn xor(self, rhs: Self) -> Self;
    fn shl(self, rhs: Self) -> Self;
    fn shrs(self, rhs: Self) -> Self;
    fn shru(self, rhs: Self) -> Self;
    fn rotr(self, rhs: Self) -> Self;
    fn rotl(self, rhs: Self) -> Self;

    // ITestOp
    fn eqz(self) -> bool;

    // IRelOp
    fn eq(self, rhs: Self) -> bool;
    fn ne(self, rhs: Self) -> bool;
    fn lts(self, rhs: Self) -> bool;
    fn ltu(self, rhs: Self) -> bool;
    fn gts(self, rhs: Self) -> bool;
    fn gtu(self, rhs: Self) -> bool;
    fn les(self, rhs: Self) -> bool;
    fn leu(self, rhs: Self) -> bool;
    fn ges(self, rhs: Self) -> bool;
    fn geu(self, rhs: Self) -> bool;

    // ConvertOp
    fn wrap(self) -> u32;
    fn to_u64(self) -> u64;
    fn to_i64(self) -> i64;
    fn convert_f32_u(self) -> f32;
    fn convert_f32_s(self) -> f32;
    fn convert_f64_u(self) -> f64;
    fn convert_f64_s(self) -> f64;
    fn reinterpret(self) -> Self::FloatType;
}

macro_rules! impl_int_op {
    ($T:ty, $S:ty, $F:ty) => {
        impl IntOp for $T {
            type FloatType = $F;

            #[inline]
            fn clz(self) -> $T {
                <$T>::leading_zeros(self) as $T
            }

            #[inline]
            fn ctz(self) -> $T {
                <$T>::trailing_zeros(self) as $T
            }

            #[inline]
            fn popcnt(self) -> $T {
                <$T>::count_ones(self) as $T
            }

            #[inline]
            fn add(self, rhs: $T) -> $T {
                <$T>::wrapping_add(self, rhs)
            }

            #[inline]
            fn sub(self, rhs: $T) -> $T {
                <$T>::wrapping_sub(self, rhs)
            }

            #[inline]
            fn mul(self, rhs: $T) -> $T {
                <$T>::wrapping_mul(self, rhs)
            }

            #[inline]
            fn divs(self, rhs: $T) -> Option<$T> {
                let s_self = self as $S;
                let s_rhs = rhs as $S;
                match <$S>::checked_div(s_self, s_rhs) {
                    Some(c) => Some(c as $T),
                    None => None,
                }
            }

            #[inline]
            fn divu(self, rhs: $T) -> Option<$T> {
                <$T>::checked_div(self, rhs)
            }

            #[inline]
            fn rems(self, rhs: $T) -> Option<$T> {
                if rhs == 0 {
                    return None;
                }
                let s_self = self as $S;
                let s_rhs = rhs as $S;
                Some(<$S>::wrapping_rem(s_self, s_rhs) as $T)
            }

            #[inline]
            fn remu(self, rhs: $T) -> Option<$T> {
                <$T>::checked_rem(self, rhs)
            }

            #[inline]
            fn and(self, rhs: $T) -> $T {
                <$T>::bitand(self, rhs)
            }

            #[inline]
            fn or(self, rhs: $T) -> $T {
                <$T>::bitor(self, rhs)
            }

            #[inline]
            fn xor(self, rhs: $T) -> $T {
                <$T>::bitxor(self, rhs)
            }

            #[inline]
            fn shl(self, rhs: $T) -> $T {
                <$T>::wrapping_shl(self, rhs as u32)
            }

            #[inline]
            fn shrs(self, rhs: $T) -> $T {
                let s_self = self as $S;
                <$S>::wrapping_shr(s_self, rhs as u32) as $T
            }

            #[inline]
            fn shru(self, rhs: $T) -> $T {
                <$T>::wrapping_shr(self, rhs as u32)
            }

            #[inline]
            fn rotr(self, rhs: $T) -> $T {
                <$T>::rotate_right(self, rhs as u32)
            }

            #[inline]
            fn rotl(self, rhs: $T) -> $T {
                <$T>::rotate_left(self, rhs as u32)
            }

            #[inline]
            fn eqz(self) -> bool {
                self == 0
            }

            #[inline]
            fn eq(self, rhs: $T) -> bool {
                self == rhs
            }

            #[inline]
            fn ne(self, rhs: $T) -> bool {
                self != rhs
            }

            #[inline]
            fn lts(self, rhs: $T) -> bool {
                (self as $S) < (rhs as $S)
            }

            #[inline]
            fn ltu(self, rhs: $T) -> bool {
                self < rhs
            }

            #[inline]
            fn gts(self, rhs: $T) -> bool {
                (self as $S) > (rhs as $S)
            }

            #[inline]
            fn gtu(self, rhs: $T) -> bool {
                self > rhs
            }

            #[inline]
            fn les(self, rhs: $T) -> bool {
                (self as $S) <= (rhs as $S)
            }

            #[inline]
            fn leu(self, rhs: $T) -> bool {
                self <= rhs
            }

            #[inline]
            fn ges(self, rhs: $T) -> bool {
                (self as $S) >= (rhs as $S)
            }

            #[inline]
            fn geu(self, rhs: $T) -> bool {
                self >= rhs
            }

            #[inline]
            fn wrap(self) -> u32 {
                self as u32
            }

            #[inline]
            fn to_u64(self) -> u64 {
                self as u64
            }

            #[inline]
            fn to_i64(self) -> i64 {
                (self as $S) as i64
            }

            #[inline]
            fn convert_f32_u(self) -> f32 {
                self as f32
            }

            #[inline]
            fn convert_f32_s(self) -> f32 {
                (self as $S) as f32
            }

            #[inline]
            fn convert_f64_u(self) -> f64 {
                self as f64
            }

            #[inline]
            fn convert_f64_s(self) -> f64 {
                (self as $S) as f64
            }

            #[inline]
            fn reinterpret(self) -> $F {
                <$F>::from_bits(self)
            }
        }
    };
}
impl_int_op!(u32, i32, f32);
impl_int_op!(u64, i64, f64);

pub trait FloatOp {
    type IntType;

    // FUnOp
    fn neg(self) -> Self;
    fn abs(self) -> Self;
    fn ceil(self) -> Self;
    fn floor(self) -> Self;
    /// round-to-nearest ties-to-even
    fn nearest(self) -> Self;
    fn sqrt(self) -> Self;

    // FBinOp
    fn add(self, rhs: Self) -> Self;
    fn sub(self, rhs: Self) -> Self;
    fn mul(self, rhs: Self) -> Self;
    fn div(self, rhs: Self) -> Self;
    fn min(self, rhs: Self) -> Self;
    fn max(self, rhs: Self) -> Self;
    fn copysign(self, rhs: Self) -> Self;

    // FRelOp
    fn eq(self, rhs: Self) -> bool;
    fn ne(self, rhs: Self) -> bool;
    fn lt(self, rhs: Self) -> bool;
    fn gt(self, rhs: Self) -> bool;
    fn le(self, rhs: Self) -> bool;
    fn ge(self, rhs: Self) -> bool;

    // Convert
    fn reinterpret(self) -> Self::IntType;

    // Canonical NaN
    fn is_canonical_nan(&self) -> bool;
}

pub trait ConvertFloat {
    fn trunc_i32(self) -> Option<i32>;
    fn trunc_i32_sat(self) -> i32;
    fn trunc_i64(self) -> Option<i64>;
    fn trunc_i64_sat(self) -> i64;
    fn trunc_u32(self) -> Option<u32>;
    fn trunc_u32_sat(self) -> u32;
    fn trunc_u64(self) -> Option<u64>;
    fn trunc_u64_sat(self) -> u64;
}

// Convert self (a float) into a signed integer of type $ty
macro_rules! impl_convert_float {
    ($f_ty:ty, $ty:ty, $min:literal, $max:literal) => {
        paste::paste! {
            #[inline]
            fn [<trunc_ $ty>](self) -> Option<$ty> {
                if self.is_nan() {
                    return None;
                }

                if self <= $min || self >= $max {
                    return None;
                }
                Some(self.trunc() as $ty)
            }

            #[inline]
            fn [<trunc_ $ty _sat>](self) -> $ty {
                if self.is_nan() {
                    return 0;
                }

                if self < $min {
                    return <$ty>::MIN;
                }
                if self > $max {
                    return <$ty>::MAX;
                }

                self.trunc() as $ty
            }
        }
    };
}

macro_rules! impl_float_op {
    ($T:ty, $I:ty) => {
        impl FloatOp for $T {
            type IntType = $I;

            #[inline]
            fn neg(self) -> $T {
                std::ops::Neg::neg(self)
            }

            #[inline]
            fn abs(self) -> $T {
                <$T>::abs(self)
            }

            #[inline]
            fn ceil(self) -> $T {
                <$T>::ceil(self)
            }

            #[inline]
            fn floor(self) -> $T {
                <$T>::floor(self)
            }

            #[inline]
            fn nearest(self) -> $T {
                // Implementation from
                // https://github.com/WebAssembly/spec/blob/fb7e7e1e381ffc283c923a87fdfea5ebbd213737/interpreter/exec/float.ml#L148

                // preserve the sign of 0
                if self == 0.0 {
                    return self;
                }
                if self.is_nan() {
                    return <$T>::NAN;
                }
                let u = self.ceil();
                let d = self.floor();
                let um = (self - u).abs();
                let dm = (self - d).abs();
                let half_u = u / 2.0;
                if um < dm || um == dm && half_u.floor() == half_u {
                    u
                } else {
                    d
                }
            }

            #[inline]
            fn sqrt(self) -> $T {
                <$T>::sqrt(self)
            }

            #[inline]
            fn add(self, rhs: $T) -> $T {
                self + rhs
            }

            #[inline]
            fn sub(self, rhs: $T) -> $T {
                self - rhs
            }

            #[inline]
            fn mul(self, rhs: $T) -> $T {
                self * rhs
            }

            #[inline]
            fn div(self, rhs: $T) -> $T {
                self / rhs
            }

            #[inline]
            fn min(self, rhs: $T) -> $T {
                // min(-0.0, 0.0) == -0.0
                if self == rhs {
                    (self.to_bits() | rhs.to_bits()).reinterpret()
                } else if self < rhs {
                    self
                } else if self > rhs {
                    rhs
                } else {
                    <$T>::NAN
                }
            }

            #[inline]
            fn max(self, rhs: $T) -> $T {
                // max(-0.0, 0.0) == 0.0
                if self == rhs {
                    (self.to_bits() & rhs.to_bits()).reinterpret()
                } else if self > rhs {
                    self
                } else if self < rhs {
                    rhs
                } else {
                    <$T>::NAN
                }
            }

            #[inline]
            #[deny(unconditional_recursion)]
            fn copysign(self, rhs: $T) -> $T {
                self.copysign(rhs)
            }

            #[inline]
            fn eq(self, rhs: $T) -> bool {
                self == rhs
            }

            #[inline]
            fn ne(self, rhs: $T) -> bool {
                self != rhs
            }

            #[inline]
            fn lt(self, rhs: $T) -> bool {
                self < rhs
            }

            #[inline]
            fn gt(self, rhs: $T) -> bool {
                self > rhs
            }

            #[inline]
            fn le(self, rhs: $T) -> bool {
                self <= rhs
            }

            #[inline]
            fn ge(self, rhs: $T) -> bool {
                self >= rhs
            }

            #[inline]
            fn reinterpret(self) -> $I {
                self.to_bits()
            }

            #[inline]
            fn is_canonical_nan(&self) -> bool {
                self.to_bits() == <$T>::NAN.to_bits() || self.to_bits() == (-<$T>::NAN).to_bits()
            }
        }
    };
}

impl_float_op!(f32, u32);
impl_float_op!(f64, u64);

// The numbers here are somehwat arbitrary (to me). Neither the spec nor Rust seems to line up with
// why these numbers exist, but the spectests were failing and this is what got them to pass...
//
// I got them from https://github.com/paritytech/wasmi/blob/72be93b3598a995efbd47df715a8910403402586/crates/core/src/value.rs#L359
#[allow(clippy::lossy_float_literal, clippy::unreadable_literal)]
impl ConvertFloat for f32 {
    impl_convert_float!(f32, i32, -2147483904.0_f32, 2147483648.0_f32);
    impl_convert_float!(f32, u32, -1.0_f32, 4294967296.0_f32);
    impl_convert_float!(
        f32,
        i64,
        -9223373136366403584.0_f32,
        9223372036854775808.0_f32
    );
    impl_convert_float!(f32, u64, -1.0_f32, 18446744073709551616.0_f32);
}
#[allow(clippy::lossy_float_literal, clippy::unreadable_literal)]
impl ConvertFloat for f64 {
    impl_convert_float!(f64, i32, -2147483649.0_f64, 2147483648.0_f64);
    impl_convert_float!(
        f64,
        i64,
        -9223372036854777856.0_f64,
        9223372036854775808.0_f64
    );
    impl_convert_float!(f64, u32, -1.0_f64, 4294967296.0_f64);
    impl_convert_float!(f64, u64, -1.0_f64, 18446744073709551616.0_f64);
}

pub trait FloatPromote {
    fn promote(self) -> f64;
}

pub trait FloatDemote {
    fn demote(self) -> f32;
}

impl FloatPromote for f32 {
    #[inline]
    fn promote(self) -> f64 {
        self as f64
    }
}

impl FloatDemote for f64 {
    #[inline]
    fn demote(self) -> f32 {
        self as f32
    }
}
