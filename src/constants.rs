/// primes that are used by elements of rsdd,
/// primarily in integer-approximating finite fields
pub mod primes {
    /// ```
    /// use rsdd::constants::primes;
    /// assert!(primes::U32_TINY < u32::MAX as u128);
    /// ```
    pub const U32_TINY: u128 = 1000001;
    /// ```
    /// use rsdd::constants::primes;
    /// assert!(primes::U32_SMALL < u32::MAX as u128);
    /// ```
    pub const U32_SMALL: u128 = 479001599;
    /// ```
    /// use rsdd::constants::primes;
    /// assert!(primes::U64_LARGEST < u64::MAX as u128);
    /// ```
    pub const U64_LARGEST: u128 = 18_446_744_073_709_551_591;
    pub const U128_LARGE_1: u128 = 46084029846212370199652019757;
    pub const U128_LARGE_2: u128 = 49703069216273825773136967137;
    pub const U128_LARGE_3: u128 = 64733603481794218985640164159;
    pub const U128_LARGE_4: u128 = 79016979402926483817096290621;
}
