/// A reference to a specific byte in a word
#[derive(Debug, PartialEq, Eq)]
pub enum ByteRef<'a> {
    Low(&'a mut u16),
    High(&'a mut u16),
}

impl ByteRef<'_> {
    /// Gets the byte
    pub fn get(&self) -> u8 {
        match self {
            ByteRef::Low(x) => (**x & 0xff) as u8,
            ByteRef::High(x) => (**x >> 8) as u8,
        }
    }

    /// Sets the byte
    pub fn set(&mut self, value: u8) {
        match self {
            ByteRef::Low(x) => **x = (**x & 0xff00) | (value as u16),
            ByteRef::High(x) => **x = (**x & 0xff) | ((value as u16) << 8),
        }
    }

    /// Swap to the other byte
    pub fn swap(self) -> Self {
        match self {
            ByteRef::Low(x) => ByteRef::High(x),
            ByteRef::High(x) => ByteRef::Low(x),
        }
    }
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum TaggedByte {
    Data(Byte),
    Address(Byte),
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Byte {
    Low(u8),
    High(u8),
}

impl From<Byte> for u8 {
    fn from(x: Byte) -> u8 {
        match x {
            Byte::Low(x) | Byte::High(x) => x,
        }
    }
}
