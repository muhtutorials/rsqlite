use anyhow::anyhow;

pub trait Payload<E> {
    fn size(&self) -> PayloadSize;
    fn load(&self, offset: usize, buf: &mut [u8]) -> Result<usize, E>;
}

impl<P: CopiablePayload> Payload<()> for P {
    fn size(&self) -> PayloadSize {
        self.size()
    }

    fn load(&self, offset: usize, buf: &mut [u8]) -> Result<usize, ()> {
        Ok(self.copy(offset, buf))
    }
}

pub trait CopiablePayload {
    fn size(&self) -> PayloadSize;
    fn copy(&self, offset: usize, buf: &mut [u8]) -> usize;
}

pub trait LocalPayload<E>: Payload<E> {
    fn buf(&self) -> &[u8];
}

pub struct SlicePayload<'a> {
    buf: &'a [u8],
    size: PayloadSize,
}

impl<'a> SlicePayload<'a> {
    #[allow(dead_code)]
    pub fn new(buf: &'a [u8]) -> anyhow::Result<Self> {
        let size = PayloadSize::try_from(buf.len() as u64)
            .map_err(|_| anyhow!("payload size too large"))?;
        Ok(Self { buf, size })
    }
}

impl CopiablePayload for SlicePayload<'_> {
    fn size(&self) -> PayloadSize {
        self.size
    }

    fn copy(&self, offset: usize, buf: &mut [u8]) -> usize {
        assert!(offset <= self.buf.len());
        let n = buf.len().min(self.buf.len() - offset);
        buf[..n].copy_from_slice(&self.buf[offset..offset + n]);
        n
    }
}

impl LocalPayload<()> for SlicePayload<'_> {
    fn buf(&self) -> &[u8] {
        self.buf
    }
}

/// This is to guarantee that payload size <= 2147483647 (i32::MAX).
#[derive(Clone, Copy, Debug)]
pub struct PayloadSize(u32);

impl PayloadSize {
    #[inline]
    pub fn get(&self) -> u32 {
        self.0
    }
}

impl TryFrom<u64> for PayloadSize {
    type Error = ();

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        // The maximum payload length is 2147483647 (= i32::MAX).
        if value <= i32::MAX as u64 {
            Ok(Self(value as u32))
        } else {
            Err(())
        }
    }
}
