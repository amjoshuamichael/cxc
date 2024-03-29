#![allow(dead_code)]

const CHUNK_SIZE: usize = 8;
const EAT_AT_ONCE: usize = 4;

fn hash<T>(t: *const T) -> usize {
    let size = std::mem::size_of::<T>();

    let mut s1: usize = 0x1FA4C0CC;

    for i in 0..(size / CHUNK_SIZE / EAT_AT_ONCE) {
        let chunk: usize = unsafe { std::ptr::read(t.add(i).cast()) };
        s1 ^= chunk;
    }

    s1
}
