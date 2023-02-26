// some utility functions for looking at the bytes inside of a value. Useful for
// debugging alignment bugs and such.

trait Byteable {
    fn next(&mut self) -> Option<u8>;
}

struct BytesIter<'a, T> {
    index: usize,
    data: &'a T,
}

impl<'a, T> BytesIter<'a, T> {
    fn new(data: &'a T) -> Self { Self { index: 0, data } }
}

impl<'a, T> Byteable for BytesIter<'a, T> {
    fn next(&mut self) -> Option<u8> {
        let ptr = self.data as *const T as *const u8;
        if self.index >= std::mem::size_of::<T>() {
            None
        } else {
            print!("{} ", self.index);
            let byte = Some(unsafe { *ptr.add(self.index) });
            self.index += 1;
            byte
        }
    }
}

pub struct BytePrinter<'a, T> {
    objects: Vec<BytesIter<'a, T>>,
}

impl<'a, T> BytePrinter<'a, T>
where
    T: std::fmt::Debug,
{
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
        }
    }

    pub fn add(&mut self, data: &'a T) {
        let iter = BytesIter::new(data);
        self.objects.push(iter);
    }

    pub fn print_binary(mut self) {
        self.print_with(|byte| {
            print!("{:08b} ", byte);
        });
    }

    pub fn print_decimal(mut self) {
        self.print_with(|byte| {
            print!("{:8} ", byte);
        });
    }

    pub fn print_with(mut self, printer: fn(u8)) {
        loop {
            let mut all_is_false = true;

            for obj in &mut self.objects {
                if let Some(byte) = obj.next() {
                    printer(byte);
                    all_is_false = false;
                }
            }

            println!();

            if all_is_false {
                break;
            }
        }
    }
}

pub fn print_binary<T>(data: &T)
where
    T: std::fmt::Debug,
{
    let mut printer = BytePrinter::new();
    printer.add(data);
    printer.print_binary();
}

pub fn print_binary_two<T>(data: &T, data2: &T)
where
    T: std::fmt::Debug,
{
    let mut printer = BytePrinter::new();
    printer.add(data);
    printer.add(data2);
    printer.print_binary();
}
