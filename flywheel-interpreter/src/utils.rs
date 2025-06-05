

pub unsafe fn copy_silly<T: Copy>(dest: *mut T, src: *const T, mut count: usize) {
    if count <= 4 {
        unsafe {
            if count != 0 {
                dest.add(0).write(src.add(0).read());
                count -= 1;
            }
            if count != 0 {
                dest.add(1).write(src.add(1).read());
                count -= 1;
            }
            if count != 0 {
                dest.add(2).write(src.add(2).read());
                count -= 1;
            }
            if count != 0 {
                dest.add(3).write(src.add(3).read());
            }
        }
    } else {
        for i in 0..count {
            unsafe { dest.add(i).write(src.add(i).read()); }
        }
    }
}