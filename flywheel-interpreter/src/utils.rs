use bytemuck::Zeroable;

pub unsafe fn set_zero_silly<T: Zeroable>(dest: *mut T, count: usize) {
    if count <= 4 {
        unsafe {
            if count > 0 {
                dest.add(0).write(T::zeroed());
                if count > 1 {
                    dest.add(1).write(T::zeroed());
                    if count > 2 {
                        dest.add(2).write(T::zeroed());
                        if count > 3 {
                            dest.add(3).write(T::zeroed());
                        }
                    }
                }
            }
        }
    } else {
        for i in 0..count {
            unsafe { dest.add(i).write(T::zeroed()); }
        }
    }
}

pub unsafe fn copy_silly<T: Copy>(dest: *mut T, src: *const T, mut count: usize) {
    if count <= 4 {
        unsafe {
            if count > 0 {
                dest.add(0).write(src.add(0).read());
                if count > 1 {
                    dest.add(1).write(src.add(1).read());
                    if count > 2 {
                        dest.add(2).write(src.add(2).read());
                        if count > 3 {
                            dest.add(3).write(src.add(3).read());
                        }
                    }
                }
            }
        }
    } else {
        for i in 0..count {
            unsafe { dest.add(i).write(src.add(i).read()); }
        }
    }
}