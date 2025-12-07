use crossbeam_channel::{Sender, Receiver};

pub struct ObjectPool<T> {
    sender: Sender<T>,
    receiver: Receiver<T>,
}

impl<T> ObjectPool<T> {
    pub fn new(count: usize, mut builder: impl FnMut() -> T) -> ObjectPool<T> {
        let (sender, receiver) = crossbeam_channel::bounded(count);
        for _ in 0..count {
            sender.send(builder()).unwrap();
        }
        ObjectPool {
            sender,
            receiver,
        }
    }

    pub fn with<O>(&self, f: impl FnOnce(&mut T) -> O) -> O {
        let mut object = self.receiver.recv().unwrap();
        let result = f(&mut object);
        self.sender.send(object).unwrap();
        result
    }
}
