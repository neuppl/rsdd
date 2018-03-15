/// Implemented by each benchmarkable routine. `setup` will be invoked for each
/// run and will not be timed, and `run` will be timed.
pub trait Timed {
    pub fn setup(&mut self) -> ();
    pub fn run(&self) -> ();
}

pub struct BenchResult {
    /// average time in nanoseconds
    time: f64,
    /// variance
    variance: f64,
}

pub fn benchmark(obj: &mut Timed) -> BenchResult {
    
}
