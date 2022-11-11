#[macro_use]
extern crate criterion;

use ckb_vm::instructions::{i, m};
use ckb_vm::new_decoder;
use ckb_vm::new_decoder::def::{RVIFactory, RVMFactory};
use ckb_vm_definitions::instructions::instruction_opcode_name;
use criterion::{BenchmarkId, Criterion};

use rand::prelude::*;

fn get_name(bits: u64) -> String {
    return String::from(instruction_opcode_name((bits & 0xffff) as u16));
}

macro_rules! bench_rand_decode {
    ($name:literal, $fn_name: ident, $gen:ident, $hand:ident) => {
        pub fn $fn_name(c: &mut Criterion) {
            let factory = $gen::new();
            let gen_inst = |base: u32, idx: usize| -> u32 { base | factory.get(idx) };
            const ITERATION: usize = 100_000_00;
            let mut group = c.benchmark_group("group");
            group.sample_size(10);

            let seed = rand::thread_rng().gen();
            let mut rng0 = SmallRng::from_seed(seed);
            let mut rng1 = SmallRng::from_seed(seed);

            // group.bench_function(BenchmarkId::new("simple-test", 0), |b| {
            //     b.iter(|| {
            //         for _ in 0..ITERATION {
            //             let idx: usize = rng1.gen();
            //             let base: u32 = rng1.gen();
            //             let bits = gen_inst(base, idx);
            //             let expected = i::factory::<u64>(bits, 0);
            //             if let Some((opcode, inst_code)) = $gen.decode(bits) {
            //                 let actual = new_decoder::$hand::factory::<u64>(opcode, inst_code, bits, 1);
            //                 assert!(
            //                     expected == actual,
            //                     "expected:{:?}, actual:{:?}, {}, bits:{:#0x}",
            //                     expected,
            //                     actual,
            //                     get_name(actual.unwrap()),
            //                     bits
            //                 );
            //             } else {
            //                 assert!(expected == None, "expected:{:?}", expected);
            //             }
            //         }
            //     })
            // });

            group.bench_function(BenchmarkId::new(concat!($name, "-gen-based"), 0), |b| {
                b.iter(|| {
                    for _ in 0..ITERATION {
                        let idx: usize = rng0.gen();
                        let base: u32 = rng0.gen();
                        let bits = gen_inst(base, idx);
                        if let Some((opcode, inst_code)) = factory.decode(bits) {
                            new_decoder::$hand::factory::<u64>(opcode, inst_code, bits, 1);
                        }
                    }
                })
            });

            group.bench_function(BenchmarkId::new(concat!($name, "-hand-based"), 0), |b| {
                b.iter(|| {
                    for _ in 0..ITERATION {
                        let idx: usize = rng1.gen();
                        let base: u32 = rng1.gen();
                        let bits = gen_inst(base, idx);
                        $hand::factory::<u64>(bits, 1);
                    }
                })
            });
            group.finish();
        }
    };
}

macro_rules! bench_groups {
    ($( ($name: literal, $fn_name:ident, $gen:ident, $hand:ident )),*) => {
        $(
            bench_rand_decode!($name, $fn_name, $gen, $hand);
        )*
        criterion_group!(
            benches,
            $( $fn_name),*
        );
    }
}

bench_groups!(
    ("rvi", bench_rvi, RVIFactory, i),
    ("rvm", bench_rvm, RVMFactory, m)
);

criterion_main!(benches);
