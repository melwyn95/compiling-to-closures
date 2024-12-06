# Benchmarks

## Haskell

The Haskell code is benchmarked using the criterion library, to run the benchmarks: 

```shell
$ stack bench
```

The result:

```shell
haskell> benchmarks
Running 1 benchmarks...
Benchmark scheme: RUNNING...
benchmarking fib/impl-lang
time                 30.59 μs   (30.05 μs .. 31.03 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 30.02 μs   (29.70 μs .. 30.34 μs)
std dev              1.091 μs   (900.2 ns .. 1.322 μs)
variance introduced by outliers: 40% (moderately inflated)
                     
benchmarking fib/interpreter
time                 23.28 ms   (20.43 ms .. 25.98 ms)
                     0.945 R²   (0.901 R² .. 0.986 R²)
mean                 23.94 ms   (22.75 ms .. 25.22 ms)
std dev              2.800 ms   (2.316 ms .. 3.613 ms)
variance introduced by outliers: 53% (severely inflated)
                     
benchmarking fib/compiler
time                 20.60 ms   (19.93 ms .. 21.30 ms)
                     0.993 R²   (0.984 R² .. 0.997 R²)
mean                 21.18 ms   (20.74 ms .. 21.85 ms)
std dev              1.268 ms   (868.8 μs .. 1.947 ms)
variance introduced by outliers: 23% (moderately inflated)
                     
benchmarking tak/impl-lang
time                 104.3 μs   (99.19 μs .. 113.2 μs)
                     0.977 R²   (0.957 R² .. 0.998 R²)
mean                 103.5 μs   (101.0 μs .. 108.1 μs)
std dev              11.10 μs   (5.983 μs .. 16.85 μs)
variance introduced by outliers: 84% (severely inflated)
                     
benchmarking tak/interpreter
time                 146.6 ms   (140.2 ms .. 154.0 ms)
                     0.998 R²   (0.991 R² .. 1.000 R²)
mean                 148.3 ms   (146.4 ms .. 150.5 ms)
std dev              2.881 ms   (1.980 ms .. 4.215 ms)
variance introduced by outliers: 12% (moderately inflated)
                     
benchmarking tak/compiler
time                 147.7 ms   (143.8 ms .. 150.4 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 149.1 ms   (147.9 ms .. 150.9 ms)
std dev              2.091 ms   (1.205 ms .. 3.247 ms)
variance introduced by outliers: 12% (moderately inflated)
                     
benchmarking sort/impl-lang
time                 352.8 ns   (329.2 ns .. 374.9 ns)
                     0.977 R²   (0.968 R² .. 0.987 R²)
mean                 358.1 ns   (337.1 ns .. 401.9 ns)
std dev              97.56 ns   (54.96 ns .. 180.4 ns)
variance introduced by outliers: 99% (severely inflated)
                     
benchmarking sort/interpreter
time                 673.2 ms   (652.0 ms .. 692.1 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 687.7 ms   (680.4 ms .. 695.0 ms)
std dev              8.496 ms   (7.375 ms .. 9.190 ms)
variance introduced by outliers: 19% (moderately inflated)
                     
benchmarking sort/compiler
time                 677.2 ms   (644.2 ms .. 748.8 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 701.1 ms   (688.2 ms .. 717.1 ms)
std dev              16.12 ms   (6.631 ms .. 22.27 ms)
variance introduced by outliers: 19% (moderately inflated)
                     
Benchmark scheme: FINISH
```

Seems like lazy evalution (Call-by-name) delays the computation, 
let's try this in OCaml and see if strict evaluation (Call-by-value) has any role to play.

## OCaml

The OCaml code is benchmarked using the Core_bech library, to run the benchmarks:

```shell
$ dune exec bench/bench.exe
```

The result:

```shell
Estimated testing time 1m30s (9 benchmarks x 10s). Change using '-quota'.
┌──────────────────┬──────────────┬───────────────┬────────────┬────────────┬────────────┐
│ Name             │     Time/Run │       mWd/Run │   mjWd/Run │   Prom/Run │ Percentage │
├──────────────────┼──────────────┼───────────────┼────────────┼────────────┼────────────┤
│ fib/impl-lang    │      30.34us │         1.87w │            │            │            │
│ fib/interpreter  │  21_871.81us │ 1_784_428.23w │    994.58w │    994.58w │      2.30% │
│ fib/compiler     │  20_962.35us │ 1_784_452.78w │    997.37w │    997.37w │      2.21% │
│ tak/impl-lang    │      80.33us │         5.60w │            │            │            │
│ tak/interpreter  │ 164_592.88us │ 5_903_194.93w │ 11_407.32w │ 11_407.32w │     17.33% │
│ tak/compiler     │ 170_674.93us │ 5_903_193.87w │ 11_414.77w │ 11_414.77w │     17.97% │
│ sort/impl-lang   │       9.77us │     7_806.97w │      4.43w │      4.43w │            │
│ sort/interpreter │ 950_004.90us │   307_007.49w │ 39_188.40w │ 39_188.40w │    100.00% │
│ sort/compiler    │ 869_993.29us │   306_998.17w │ 39_362.60w │ 39_362.60w │     91.58% │
└──────────────────┴──────────────┴───────────────┴────────────┴────────────┴────────────┘
Benchmarks that take 1ns to 100ms can be estimated precisely. For more reliable 
estimates, redesign your benchmark to have a shorter execution time.
```

Apparently there is almost no difference between the interpreted vs compiled approaches.