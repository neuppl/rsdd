# About

This file contains some working notes for keeping track of the changes that are
made in the BDD directory. The idea is to keep track of what has already been
done, as well as performance metrics, in order to iteratively improve the
library and have a record of the process.

# Notes

* I am comparing Cudd to my implementation pretty regularly at this point. Some
  interesting observations:
  * It looks like closed-address hash-tables are actually not a great idea for
    this kind of usage

Mysterious benchmark: Cudd uses way fewer nodes and lookups than I do!

```
my stats:

apply cache stats: BddCacheStats { lookup_count: 531080944, miss_count: 372555308, confli
ct_count: 343789509, avg_probe: 0, num_applications: 0 }                                
num apply nodes: 125047051
node count: 8188213
backing store stats: BackingCacheStats { lookup_count: 354861653, hit_count: 229814602, n
um_elements: 125047051 }                                                                

Cudd stats for the same BDD and ordering:

Hard limit for cache size: 2796202
Cache hit threshold for resizing: 30%
Garbage collection enabled: yes
Limit for fast unique table growth: 1677721
Maximum number of variables sifted per reordering: 1000
Maximum number of variable swaps per reordering: 2000000
Maximum growth while sifting a variable: 1.2
Dynamic reordering of BDDs enabled: no
Default BDD reordering method: 4
Dynamic reordering of ZDDs enabled: no
Default ZDD reordering method: 4
Realignment of ZDDs to BDDs enabled: no
Realignment of BDDs to ZDDs enabled: no
Dead nodes counted in triggering reordering: no
Group checking criterion: 7
Recombination threshold: 0
Symmetry violation threshold: 0
Arc violation threshold: 0
GA population size: 0
Number of crossovers for GA: 0
Next reordering threshold: 4004
**** CUDD non-modifiable parameters ****
Memory in use: 3283378040
Peak number of nodes: 92080156
Peak number of live nodes: 92079904
Number of BDD variables: 227
Number of ZDD variables: 0
Number of cache entries: 2097152
Number of cache look-ups: 144901874
Number of cache hits: 55726367
Number of cache insertions: 89676348
Number of cache collisions: 86170047
Number of cache deletions: 1409149
Cache used slots = 100.00% (expected 100.00%)
Soft limit for cache size: 2796202
Number of buckets in unique table: 33088000
Used buckets in unique table: 92.88% (expected 93.08%)
Number of BDD and ADD nodes: 92079904
Number of ZDD nodes: 0
Number of dead BDD and ADD nodes: 0
Number of dead ZDD nodes: 0
Total number of nodes allocated: 96020531
Total number of nodes reclaimed: 0
Garbage collections so far: 1
Time for garbage collection: 0.21 sec
Reorderings so far: 0
Time for reordering: 0.00 sec
```

Really weird...
