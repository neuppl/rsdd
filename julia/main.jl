rsdd_path = "target/debug/librsdd"

struct BddManager end

bdd_manager = ccall(
  (:rsdd_mk_bdd_manager_default_order, rsdd_path),
  Ptr{Cvoid}, (Int64,), 10
)

ccall(
  (:rsdd_print_stats, rsdd_path),
  Nothing, (Ptr{Cvoid}, Int64, Bool), bdd_manager, 1, true
)

recur_calls = ccall(
  (:rsdd_num_recursive_calls, rsdd_path),
  Int64, (Ptr{Cvoid},), bdd_manager
)
println(recur_calls)

println("all done")
