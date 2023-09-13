#include "rsdd.h"
#include <stdio.h>

char* cnf_string =
"p cnf 6 3\n"
"1 2 3 4 0\n"
"-2 -3 4 5 0\n"
"-4 -5 6 6 0\n";

int main() {
  VarOrder* order = var_order_linear(6);
  Cnf* cnf = cnf_from_dimacs(cnf_string);
  RsddBddBuilder* builder = robdd_builder_all_table(order);
  BddPtr* bdd = robdd_builder_compile_cnf(builder, cnf);
  uint64_t mc = robdd_model_count(builder, bdd);
  printf("Model Count: %llu\n", mc);
}
