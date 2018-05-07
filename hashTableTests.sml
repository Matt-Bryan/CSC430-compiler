(* open the HashTable module or prefix each function below *)
open HashTable; 

(* define a new exception for use when a lookup fails - name courtesy of Sunjay *)
exception CannotFindIt;

(* this is the function that will be used to hash the "keys" in the hashtable
   -- using "string" as the key in this example *)
val hash_fn : string->word = HashString.hashString;

(* this is the function that will be used to compare keys for equality
   -- using "string" as the key in this example *)
val cmp_fn : string*string->bool = (op =);

(* this will be used to define the initial size for the table -- selected arbitrarily for this example *)
val initial_size : int = 101;

(* create the table configured with the parameters from above *)
val tbl : (string,int) hash_table = mkTable (hash_fn, cmp_fn) (initial_size, CannotFindIt);

(* example uses below *)

insert tbl ("foo",9);
insert tbl ("bar",27);

find tbl "foo";
find tbl "bar";

lookup tbl "foo";
lookup tbl "bar";