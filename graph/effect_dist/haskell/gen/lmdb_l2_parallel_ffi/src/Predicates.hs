module Predicates where

import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict as IM
import WamTypes

-- Fact shape classification:
-- dimension_n/1: fact_only=true, clauses=1, first_arg=all_ground, layout=compiled
-- max_depth/1: fact_only=true, clauses=1, first_arg=all_ground, layout=compiled
-- category_ancestor/4: fact_only=false, clauses=2, first_arg=all_variable, layout=compiled
-- category_ancestor$power_sum_bound/3: fact_only=false, clauses=1, first_arg=all_variable, layout=compiled
-- category_ancestor$power_sum_selected/3: fact_only=false, clauses=3, first_arg=mixed, layout=compiled
-- category_ancestor$effective_distance_sum_selected/3: fact_only=false, clauses=1, first_arg=all_variable, layout=compiled
-- category_ancestor$effective_distance_sum_bound/3: fact_only=false, clauses=1, first_arg=all_variable, layout=compiled

-- | Merged WAM code for all predicates.
allCode :: [Instruction]
allCode =
    [ GetConstant (Integer 5) 1
    , Proceed
    , GetConstant (Integer 10) 1
    , Proceed
    , ParTryMeElse "L_category_ancestor_4_2"
    , Allocate
    , GetVariable 103 1
    , GetVariable 201 2
    , GetConstant (Integer 1) 3
    , GetVariable 202 4
    , PutValue 103 1
    , PutValue 201 2
    , Call "category_parent/2" 2
    , Deallocate
    , PutStructure 5 1 2
    , SetValue 201
    , SetValue 202
    , BuiltinCall "\\+/1" 1
    , Proceed
    , ParTrustMe
    , Allocate
    , GetVariable 203 1
    , GetVariable 205 2
    , GetVariable 207 3
    , GetVariable 206 4
    , PutVariable 202 1
    , Call "max_depth/1" 1
    , PutValue 206 1
    , PutVariable 201 2
    , BuiltinCall "length/2" 2
    , PutValue 201 1
    , PutValue 202 2
    , BuiltinCall "</2" 2
    , BuiltinCall "!/0" 0
    , PutValue 203 1
    , PutVariable 204 2
    , Call "category_parent/2" 2
    , PutStructure 5 1 2
    , SetValue 204
    , SetValue 206
    , BuiltinCall "\\+/1" 1
    , PutValue 204 1
    , PutValue 205 2
    , PutVariable 208 3
    , PutList 4
    , SetValue 204
    , SetValue 206
    , Call "category_ancestor/4" 4
    , PutValue 207 1
    , PutStructure 6 2 2
    , SetValue 208
    , SetConstant (Integer 1)
    , BuiltinCall "is/2" 2
    , Deallocate
    , Proceed
    , Allocate
    , GetVariable 202 1
    , GetVariable 203 2
    , GetVariable 208 3
    , PutValue 203 1
    , BuiltinCall "nonvar/1" 1
    , PutVariable 201 1
    , Call "dimension_n/1" 1
    , PutVariable 207 1
    , PutStructure 7 2 1
    , SetValue 201
    , BuiltinCall "is/2" 2
    , PutVariable 205 205
    , BeginAggregate "sum" 205 208
    , PutValue 202 1
    , PutValue 203 2
    , PutVariable 204 3
    , PutList 4
    , SetValue 202
    , SetConstant (Atom 2)
    , Call "category_ancestor/4" 4
    , PutVariable 206 1
    , PutStructure 6 2 2
    , SetValue 204
    , SetConstant (Integer 1)
    , BuiltinCall "is/2" 2
    , PutValue 205 1
    , PutStructure 8 2 2
    , SetValue 206
    , SetValue 207
    , BuiltinCall "is/2" 2
    , EndAggregate 205
    , PutValue 208 1
    , PutConstant (Integer 0) 2
    , BuiltinCall ">/2" 2
    , Deallocate
    , Proceed
    , Allocate
    , GetVariable 201 1
    , GetVariable 202 2
    , GetVariable 203 3
    , ParTryMeElse "L_ite_else_2"
    , PutValue 202 1
    , BuiltinCall "nonvar/1" 1
    , CutIte
    , PutValue 201 1
    , PutValue 202 2
    , PutValue 203 3
    , Call "category_ancestor$power_sum_bound/3" 3
    , Jump "L_ite_cont_2"
    , ParTrustMe
    , PutValue 201 1
    , PutValue 202 2
    , PutValue 203 3
    , Call "category_ancestor$power_sum_grouped/3" 3
    , Deallocate
    , Proceed
    , Allocate
    , GetVariable 101 1
    , GetVariable 102 2
    , GetVariable 103 3
    , PutValue 101 1
    , PutValue 102 2
    , PutValue 103 3
    , Deallocate
    , Execute "category_ancestor$power_sum_selected/3"
    , Allocate
    , GetVariable 101 1
    , GetVariable 102 2
    , GetVariable 103 3
    , PutValue 101 1
    , PutValue 102 2
    , PutValue 103 3
    , Deallocate
    , Execute "category_ancestor$power_sum_bound/3"
    ]

-- | Merged label map for all predicates.
allLabels :: Map.HashMap String Int
allLabels = Map.fromList
    [ ("dimension_n/1", 1)
    , ("max_depth/1", 3)
    , ("category_ancestor/4", 5)
    , ("L_category_ancestor_4_2", 20)
    , ("category_ancestor$power_sum_bound/3", 56)
    , ("category_ancestor$power_sum_selected/3", 93)
    , ("L_ite_else_2", 106)
    , ("L_ite_cont_2", 111)
    , ("category_ancestor$effective_distance_sum_selected/3", 113)
    , ("category_ancestor$effective_distance_sum_bound/3", 122)
    ]


compileTimeAtomTable :: InternTable
compileTimeAtomTable =
  let pairs = [(0, "true"), (1, "fail"), (2, "[]"), (3, "."), (4, ""), (5, "member/2"), (6, "+/2"), (7, "-/1"), (8, "**/2")]
  in InternTable (Map.fromList [(s, i) | (i, s) <- pairs])
                (IM.fromList pairs)
                9
