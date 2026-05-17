{-# LANGUAGE BangPatterns #-}
-- WAM-lowered Haskell predicates.
--
-- One function per predicate in the lowered partition, plus a
-- dispatch map wired into WamContext.wcLoweredPredicates by Main.hs.
module Lowered where

import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict as IM
import WamTypes
import Data.Maybe (fromMaybe)
import WamRuntime

-- | Lowered: lowered_dimension_n_1
lowered_dimension_n_1 :: WamContext -> WamState -> Maybe WamState
lowered_dimension_n_1 !ctx s_init = do
  s_0 <- step ctx (s_init { wsPC = 1 }) (GetConstant (Integer 5) 1)
  let ret_ = wsCP s_0
  if ret_ == 0 then Just (s_0 { wsPC = 0 }) else Just (s_0 { wsPC = ret_, wsCP = 0 })

-- | Lowered: lowered_max_depth_1
lowered_max_depth_1 :: WamContext -> WamState -> Maybe WamState
lowered_max_depth_1 !ctx s_init = do
  s_0 <- step ctx (s_init { wsPC = 3 }) (GetConstant (Integer 10) 1)
  let ret_ = wsCP s_0
  if ret_ == 0 then Just (s_0 { wsPC = 0 }) else Just (s_0 { wsPC = ret_, wsCP = 0 })

-- | Lowered: lowered_category_ancestor_power_sum_selected_3
lowered_category_ancestor_power_sum_selected_3 :: WamContext -> WamState -> Maybe WamState
lowered_category_ancestor_power_sum_selected_3 !ctx s_init = do
  let s_0 = s_init { wsStack = EnvFrame (wsCP s_init) IM.empty : wsStack s_init, wsCutBar = wsCPsLen s_init }
  let s_1 = s_0 { wsRegs = IM.insert 201 (derefVar (wsBindings s_0) (fromMaybe (Atom atomEmpty) (IM.lookup 1 (wsRegs s_0)))) (wsRegs s_0) }
  let s_2 = s_1 { wsRegs = IM.insert 202 (derefVar (wsBindings s_1) (fromMaybe (Atom atomEmpty) (IM.lookup 2 (wsRegs s_1)))) (wsRegs s_1) }
  let s_3 = s_2 { wsRegs = IM.insert 203 (derefVar (wsBindings s_2) (fromMaybe (Atom atomEmpty) (IM.lookup 3 (wsRegs s_2)))) (wsRegs s_2) }
  case (do
        let s_4 = s_3 { wsRegs = IM.insert 1 (fromMaybe (Atom atomEmpty) (getReg 202 s_3)) (wsRegs s_3) }
        s_5 <- step ctx (s_4 { wsPC = 99 }) (BuiltinCall "nonvar/1" 1)
        return s_5
    ) of
      Just s_4 -> do
        let s_5 = s_4 { wsRegs = IM.insert 1 (fromMaybe (Atom atomEmpty) (getReg 201 s_4)) (wsRegs s_4) }
        let s_6 = s_5 { wsRegs = IM.insert 2 (fromMaybe (Atom atomEmpty) (getReg 202 s_5)) (wsRegs s_5) }
        let s_7 = s_6 { wsRegs = IM.insert 3 (fromMaybe (Atom atomEmpty) (getReg 203 s_6)) (wsRegs s_6) }
        s_8 <- dispatchCall ctx "category_ancestor$power_sum_bound/3" (s_7 { wsCP = 105 })
        return s_8
      Nothing -> do
        let s_4 = s_3 { wsRegs = IM.insert 1 (fromMaybe (Atom atomEmpty) (getReg 201 s_3)) (wsRegs s_3) }
        let s_5 = s_4 { wsRegs = IM.insert 2 (fromMaybe (Atom atomEmpty) (getReg 202 s_4)) (wsRegs s_4) }
        let s_6 = s_5 { wsRegs = IM.insert 3 (fromMaybe (Atom atomEmpty) (getReg 203 s_5)) (wsRegs s_5) }
        s_7 <- dispatchCall ctx "category_ancestor$power_sum_grouped/3" (s_6 { wsCP = 111 })
        s_8 <- step ctx (s_7 { wsPC = 111 }) Deallocate
        let ret_ = wsCP s_8
        if ret_ == 0 then Just (s_8 { wsPC = 0 }) else Just (s_8 { wsPC = ret_, wsCP = 0 })

-- | Lowered: lowered_category_ancestor_effective_distance_sum_selected_3
lowered_category_ancestor_effective_distance_sum_selected_3 :: WamContext -> WamState -> Maybe WamState
lowered_category_ancestor_effective_distance_sum_selected_3 !ctx s_init = do
  let s_0 = s_init { wsStack = EnvFrame (wsCP s_init) IM.empty : wsStack s_init, wsCutBar = wsCPsLen s_init }
  let s_1 = s_0 { wsRegs = IM.insert 101 (derefVar (wsBindings s_0) (fromMaybe (Atom atomEmpty) (IM.lookup 1 (wsRegs s_0)))) (wsRegs s_0) }
  let s_2 = s_1 { wsRegs = IM.insert 102 (derefVar (wsBindings s_1) (fromMaybe (Atom atomEmpty) (IM.lookup 2 (wsRegs s_1)))) (wsRegs s_1) }
  let s_3 = s_2 { wsRegs = IM.insert 103 (derefVar (wsBindings s_2) (fromMaybe (Atom atomEmpty) (IM.lookup 3 (wsRegs s_2)))) (wsRegs s_2) }
  let s_4 = s_3 { wsRegs = IM.insert 1 (fromMaybe (Atom atomEmpty) (getReg 101 s_3)) (wsRegs s_3) }
  let s_5 = s_4 { wsRegs = IM.insert 2 (fromMaybe (Atom atomEmpty) (getReg 102 s_4)) (wsRegs s_4) }
  let s_6 = s_5 { wsRegs = IM.insert 3 (fromMaybe (Atom atomEmpty) (getReg 103 s_5)) (wsRegs s_5) }
  s_7 <- step ctx (s_6 { wsPC = 120 }) Deallocate
  dispatchCall ctx "category_ancestor$power_sum_selected/3" s_7

-- | Lowered: lowered_category_ancestor_effective_distance_sum_bound_3
lowered_category_ancestor_effective_distance_sum_bound_3 :: WamContext -> WamState -> Maybe WamState
lowered_category_ancestor_effective_distance_sum_bound_3 !ctx s_init = do
  let s_0 = s_init { wsStack = EnvFrame (wsCP s_init) IM.empty : wsStack s_init, wsCutBar = wsCPsLen s_init }
  let s_1 = s_0 { wsRegs = IM.insert 101 (derefVar (wsBindings s_0) (fromMaybe (Atom atomEmpty) (IM.lookup 1 (wsRegs s_0)))) (wsRegs s_0) }
  let s_2 = s_1 { wsRegs = IM.insert 102 (derefVar (wsBindings s_1) (fromMaybe (Atom atomEmpty) (IM.lookup 2 (wsRegs s_1)))) (wsRegs s_1) }
  let s_3 = s_2 { wsRegs = IM.insert 103 (derefVar (wsBindings s_2) (fromMaybe (Atom atomEmpty) (IM.lookup 3 (wsRegs s_2)))) (wsRegs s_2) }
  let s_4 = s_3 { wsRegs = IM.insert 1 (fromMaybe (Atom atomEmpty) (getReg 101 s_3)) (wsRegs s_3) }
  let s_5 = s_4 { wsRegs = IM.insert 2 (fromMaybe (Atom atomEmpty) (getReg 102 s_4)) (wsRegs s_4) }
  let s_6 = s_5 { wsRegs = IM.insert 3 (fromMaybe (Atom atomEmpty) (getReg 103 s_5)) (wsRegs s_5) }
  s_7 <- step ctx (s_6 { wsPC = 129 }) Deallocate
  dispatchCall ctx "category_ancestor$power_sum_bound/3" s_7

loweredPredicates :: Map.HashMap String (WamContext -> WamState -> Maybe WamState)
loweredPredicates = Map.fromList
    [ ("dimension_n/1", lowered_dimension_n_1)
    , ("max_depth/1", lowered_max_depth_1)
    , ("category_ancestor$power_sum_selected/3", lowered_category_ancestor_power_sum_selected_3)
    , ("category_ancestor$effective_distance_sum_selected/3", lowered_category_ancestor_effective_distance_sum_selected_3)
    , ("category_ancestor$effective_distance_sum_bound/3", lowered_category_ancestor_effective_distance_sum_bound_3)
    ]
