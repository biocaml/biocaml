(** Intended usage is to do "open Sesame", which replaces several third-party modules with modified versions, and provides some new modules. *)

include Pervasives2

module Array = Array2
module Char = Char2
module DynArray = DynArray2
module List = List2
module Map = Map2
module PMap = PMap2
module Set = Set2
module Stream = Stream2
module String = String2
module Option = Option2
module Int = Int
(* module RomanNum = RomanNum *)

module MMap = MMap
module Msg = Msg
module Pos = Pos
module SparseArray = SparseArray
module Test = Test
module ArrayPrint = ArrayPrint
module RedBlack = RedBlack
(* module Range = Range *)
(* module RSet = RSet *)
module Monster = Monster
module IntervalTree = IntervalTree

module Fn = Fn
module Tuple = Tuple
module Order = Order
module Ordered = Ordered
module Lines = Lines

module IntMap = IntMap
module StringMap = StringMap
module IntSet = IntSet
module StringSet = StringSet

module Show = Show
module SmallCheck = SmallCheck

(* module About = About *)

