{-# LANGUAGE TemplateHaskell #-}
module Data.Vector.Instances.Collections.Internal (declareInstances) where

import Language.Haskell.TH

import Data.Collections 
  ( Foldable(..)
  , Unfoldable(..)
  , Collection(..)
  , Sequence(..)
  , Indexed(..)
  )

import qualified Data.Collections as C
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Primitive as VP

import Control.Monad (guard)

declareInstances :: [Name] -> [Name] -> DecsQ
declareInstances cs ts =
  do
    is <- mapM (\c -> mapM (declareInstance c) ts) cs
    return $ concat is

declareInstance :: Name -> Name -> DecQ
declareInstance cls ty =
  do
    n <- newName "a"
    let 
      a = varT n
      context = cxt $ map (\c -> classP c [a]) (constr ty)
    b <- body cls
    instanceD context
          (header cls ty a)
          (map return b)

header :: Name -> Name -> TypeQ -> TypeQ
header cls ty a
  | cls == ''Indexed = 
      appT 
        (appT 
          (appT 
            (conT cls) 
            (appT 
              (conT ty) 
              a)) 
          (conT ''Int)) 
        a
  | otherwise = 
      appT 
        (appT 
          (conT cls) 
          (appT 
            (conT ty) 
            a)) 
        a

constr :: Name -> [Name]
constr n
  | n == ''V.Vector = []
  | n == ''VU.Vector = [''VU.Unbox]
  | n == ''VP.Vector = [''VP.Prim]
  | n == ''VS.Vector = [''VS.Storable]
  | otherwise = undefined

sFun :: String -> ExpQ -> DecQ
sFun name body = funD (mkName name) [clause [] (normalB body) []]

body :: Name -> DecsQ
body n 
  | n == ''Foldable = 
      sequence $ map (uncurry sFun)
        [ ("foldr", [|VG.foldr|])
        , ("foldl", [|VG.foldl|])
        , ("foldr1", [|VG.foldr1|])
        , ("foldl1", [|VG.foldl1|])
        , ("null", [|VG.null|])
        , ("size", [|VG.length|]) 
        ]

  | n == ''Unfoldable =
      sequence $ map (uncurry sFun)
        [ ("insert", [| flip VG.snoc |])
        , ("empty", [|VG.empty|])
        , ("singleton", [|VG.singleton|])
        , ("insertMany", [|\src dst -> dst VG.++ (VG.fromList $ C.toList src)|])
        , ("insertManySorted", [|C.insertMany|])
        ]

  | n == ''Collection =
      sequence $ map (uncurry sFun)
        [ ("filter", [|VG.filter|]) ]

  | n == ''Sequence =
      sequence $ map (uncurry sFun)
        [ ("take", [|VG.take|])
        , ("drop", [|VG.drop|])
        , ("splitAt", [|VG.splitAt|])
        , ("reverse", [|VG.reverse|])
        , ("front",
          [|\v -> do
            guard $ not $ VG.null v
            return (VG.head v, VG.tail v)
          |])
        , ("back",
          [|\v -> do
            guard $ not $ VG.null v
            return (VG.init v, VG.last v)
          |])
        , ("cons", [|VG.cons|])
        , ("snoc", [|VG.snoc|])
        , ("isPrefix",
          [|\a b ->
            let
              na = VG.length a 
              nb = VG.length b
              bs = VG.slice 0 na b
            in
              na <= nb && a `VG.eq` bs
          |])
        ]

  | n == ''Indexed =
      sequence $ map (uncurry sFun)
        [ ("index", [|flip (VG.!)|])
        -- this seems inefficient, but I don't see a better way to do it
        , ("adjust", [|\f k v -> v VG.// [ (k, f $ v VG.! k) ]|])
        , ("inDomain", [|\k v -> k < VG.length v|])
        , ("//", [|\v l -> v VG.// (C.toList l)|])
        , ("accum", [|\f v l -> VG.accum f v (C.toList l)|])
        ]
        

  | otherwise = undefined
