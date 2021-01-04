{- This module implements a simplified version of a C99 AST. It omits a lot of
 - specific and rarely used language constructs and features, which typically
 - are not used by code generators anyway. Some parts of the AST differ quite a
 - lot from the C99 one, and do not necessarily mimic their counterparts, even
 - if the names are similar.
 -
 - A total translation function proves that the simplified AST can be rewritten
 - in terms of the full AST, and thus is a subset.
-}

module Language.C99.Simple.AST where

import Prelude hiding (LT, GT)
import Data.List.NonEmpty (NonEmpty)

type Ident    = String

data BlockItem = Decln Decln
               | Stmt Stmt
  deriving ( Show, Eq )

data TransUnit = TransUnit [Decln] [FunDef]
  deriving ( Show, Eq )

data FunDef = FunDef (Maybe Attributes) Type Ident [Param] [BlockItem]
  deriving ( Show, Eq )

data Param = Param Type Ident
  deriving ( Show, Eq )

type Attributes = String

data Decln = VarDecln (Maybe Attributes) (Maybe StorageSpec) Type Ident (Maybe Init)
           | FunDecln (Maybe Attributes) (Maybe StorageSpec) Type Ident [Param]
           | TypeDecln Type
  deriving ( Show, Eq )

data StorageSpec = Typedef
                 | Extern
                 | Static
                 | Auto
                 | Register
  deriving ( Show, Eq )

data Type = Type     Type
          | TypeSpec TypeSpec
          | Ptr      Type
          | Array    Type (Maybe Expr)

          | Const    Type
          | Restrict Type
          | Volatile Type
  deriving ( Show, Eq )

data TypeSpec = Void
              | Char
              | Signed_Char
              | Unsigned_Char

              | Short
              | Signed_Short
              | Short_Int
              | Signed_Short_Int

              | Unsigned_Short
              | Unsigned_Short_Int

              | Int
              | Signed
              | Signed_Int

              | Unsigned
              | Unsigned_Int

              | Long
              | Signed_Long
              | Long_Int
              | Signed_Long_Int

              | Unsigned_Long
              | Unsgined_Long_Int

              | Long_Long
              | Signed_Long_Long
              | Long_Long_Int
              | Signed_Long_Long_Int

              | Unsigned_Long_Long
              | Unsigned_Long_Long_Int

              | Float
              | Double
              | Long_Double
              | Bool
              | Float_Complex
              | Double_Complex
              | Long_Double_Complex
              | TypedefName Ident

              | Struct      Ident
              | StructDecln (Maybe Ident) (NonEmpty FieldDecln)

              | Union      Ident
              | UnionDecln (Maybe Ident) (NonEmpty FieldDecln)

              | Enum      Ident
              | EnumDecln (Maybe Ident) (NonEmpty Ident)
  deriving ( Show, Eq )

data FieldDecln = FieldDecln Type Ident
  deriving ( Show, Eq )

data Init = InitExpr  Expr
          | InitMultiple (NonEmpty InitItem)
  deriving ( Show, Eq )

data InitItem = InitItem (Maybe Ident) Init
  deriving ( Show, Eq )

data Expr = Ident     Ident
          | LitBool   Bool
          | LitInt    Integer
          | LitFloat  Float
          | LitDouble Double
          | LitString String

          | Index   Expr Expr
          | Funcall Expr [Expr]
          | Dot     Expr Ident
          | Arrow   Expr Ident
          | InitVal TypeName (NonEmpty InitItem)

          | UnaryOp UnaryOp Expr

          | Cast  TypeName Expr

          | BinaryOp BinaryOp Expr Expr

          | Cond Expr Expr Expr

          | AssignOp AssignOp Expr Expr
  deriving ( Show, Eq )

data UnaryOp = Inc
             | Dec
             | Ref
             | DeRef
             | Plus
             | Min
             | BoolNot
             | Not
  deriving ( Show, Eq )

data BinaryOp = Mult
              | Div
              | Mod
              | Add
              | Sub
              | ShiftL
              | ShiftR
              | LT
              | GT
              | LE
              | GE
              | Eq
              | NEq
              | And
              | XOr
              | Or
              | LAnd
              | LOr
  deriving ( Show, Eq )

data AssignOp = Assign
              | AssignMult
              | AssignDiv
              | AssignMod
              | AssignAdd
              | AssignSub
              | AssignShiftL
              | AssignShiftR
              | AssignAnd
              | AssignXOr
              | AssignOr
  deriving ( Show, Eq )

data TypeName = TypeName Type
  deriving ( Show, Eq )

data Case = Case    Expr Stmt
          | Default      Stmt
  deriving ( Show, Eq )

data Stmt = Expr     Expr
          | If       Expr [BlockItem]
          | IfElse   Expr [BlockItem] [BlockItem]
          | Switch   Expr [Case]
          | While    Expr [BlockItem]
          | For      Expr Expr Expr [BlockItem]
          | ForInf   [BlockItem]
          | Continue
          | Break
          | Label    String Stmt
          | Block [BlockItem]
          | Return   (Maybe Expr)
  deriving ( Show, Eq )
