{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.SpiderMonkey.Parser where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Int
import Data.Word
import Control.Applicative
import Data.Monoid
import Control.Monad
import Data.String
import Data.Text hiding (filter)
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Scientific (Scientific)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- low level types

data SourceLocation = SourceLocation {source :: Maybe String
                                     ,start :: Position
                                     ,end :: Position}
                    deriving (Eq, Show)

data Position = Position {line :: Int32
                         ,column :: Int32
                         }
                deriving (Eq, Show)


$(deriveJSON defaultOptions ''SourceLocation)

$(deriveJSON defaultOptions ''Position)


data Node a = Node {nodeType    :: String
                   ,nodeBuilder :: Builder a}

instance Functor Node where
  fmap f node = node {nodeBuilder = f <$> nodeBuilder node}

instance Applicative Node where
  pure x      = Node {nodeType = "", nodeBuilder = pure x}
  ndf <*> ndx = let f = nodeBuilder ndf
                    ty= nodeType ndf
                    x = nodeBuilder ndx
                in Node ty (f <*> x)

cases :: [Node a] -> (Value -> Parser a)
cases nds = \v -> case v of
  Object o -> do type_ <- getType o
                 case matchType type_ nds of
                   Just node -> runBuilder (nodeBuilder node) o
                   Nothing   -> fail "Unexpected node type"
  _        -> typeMismatch "Node" v

node :: String -> (SourceLocation -> f) -> Node f
node name ctor = Node name $ ctor <$> getLocation

single :: String -> (SourceLocation -> f) -> (Value -> Parser f)
single name ctor = cases [node name ctor]

liftJSON :: (FromJSON a) => Node a
liftJSON = Node {nodeType = ""
                ,nodeBuilder = do o <- ask
                                  lift $ parseJSON $ Object o}

type Builder a = ReaderT Object Parser a

runBuilder :: Builder a -> Object -> Parser a
runBuilder = runReaderT

field :: FromJSON a => Text -> Node a
field name = Node {nodeType = ""
                  ,nodeBuilder = do o <- ask
                                    lift (o .: name)}

-- instance FromJSON a => IsString (Node (Maybe a)) where
--   fromString s = field (fromString s)

instance FromJSON a => IsString (Node a) where
  fromString s = field (fromString s)


matchType :: String -> [Node a] -> Maybe (Node a)
matchType type_ = safeHead . filter (\n -> type_ == nodeType n)
  where safeHead [] = Nothing
        safeHead (x:_) = Just x
                   
getType :: Object -> Parser String
getType o = o .: "type"

getLocation :: Builder SourceLocation
getLocation = ask >>= \o -> lift (o .: "loc")
  
data Program = Program SourceLocation [Statement]

instance FromJSON Program where
  parseJSON = single "Program" $ Program <*> "body"
  
data Function = Function {funcId :: Maybe Identifier
                         ,funcParams :: [Pattern]
                         ,funcDefaults :: [Expression]
                         ,funcRest :: Maybe Identifier
                         ,funcBody :: Either [Statement] Expression
                         ,funcGenerator :: Bool}

instance FromJSON Function where
  parseJSON (Object o) = Function <$>
                         o .: "id" <*>
                         o .: "params" <*>
                         o .: "defaults" <*>
                         o .: "rest" <*>
                         o .: "body" <*>
                         o .: "generator"
  parseJSON _ = mzero 
                         
data Statement = EmptyStatement SourceLocation
               | BlockStatement SourceLocation [Statement]
               | ExpressionStatement SourceLocation Expression
               | IfStatement SourceLocation Expression Statement (Maybe Statement)
               | LabeledStatement SourceLocation Identifier Statement
               | BreakStatement SourceLocation (Maybe Identifier)
               | ContinueStatement SourceLocation (Maybe Identifier)
               | WithStatement SourceLocation Expression Statement
               | SwitchStatement SourceLocation Expression [SwitchCase] Bool
               | ReturnStatement SourceLocation (Maybe Expression)
               | ThrowStatement SourceLocation Expression
               | TryStatement SourceLocation [Statement] (Maybe CatchClause) [CatchClause] (Maybe [Statement])
               | WhileStatement SourceLocation Expression Statement
               | DoWhileStatement SourceLocation Statement Expression
               | ForStatement SourceLocation ForInit Expression Expression Statement
               | ForInStatement SourceLocation (Either VariableDeclaration Expression) Expression Statement Bool
               | ForOfStatement SourceLocation (Either VariableDeclarator Expression) Expression Statement
               | LetStatement SourceLocation [VariableDeclarator] Statement
               | DebuggerStatement SourceLocation
               | FunctionDeclarationStatement SourceLocation Function
               | VariableDeclarationStatement SourceLocation VariableDeclaration

instance FromJSON Statement where
  parseJSON =
    cases
    [node "EmptyStatement" EmptyStatement
    ,node "BlockStatement" BlockStatement <*> "body"
    ,node "ExpressionStatement" BlockStatement <*> "expression"
    ,node "IfStatement" IfStatement <*> "text" <*> "consequent" <*> "alternate"
    ,node "LabeledStatement" LabeledStatement <*> "label" <*> "body"
    ,node "BreakStatement" BreakStatement <*> "label"
    ,node "ContinueStatement" ContinueStatement <*> "label"
    ,node "WithStatement" WithStatement <*> "object" <*> "body"
    ,node "SwitchStatement" SwitchStatement <*> "discriminant" <*> "cases" <*> "lexical"
    ,node "ReturnStatement" ReturnStatement <*> "argument"
    ,node "ThrowStatement" ThrowStatement <*> "argument"
    ,node "TryStatement" TryStatement <*> "block" <*> "handler" <*> "guardedHandlers" <*> "finalizer"
    ,node "WhileStatement" WhileStatement <*> "test" <*> "body"
    ,node "DoWhileStatement" DoWhileStatement <*> "body" <*> "test"
    ,node "ForStatement" ForStatement <*> "init" <*> "test" <*> "update" <*> "body"
    ,node "ForInStatement" ForInStatement <*> "left" <*> "right" <*> "body" <*> "each"
    ,node "ForOfStatement" ForOfStatement <*> "left" <*> "right" <*> "body"
    ,node "LetStatement" LetStatement <*> "head" <*> "body"
    ,node "DebuggerStatement" DebuggerStatement
    ,node "FunctionDeclaration" FunctionDeclarationStatement <*> liftJSON
    ,node "VariableDeclaration" VariableDeclarationStatement <*> liftJSON
    ]
                 
data ForInit = VarInit VariableDeclaration
             | ExprInit Expression
             | NoInit

instance FromJSON ForInit where
  parseJSON v = case v of
    (Object _)-> parseJSON v >>= \e -> case e of
                  Left vd  -> return $ VarInit vd
                  Right ex -> return $ ExprInit ex
    Null      -> return $ NoInit
                   

data VariableDeclaration = VariableDeclaration SourceLocation [VariableDeclarator] DeclarationKind

instance FromJSON VariableDeclaration where
  parseJSON = cases [node "VariableDeclaration" VariableDeclaration <*>
                     "declarations" <*> "kind"]

data VariableDeclarator = VariableDeclarator SourceLocation Pattern (Maybe Expression)

instance FromJSON VariableDeclarator where
  parseJSON = single "VariableDeclarator" $ VariableDeclarator <*> "id" <*> "init"

data DeclarationKind = DVar | DLet | DConst

instance FromJSON DeclarationKind where
  parseJSON (String t) = pure $ case t of
    "var" -> DVar
    "let" -> DLet
    "const" -> DConst
  parseJSON _ = mzero

data Expression = ThisExpression SourceLocation
                | ArrayExpression SourceLocation [Maybe Expression]
                | ObjectExpression SourceLocation [Property]
                | FunctionExpression SourceLocation Function
                | ArrowExpression SourceLocation Function
                | SequenceExpression SourceLocation [Expression]
                | UnaryExpression SourceLocation UnaryOperator Bool Expression
                | BinaryExpression SourceLocation BinaryOperator Expression Expression
                | AssignmentExpression SourceLocation AssignmentOperator Expression Expression
                | UpdateExpression SourceLocation UpdateOperator Expression Bool
                | LogicalExpression SourceLocation LogicalOperator Expression Expression
                | ConditionalExpression SourceLocation Expression Expression Expression
                | NewExpression SourceLocation Expression [Expression]
                | CallExpression SourceLocation Expression [Expression]
                | MemberExpression SourceLocation Expression (Either Identifier Expression)
                  -- ^ no "computed: boolean" because it is uniquely
                  -- determined from the value of "property:
                  -- Identifier | Expression"
                | YieldExpression SourceLocation (Maybe Expression)
                | ComprehensionExpression SourceLocation Expression [ComprehensionBlock] (Maybe Expression)
                | GeneratorExpression SourceLocation Expression [ComprehensionBlock] (Maybe Expression)
                | GraphExpression SourceLocation Word32 Literal
                | LetExpression SourceLocation [VariableDeclarator] Expression

instance FromJSON Expression where
  parseJSON =
    cases
    [node "ThisExpression" ThisExpression
    ,node "ArrayExpression" ArrayExpression <*> "elements"
    ,node "ObjectExpression" ObjectExpression <*> "properties"
    ,node "FunctionExpression" FunctionExpression <*> liftJSON
    ,node "ArrowExpression" ArrowExpression <*> liftJSON
    ]

data Property = Property SourceLocation (Either Literal Identifier) Expression PropertyKind

instance FromJSON Property where
  parseJSON = cases [node "Property" Property <*> "key" <*> "value" <*> "kind"]

data PropertyKind = PInit | PGet | PSet

instance FromJSON PropertyKind where
  parseJSON (String t) = pure $ case t of
                                 "init" -> PInit
                                 "get"  -> PGet
                                 "set"  -> PSet
  parseJSON _ = mzero

data Pattern = ObjectPattern SourceLocation [PatternProperty]
             | ArrayPattern SourceLocation [Maybe Pattern]
             | IdentifierPattern SourceLocation Identifier

instance FromJSON Pattern where
  parseJSON = cases
              [node "ObjectPattern" ObjectPattern <*> "properties"
              ,node "ArrayPattern" ArrayPattern <*> "elements"
              ,node "Identifier" IdentifierPattern <*> liftJSON
              ]

data PatternProperty = PatternProperty (Either Literal Identifier) Pattern

instance FromJSON PatternProperty where
  parseJSON (Object o) = PatternProperty <$>
                         o .: "key" <*>
                         o .: "value"
  parseJSON _          = mzero

data SwitchCase = SwitchCase SourceLocation (Maybe Expression) [Statement]

instance FromJSON SwitchCase where
  parseJSON = single "SwitchCase" $ SwitchCase <*> "test" <*> "consequent"

data CatchClause = CatchClause SourceLocation Pattern (Maybe Expression) [Statement]

instance FromJSON CatchClause where
  parseJSON = single "CatchClause" $ CatchClause <*> "param" <*> "guard" <*> "body"

data ComprehensionBlock = ComprehensionBlock SourceLocation Pattern Expression Bool

instance FromJSON ComprehensionBlock where
  parseJSON = single "ComprehensionBlock" $ ComprehensionBlock <*> "left" <*> "right" <*> "each"

data Identifier = Identifier SourceLocation Text

instance FromJSON Identifier where
  parseJSON = single "Identifier" $ Identifier <*> "name"

data Literal = LString SourceLocation Text
             | LBool SourceLocation Bool
             | LNull SourceLocation
             | LNumber SourceLocation Scientific
 --          | RegExp SourceLocation String

instance FromJSON Literal where
  parseJSON (Object o) = do ty <- getType o
                            unless (ty == "Literal") mzero
                            loc <- o .: "loc"
                            v <- o .: "value"
                            return $ case v of
                             String s -> LString loc s
                             Bool   b -> LBool loc b
                             Null     -> LNull loc
                             Number n -> LNumber loc n
                             
  parseJSON _ = mzero

enum :: [(Text, a)] -> Value -> Parser a
enum m = let mp = Map.fromList m
         in \v -> case v of
                   String t -> case Map.lookup t mp of
                                Just a -> return a
                                Nothing -> mzero
                   _        -> mzero

data UnaryOperator = (:.-:) | (:.+:) | (:!:) | (:~:) | Typeof | Void | Delete

instance FromJSON UnaryOperator where
  parseJSON = enum [("-", (:.-:)), ("+", (:.+:)), ("!", (:!:)), ("~", (:~:)), ("typeof", Typeof), ("void", Void), ("delete", Delete)]

data BinaryOperator = (:==:) | (:!=:) | (:===:) | (:!==:)
                    | (:<:) | (:<=:) | (:>:) | (:>=:) | (:<<:) | (:>>:)
                    | (:>>>:) | (:+:) | (:-:) | (:*:) | (:/:)
                    | (:%:) | (:|:) | (:^:) | (:&:) | In | Instanceof | (:..:)

instance FromJSON BinaryOperator where
  parseJSON = enum [("==", (:==:)), ("!=", (:!=:)), ("===", (:===:)), ("!==", (:!==:)), ("<", (:<:)), ("<=", (:<=:)), (">", (:>:)), (">=", (:>=:)), ("<<", (:<<:)), (">>", (:>>:)), (">>>", (:>>>:)), ("+", (:+:)), ("-", (:-:)), ("*", (:*:)), ("/", (:/:)), ("%", (:%:)), ("|", (:|:)), ("^", (:^:)), ("&", (:&:)), ("in", In), ("instanceof", Instanceof), ("..", (:..:))]

data LogicalOperator = (:||:) | (:&&:)

instance FromJSON LogicalOperator where
  parseJSON = enum [("||", (:||:)), ("&&", (:&&:))]

data AssignmentOperator = (:=:) | (:+=:) | (:-=:) | (:*=:) | (:/=:) | (:%=:)
                        | (:<<=:) | (:>>=:) | (:>>>=:) | (:|=:) | (:^=:) | (:&=:)

instance FromJSON AssignmentOperator where
  parseJSON = enum [("=", (:=:)), ("+=", (:+=:)), ("-=", (:-=:)), ("*=", (:*=:)), ("/=", (:/=:)), ("%=", (:%=:)), ("<<=", (:<<=:)), (">>=", (:>>=:)), (">>>=", (:>>>=:)), ("|=", (:|=:)), ("^=", (:^=:)), ("&=", (:&=:))]

data UpdateOperator = (:++:) | (:--:)

instance FromJSON UpdateOperator where
  parseJSON = enum [("++", (:++:)), ("--", (:--:))]
