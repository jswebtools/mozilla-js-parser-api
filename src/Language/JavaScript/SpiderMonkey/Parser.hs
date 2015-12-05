{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.JavaScript.SpiderMonkey.Parser where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Int
import Control.Applicative
import Data.Monoid()
import Control.Monad
import Data.String
import Data.Text hiding (filter)
import Control.Monad.Reader
import Data.Scientific (Scientific)
import qualified Data.Map.Strict as Map
import Prelude hiding (either)

-- low level types

data SourceLocation = SourceLocation {source :: Maybe String
                                     ,start :: Position
                                     ,end :: Position}
                    | NoLocation
                    deriving (Eq, Show)

instance FromJSON SourceLocation where
  parseJSON (Object o) = SourceLocation <$>
                         o .: "source" <*>
                         o .: "start" <*>
                         o .: "end"
  parseJSON Null       = pure NoLocation

data Position = Position {line :: Int32
                         ,column :: Int32
                         }
              deriving (Eq, Show)

$(deriveJSON defaultOptions ''Position)


data Node a = Node {nodeType    :: String
                   ,nodeBuilder :: Builder a}

instance Functor Node where
  fmap f node' = node' {nodeBuilder = f <$> nodeBuilder node'}

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
                   Just node' -> runBuilder (nodeBuilder node') o
                   Nothing   -> fail $ "Unexpected node type: " ++ (show type_) ++ " options were: " ++ (Data.String.unwords $ Prelude.map nodeType nds)
  _        -> typeMismatch "Node" v

node :: String -> (SourceLocation -> f) -> Node f
node name ctor = Node name $ ctor <$> getLocation

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
  
data Program = Program { loc :: SourceLocation, body :: [Statement] }
             deriving (Eq, Show)

instance FromJSON Program where
  parseJSON (Object o') = p' o'
    where p' o = Program <$>
               o .: "loc" <*>
               o .: "body" --cases [node "Program" Program <*> "body"]
  parseJSON _ = mzero
  
data Function = Function {funcId :: Maybe Identifier
                         ,funcParams :: [Pattern]
                         ,funcDefaults :: [Expression]
                         ,funcRest :: Maybe Identifier
                          -- spidermonkey supports closure expressions, which means body could theoretically also be an Expression
                          -- We don't support it because it isn't standard in ES5.
                          -- See: https://developer.mozilla.org/en-US/docs/Mozilla/Projects/SpiderMonkey/Parser_API#Functions
                         ,funcBody :: Statement 
                         ,funcGenerator :: Bool}
              deriving (Eq, Show)

instance FromJSON Function where
  parseJSON (Object o) = Function <$>
                         o .: "id" <*>
                         o .: "params" <*>
                         o .: "defaults" <*>
                         o .: "rest" <*>
                         o .: "body" <*>
                         o .: "generator"
  parseJSON _ = mzero

-- | The Aeson instance for Either is pretty inuntuitive and
-- unimaginative, so we need to circumvent that
either :: forall a b. (FromJSON a, FromJSON b) => Text -> Node (Either a b)
either name = Node {nodeType = ""
                   ,nodeBuilder = do o <- ask
                                     lift ((Left <$> (o .: name :: Parser a)) <|>
                                           (Right <$> (o .: name :: Parser b)))
                   }

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
               | TryStatement SourceLocation Statement (Maybe CatchClause) [CatchClause] (Maybe Statement)
               | WhileStatement SourceLocation Expression Statement
               | DoWhileStatement SourceLocation Statement Expression
               | ForStatement SourceLocation ForInit (Maybe Expression) (Maybe Expression) Statement
               | ForInStatement SourceLocation (Either VariableDeclaration Expression) Expression Statement Bool
               | ForOfStatement SourceLocation (Either VariableDeclarator Expression) Expression Statement
               | LetStatement SourceLocation [VariableDeclarator] Statement
               | DebuggerStatement SourceLocation
               | FunctionDeclarationStatement SourceLocation Function
               | VariableDeclarationStatement SourceLocation VariableDeclaration
               deriving (Eq, Show)

instance FromJSON Statement where
  parseJSON = parse'
    where parse' =
            cases
            [node "EmptyStatement" EmptyStatement
            ,node "BlockStatement" BlockStatement <*> "body"
            ,node "ExpressionStatement" ExpressionStatement <*> "expression"
            ,node "IfStatement" IfStatement <*> "test" <*> "consequent" <*> "alternate"
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
            ,node "ForInStatement" ForInStatement <*> either "left" <*> "right" <*> "body" <*> "each"
            ,node "ForOfStatement" ForOfStatement <*> either "left" <*> "right" <*> "body"
            ,node "LetStatement" LetStatement <*> "head" <*> "body"
            ,node "DebuggerStatement" DebuggerStatement
            ,node "FunctionDeclaration" FunctionDeclarationStatement <*> liftJSON
            ,node "VariableDeclaration" VariableDeclarationStatement <*> liftJSON
            ]
                 
data ForInit = VarInit VariableDeclaration
             | ExprInit Expression
             | NoInit
             deriving (Eq, Show)

instance FromJSON ForInit where
  parseJSON v = case v of
    (Object _)-> parseJSON v >>= \e -> case e of
                  Left vd  -> return $ VarInit vd
                  Right ex -> return $ ExprInit ex
    Null      -> return $ NoInit
                   

data VariableDeclaration = VariableDeclaration SourceLocation [VariableDeclarator] DeclarationKind
                         deriving (Eq, Show)

instance FromJSON VariableDeclaration where
  parseJSON = cases [node "VariableDeclaration" VariableDeclaration <*>
                     "declarations" <*> "kind"]

data VariableDeclarator = VariableDeclarator SourceLocation Pattern (Maybe Expression)
                        deriving (Eq, Show)

instance FromJSON VariableDeclarator where
  parseJSON = cases [node "VariableDeclarator" VariableDeclarator <*> "id" <*> "init"]

data DeclarationKind = DVar | DLet | DConst
                     deriving (Eq, Show)

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
                | MemberExpression SourceLocation Expression (Either Identifier Expression) Bool
                  
                  -- The following expression types are spidermonkey-specific, so althought it would be nice to parse
                  -- them, they are not supposed to show up in valid ecma262 syntax.
--                | YieldExpression SourceLocation (Maybe Expression)
--                | ComprehensionExpression SourceLocation Expression [ComprehensionBlock] (Maybe Expression)
--                | GeneratorExpression SourceLocation Expression [ComprehensionBlock] (Maybe Expression)
--                | GraphExpression SourceLocation Word32 Literal
--                | LetExpression SourceLocation [VariableDeclarator] Expression
                | LiteralExpression SourceLocation Literal
                | IdentifierExpression SourceLocation Identifier
                deriving (Eq, Show)
                  

instance FromJSON Expression where
  parseJSON =
    cases
    [node "ThisExpression" ThisExpression
    ,node "ArrayExpression" ArrayExpression <*> "elements"
    ,node "ObjectExpression" ObjectExpression <*> "properties"
    ,node "FunctionExpression" FunctionExpression <*> liftJSON
    ,node "ArrowExpression" ArrowExpression <*> liftJSON
    ,node "SequenceExpression" SequenceExpression <*> "expressions"
    ,node "UnaryExpression" UnaryExpression <*> "operator" <*> "prefix" <*> "argument"
    ,node "BinaryExpression" BinaryExpression <*> "operator" <*> "left" <*> "right"
    ,node "AssignmentExpression"  AssignmentExpression <*> "operator" <*> "left" <*> "right"
    ,node "UpdateExpression" UpdateExpression <*> "operator" <*> "argument" <*> "prefix"
    ,node "LogicalExpression" LogicalExpression <*> "operator" <*> "left" <*> "right"
    ,node "ConditionalExpression" ConditionalExpression <*> "test" <*> "alternate" <*> "consequent"
    ,node "NewExpression" NewExpression <*> "callee" <*> "arguments"
    ,node "CallExpression" CallExpression <*> "callee" <*> "arguments"
    ,node "MemberExpression" MemberExpression <*> "object" <*> either "property" <*> "computed"
    ,node "Literal" LiteralExpression <*> liftJSON
    ,node "Identifier" IdentifierExpression <*> liftJSON
    ]

data Property = Property SourceLocation (Either Literal Identifier) Expression PropertyKind
              deriving (Eq, Show)

instance FromJSON Property where
  parseJSON = cases [node "Property" Property <*> either "key" <*> "value" <*> "kind"]

data PropertyKind = PInit | PGet | PSet
                  deriving (Eq, Show)

instance FromJSON PropertyKind where
  parseJSON (String t) = pure $ case t of
                                 "init" -> PInit
                                 "get"  -> PGet
                                 "set"  -> PSet
  parseJSON _ = mzero

data Pattern = ObjectPattern SourceLocation [PatternProperty]
             | ArrayPattern SourceLocation [Maybe Pattern]
             | IdentifierPattern SourceLocation Identifier
             deriving (Eq, Show)

instance FromJSON Pattern where
  parseJSON = cases
              [node "ObjectPattern" ObjectPattern <*> "properties"
              ,node "ArrayPattern" ArrayPattern <*> "elements"
              ,node "Identifier" IdentifierPattern <*> liftJSON
              ]

data PatternProperty = PatternProperty (Either Literal Identifier) Pattern
                     deriving (Eq, Show)

instance FromJSON PatternProperty where
  parseJSON (Object o) = PatternProperty <$>
                         ((Left <$> (o .: "key" :: Parser Literal)) <|>
                          (Right <$> (o .: "key" :: Parser Identifier))) <*>
                         o .: "value"
  parseJSON _          = mzero

data SwitchCase = SwitchCase SourceLocation (Maybe Expression) [Statement]
                deriving (Eq, Show)

instance FromJSON SwitchCase where
  parseJSON = cases [node "SwitchCase" SwitchCase <*> "test" <*> "consequent"]

data CatchClause = CatchClause SourceLocation Pattern (Maybe Expression) Statement
                 deriving (Eq, Show)

instance FromJSON CatchClause where
  parseJSON = cases [node "CatchClause" CatchClause <*> "param" <*> "guard" <*> "body"]

data ComprehensionBlock = ComprehensionBlock SourceLocation Pattern Expression Bool
                        deriving (Eq, Show)

instance FromJSON ComprehensionBlock where
  parseJSON = cases [node "ComprehensionBlock" ComprehensionBlock <*> "left" <*> "right" <*> "each"]

data Identifier = Identifier SourceLocation Text
                deriving (Eq, Show)

instance FromJSON Identifier where
  parseJSON = cases [node "Identifier" Identifier <*> "name"]

data Literal = LString SourceLocation Text
             | LBool SourceLocation Bool
             | LNull SourceLocation
             | LNumber SourceLocation Scientific
 --          | RegExp SourceLocation String
             deriving (Eq, Show)

instance FromJSON Literal where
  parseJSON (Object o) = do ty <- getType o
                            unless (ty == "Literal") mzero
                            loc' <- o .: "loc"
                            v <- o .: "value"
                            return $ case v of
                             String s -> LString loc' s
                             Bool   b -> LBool loc' b
                             Null     -> LNull loc'
                             Number n -> LNumber loc' n
                             
  parseJSON _ = mzero

enum :: [(Text, a)] -> Value -> Parser a
enum m = let mp = Map.fromList m
         in \v -> case v of
                   String t -> case Map.lookup t mp of
                                Just a -> return a
                                Nothing -> mzero
                   _        -> mzero

data UnaryOperator = (:.-:) | (:.+:) | (:!:) | (:~:) | Typeof | Void | Delete
                   deriving (Eq, Show)

instance FromJSON UnaryOperator where
  parseJSON = enum [("-", (:.-:)), ("+", (:.+:)), ("!", (:!:)), ("~", (:~:)), ("typeof", Typeof), ("void", Void), ("delete", Delete)]

data BinaryOperator = (:==:) | (:!=:) | (:===:) | (:!==:)
                    | (:<:) | (:<=:) | (:>:) | (:>=:) | (:<<:) | (:>>:)
                    | (:>>>:) | (:+:) | (:-:) | (:*:) | (:/:)
                    | (:%:) | (:|:) | (:^:) | (:&:) | In | Instanceof | (:..:)
                    deriving (Eq, Show)

instance FromJSON BinaryOperator where
  parseJSON = enum [("==", (:==:)), ("!=", (:!=:)), ("===", (:===:)), ("!==", (:!==:)), ("<", (:<:)), ("<=", (:<=:)), (">", (:>:)), (">=", (:>=:)), ("<<", (:<<:)), (">>", (:>>:)), (">>>", (:>>>:)), ("+", (:+:)), ("-", (:-:)), ("*", (:*:)), ("/", (:/:)), ("%", (:%:)), ("|", (:|:)), ("^", (:^:)), ("&", (:&:)), ("in", In), ("instanceof", Instanceof), ("..", (:..:))]

data LogicalOperator = (:||:) | (:&&:)
                     deriving (Eq, Show)

instance FromJSON LogicalOperator where
  parseJSON = enum [("||", (:||:)), ("&&", (:&&:))]

data AssignmentOperator = (:=:) | (:+=:) | (:-=:) | (:*=:) | (:/=:) | (:%=:)
                        | (:<<=:) | (:>>=:) | (:>>>=:) | (:|=:) | (:^=:) | (:&=:)
                        deriving (Eq, Show)

instance FromJSON AssignmentOperator where
  parseJSON = enum [("=", (:=:)), ("+=", (:+=:)), ("-=", (:-=:)), ("*=", (:*=:)), ("/=", (:/=:)), ("%=", (:%=:)), ("<<=", (:<<=:)), (">>=", (:>>=:)), (">>>=", (:>>>=:)), ("|=", (:|=:)), ("^=", (:^=:)), ("&=", (:&=:))]

data UpdateOperator = (:++:) | (:--:)
                    deriving (Eq, Show)
                             
instance FromJSON UpdateOperator where
  parseJSON = enum [("++", (:++:)), ("--", (:--:))]
