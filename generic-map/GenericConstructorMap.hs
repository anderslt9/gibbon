{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- A constructor witness is different from a value-level function.  Generic
-- metadata is used to reject names that are not constructors of the datatype.
module GenericConstructorMap where

import Data.Kind (Constraint, Type)
import GHC.Generics hiding (Constructor)
import GHC.TypeLits
    ( TypeError, Symbol, ErrorMessage(ShowType, (:<>:), Text) )
import Unsafe.Coerce (unsafeCoerce)

-- import Control.Exception (SomeException, try, evaluate)

-- helper typeclass for appending 2 lists of types together
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[] ys = ys
    Append (x ': xs) ys = x ': Append xs ys

----------------------------------------------------------------------------------
------------------------ Ensure constructor a within type ------------------------ 
----------------------------------------------------------------------------------

-- list of all constructors given data type
type family ConstructorNames (rep :: Type -> Type) :: [Symbol] where
    ConstructorNames (M1 D _ rep) = ConstructorNames rep
    ConstructorNames (left :+: right) = Append (ConstructorNames left) (ConstructorNames right)
    ConstructorNames (M1 C ('MetaCons name _ _) fields) = '[name]

type family HasConstructor (name :: Symbol) (names :: [Symbol]) :: Constraint where
    HasConstructor name (name ': names) = ()
    HasConstructor name (other ': names) = HasConstructor name names
    HasConstructor name '[] = TypeError ( 'Text "The datatype has no constructor named " ':<>: 'ShowType name )

type ConstructorsOf a = ConstructorNames (Rep a)

data Constructor (a :: Type) (name :: Symbol) where
    Constructor :: (Generic a, HasConstructor name (ConstructorsOf a)) => Constructor a name

constructor :: forall a name. (Generic a, HasConstructor name (ConstructorsOf a)) => Constructor a name
constructor = Constructor

type family ConstructorRep (name :: Symbol) (rep :: Type -> Type) :: Type -> Type where
    ConstructorRep name (M1 D _ rep) = ConstructorRep name rep
    ConstructorRep name rep = UnwrapConstructorRep (FindConstructorRep name rep)

type family FindConstructorRep (name :: Symbol) (rep :: Type -> Type) :: Maybe (Type -> Type) where
    FindConstructorRep name (left :+: right) = ChooseConstructorRep (FindConstructorRep name left) (FindConstructorRep name right)
    FindConstructorRep name (M1 C ('MetaCons name _ _) fields) = 'Just fields
    FindConstructorRep name (M1 C ('MetaCons other _ _) _) = 'Nothing

type family ChooseConstructorRep (left :: Maybe (Type -> Type)) (right :: Maybe (Type -> Type)) :: Maybe (Type -> Type) where
    ChooseConstructorRep ('Just fields) right = 'Just fields
    ChooseConstructorRep 'Nothing ('Just fields) = 'Just fields
    ChooseConstructorRep 'Nothing 'Nothing = 'Nothing

type family UnwrapConstructorRep (found :: Maybe (Type -> Type)) :: Type -> Type where
    UnwrapConstructorRep ('Just fields) = fields
    UnwrapConstructorRep 'Nothing = TypeError ('Text "Constructor name was not found in the Generic representation.")

type family IsRecursive (root :: Type) (field :: Type) :: Bool where
    IsRecursive root root = 'True
    IsRecursive root field = 'False

type family Shape (root :: Type) (rep :: Type -> Type) :: [Bool] where
    Shape root U1 = '[]
    Shape root (M1 S _ field) = Shape root field
    Shape root (K1 R field) = '[IsRecursive root field]
    Shape root (left :*: right) = Append (Shape root left) (Shape root right)

type ConstructorShape a name = Shape a (ConstructorRep name (Rep a))

type family PayloadTypes (root :: Type) (rep :: Type -> Type) :: [Type] where
    PayloadTypes root U1 = '[]
    PayloadTypes root (M1 S _ field) = PayloadTypes root field
    PayloadTypes root (K1 R root) = '[]
    PayloadTypes root (K1 R field) = '[field]
    PayloadTypes root (left :*: right) = Append (PayloadTypes root left) (PayloadTypes root right)

type family Payload (types :: [Type]) :: Type where
    Payload '[] = ()
    Payload '[a] = a
    Payload '[a, b] = (a, b)
    Payload '[a, b, c] = (a, b, c)
    Payload '[a, b, c, d] = (a, b, c, d)
    Payload '[a, b, c, d, e] = (a, b, c, d, e)
    Payload '[a, b, c, d, e, f] = (a, b, c, d, e, f)
    Payload '[a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
    Payload types = TypeError ('Text "Payload supports at most seven non-recursive fields.")

-- type family Payload (types :: [Type]) :: (result :: Type) | result -> types where
--     Payload '[] = ()
--     Payload '[a] = a
--     Payload '[a, b] = (a, b)
--     Payload '[a, b, c] = (a, b, c)
--     Payload '[a, b, c, d] = (a, b, c, d)
--     Payload '[a, b, c, d, e] = (a, b, c, d, e)
--     Payload '[a, b, c, d, e, f] = (a, b, c, d, e, f)
--     Payload '[a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
--     Payload types = TypeError ('Text "Payload supports at most seven non-recursive fields.")

type ConstructorPayload a name = Payload (PayloadTypes a (ConstructorRep name (Rep a)))

type family SameRecursiveShape (left :: [Bool]) (right :: [Bool]) :: Constraint where
    SameRecursiveShape '[] '[] = ()
    SameRecursiveShape xs ('False ': ys) = SameRecursiveShape xs ys
    SameRecursiveShape ('False ': xs) ys = SameRecursiveShape xs ys
    SameRecursiveShape ('True ': xs) ('True ': ys) = SameRecursiveShape xs ys
    SameRecursiveShape left right = TypeError ('Text "Input and output constructors have different recursive shapes.")

data Transform a b (ca :: Symbol) (cb :: Symbol) where
    Transform
        :: ( Generic a, Generic b
           , HasConstructor ca (ConstructorsOf a), HasConstructor cb (ConstructorsOf b)
           , SameRecursiveShape (ConstructorShape a ca) (ConstructorShape b cb)
           , SelectConstructor ca (Rep a) (Rep a)
           , PayloadCodec (PayloadTypes a (ConstructorRep ca (Rep a)))
           , PayloadCodec (PayloadTypes b (ConstructorRep cb (Rep b)))
           , GFields a b (ConstructorRep ca (Rep a)) (ConstructorRep cb (Rep b))
           , BuildConstructor a b ca cb (Rep a) (Rep b) (Rep b)
           )
        => Constructor a ca -> Constructor b cb -> (ConstructorPayload a ca -> ConstructorPayload b cb) -> Transform a b ca cb

transform
    :: ( Generic a, Generic b
       , HasConstructor ca (ConstructorsOf a), HasConstructor cb (ConstructorsOf b)
       , SameRecursiveShape (ConstructorShape a ca) (ConstructorShape b cb)
         , SelectConstructor ca (Rep a) (Rep a)
         , PayloadCodec (PayloadTypes a (ConstructorRep ca (Rep a)))
         , PayloadCodec (PayloadTypes b (ConstructorRep cb (Rep b)))
        , GFields a b (ConstructorRep ca (Rep a)) (ConstructorRep cb (Rep b))
         , BuildConstructor a b ca cb (Rep a) (Rep b) (Rep b)
       )
    => Constructor a ca -> Constructor b cb -> (ConstructorPayload a ca -> ConstructorPayload b cb) -> Transform a b ca cb
transform = Transform

----------------------------------------------------------------------------------
------ Define TransformList for checking overall transformation is complete ------
----------------------------------------------------------------------------------

data TransformList a b (inputs :: [Symbol]) where
    TNil :: TransformList a b '[]
    TCons :: Transform a b ca cb -> TransformList a b cas -> TransformList a b (ca ': cas)

infixr 5 |+|
(|+|) :: Transform a b ca cb -> TransformList a b cas -> TransformList a b (ca ': cas)
(|+|) = TCons

tNil :: TransformList a b '[]
tNil = TNil

----------------------------------------------------------------------------------
------------ Check that input constructors unique and all are covered ------------
----------------------------------------------------------------------------------

-- checks if constructor not present in list of constructors (opposite of HasConstructor)
type family NotPresent (name :: Symbol) (names :: [Symbol]) :: Constraint where
    NotPresent name '[] = ()
    NotPresent name (name ': names) = TypeError ( 'Text "Duplicate input constructor " ':<>: 'ShowType name )
    NotPresent name (other ': names) = NotPresent name names

-- ensures all constructors are unique
type family Unique (names :: [Symbol]) :: Constraint where
    Unique '[] = ()
    Unique (name ': names) = (NotPresent name names, Unique names)

-- removes specified name from list
type family Remove (name :: Symbol) (names :: [Symbol]) :: [Symbol] where
    Remove name (name ': names) = names
    Remove name (other ': names) = other ': Remove name names
    Remove name '[] = TypeError ( 'Text "The transform list is missing input constructor " ':<>: 'ShowType name )

-- checks if transformations cover all input constructors
type family SameInputConstructors (inputs :: [Symbol]) (constructors :: [Symbol]) :: Constraint where
    SameInputConstructors '[] '[] = ()
    SameInputConstructors '[] constructors = TypeError ( 'Text "The transform list contains too few input constructors." )
    SameInputConstructors (input ': inputs) constructors = ( HasConstructor input constructors, NotPresent input inputs, SameInputConstructors inputs (Remove input constructors))

----------------------------------------------------------------------------------
---------------------------- Define overall Transforms ---------------------------
----------------------------------------------------------------------------------

-- data Transforms' a b (inputs :: [Symbol]) where
--     Transforms'
--         :: (Generic a, Generic b, Unique inputs,
--             SameInputConstructors inputs (ConstructorsOf a))
--         => TransformList a b inputs
--         -> Transforms' a b inputs

data Transforms a b where
    Transforms
        :: (Generic a, Generic b, Unique inputs,
            SameInputConstructors inputs (ConstructorsOf a))
        => TransformList a b inputs
        -> Transforms a b



----------------------------------------------------------------------------------
----------------------------- Extract transform info -----------------------------
----------------------------------------------------------------------------------

withTransformList 
    :: Transforms a b 
    -> (forall inputs. (Unique inputs, SameInputConstructors inputs (ConstructorsOf a)) 
        => TransformList a b inputs -> r) 
    -> r
withTransformList (Transforms list) handler = handler list

withTransform
    :: TransformList a b inputs
    -> (forall ca cb. Transform a b ca cb -> r)
    -> r
withTransform (TCons t _) handler = handler t 
withTransform TNil _ = error "No matching constructor found."


-- withTransform2
--     :: Transform 

-- withNextTransformList
--     :: TransformList a b inputs
--     -> (Transform a b ca cb -> TransformList a b cas -> r)
--     -> r
-- withNextTransformList (TCons t l) handler = handler t l
-- withNextTransformList TNil _ = TNil

withFunc 
    :: Transform a b ca cb
    -> Constructor a ca
    -> Constructor b cb
    -> ((ConstructorPayload a ca -> ConstructorPayload b cb) -> r)
    -> r
withFunc (Transform _ _ f) _ _ handler = handler f


data TransformWrapper a b where
    TransformWrapper :: Transform a b ca cb -> TransformWrapper a b


-- class CollectFields root rep where
--     type PayloadFields root rep :: [Type]

--     collectFields :: rep x -> Payload (PayloadFields root rep)

-- class RebuildFields root rep where
--     rebuildFields :: Payload (PayloadFields root rep) -> [root] -> rep x



-- instance CollectFields root U1 where
--     type PayloadFields root U1 = '[]
--     collectFields U1 = ()

-- instance RebuildFields root U1 where
--     rebuildFields () _ = U1


-- instance CollectFields root (M1 S meta fields) where
--     type PayloadFields root (M1 S meta fields) = PayloadFields root fields
--     collectFields (M1 fields) = collectFields fields


-- store heterogeneous fields
data HList (xs :: [Type]) where
    HNil :: HList '[]
    HCons :: x -> HList xs -> HList (x ': xs)

-- transform between HList and Payload
class PayloadCodec (xs :: [Type]) where
    toHList :: Payload xs -> HList xs
    fromHList :: HList xs -> Payload xs

instance PayloadCodec '[] where
    toHList () = HNil
    fromHList HNil = ()

instance PayloadCodec '[a] where
    toHList value = HCons value HNil
    fromHList (HCons value HNil) = value

instance PayloadCodec '[a, b] where
    toHList (a, b) = HCons a (HCons b HNil)
    fromHList (HCons a (HCons b HNil)) = (a, b)

instance PayloadCodec '[a, b, c] where
    toHList (a, b, c) = HCons a (HCons b (HCons c HNil))
    fromHList (HCons a (HCons b (HCons c HNil))) = (a, b, c)

instance PayloadCodec '[a, b, c, d] where
    toHList (a, b, c, d) = HCons a (HCons b (HCons c (HCons d HNil)))
    fromHList (HCons a (HCons b (HCons c (HCons d HNil)))) = (a, b, c, d)

instance PayloadCodec '[a, b, c, d, e] where
    toHList (a, b, c, d, e) = HCons a (HCons b (HCons c (HCons d (HCons e HNil))))
    fromHList (HCons a (HCons b (HCons c (HCons d (HCons e HNil))))) = (a, b, c, d, e)

instance PayloadCodec '[a, b, c, d, e, f] where
    toHList (a, b, c, d, e, f) = HCons a (HCons b (HCons c (HCons d (HCons e (HCons f HNil)))))
    fromHList (HCons a (HCons b (HCons c (HCons d (HCons e (HCons f HNil)))))) = (a, b, c, d, e, f)

instance PayloadCodec '[a, b, c, d, e, f, g] where
    toHList (a, b, c, d, e, f, g) = HCons a (HCons b (HCons c (HCons d (HCons e (HCons f (HCons g HNil))))))
    fromHList (HCons a (HCons b (HCons c (HCons d (HCons e (HCons f (HCons g HNil))))))) = (a, b, c, d, e, f, g)

class SplitHList (xs :: [Type]) (ys :: [Type]) where
    splitHList :: HList (Append xs ys) -> (HList xs, HList ys)

instance SplitHList '[] ys where
    splitHList values = (HNil, values)

instance SplitHList xs ys => SplitHList (x ': xs) ys where
    splitHList (HCons value rest) =
        let (left, right) = splitHList @xs @ys rest
        in (HCons value left, right)

data Count = Zero | Succ Count

type family RecursiveFields (root :: Type) (rep :: Type -> Type) :: [Type] where
    RecursiveFields root U1 = '[]
    RecursiveFields root (M1 S _ field) = RecursiveFields root field
    RecursiveFields root (K1 R root) = '[root]
    RecursiveFields root (K1 R field) = '[]
    RecursiveFields root (left :*: right) = Append (RecursiveFields root left) (RecursiveFields root right)

type family RecursiveCount (root :: Type) (rep :: Type -> Type) :: Count where
    RecursiveCount root U1 = 'Zero
    RecursiveCount root (M1 S _ field) = RecursiveCount root field
    RecursiveCount root (K1 R root) = 'Succ 'Zero
    RecursiveCount root (K1 R field) = 'Zero
    RecursiveCount root (left :*: right) = AddCount (RecursiveCount root left) (RecursiveCount root right)

type family AddCount (left :: Count) (right :: Count) :: Count where
    AddCount 'Zero right = right
    AddCount ('Succ left) right = 'Succ (AddCount left right)

class SplitHListN (count :: Count) xs where
    splitHListN :: HList xs -> (HList (Take count xs), HList (Drop count xs))

type family Take (count :: Count) (xs :: [Type]) :: [Type] where
    Take 'Zero xs = '[]
    Take ('Succ count) (x ': xs) = x ': Take count xs

type family Drop (count :: Count) (xs :: [Type]) :: [Type] where
    Drop 'Zero xs = xs
    Drop ('Succ count) (x ': xs) = Drop count xs

instance SplitHListN 'Zero xs where
    splitHListN values = (HNil, values)

instance SplitHListN count xs => SplitHListN ('Succ count) (x ': xs) where
    splitHListN (HCons value rest) =
        let (left, right) = splitHListN @count @xs rest
        in (HCons value left, right)

-- separate payload fields from recursive fields
class ExtractFields root fields where
    extractPayloads :: fields root -> HList (PayloadTypes root fields)
    extractRecursiveValues :: fields root -> HList (RecursiveFields root fields)

instance ExtractFields root U1 where
    extractPayloads U1 = HNil
    extractRecursiveValues U1 = HNil

instance ExtractFields root fields => ExtractFields root (M1 S meta fields) where
    extractPayloads (M1 fields) = extractPayloads @root @fields fields
    extractRecursiveValues (M1 fields) = extractRecursiveValues @root @fields fields

instance {-# OVERLAPPING #-} ExtractFields root (K1 R root) where
    extractPayloads (K1 _) = HNil
    extractRecursiveValues (K1 value) = HCons value HNil

instance {-# OVERLAPPABLE #-} ExtractFields root (K1 R field) where
    extractPayloads (K1 value) = unsafeCoerce (HCons value HNil)
    
    -- unsafeCoerce used to avoid type error due to type ambiguity
    extractRecursiveValues (K1 _) = unsafeCoerce HNil

instance (ExtractFields root left, ExtractFields root right) => ExtractFields root (left :*: right) where
    extractPayloads (left :*: right) = appendHList (extractPayloads left) (extractPayloads right)
    extractRecursiveValues (left :*: right) = appendHList (extractRecursiveValues left) (extractRecursiveValues right)

-- builds output fields from payloads and recursive fields
class BuildFields root target inputRec fields where
    buildOutputFields :: (root -> target)
        -> HList (PayloadTypes target fields)
        -> HList inputRec
        -> fields target

instance BuildFields root target inputRec U1 where
    buildOutputFields _ HNil HNil = U1

instance BuildFields root target inputRec fields => BuildFields root target inputRec (M1 S meta fields) where
    buildOutputFields mapRoot payloads recursives = M1 (buildOutputFields @root @target @inputRec @fields mapRoot payloads recursives)

instance {-# OVERLAPPING #-} BuildFields root target (root ': rest) (K1 R target) where
    buildOutputFields mapRoot HNil (HCons value _) = K1 (mapRoot value)

instance {-# OVERLAPPABLE #-} BuildFields root target inputRec (K1 R field) where
    -- unsafeCoerce used to avoid type error due to type ambiguity
    buildOutputFields _ (HCons value HNil) _ = unsafeCoerce (K1 value)

instance (BuildFields root target (Take (RecursiveCount target left) inputRec) left,
          BuildFields root target (Drop (RecursiveCount target left) inputRec) right,
          SplitHList (PayloadTypes target left) (PayloadTypes target right),
          SplitHListN (RecursiveCount target left) inputRec)
    => BuildFields root target inputRec (left :*: right) where
    buildOutputFields mapRoot payloads recursives =
        let (leftPayloads, rightPayloads) = splitHList @(PayloadTypes target left) @(PayloadTypes target right) payloads
            (leftRecursives, rightRecursives) = splitHListN @(RecursiveCount target left) recursives
          in buildOutputFields @root @target @(Take (RecursiveCount target left) inputRec) @left mapRoot leftPayloads leftRecursives
              :*: buildOutputFields @root @target @(Drop (RecursiveCount target left) inputRec) @right mapRoot rightPayloads rightRecursives

class GFields root target fieldsIn fieldsOut where
    extractFields :: fieldsIn root -> HList (PayloadTypes root fieldsIn)
    extractRecursives :: fieldsIn root -> HList (RecursiveFields root fieldsIn)
    buildFields :: (root -> target) -> HList (PayloadTypes target fieldsOut)
        -> HList (RecursiveFields root fieldsIn) -> fieldsOut target

instance (ExtractFields root fieldsIn, BuildFields root target (RecursiveFields root fieldsIn) fieldsOut)
    => GFields root target fieldsIn fieldsOut where
    extractFields = extractPayloads
    extractRecursives = extractRecursiveValues
    buildFields = buildOutputFields @root @target @(RecursiveFields root fieldsIn) @fieldsOut

appendHList :: HList xs -> HList ys -> HList (Append xs ys)
appendHList HNil right = right
appendHList (HCons value rest) right = HCons value (appendHList rest right)

class SelectConstructor (name :: Symbol) full rep where
    selectConstructor :: rep x -> Maybe (ConstructorRep name full x)

instance SelectConstructor name full rep
    => SelectConstructor name full (M1 D meta rep) where
    selectConstructor (M1 value) = selectConstructor @name @full value

-- select constructor from left or right
instance (SelectConstructor name full left,
          SelectConstructor name full right)
    => SelectConstructor name full (left :+: right) where
    selectConstructor (L1 value) = selectConstructor @name @full value
    selectConstructor (R1 value) = selectConstructor @name @full value

-- check if constructor matches name
instance {-# OVERLAPPING #-}
    (ConstructorRep name full ~ fields)
        => SelectConstructor name full
                (M1 C ('MetaCons name fixity selectors) fields) where
    selectConstructor (M1 value) = Just value

-- constructor doesn't match name, return nothing
instance {-# OVERLAPPABLE #-}
    SelectConstructor name full (M1 C metadata fields) where
    selectConstructor _ = Nothing

class BuildConstructor source target inputName outputName inputFull outputFull rep where
    buildConstructor
        :: (source -> target)
        -> HList (PayloadTypes target (ConstructorRep outputName outputFull))
        -> HList (RecursiveFields source (ConstructorRep inputName inputFull))
        -> ConstructorRep inputName inputFull source
        -> Maybe (rep target)

instance BuildConstructor source target inputName outputName inputFull outputFull rep
    => BuildConstructor source target inputName outputName inputFull outputFull (M1 D meta rep) where
    buildConstructor mapRoot values recursives input =
        M1 <$> buildConstructor @source @target @inputName @outputName @inputFull @outputFull mapRoot values recursives input

-- build constructor from left or right, return first successful match
instance (BuildConstructor source target inputName outputName inputFull outputFull left,
          BuildConstructor source target inputName outputName inputFull outputFull right)
    => BuildConstructor source target inputName outputName inputFull outputFull (left :+: right) where
    buildConstructor mapRoot values recursives input =
        case buildConstructor @source @target @inputName @outputName @inputFull @outputFull mapRoot values recursives input of
            Just result -> Just (L1 result)
            Nothing -> R1 <$> buildConstructor @source @target @inputName @outputName @inputFull @outputFull mapRoot values recursives input

-- build constructor if constructor matches name, otherwise return Nothing
instance {-# OVERLAPPING #-}
    (ConstructorRep outputName outputFull ~ fields,
    GFields source target (ConstructorRep inputName inputFull) fields,
    PayloadCodec (PayloadTypes target fields))
    => BuildConstructor source target inputName outputName inputFull outputFull
        (M1 C ('MetaCons outputName fixity selectors) fields) where
    buildConstructor mapRoot values recursives input =
        Just (M1 (buildFields @source @target
            @(ConstructorRep inputName inputFull) @fields
            mapRoot values recursives))

-- build constructor if constructor doesn't match name, return Nothing
instance {-# OVERLAPPABLE #-}
    BuildConstructor source target inputName outputName inputFull outputFull
    (M1 C metadata fields) where
    buildConstructor _ _ _ _ = Nothing

getTransforms :: TransformList a b inputs -> [TransformWrapper a b]
getTransforms TNil = []
getTransforms (TCons t l) = TransformWrapper t : getTransforms l

applyTransform :: forall a b. (Generic a, Generic b) => [TransformWrapper a b] -> a -> b
applyTransform transforms = applyTransformFrom transforms transforms

applyTransformFrom :: forall a b. (Generic a, Generic b)
    => [TransformWrapper a b] -> [TransformWrapper a b] -> a -> b
applyTransformFrom _ [] _ = error "No matching constructor found."
applyTransformFrom allTransforms (TransformWrapper
        (Transform
            (Constructor :: Constructor a ca)
            (Constructor :: Constructor b cb)
            payloadFunction)
        : rest)
    input =
    
    -- match correct input constructor
    case selectConstructor @ca @(Rep a) @(Rep a) (from input) of
        -- wrong input constructor, check rest
        Nothing -> applyTransformFrom allTransforms rest input
        
        -- correct input constructor, continue
        Just inputFields ->
            let payload = fromHList
                    (extractFields @a @b
                        @(ConstructorRep ca (Rep a))
                        @(ConstructorRep cb (Rep b))
                        inputFields)
                recursiveFields = extractRecursives @a @b
                    @(ConstructorRep ca (Rep a))
                    @(ConstructorRep cb (Rep b))
                    inputFields
                outputPayload = payloadFunction payload
                outputFields = toHList outputPayload
                mapRecursive = applyTransform allTransforms
            in case buildConstructor
                    @a @b @ca @cb @(Rep a) @(Rep b)
                    mapRecursive outputFields recursiveFields inputFields of
                Just output -> to output
                Nothing -> error "The output constructor could not be rebuilt."

mapGeneric :: (Generic a, Generic b) => Transforms a b -> a -> b
mapGeneric transforms input =
    withTransformList transforms $ \transformList ->
        applyTransform (getTransforms transformList) input


--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------- TESTING --------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------
------------------------------- Testing structures -------------------------------
----------------------------------------------------------------------------------

data TreeITT
    = NodeA Int TreeITT TreeITT
    | EmptyA
    deriving (Generic, Show)

data TreeFTT
    = NodeB Float TreeFTT TreeFTT
    | EmptyB
    deriving (Generic, Show)

data TreeIITT
    = NodeC Int Int TreeIITT TreeIITT
    | EmptyC
    deriving (Generic, Show)

data TreeFFTT
    = NodeD Float Float TreeFFTT TreeFFTT
    | EmptyD
    deriving (Generic, Show)

data TreeITFT
    = NodeE Int TreeITFT Float TreeITFT
    | EmptyE
    deriving (Generic, Show)

data TreeListA
    = TLNodeA TreeListA TreeListA Int
    | TLListA Float TreeListA
    | TLEmptyA
    deriving (Generic, Show)

data TreeListB
    = TLNodeB TreeListB Float TreeListB 
    | TLEmptyB
    | TLListB Int TreeListB Int
    deriving (Generic, Show)

data OtherStruct1
    = OtherI1 Int
    | OtherF1 Float
    deriving (Generic, Show)

data TreeListOther
    = TLOther1 OtherStruct1 TreeListOther TreeListOther
    | TLOther1Empty
    deriving (Generic, Show)

----------------------------------------------------------------------------------
------------------------------- Testing functions --------------------------------
----------------------------------------------------------------------------------

fITTtoFTT :: Int -> Float
fITTtoFTT value = fromIntegral value + 0.5

fIITTtoFFTT :: (Int, Int) -> (Float, Float)
fIITTtoFFTT (x, y) = (fromIntegral x + 0.5, fromIntegral y + 0.5)

fFFTTtoITFT :: (Float, Float) -> (Int, Float)
fFFTTtoITFT (x, y) = (floor x, y + 0.5)

fFFTTtoITFT2 :: (Float, Float) -> (Float, Int)
fFFTTtoITFT2 (x, y) = (x + 0.5, floor y)

fTLAtoTLBNode :: Int -> Float
fTLAtoTLBNode x = fromIntegral x + 0.5

fTLAtoTLBList :: Float -> (Int, Int)
fTLAtoTLBList x = (floor x, floor x)

fTLOtherToFFTT :: OtherStruct1 -> (Float, Float)
fTLOtherToFFTT (OtherI1 x) = (fromIntegral x + 0.5, fromIntegral x + 1.5)
fTLOtherToFFTT (OtherF1 x) = (x + 0.5, x + 1.5)

-- createTransform :: Constructor a b -> Constructor c d -> (e -> f) -> Transforms TreeITT

----------------------------------------------------------------------------------
--------------------------------- Example Passes ---------------------------------
----------------------------------------------------------------------------------
-- We want the following to pass at compile time:
    -- all input constructors present
    -- no duplicate input constructors
    -- correct order of payloads (can only check this via types, if same types, user error)
    -- number of recursive fields between input/output constructor match
    -- payloads and recursive fields can be interspersed
    -- constructors with payloads > 1 will be tuple types
    -- 
type TestName = String
type ADTInput = String
type ADTOutput = String
type FunctionApplied = String
data PassInfo = PassInfo {
    testName :: TestName,
    adtInput :: ADTInput,
    adtOutput :: ADTOutput,
    functionsApplied :: [FunctionApplied]
}

-- most basic example (Int -> Float)
examplePass1 :: Transforms TreeITT TreeFTT
examplePass1 =
    Transforms (   
            transform (constructor @TreeITT @"NodeA") (constructor @TreeFTT @"NodeB") fITTtoFTT
        |+| transform (constructor @TreeITT @"EmptyA") (constructor @TreeFTT @"EmptyB") (\() -> ())
        |+| tNil )

-- how I might actually have it written on frontend:
-- ***note it can imply the type from the constructor, and I don't need to include empty mappings
-- transformList [
--    transform NodeA NodeB fITTtoFTT
-- ]
-- -->
-- TransformList [((VarE "NodeA", PackedTy "TreeITT" []), (VarE "NodeB", PackedTy "TreeFTT" []), VarE "fITTtoFTT"), 
--               ((EmptyA, TreeITT), (EmptyB, TreeFTT), \() -> ())]

pass1 :: PassInfo
pass1 = PassInfo {
    testName = "Basic Example",
    adtInput = "TreeITT = NodeA Int R R | EmptyA",
    adtOutput = "TreeFTT = NodeB Float R R | EmptyA",
    functionsApplied = ["fITTtoFTT :: Int -> Float = fromIntegral value + 0.5"]
}

-- -- multiple length payload (Int, Int) -> (Float, Float)
examplePass2 :: Transforms TreeIITT TreeFFTT
examplePass2 =
    Transforms (   
            transform (constructor @TreeIITT @"NodeC") (constructor @TreeFFTT @"NodeD") fIITTtoFFTT
        |+| transform (constructor @TreeIITT @"EmptyC") (constructor @TreeFFTT @"EmptyD") (\() -> ())
        |+| tNil )
pass2 :: PassInfo
pass2 = PassInfo {
    testName = "Multiple Length Payload",
    adtInput = "TreeIITT = NodeC Int Int R R | EmptyC",
    adtOutput = "TreeFFTT = NodeD Float Float R R | EmptyD",
    functionsApplied = ["fIITTtoFFTT :: (Int, Int) -> (Float, Float) = (fromIntegral x + 0.5, fromIntegral y + 0.5)"]
}


-- -- multiple length payload with different order for recursive fields (Float, Float) -> (Int, Float)
examplePass3 :: Transforms TreeFFTT TreeITFT
examplePass3 =
    Transforms (
            transform (constructor @TreeFFTT @"NodeD") (constructor @TreeITFT @"NodeE") fFFTTtoITFT
        |+| transform (constructor @TreeFFTT @"EmptyD") (constructor @TreeITFT @"EmptyE") (\() -> ())
        |+| tNil )
pass3 :: PassInfo
pass3 = PassInfo {
    testName = "Multiple Length Payload with Different Order for Recursive Fields",
    adtInput = "TreeFFTT = NodeD Float Float R R | EmptyD",
    adtOutput = "TreeITFT = NodeE Int R Float R | EmptyE",
    functionsApplied = ["fFFTTtoITFT :: (Float, Float) -> (Int, Float) = (floor x, y + 0.5)"]
}

-- -- basic transformation with multiple recursive constructors, different orders for fields, different order of constructors, different number non-recursive fields
examplePass4 :: Transforms TreeListA TreeListB
examplePass4 =
    Transforms (
            transform (constructor @TreeListA @"TLNodeA") (constructor @TreeListB @"TLNodeB") fTLAtoTLBNode
        |+| transform (constructor @TreeListA @"TLListA") (constructor @TreeListB @"TLListB") fTLAtoTLBList
        |+| transform (constructor @TreeListA @"TLEmptyA") (constructor @TreeListB @"TLEmptyB") (\() -> ())
        |+| tNil )
pass4 :: PassInfo
pass4 = PassInfo {
    testName = "Basic Transformation with Multiple Recursive Constructors",
    adtInput = "TreeListA = TLNodeA R R Int | TLListA Float R | TLEmptyA",
    adtOutput = "TreeListB = TLNodeB R Float R | TLEmptyB | TLListB Int R Int",
    functionsApplied = ["fTLAtoTLBNode :: Int -> Float = fromIntegral x + 0.5", 
                        "fTLAtoTLBList :: Float -> (Int, Int) = (floor x, floor x)"]
}

examplePass5 :: Transforms TreeListOther TreeFFTT
examplePass5 =
    Transforms (
            transform (constructor @TreeListOther @"TLOther1") (constructor @TreeFFTT @"NodeD") fTLOtherToFFTT
        |+| transform (constructor @TreeListOther @"TLOther1Empty") (constructor @TreeFFTT @"EmptyD") (\() -> ())
        |+| tNil )
pass5 :: PassInfo
pass5 = PassInfo {
    testName = "Basic Transformation with other structure",
    adtInput = "TreeListOther = TLOther1 OtherStruct1 TreeListOther TreeListOther | TLOther1Empty",
    adtOutput = "TreeFFTT = NodeD Float Float R R | EmptyD",
    functionsApplied = ["fTLOtherToFFTT :: OtherStruct1 -> (Float, Float) =\n\t(OtherI1 x) = (fromIntegral x + 0.5, fromIntegral x + 1.5)\n\t(OtherF1 x) = (x + 0.5, x + 1.5)"]
}

----------------------------------------------------------------------------------
--------------------------------- Example Fails ----------------------------------
----------------------------------------------------------------------------------
-- We want the following to fail at compile time:
    -- don't have all input constructors 
    -- duplicate input constructors
    -- wrong order of payloads (can only check this via types, if same types, user error)
    -- number of recursive fields between input/output constructor don't match


-- -- not enough constructors
-- exampleFail1 :: Transforms TreeITT TreeFTT
-- exampleFail1 =
--     Transforms (
--             transform (constructor @TreeITT @"NodeA") (constructor @TreeFTT @"NodeB") fITTtoFTT
--         |+| tNil
--     )

-- -- duplicate input constructor
-- exampleFail2 :: Transforms TreeITT TreeFTT
-- exampleFail2 =
--     Transforms (
--             transform (constructor @TreeITT @"NodeA") (constructor @TreeFTT @"NodeB") fITTtoFTT
--         |+| transform (constructor @TreeITT @"EmptyA") (constructor @TreeFTT @"EmptyB") (\() -> ())
--         |+| transform (constructor @TreeITT @"EmptyA") (constructor @TreeFTT @"EmptyB") (\() -> ())
--         |+| tNil
--     )

-- -- wrong order of payloads (Float, Float) -> (Float, Int)
-- exampleFail3 :: Transforms TreeFFTT TreeITFT
-- exampleFail3 =
--     Transforms (   
--             transform (constructor @TreeFFTT @"NodeD") (constructor @TreeITFT @"NodeE") fFFTTtoITFT2
--         |+| transform (constructor @TreeFFTT @"EmptyD") (constructor @TreeITFT @"EmptyE") (\() -> ())
--         |+| tNil )

-- -- number of recursive fields don't match (NodeA has 2, EmptyB has 0)
-- exampleFail4 :: Transforms TreeITT TreeFTT
-- exampleFail4 =
--     Transforms (   
--             transform (constructor @TreeITT @"NodeA") (constructor @TreeFTT @"EmptyB") (const ())
--         |+| transform (constructor @TreeITT @"EmptyA") (constructor @TreeFTT @"EmptyB") (\() -> ())
--         |+| tNil )

-- These should fail at compile time:
-- badName = constructor @TreeA @"NotAConstructor"
-- badArgument = transform (\x -> x) (constructor @TreeB @"NodeB") fAB

displayTree :: (Show a, Generic a, Show b, Generic b) => a -> Transforms a b -> PassInfo -> IO()
displayTree tree transforms passInfo = do
    putStrLn $ "Running test: " ++ testName passInfo
    putStrLn $ adtInput passInfo ++ " -> " ++ adtOutput passInfo
    putStrLn $ "Functions applied: " ++ show (functionsApplied passInfo)
    let mappedTree = mapGeneric transforms tree
    putStrLn $ show tree ++ " -> \n" ++ show mappedTree ++ "\n"


main :: IO ()
main = let  treeA = NodeA 1 (NodeA 2 EmptyA EmptyA) EmptyA
            treeC = NodeC 1 2 (NodeC 3 4 EmptyC EmptyC) EmptyC
            treeD = NodeD 1.0 2.0 (NodeD 3.0 4.0 EmptyD EmptyD) EmptyD
            treeE = TLNodeA (TLNodeA (TLListA 1.3 (TLNodeA TLEmptyA TLEmptyA 5)) TLEmptyA 5) (TLNodeA TLEmptyA TLEmptyA 6) 1
            treeF = TLOther1 (OtherI1 5) (TLOther1 (OtherF1 3.5) TLOther1Empty TLOther1Empty) TLOther1Empty
       in do
            displayTree treeA examplePass1 pass1 
            displayTree treeC examplePass2 pass2
            displayTree treeD examplePass3 pass3
            displayTree treeE examplePass4 pass4
            displayTree treeF examplePass5 pass5


-- To do all of this, you essentially need to do the following:
-- 1. Get representation (Rep) of the data type using GHC.Generics
-- 2. Extract constructors, fields and user-specified mappings to check for compile time errors
-- 3. At runtime now, it needs to view the representation of the input data type and
--    * extract payloads (turn from Rep into Payload)
--    * extract recursive fields (turn from Rep into list of recursive fields)
-- 4. Apply user-specified functions to payloads which match corresponding constructor
-- 5. Rebuild the output data type according to the structure (Rep) of the input data type
--    * Rebuild each output constructor using list of payloads and recursive fields


-- For the map function itself:
-- 1. Extract list of transformation rules
-- 2. Convert input value to Rep with from
-- 3. Try each rule in order
-- 4. Use selectConstructor to to determine if current input value has rule's input constructor
-- 5. Extract ordinary fields and recursive children
-- 6. Apply payload function
-- 7. Recursively call applyTransform
-- 8. Rebuild with buildConstructor
-- 9. Convert back to output type with to