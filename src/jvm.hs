module JVM
    ( PoolConstant(..)
    , FieldInfo(..)
    , MethodInfo(..)
    , ExceptionInfo(..)
    , AttributeInfo(..)
    , ClassFile(..)
    , getClassBytes
    , JVMOpcode(..)
    , getIns
    ) where

import Data.Bits
import Data.Word
import Data.ByteString hiding (map, concat)

type UInt8 = Word8
type UInt16 = Word16
type UInt32 = Word32

type PoolIndex = UInt16

data PoolConstant 
    = StringConstant String
    | ClassRef PoolIndex
    | StringRef PoolIndex
    | FieldRef PoolIndex PoolIndex
    | MethodRef PoolIndex PoolIndex
    | NameAndType PoolIndex PoolIndex

data FieldInfo = FieldInfo
    { fieldAccessFlags     :: UInt16
    , fieldName            :: PoolIndex
    , fieldDescriptor      :: PoolIndex
    , fieldAttributes      :: [AttributeInfo]
    }

data MethodInfo = MethodInfo
    { methodAccessFlags     :: UInt16
    , methodName            :: PoolIndex
    , methodDescriptor      :: PoolIndex
    , methodAttributes      :: [AttributeInfo]
    }

data ExceptionInfo = ExceptionInfo
    { exceptionStartPc   :: UInt16
    , exceptionEndPc     :: UInt16
    , exceptionHandlerPc :: UInt16
    , exceptionCatchType :: UInt16
    }

data AttributeInfo 
    = AttributeInfo
        { attributeName   :: PoolIndex
        , attributeLength :: UInt32
        , attributeInfo   :: [UInt8]
        }
    | CodeAttributeInfo
        { attributeName       :: PoolIndex
        , attributeLength     :: UInt32
        , codeMaxStack        :: UInt16
        , codeMaxLocals       :: UInt16
        , code                :: [UInt8]
        , codeExceptions      :: [ExceptionInfo]
        , codeAttributes      :: [AttributeInfo]
        }

data ClassFile = ClassFile
    { magicNumber     :: [UInt8]
    , versionMinor    :: UInt16
    , versionMajor    :: UInt16
    , constantPool    :: [PoolConstant]
    , accessFlags     :: UInt16
    , thisClass       :: PoolIndex
    , superClass      :: PoolIndex
    , interfaces      :: [PoolIndex]
    , fields          :: [FieldInfo]
    , methods         :: [MethodInfo]   
    , attributes      :: [AttributeInfo]
    }

getByte b num = fromIntegral $ (shiftL num (8 * b)) .&. 0xFF

getUint16Len :: [a] -> UInt16
getUint16Len a = fromIntegral (Prelude.length a)

getUint32Len :: [a] -> UInt32
getUint32Len a = fromIntegral (Prelude.length a)

stringToBytes :: String -> [UInt8]
stringToBytes str = map (toEnum . fromEnum) str 

uint8ToBytes :: UInt8 -> [UInt8]
uint8ToBytes i = [i]

uint16ToBytes :: UInt16 -> [UInt8]
uint16ToBytes i = 
    uint8ToBytes (getByte 1 i) ++ 
    uint8ToBytes (getByte 0 i)

uint32ToBytes :: UInt32 -> [UInt8]
uint32ToBytes i =
    uint8ToBytes (getByte 3 i) ++
    uint8ToBytes (getByte 2 i) ++ 
    uint8ToBytes (getByte 1 i) ++ 
    uint8ToBytes (getByte 0 i)

tableToBytes :: (a -> [UInt8]) -> [a] -> [UInt8]
tableToBytes f l = concat (map f l)

poolConstantToBytes :: PoolConstant -> [UInt8]
poolConstantToBytes (StringConstant str)  = uint8ToBytes 1 ++ uint16ToBytes (getUint16Len str) ++ stringToBytes str
poolConstantToBytes (ClassRef si)         = uint8ToBytes 7 ++ uint16ToBytes si
poolConstantToBytes (StringRef si)        = uint8ToBytes 8 ++ uint16ToBytes si
poolConstantToBytes (FieldRef c ntd)      = uint8ToBytes 9 ++ uint16ToBytes c ++ uint16ToBytes ntd
poolConstantToBytes (MethodRef c ntd)     = uint8ToBytes 10 ++ uint16ToBytes c ++ uint16ToBytes ntd
poolConstantToBytes (NameAndType nsi tdi) = uint8ToBytes 12 ++ uint16ToBytes nsi ++ uint16ToBytes tdi

fieldToBytes :: FieldInfo -> [UInt8]
fieldToBytes f = 
    uint16ToBytes (fieldAccessFlags f) ++
    uint16ToBytes (fieldName f) ++
    uint16ToBytes (fieldDescriptor f) ++
    uint16ToBytes (getUint16Len attributes) ++
    tableToBytes attributeToBytes attributes 
    where
        attributes = fieldAttributes f

methodToBytes :: MethodInfo -> [UInt8]
methodToBytes m = 
    uint16ToBytes (methodAccessFlags m) ++
    uint16ToBytes (methodName m) ++
    uint16ToBytes (methodDescriptor m) ++
    uint16ToBytes (getUint16Len attributes) ++
    tableToBytes attributeToBytes attributes
    where
        attributes = methodAttributes m


        {-data ExceptionInfo = ExceptionInfo
    { exceptionStartPc   :: UInt16
    , exceptionEndPc     :: UInt16
    , exceptionHandlerPc :: UInt16
    , exceptionCatchType :: UInt16
    }-}

--TODO take care of codeattribute
attributeToBytes :: AttributeInfo -> [UInt8]
attributeToBytes a@(AttributeInfo{}) =
    uint16ToBytes (attributeName a) ++
    uint32ToBytes (attributeLength a) ++
    tableToBytes uint8ToBytes (attributeInfo a)
attributeToBytes a@(CodeAttributeInfo{}) =
    uint16ToBytes (attributeName a) ++
    uint32ToBytes (attributeLength a) ++
    uint16ToBytes (codeMaxStack a) ++
    uint16ToBytes (codeMaxLocals a) ++
    uint32ToBytes (getUint32Len (code a)) ++
    tableToBytes uint8ToBytes (code a) ++
    uint16ToBytes (getUint16Len (codeExceptions a)) ++
    tableToBytes exceptionToBytes (codeExceptions a) ++
    uint16ToBytes (getUint16Len (codeAttributes a)) ++
    tableToBytes attributeToBytes (codeAttributes a)

exceptionToBytes :: ExceptionInfo -> [UInt8]
exceptionToBytes e =
    uint16ToBytes (exceptionStartPc e) ++
    uint16ToBytes (exceptionEndPc e) ++
    uint16ToBytes (exceptionHandlerPc e) ++
    uint16ToBytes (exceptionCatchType e)


getClassBytes :: ClassFile -> ByteString
getClassBytes cl = 
    pack (
        tableToBytes uint8ToBytes (magicNumber cl) ++
        uint16ToBytes (versionMinor cl) ++
        uint16ToBytes (versionMajor cl) ++
        uint16ToBytes ((getUint16Len (constantPool cl)) + 1) ++
        tableToBytes poolConstantToBytes (constantPool cl) ++
        uint16ToBytes (accessFlags cl) ++
        uint16ToBytes (thisClass cl) ++
        uint16ToBytes (superClass cl) ++
        uint16ToBytes (getUint16Len (interfaces cl)) ++
        tableToBytes uint16ToBytes (interfaces cl) ++
        uint16ToBytes (getUint16Len (fields cl)) ++
        tableToBytes fieldToBytes (fields cl) ++
        uint16ToBytes (getUint16Len (methods cl)) ++
        tableToBytes methodToBytes (methods cl) ++
        uint16ToBytes (getUint16Len (attributes cl)) ++
        tableToBytes attributeToBytes (attributes cl)
    )

data JVMOpcode = JVM_aaload | JVM_aastore

-- per https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings
getIns :: JVMOpcode -> Int
getIns op = case op of
    JVM_aaload  -> 0x32
    JVM_aastore -> 0x53