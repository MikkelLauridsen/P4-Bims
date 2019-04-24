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

import Data.Char
import Data.Bits

class Encodable a where
    toBytes :: a -> [Char]

type UInt8 = Int
type UInt16 = Int
type UInt32 = Int

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
    , fieldAttributesCount :: UInt16
    , fieldAttributes      :: [AttributeInfo]
    }

data MethodInfo = MethodInfo
    { methodAccessFlags     :: UInt16
    , methodName            :: PoolIndex
    , methodDescriptor      :: PoolIndex
    , methodAttributesCount :: UInt16
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
        , codeLength          :: UInt32
        , code                :: [UInt8]
        , codeExceptionsCount :: UInt16
        , codeExceptions      :: [ExceptionInfo]
        , codeAttributesCount :: UInt16
        , codeAttributes      :: [AttributeInfo]
        }

data ClassFile = ClassFile
    { magicNumber     :: [UInt8]
    , versionMinor    :: UInt16
    , versionMajor    :: UInt16
    , cpCount         :: UInt16
    , constantPool    :: [PoolConstant]
    , accessFlags     :: UInt16
    , thisClass       :: PoolIndex
    , superClass      :: PoolIndex
    , interfacesCount :: UInt16
    , interfaces      :: [PoolIndex]
    , fieldsCount     :: UInt16
    , fields          :: [FieldInfo]
    , methodsCount    :: UInt16
    , methods         :: [MethodInfo]   
    , attributesCount :: UInt16
    , attributes      :: [AttributeInfo]
    }

uint8ToBytes :: UInt8 -> [Char]
uint8ToBytes i = [chr (i .&. 0xFF)] --Todo: fix

uint16ToBytes :: UInt16 -> [Char]
uint16ToBytes i = 
    uint8ToBytes (shiftL i 8) ++ 
    uint8ToBytes i

uint32ToBytes :: UInt32 -> [Char]
uint32ToBytes i =
    uint8ToBytes (shiftL i 24) ++ 
    uint8ToBytes (shiftL i 16) ++ 
    uint8ToBytes (shiftL i 8) ++ 
    uint8ToBytes i

tableToBytes :: (a -> [Char]) -> [a] -> [Char]
tableToBytes f l = concat (map f l)

poolConstantToBytes :: PoolConstant -> [Char]
poolConstantToBytes (StringConstant str)  = uint8ToBytes 3 ++ uint16ToBytes (length str) ++ str
poolConstantToBytes (ClassRef si)         = uint8ToBytes 7 ++ uint16ToBytes si
poolConstantToBytes (StringRef si)        = uint8ToBytes 8 ++ uint16ToBytes si
poolConstantToBytes (FieldRef c ntd)      = uint8ToBytes 9 ++ uint16ToBytes c ++ uint16ToBytes ntd
poolConstantToBytes (MethodRef c ntd)     = uint8ToBytes 10 ++ uint16ToBytes c ++ uint16ToBytes ntd
poolConstantToBytes (NameAndType nsi tdi) = uint8ToBytes 12 ++ uint16ToBytes nsi ++ uint16ToBytes tdi

fieldToBytes :: FieldInfo -> [Char]
fieldToBytes f = 
    uint16ToBytes (fieldAccessFlags f) ++
    uint16ToBytes (fieldName f) ++
    uint16ToBytes (fieldDescriptor f) ++
    uint16ToBytes (fieldAttributesCount f) ++
    tableToBytes attributeToBytes (fieldAttributes f)

methodToBytes :: MethodInfo -> [Char]
methodToBytes m = 
    uint16ToBytes (methodAccessFlags m) ++
    uint16ToBytes (methodName m) ++
    uint16ToBytes (methodDescriptor m) ++
    uint16ToBytes (methodAttributesCount m) ++
    tableToBytes attributeToBytes (methodAttributes m)

--TODO take care of codeattribute
attributeToBytes :: AttributeInfo -> [Char]
attributeToBytes a =
    uint16ToBytes (attributeName a) ++
    uint32ToBytes (attributeLength a) ++
    tableToBytes uint8ToBytes (attributeInfo a)

getClassBytes :: ClassFile -> [Char]
getClassBytes cl = 
    tableToBytes uint8ToBytes (magicNumber cl) ++
    uint16ToBytes (versionMinor cl) ++
    uint16ToBytes (versionMajor cl) ++
    uint16ToBytes (cpCount cl) ++
    tableToBytes poolConstantToBytes (constantPool cl) ++
    uint16ToBytes (accessFlags cl) ++
    uint16ToBytes (thisClass cl) ++
    uint16ToBytes (superClass cl) ++
    uint16ToBytes (interfacesCount cl) ++
    tableToBytes uint16ToBytes (interfaces cl) ++
    uint16ToBytes (fieldsCount cl) ++
    tableToBytes fieldToBytes (fields cl) ++
    uint16ToBytes (methodsCount cl) ++
    tableToBytes methodToBytes (methods cl) ++
    uint16ToBytes (attributesCount cl) ++
    tableToBytes attributeToBytes (attributes cl)

data JVMOpcode = JVM_aaload | JVM_aastore

-- per https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings
getIns :: JVMOpcode -> Int
getIns op = case op of
    JVM_aaload  -> 0x32
    JVM_aastore -> 0x53