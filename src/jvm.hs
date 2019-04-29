module JVM
    ( PoolConstant(..)
    , FieldInfo(..)
    , MethodInfo(..)
    , ExceptionInfo(..)
    , AttributeInfo(..)
    , ClassFile(..)
    , getClassBytes
    , JVMInstruction(..)
    , getCodeBytes
    , UInt8
    , UInt16
    , UInt32
    ) where

import Data.Bits
import Data.Word
import Data.ByteString hiding (map, concat)

-- types
type UInt8 = Word8
type UInt16 = Word16
type UInt32 = Word32
type PoolIndex = UInt16

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1
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

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.4
data PoolConstant 
    = StringConstant String
    | IntegerConstant UInt32
    | ClassRef PoolIndex
    | StringRef PoolIndex
    | FieldRef PoolIndex PoolIndex
    | MethodRef PoolIndex PoolIndex
    | NameAndType PoolIndex PoolIndex

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.5
data FieldInfo = FieldInfo
    { fieldAccessFlags     :: UInt16
    , fieldName            :: PoolIndex
    , fieldDescriptor      :: PoolIndex
    , fieldAttributes      :: [AttributeInfo]
    }

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.6
data MethodInfo = MethodInfo
    { methodAccessFlags     :: UInt16
    , methodName            :: PoolIndex
    , methodDescriptor      :: PoolIndex
    , methodAttributes      :: [AttributeInfo]
    }

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.3
data ExceptionInfo = ExceptionInfo
    { exceptionStartPc   :: UInt16
    , exceptionEndPc     :: UInt16
    , exceptionHandlerPc :: UInt16
    , exceptionCatchType :: UInt16
    }

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7
-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.3
data AttributeInfo 
    = AttributeInfo
        { attributeName   :: PoolIndex
        , attributeInfo   :: [UInt8]
        }
    | CodeAttributeInfo
        { attributeName       :: PoolIndex
        , codeMaxStack        :: UInt16
        , codeMaxLocals       :: UInt16
        , code                :: [UInt8]
        , codeExceptions      :: [ExceptionInfo]
        , codeAttributes      :: [AttributeInfo]
        }

getByte b num = fromIntegral $ (shiftR num (8 * b)) .&. 0xFF

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
poolConstantToBytes (IntegerConstant i)   = uint8ToBytes 3 ++ uint32ToBytes i
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

attributeToBytes :: AttributeInfo -> [UInt8]
attributeToBytes a@(AttributeInfo{}) =
    uint16ToBytes (attributeName a) ++
    uint32ToBytes (getUint32Len (attributeInfo a)) ++
    tableToBytes uint8ToBytes (attributeInfo a)

attributeToBytes a@(CodeAttributeInfo{}) =
    uint16ToBytes (attributeName a) ++
    uint32ToBytes (fromIntegral (
        12 +
        Prelude.length (code a) +
        Prelude.length (codeExceptions a) +
        Prelude.length (codeAttributes a)
    )) ++
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

data JVMInstruction 
    = JVMldc UInt8            --0x12
    | JVMldc_w UInt16         --0x12
    | JVMiload UInt8          --0x15
    | JVMaload_0              --0x2a
    | JVMistore UInt8         --0x36
    | JVMgoto UInt16          --0xa7
    | JVMreturn               --0xb1
    | JVMgetstatic UInt16     --0xb2
    | JVMinvokevirtual UInt16 --0xb6
    | JVMinvokespecial UInt16 --0xb7

-- per https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings
getInstructionBytes :: JVMInstruction -> [UInt8]
getInstructionBytes ins = case ins of
    (JVMldc i)           -> 0x12 : uint8ToBytes i
    (JVMldc_w i)         -> 0x13 : uint16ToBytes i
    (JVMiload i)         -> 0x15 : uint8ToBytes i
    (JVMistore i)        -> 0x36 : uint8ToBytes i
    (JVMaload_0)         -> 0x2a : []
    (JVMgoto b)          -> 0xa7 : uint16ToBytes b
    (JVMreturn)          -> 0xb1 : []
    (JVMgetstatic i)     -> 0xb2 : uint16ToBytes i
    (JVMinvokevirtual i) -> 0xb6 : uint16ToBytes i
    (JVMinvokespecial i) -> 0xb7 : uint16ToBytes i

getCodeBytes :: [JVMInstruction] -> [UInt8]
getCodeBytes ins = concat (map getInstructionBytes ins)

