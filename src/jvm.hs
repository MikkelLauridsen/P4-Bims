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
import Data.Int
import Data.ByteString hiding (map, concat)

-- types
type UInt8 = Word8
type UInt16 = Word16
type UInt32 = Word32
type PoolIndex = UInt16

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.1
data ClassFile = ClassFile
    [UInt8]         -- magicNumber
    UInt16          -- versionMinor
    UInt16          -- versionMajor
    [PoolConstant]  -- constantPool
    UInt16          -- accessFlags
    PoolIndex       --thisClass
    PoolIndex       --superClass
    [PoolIndex]     --interfaces
    [FieldInfo]     --fields
    [MethodInfo]    --methods
    [AttributeInfo] --attributes

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
    UInt16          -- access flags
    PoolIndex       -- name
    PoolIndex       -- descriptor
    [AttributeInfo] -- attributes

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.6
data MethodInfo = MethodInfo
    UInt16          -- access flags
    PoolIndex       -- name
    PoolIndex       -- descriptor
    [AttributeInfo] -- attributes

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.3
data ExceptionInfo = ExceptionInfo
    UInt16 -- start pc
    UInt16 -- end pc
    UInt16 -- handler pc
    UInt16 -- catch type

-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7
-- https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.3
data AttributeInfo 
    = AttributeInfo
        PoolIndex        -- name
        [UInt8]          -- info
    | CodeAttributeInfo
        PoolIndex        -- name
        UInt16           -- maxStack
        UInt16           -- maxLocals
        [JVMInstruction] -- code
        [ExceptionInfo]  -- exceptions
        [AttributeInfo]  -- attributes

getByte b num = fromIntegral $ (shiftR num (8 * b)) .&. 0xFF

getBytes c num = 
    if c == 0
    then []
    else (getByte (c - 1) num : getBytes (c - 1) num)

getUint16Len :: [a] -> UInt16
getUint16Len a = fromIntegral (Prelude.length a)

getUint32Len :: [a] -> UInt32
getUint32Len a = fromIntegral (Prelude.length a)

stringToBytes :: String -> [UInt8]
stringToBytes str = map (toEnum . fromEnum) str 

uint8ToBytes :: UInt8 -> [UInt8]
uint8ToBytes = getBytes 1

uint16ToBytes :: UInt16 -> [UInt8]
uint16ToBytes = getBytes 2

uint32ToBytes :: UInt32 -> [UInt8]
uint32ToBytes = getBytes 4

int16ToBytes :: Int16 -> [UInt8]
int16ToBytes i = uint16ToBytes (fromIntegral i)

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
fieldToBytes (FieldInfo accessFlags name descriptor attributes) = 
    uint16ToBytes accessFlags ++
    uint16ToBytes name ++
    uint16ToBytes descriptor ++
    uint16ToBytes (getUint16Len attributes) ++
    tableToBytes attributeToBytes attributes 

methodToBytes :: MethodInfo -> [UInt8]
methodToBytes (MethodInfo accessFlags name descriptor attributes) = 
    uint16ToBytes accessFlags ++
    uint16ToBytes name ++
    uint16ToBytes descriptor ++
    uint16ToBytes (getUint16Len attributes) ++
    tableToBytes attributeToBytes attributes

attributeToBytes :: AttributeInfo -> [UInt8]
attributeToBytes (AttributeInfo name info) =
    uint16ToBytes name ++
    uint32ToBytes (getUint32Len info) ++
    tableToBytes uint8ToBytes info

attributeToBytes (CodeAttributeInfo name maxStack maxLocals code exceptions attributes) =
    uint16ToBytes name ++
    uint32ToBytes (fromIntegral (
        12 +
        Prelude.length codeBytes +
        Prelude.length exceptions +
        Prelude.length attributes
    )) ++
    uint16ToBytes maxStack ++
    uint16ToBytes maxLocals ++
    uint32ToBytes (getUint32Len codeBytes) ++
    codeBytes ++
    uint16ToBytes (getUint16Len exceptions) ++
    tableToBytes exceptionToBytes exceptions ++
    uint16ToBytes (getUint16Len attributes) ++
    tableToBytes attributeToBytes attributes
    where
        codeBytes = tableToBytes getInstructionBytes code

exceptionToBytes :: ExceptionInfo -> [UInt8]
exceptionToBytes (ExceptionInfo startPc endPc handlerPc catchType) =
    uint16ToBytes startPc ++
    uint16ToBytes endPc ++
    uint16ToBytes handlerPc ++
    uint16ToBytes catchType

getClassBytes :: ClassFile -> ByteString
getClassBytes 
    (ClassFile magicNumber versionMinor versionMajor constantPool accessFlags 
      thisClass superClass interfaces fields methods attributes) = 
    pack (
        tableToBytes uint8ToBytes magicNumber ++
        uint16ToBytes versionMinor ++
        uint16ToBytes versionMajor ++
        uint16ToBytes ((getUint16Len constantPool) + 1) ++
        tableToBytes poolConstantToBytes constantPool ++
        uint16ToBytes accessFlags ++
        uint16ToBytes thisClass ++
        uint16ToBytes superClass ++
        uint16ToBytes (getUint16Len interfaces) ++
        tableToBytes uint16ToBytes interfaces ++
        uint16ToBytes (getUint16Len fields) ++
        tableToBytes fieldToBytes fields ++
        uint16ToBytes (getUint16Len methods) ++
        tableToBytes methodToBytes methods ++
        uint16ToBytes (getUint16Len attributes) ++
        tableToBytes attributeToBytes attributes
    )

data JVMInstruction 
    = JVMiconst_0             -- Push integer 0 onto stack
    | JVMiconst_1             -- Push integer 1 onto stack
    | JVMldc UInt8            -- Push constant from cp onto stack
    | JVMldc_w UInt16         -- Push constant from cp onto stack (wide index)
    | JVMiload UInt8          -- Push integer from local variable onto stack
    | JVMaload_0              -- Push reference from cp onto stack
    | JVMistore UInt8         -- Store top of stack to local variable
    | JVMiadd                 -- Add two numbers on stack
    | JVMisub                 -- Subtract two numbers on stack
    | JVMimul                 -- Multiply two numbers on stack
    | JVMif_icmpeq Int16      -- Jump if equal
    | JVMif_icmpne Int16      -- Jump if not equal
    | JVMif_icmplt Int16      -- Jump if less than
    | JVMif_icmpgt Int16      -- Jump if greater than
    | JVMgoto Int16           -- Jump to
    | JVMreturn               -- Return from method
    | JVMgetstatic UInt16     -- Push static field onto stack
    | JVMinvokevirtual UInt16 -- Invoke virtual method
    | JVMinvokespecial UInt16 -- Invoke special method (used for constructor)

-- per https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings
getInstructionBytes :: JVMInstruction -> [UInt8]
getInstructionBytes ins = case ins of
    JVMiconst_0        -> 0x03 : []
    JVMiconst_1        -> 0x04 : []
    JVMldc i           -> 0x12 : uint8ToBytes i
    JVMldc_w i         -> 0x13 : uint16ToBytes i
    JVMiload i         -> 0x15 : uint8ToBytes i
    JVMaload_0         -> 0x2a : []
    JVMistore i        -> 0x36 : uint8ToBytes i
    JVMiadd            -> 0x60 : []
    JVMisub            -> 0x64 : []
    JVMimul            -> 0x68 : []
    JVMif_icmpeq b     -> 0x9f : int16ToBytes b
    JVMif_icmpne b     -> 0xa0 : int16ToBytes b
    JVMif_icmplt b     -> 0xa1 : int16ToBytes b
    JVMif_icmpgt b     -> 0xa3 : int16ToBytes b
    JVMgoto b          -> 0xa7 : int16ToBytes b
    JVMreturn          -> 0xb1 : []
    JVMgetstatic i     -> 0xb2 : uint16ToBytes i
    JVMinvokevirtual i -> 0xb6 : uint16ToBytes i
    JVMinvokespecial i -> 0xb7 : uint16ToBytes i

getCodeBytes :: [JVMInstruction] -> [UInt8]
getCodeBytes ins = tableToBytes getInstructionBytes ins