module Main where

import Ast
import Semantics
import JVM
import Data.ByteString

program = AssignmentNode "result" (AddExprNode (NumExprNode "42") (NumExprNode "42"))

jvmCode = [
    178,0,2,
    18,3,
    182,0,4,
    177
    ]

cl_magicNumber = [0xCA, 0xFE, 0xBA, 0xBE]
cl_versionMinor = 0
cl_versionMajor = 52
cl_constantPool = [
    MethodRef 6 15,
    FieldRef 16 17,
    StringRef 18,
    MethodRef 19 20,
    ClassRef 21,
    ClassRef 22,
    StringConstant "<init>",
    StringConstant "()V",
    StringConstant "Code",
    StringConstant "LineNumberTable", --Can get rid of?
    StringConstant "main",
    StringConstant "([Ljava/lang/String;)V",
    StringConstant "SourceFile",
    StringConstant "Main.java",
    NameAndType 7 8,
    ClassRef 23,
    NameAndType 24 25,
    StringConstant "Hello, World",
    ClassRef 26,
    NameAndType 27 28,
    StringConstant "Main",
    StringConstant "java/lang/Object",
    StringConstant "java/lang/System",
    StringConstant "out",
    StringConstant "Ljava/io/PrintStream;",
    StringConstant "java/io/PrintStream",
    StringConstant "println",
    StringConstant "(Ljava/lang/String;)V"
    ]
cl_accessFlags = 0x0021
cl_thisClass = 5
cl_superClass = 6
cl_interfaces = []
cl_fields = []
cl_methods = [
    (MethodInfo 0x0001 7 8 [(AttributeInfo 9 17 [0,1,0,1,0,0,0,5,42,183,0,1,177,0,0,0,0])]),
    (MethodInfo 0x0009 11 12 [(CodeAttributeInfo 9 21 2 1 jvmCode [] [])])
    ]
cl_attributes = []

cl :: ClassFile
cl = (
    ClassFile 
    cl_magicNumber
    cl_versionMinor
    cl_versionMajor
    cl_constantPool
    cl_accessFlags 
    cl_thisClass
    cl_superClass 
    cl_interfaces 
    cl_fields 
    cl_methods 
    cl_attributes
    )

bytes = getClassBytes cl

main :: IO ()
main = Data.ByteString.writeFile "Main.class" bytes