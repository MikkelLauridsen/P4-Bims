module Main where

import Ast
import Semantics
import JVM
import Data.ByteString

program = AssignmentNode "result" (AddExprNode (NumExprNode "42") (NumExprNode "42"))

jvmInit = [
    JVMaload_0,
    JVMinvokespecial 1,
    JVMreturn
    ]

jvmMain = [
    JVMgetstatic 2,
    JVMldc 3,
    JVMinvokevirtual 4,
    JVMreturn
    ]

cl_constantPool = [
    {- 1 -}  MethodRef 6 14,
    {- 2 -}  FieldRef 15 16,
    {- 3 -}  StringRef 17,
    {- 4 -}  MethodRef 18 19,
    {- 5 -}  ClassRef 20,
    {- 6 -}  ClassRef 21,
    {- 7 -}  StringConstant "<init>",
    {- 8 -}  StringConstant "()V",
    {- 9 -}  StringConstant "Code",
    {- 10 -} StringConstant "main",
    {- 11 -} StringConstant "([Ljava/lang/String;)V",
    {- 12 -} StringConstant "SourceFile",
    {- 13 -} StringConstant "Main.java",
    {- 14 -} NameAndType 7 8,
    {- 15 -} ClassRef 22,
    {- 16 -} NameAndType 23 24,
    {- 17 -} StringConstant "Hello, World",
    {- 18 -} ClassRef 25,
    {- 19 -} NameAndType 26 27,
    {- 20 -} StringConstant "Main",
    {- 21 -} StringConstant "java/lang/Object",
    {- 22 -} StringConstant "java/lang/System",
    {- 23 -} StringConstant "out",
    {- 24 -} StringConstant "Ljava/io/PrintStream;",
    {- 25 -} StringConstant "java/io/PrintStream",
    {- 26 -} StringConstant "println",
    {- 27 -} StringConstant "(Ljava/lang/String;)V"
    ]

    cl_methods = [
    (MethodInfo 0x0001 7 8 [(CodeAttributeInfo 9 1 1 (getCodeBytes jvmInit) [] [])]),  -- <init> method
    (MethodInfo 0x0009 10 11 [(CodeAttributeInfo 9 2 1 (getCodeBytes jvmMain) [] [])]) -- main method
    ]

cl :: ClassFile
cl = (
    ClassFile 
    [0xCA, 0xFE, 0xBA, 0xBE] -- magic number
    0                        -- version minor
    52                       -- version major
    cl_constantPool
    0x0021                   -- access flags 
    5                        -- this class
    6                        -- super class 
    []                       -- interfaces 
    []                       -- fields
    cl_methods 
    []                       --attributes
    )

bytes = getClassBytes cl

main :: IO ()
main = Data.ByteString.writeFile "Main.class" bytes