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
    {- 1 -}  MethodRef 6 13,
    {- 2 -}  FieldRef 14 15,
    {- 3 -}  StringRef 16,
    {- 4 -}  MethodRef 17 18,
    {- 5 -}  ClassRef 19,
    {- 6 -}  ClassRef 20,
    {- 7 -}  StringConstant "<init>",
    {- 8 -}  StringConstant "()V",
    {- 9 -}  StringConstant "Code",
    {- 10 -} StringConstant "main",
    {- 11 -} StringConstant "([Ljava/lang/String;)V",
    {- 12 -} StringConstant "Main.java",
    {- 13 -} NameAndType 7 8,
    {- 14 -} ClassRef 21,
    {- 15 -} NameAndType 22 23,
    {- 16 -} StringConstant "Hello, World",
    {- 17 -} ClassRef 24,
    {- 18 -} NameAndType 25 26,
    {- 19 -} StringConstant "Main",
    {- 20 -} StringConstant "java/lang/Object",
    {- 21 -} StringConstant "java/lang/System",
    {- 22 -} StringConstant "out",
    {- 23 -} StringConstant "Ljava/io/PrintStream;",
    {- 24 -} StringConstant "java/io/PrintStream",
    {- 25 -} StringConstant "println",
    {- 26 -} StringConstant "(Ljava/lang/String;)V"
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