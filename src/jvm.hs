module JVM
    ( JVMOpcode(..)
    , getIns
    ) where

data JVMOpcode = JVM_aaload | JVM_aastore

-- per https://en.wikipedia.org/wiki/Java_bytecode_instruction_listings
getIns :: JVMOpcode -> Int
getIns op = case op of
    JVM_aaload  -> 0x32
    JVM_aastore -> 0x53