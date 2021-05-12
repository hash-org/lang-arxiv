-- | Hash runtime primitives. Primitive constructs that can be used at
-- | runtime to interface with a running program and native intrinsic
-- | function calls.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Runtime.Primitives where

import qualified Data.Vector as V
import Runtime.Boot
import Runtime.Heap (newHeapVal)

-- | A function to create a 'Boolean' enum within the runtime
boolV :: Bool -> CopyValue
boolV True = boolTrueV
boolV False = boolFalseV

-- | A value representing the boolean "false" type.
--
-- This is always the "first" enum variant.
boolFalseV :: CopyValue
boolFalseV = EnumV (EnumVariant 0) Nothing

-- | A value representing the boolean "true" type.
--
-- This is always the "second" enum variant.
boolTrueV :: CopyValue
boolTrueV = EnumV (EnumVariant 1) Nothing

-- | A value representing the Option.Some variant.
--
-- This is always the "third" enum variant.
optionSomeV :: CopyValue -> Execution CopyValue
optionSomeV val = do
  argRef <- newHeapVal (EnumArgs (V.singleton val))
  return $ EnumV (EnumVariant 2) (Just argRef)

-- | A value representing the Option.None variant.
--
-- This is always the "fourth" enum variant.
optionNoneV :: CopyValue
optionNoneV = EnumV (EnumVariant 3) Nothing

-- | A value representing the Result.Ok variant.
--
-- This is always the "fifth" enum variant.
resultOkV :: CopyValue -> Execution CopyValue
resultOkV val = do
  argRef <- newHeapVal (EnumArgs (V.singleton val))
  return $ EnumV (EnumVariant 4) (Just argRef)

-- | A value representing the Result.Err enum variant
--
-- This is always the "sixth" enum variant.
resultErrV :: CopyValue -> Execution CopyValue
resultErrV val = do
  argRef <- newHeapVal (EnumArgs (V.singleton val))
  return $ EnumV (EnumVariant 5) (Just argRef)

-- | Make a tuple that resembles the primitive return type of an IO Error
-- | intrinsic. It first take an Integer (Type of IOError) and the message
-- | that is in the form of a string.
mkIoErrReturnTup :: CopyValue -> CopyValue -> Instruction
mkIoErrReturnTup typ msg = ITuple $ V.fromList [ILiteral typ, ILiteral msg]
