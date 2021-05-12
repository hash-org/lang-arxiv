-- | Hash compiler program scope implementation.
-- |
-- | All rights reserved 2021 (c) The Hash Language authors
-- |
module Runtime.Scope where

import Control.Lens (over, views)
import qualified Data.HashMap.Strict as HM
import Panic
import Runtime.Boot

-- | Add an entry to the current scope in the given scope group.
addSymbol :: Name -> CopyValue -> ScopeGroup -> ScopeGroup
addSymbol k v = over (currScope . symbolMap) (HM.insert k v)

-- | Lookup the given name in the given scope group.
--
-- Traverses all scopes in the current scope group.
lookupSymbol :: Name -> ScopeGroup -> CopyValue
lookupSymbol name s = case takeScopeWhile findSymbol s of
  Just v -> v
  _ -> internalPanicPure $ "Could not find symbol: " ++ show name
  where
    findSymbol :: Scope -> Maybe CopyValue
    findSymbol (Scope tab) = HM.lookup name tab

-- | Modify the value of the given name in the given scope group.
--
-- Traverses all scopes in the current scope group.
modifySymbol :: Name -> (CopyValue -> CopyValue) -> ScopeGroup -> ScopeGroup
modifySymbol name f =
  modifyScopeIf
    ( \scope ->
        views symbolMap (HM.lookup name) scope >> Just modifySymbolInScope
        -- views symbolMap ((>> return modifySymbolInScope) . HM.lookup name)
    )
  where
    modifySymbolInScope :: Scope -> Scope
    modifySymbolInScope = over symbolMap $ HM.adjust f name

-- | Pop the last scope off the given scope group, and discard it.
popScope :: ScopeGroup -> ScopeGroup
popScope (ScopeGroup _ []) = internalPanicPure "Attempting to evict empty scope"
popScope (ScopeGroup _ (x : xs)) = ScopeGroup x xs

-- | Add the given scope to the given scope group.
addScope :: Scope -> ScopeGroup -> ScopeGroup
addScope f (ScopeGroup prev xs) = ScopeGroup f (prev : xs)

-- | Add an empty scope to the given scope group.
newScope :: ScopeGroup -> ScopeGroup
newScope (ScopeGroup prev xs) = ScopeGroup (Scope {_symbolMap = HM.empty}) (prev : xs)

-- | Traverse the stack upwards checking if some given condition
-- | passes, if so applying the given transformation to the scope.
--
-- Return the modified ScopeGroup.
modifyScopeIf :: (Scope -> Maybe (Scope -> Scope)) -> ScopeGroup -> ScopeGroup
modifyScopeIf f (ScopeGroup a []) = case f a of
  Nothing -> ScopeGroup a []
  Just g -> ScopeGroup (g a) []
modifyScopeIf f (ScopeGroup a (x : xs)) = case f a of
  Nothing -> let (ScopeGroup m ms) = modifyScopeIf f (ScopeGroup x xs) in ScopeGroup a (m : ms)
  Just g -> ScopeGroup (g a) (x : xs)

-- | Traverse the stack upwards checking if some given condition
-- | passes.
-- |
-- | If the top of the stack is reached, return `Nothing`.  If the condition is
-- | satisfied, use the provided function to fetch the given value. This is
-- | wrapped in a `Maybe` since we might not find a scope that matches.
takeScopeWhile :: (Scope -> Maybe a) -> ScopeGroup -> Maybe a
takeScopeWhile f (ScopeGroup a []) = f a
takeScopeWhile f (ScopeGroup a (x : xs)) = case f a of
  Just v -> Just v
  Nothing -> takeScopeWhile f (ScopeGroup x xs)
