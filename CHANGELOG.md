0.5
-----------------------------------------------------------------------------
- Update dependencies constraints.

0.4
-----------------------------------------------------------------------------
- Use `singletons` library instead of `GHC.TypeLits` from base.
- Remove `mapResult` (use `fmap` instead).

0.3.2
-----------------------------------------------------------------------------
- Update dependencies constraints.

0.3.1
-----------------------------------------------------------------------------
- Add `fromVault` predicate and `HasVault` type-class.

0.3
-----------------------------------------------------------------------------
- Change `header`, `hasHeader` and `lookupHeader` to use `HeaderName` instead
  of `ByteString` for the lookup key.

0.2.1
-----------------------------------------------------------------------------
- Update WAI version bounds to include 2.1

0.2
-----------------------------------------------------------------------------
- Rename `getRequest` predicate to `request` and `request` method of
  `HasRequest` to `getRequest`.
- Add `MonadTrans` and `MonadIO` instance declarations for `ResultT` and
  some helper functions.

0.1
-----------------------------------------------------------------------------
- Initial release.
