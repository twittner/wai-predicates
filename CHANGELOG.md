0.3
---
- Change `header`, `hasHeader` and `lookupHeader` to use `HeaderName` instead
  of `ByteString` for the lookup key.

0.2.1
-----
- Update WAI version bounds to include 2.1

0.2
---
- Rename `getRequest` predicate to `request` and `request` method of
  `HasRequest` to `getRequest`.
- Add `MonadTrans` and `MonadIO` instance declarations for `ResultT` and
  some helper functions.

0.1
---
- Initial release.
