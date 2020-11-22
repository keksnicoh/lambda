# Changelog for lambda

- [22.11.2020]
    - introduced subsitution expressions of form `a[b/a]`
- [19.11.2020]
    - status to avoid multple parallel kernel executions
    - introduced `HasStdOut`
- [18.11.2020]
    - integrated conduit to stream output from statement evaluation
- [17.11.2020]
    - bootstrap prototype framework for notebook service using Servant
    - project files restructurization
- [15.11.2020]
    - add `-Wall` flag and fixes all messages
    - refine parsing of lists such that the item type can be specified.
      this change was required due to ambigious resolution of
      ad-hoc polymorphism in case of empty lists
    - identifiers are now parsed with a leading '#'
- [14.11.2020] initial commit

## Unreleased changes
