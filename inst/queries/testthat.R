list(
  kind = "testthat",
  parts = list(list(
    callName = "expect(_.*)?",
    subkind = "expect"
  ), list(
    callName = "fail",
    subkind = "fail"
  ), list(
    callName = "succeed",
    subkind = "succeed"
  ))
)
