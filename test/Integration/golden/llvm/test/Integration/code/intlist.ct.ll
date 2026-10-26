; ModuleID = 'catln'

declare void @exit(i32)

define i32 @main() {
entry:
  ; TODO unsupported: a call of a function that has no one definition
  ; TODO unsupported: an operator of operands of different types
  call void @exit(i32 undef)
  ret i32 0
}
