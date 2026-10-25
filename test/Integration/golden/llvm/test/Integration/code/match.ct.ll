; ModuleID = 'catln'

declare void @exit(i32)

define i32 @main() {
entry:
  ; TODO unsupported: an expression no arm of llvmOperand compiles
  call void @exit(i32 undef)
  ret i32 0
}
