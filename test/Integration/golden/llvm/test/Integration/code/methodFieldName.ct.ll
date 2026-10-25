; ModuleID = 'catln'

declare void @exit(i32)

define i32 @main() {
entry:
  ; TODO unsupported: an expression no arm of llvmOperand compiles
  %r0 = trunc i64 0 to i32
  call void @exit(i32 %r0)
  ret i32 0
}
