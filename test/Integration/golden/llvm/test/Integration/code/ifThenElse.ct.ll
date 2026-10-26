; ModuleID = 'catln'

declare void @exit(i32)

define i32 @main() {
entry:
  %r0 = call i64 @"/abs"(i64 0)
  %r1 = trunc i64 %r0 to i32
  call void @exit(i32 %r1)
  ret i32 0
}

define i32 @"/abs"(i64 %x) {
entry:
  ; TODO unsupported: an expression no arm of llvmOperand compiles
  ret i32 undef
}
