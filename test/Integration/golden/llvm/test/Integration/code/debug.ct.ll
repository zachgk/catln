; ModuleID = 'catln'

declare void @exit(i32)

define i32 @main() {
entry:
  ; TODO unsupported: an expression no arm of llvmOperand compiles
  %r0 = call i64 @"/inc"(i32 undef)
  %r1 = trunc i64 %r0 to i32
  call void @exit(i32 %r1)
  ret i32 0
}

define i32 @"/inc"(i32 %x) {
entry:
  ; TODO unsupported: an operator of operands of different types
  ret i32 undef
}
