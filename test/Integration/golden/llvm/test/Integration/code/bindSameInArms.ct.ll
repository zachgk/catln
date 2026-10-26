; ModuleID = 'catln'

declare void @exit(i32)

define i32 @main() {
entry:
  ; TODO unsupported: an expression no arm of llvmOperand compiles
  %r0 = call i64 @"/Data/Context/unwrap"(i32 undef)
  ; TODO unsupported: an expression no arm of llvmOperand compiles
  ; TODO unsupported: an operator of operands of different types
  ; TODO unsupported: an operator of operands of different types
  call void @exit(i32 undef)
  ret i32 0
}

define i32 @"/Data/Context/unwrap"(i32 %this) {
entry:
  ret i32 undef
}
