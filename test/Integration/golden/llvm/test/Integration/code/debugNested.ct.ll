; ModuleID = 'catln'

declare void @exit(i32)

define i32 @main() {
entry:
  %r0 = call i64 @"/total"(i64 1, i64 2)
  %r1 = sub i64 %r0, 63
  %r2 = trunc i64 %r1 to i32
  call void @exit(i32 %r2)
  ret i32 0
}

define i32 @"/total"(i64 %a, i64 %b) {
entry:
  ; TODO unsupported: an expression no arm of llvmOperand compiles
  %r0 = call i64 @"/scale"(i32 undef)
  ; TODO unsupported: an expression no arm of llvmOperand compiles
  %r1 = call i64 @"/total.sum"(i32 undef, i64 %b)
  ; TODO unsupported: an expression no arm of llvmOperand compiles
  ; TODO unsupported: an operator of operands of different types
  ; TODO unsupported: an operator of operands of different types
  ret i32 undef
}

define i32 @"/total.sum"(i32 %a, i64 %b) {
entry:
  ; TODO unsupported: an expression no arm of llvmOperand compiles
  ; TODO unsupported: an operator of operands of different types
  ret i32 undef
}

define i32 @"/scale"(i32 %v) {
entry:
  ; TODO unsupported: an operator of operands of different types
  ret i32 undef
}
