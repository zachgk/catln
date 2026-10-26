; ModuleID = 'catln'

declare void @exit(i32)

define i32 @main() {
entry:
  %r0 = call i64 @"/sumOf"(i64 1, i64 2)
  %r1 = sub i64 %r0, 3
  %r2 = trunc i64 %r1 to i32
  call void @exit(i32 %r2)
  ret i32 0
}

define i32 @"/sumOf"(i64 %a, i64 %d) {
entry:
  ; TODO unsupported: an expression no arm of llvmOperand compiles
  ret i32 undef
}
