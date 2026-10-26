; ModuleID = 'catln'

declare void @exit(i32)

define i32 @main() {
entry:
  ; TODO unsupported: an expression no arm of llvmOperand compiles
  %r0 = call i64 @"/Data/Context/unwrap"(i32 undef)
  %r1 = sub i64 %r0, 1
  %r2 = trunc i64 %r1 to i32
  call void @exit(i32 %r2)
  ret i32 0
}

define i32 @"/Data/Context/unwrap"(i32 %this) {
entry:
  ret i32 undef
}
