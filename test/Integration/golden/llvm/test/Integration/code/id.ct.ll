; ModuleID = 'catln'

declare void @exit(i32)

define i32 @main() {
entry:
  %r0 = call i64 @"/id"(i64 0)
  %r1 = trunc i64 %r0 to i32
  call void @exit(i32 %r1)
  ret i32 0
}

define i64 @"/id"(i64 %x) {
entry:
  ret i64 %x
}
