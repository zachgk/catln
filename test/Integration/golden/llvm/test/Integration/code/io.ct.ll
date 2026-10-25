; ModuleID = 'catln'

declare void @exit(i32)
@.str0 = private unnamed_addr constant [12 x i8] c"hello world\00"
declare i32 @puts(ptr)

define i32 @main() {
entry:
  call i32 @puts(ptr @.str0)
  %r0 = trunc i64 0 to i32
  call void @exit(i32 %r0)
  ret i32 0
}
