; ModuleID = 'LOL'
source_filename = "LOL"

declare void @println(i8*)

declare void @print(i8*)

declare i32 @int_of_float(float)

declare float @float_of_int(i32)

declare i8* @str_of_int(i32)

declare i32 @int_of_str(i8*)

declare i8* @str_of_bool(i1)

declare i8* @str_of_float(float)

declare i8* @string_concat(i8*, i8*)

declare i32 @string_equals(i8*, i8*)

define i32 @main() {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %env = bitcast i8* %malloccall to i8**
  store i8* null, i8** %env
  %env_p = load i8*, i8** %env
  %malloccall1 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32))
  %main = bitcast i8* %malloccall1 to { i32 ()*, i8* }*
  %tmp__ = insertvalue { i32 ()*, i8* } { i32 ()* @main, i8* null }, i8* %env_p, 1
  store { i32 ()*, i8* } %tmp__, { i32 ()*, i8* }* %main
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %a = bitcast i8* %malloccall2 to i32*
  store i32 1, i32* %a
  %malloccall3 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %b = bitcast i8* %malloccall3 to i32*
  store i32 2, i32* %b
  %malloccall4 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %sum = bitcast i8* %malloccall4 to i32*
  %a5 = load i32, i32* %a
  %b6 = load i32, i32* %b
  %tmp = add i32 %a5, %b6
  store i32 %tmp, i32* %sum
  %sum7 = load i32, i32* %sum
  %_result = call i8* @str_of_int(i32 %sum7)
  call void @println(i8* %_result)
  ret i32 0
}

declare noalias i8* @malloc(i32)
